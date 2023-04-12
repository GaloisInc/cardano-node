{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Test.Acceptor
  ( AcceptorsMode (..)
  , launchAcceptorsSimple
  , launchInitiators
  ) where

import           Control.Concurrent.STM.TVar (newTVarIO, readTVarIO)
import           Control.Concurrent.Async.Extra (sequenceConcurrently)
import           Control.Concurrent.Extra (newLock)
import           Control.Monad (forever, forM_, unless, void)
import qualified Data.ByteString.Lazy as LBS
import           Data.Fixed (Pico)
import           Data.IORef
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime(..))
import           Data.Time.Calendar (Day(..))
import           System.IO
import           System.Time.Extra (sleep,Seconds)

import           Data.Aeson (Value(..),decode,encode)
                  
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Acceptors.Run
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.Run
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils
import           Trace.Forward.Utils.DataPoint

data AcceptorsMode = Initiator | Responder

launchAcceptorsSimple
  :: AcceptorsMode
  -> FilePath
  -> [(Seconds,[DPName])] -- for each period, a groups of datapoints to query
  -> IO ()
launchAcceptorsSimple mode localSock dpGroups = do
  -- print dpGroups -- FIXME: debug
  protocolsBrake <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  connectedNodes <- initConnectedNodes
  connectedNodesNames <- initConnectedNodesNames
  acceptedMetrics <- initAcceptedMetrics
  savedTO <- initSavedTraceObjects
  currentLogLock <- newLock
  currentDPLock <- newLock
  eventsQueues <- initEventsQueues Nothing connectedNodesNames dpRequestors currentDPLock

  chainHistory <- initBlockchainHistory
  resourcesHistory <- initResourcesHistory
  txHistory <- initTransactionsHistory

  rtViewPageOpened <- newTVarIO False

  tr <- mkTracerTracer $ SeverityF $ Just Warning

  let tracerEnv =
        TracerEnv
          { teConfig              = mkConfig
          , teConnectedNodes      = connectedNodes
          , teConnectedNodesNames = connectedNodesNames
          , teAcceptedMetrics     = acceptedMetrics
          , teSavedTO             = savedTO
          , teBlockchainHistory   = chainHistory
          , teResourcesHistory    = resourcesHistory
          , teTxHistory           = txHistory
          , teCurrentLogLock      = currentLogLock
          , teCurrentDPLock       = currentDPLock
          , teEventsQueues        = eventsQueues
          , teDPRequestors        = dpRequestors
          , teProtocolsBrake      = protocolsBrake
          , teRTViewPageOpened    = rtViewPageOpened
          , teRTViewStateDir      = Nothing
          , teTracer              = tr
          }

  void . sequenceConcurrently $
      runAcceptors tracerEnv
    : [ runDataPointsPrinter dpNames period dpRequestors 1
      | (period,dpNames) <- dpGroups
      ]
 where
  mkConfig = TracerConfig
    { networkMagic   = 764824073
    , network        = case mode of
                         Initiator -> ConnectTo $ NE.fromList [LocalSocket localSock]
                         Responder -> AcceptAt (LocalSocket localSock)
    , loRequestNum   = Just 1
    , ekgRequestFreq = Just 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , hasRTView      = Nothing
    , logging        = NE.fromList [LoggingParams "/tmp/demo-acceptor" FileMode ForHuman]
    , rotation       = Nothing
    , verbosity      = Just Minimum
    , metricsComp    = Nothing
    }


launchInitiators
  :: Pico
  -> [FilePath]
  -> [(Seconds,[DPName])] -- for each period, a groups of datapoints to query
  -> IO ()
launchInitiators ekgFreq localSocks dpGroups = do
  protocolsBrake <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  connectedNodes <- initConnectedNodes
  acceptedMetrics <- initAcceptedMetrics
  savedTO <- initSavedTraceObjects
  currentLogLock <- newLock
  void . sequenceConcurrently $
      runAcceptors mkConfig connectedNodes acceptedMetrics savedTO
                   dpRequestors protocolsBrake currentLogLock
    : [ runDataPointsPrinter dpNames period dpRequestors (length localSocks)
      | (period,dpNames) <- dpGroups
      ]
 where
  mkConfig = TracerConfig
    { networkMagic   = 764824073
    , network        = ConnectTo $ NE.fromList $ map LocalSocket localSocks
      -- NOTE: Nothing in the following two implies "default" values
    , loRequestNum   = Just 1
                       -- just one at a time, and what happens if we
                       -- don't request enough?
    , ekgRequestFreq = Just ekgFreq
                     -- N.B.: this is the period (secs), not frequency (/sec)
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , hasRTView      = Nothing
    , logging        = NE.fromList [LoggingParams "/tmp/demo-acceptor" FileMode ForHuman]
    , rotation       = Nothing
    , verbosity      = Just Minimum
      -- TODO: So, we can log minimum 'here', even if the "trace server"
      -- is logging at a different verbosity?
    }

    -- TODO: figure out how to turn off logging.
    
---- new Datapoint abstractions ------------------------------------

type DPName = String
              -- hmmm,
              --   - String because we used getArgs
              --   - but elsewhere: type DataPointName   = Text
              
type Period = Seconds

data DPType = DPT_Digest Int Period
            | DPT_Value (Maybe Period)  -- ^ may not have a known period.


-- the output of getDPs:

data OutVal = OV_List [Value]  -- for digests, other?
            | OV_Value Value   -- std Datapoints
            deriving (Eq,Ord,Read,Show)

data DPOut = DPO { dp       :: DPName
                 , recvTime :: UTCTime
                 , out      :: OutVal
                 }
             deriving (Eq,Ord,Read,Show)

dpTypes :: M.Map DataPointName DPType
dpTypes = M.fromList
            [ ("Static.DatapointList" , DPT_Value Nothing )
              -- Deprecated: remove soon!
            , ("NodeInfo"             , DPT_Value Nothing )
            , ("NodeState"            , DPT_Value Nothing )   
            , ("NodePeers"            , DPT_Value Nothing )   
            , ("New.BlockFetch.Client", DPT_Digest 30 50 )
            , ("New.ChainSync.Client" , DPT_Digest 30 50 )
            ]

  -- statically known for each version.

---- handle datapoints ---------------------------------------------

-- | To be able to ask any 'DataPoint' by the name without knowing the
--   actual type, we print it out as a raw 'ByteString'.
--
-- However, each ByteString *should*
--  1. be parseable as JSON
--  2. succeed for parseJSON (in FromJSON class) for the type
--     that was originally encoded into the datapoint.

runDataPointsPrinter
  :: [DPName]
  -> Seconds
  -> DataPointRequestors
  -> Int
  -> IO ()
runDataPointsPrinter dpNames wait dpRequestors lenRequestors =
  do
  digestLastTimeMaps <- do
    let zeroDay = UTCTime (ModifiedJulianDay 0) 0 
               -- lowest value, in case clocks not syncd
        digestDpMap = M.filter
                        (\case {DPT_Digest{} -> True; _ -> False})
                        dpTypes
    sequence $ replicate lenRequestors
             $ mapM (\_->newIORef zeroDay) digestDpMap

  forever $ do
    dpReqs <- M.toList <$> readTVarIO dpRequestors
      -- length should expect to be either 0 or numRequestors!
    putStrLn $ ":DEBUG: dpReqs: " ++ show(length dpReqs)
    hFlush stdout 
    forM_ (zip dpReqs [0..]) $ \((nid, dpReq),reqIndex) -> do
      dpValues <- askForDataPoints dpReq (map T.pack dpNames)
      forM_ dpValues $ \(dpName, mValue) ->
        case mValue of
          Nothing -> do
            putStrLn $ ":WARN: no datapoint returned ("
                       <> T.unpack dpName <> ")"
            hFlush stdout
          Just value -> do
            case M.lookup dpName dpTypes of
              Just (DPT_Digest m p) -> do
                      putStr $ ":INFO: DataPoint Digest:"
                            <> " node: " <> show nid
                            <> " name: " <> T.unpack dpName <> " "
                      handleDP_Digest
                        (digestLastTimeMaps !! reqIndex M.! dpName) m p value
              Just (DPT_Value  mp ) -> handleDP_Value  mp  dpName value
              Nothing               -> putStrLn $ ":ERROR: unknown datapoint"

    sleep wait
      -- FIXME[F3]: the "period" is not exactly 'wait'
  
    -- FIXME[F3]: The first time through:
    --   - this will sleep for 'wait' secs
    --   - as dpRequestors contains Map.empty in this case.

  
handleDP_Digest ::
  IORef UTCTime -> Int -> Period -> DataPointValue -> IO ()
handleDP_Digest rLasttime _ _ v = do
  case decode v :: Maybe [(UTCTime,Value)] of
    Nothing   -> putStrLn "ERROR: <CANNOT PARSE>"
    Just logs -> do
                 lasttime <- readIORef rLasttime
                 let logs' = takeWhile (\(t,_)-> t > lasttime) logs
                     llogs' = length logs'
                 putStrLn $ show llogs'
                            <> " messages after time "
                            <> show lasttime  -- FIXME
                            <> ":"
                 putStrLn $ ":INFO: from "
                            <> show (length logs)
                            <> " digest messages"
                 hFlush stdout
                 unless (null logs') $
                   do
                   forM_ (reverse logs') $
                     \(t,v')-> do
                               LBS.putStr (encode (t,v'))
                               putChar '\n'
                               hFlush stdout
                   writeIORef rLasttime (fst $ head logs')
                 -- FIXME[F3]: TODO:
                 --  We have a lot of information to determine if
                 --  we have lost any events ...
                 
  putChar '\n'
  hFlush stdout

handleDP_Value  :: Maybe Period -> DataPointName -> DataPointValue -> IO ()
handleDP_Value _ dpName v = do
  putStr $ "DataPoint, name: " <> T.unpack dpName <> ", raw value: "
  LBS.putStr v
  putChar '\n'
  hFlush stdout
  
