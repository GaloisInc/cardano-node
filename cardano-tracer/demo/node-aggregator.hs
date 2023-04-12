{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.Functor.Contravariant
import           Data.List (foldr,map,maximum,minimum,sortBy)
import qualified Data.Map.Strict                      as Map
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
-- import qualified Data.ByteString.Lazy.Char8           as LBS
import qualified Data.ByteString                      as B
import qualified Data.ByteString.UTF8                 as B
import qualified Data.Text                            as T
import           Data.Aeson

-- package time:
import Data.Time.Clock

-- package formatting:
import Formatting


-- local:
import Cardano.Tracer.Test.Acceptor ()
import Cardano.Node.Tracing.Peers

-- TODO: use Cardano.Prelude

main :: IO ()
main =
  do
  as <- getArgs
  case as of
    ["nodepeers"] -> cmd_nodepeers
    ["chainsync"] -> cmd_chainsync
    _             -> usageFail ""

usageFail :: String -> IO a
usageFail s = do
  unless (null s) $
    do
    putStr "Error:\n  "
    putStrLn s
  putStrLn "Usage: node-aggregator (nodepeers|chainsync)"
  exitFailure

      
---- chainsync command ---------------------------------------------

cmd_chainsync = go
  where
  go = do
    s <- B.getLine
    x <- decodeStrict s `orFail` ("fail decoding json: " <> s)
    print (x :: Value)

  -- TODO: fromJson directly into good/right type.
{-

the Value the above goes into is:

extract (Array [String t, Array [Object 

Array [String "2023-03-19T08:16:17.135514091Z"
      ,Array [Object (fromList
               [("localAddress",Object (fromList [("address",String "192.168.61.186"),("port",String "40679")]))
               ,("remoteAddress",Object (fromList [("address",String "18.133.86.86"),("port",String "3001")]))
               ])
             ,Object (fromList
                [("block",String "55cc5fa979668c3a30722c03886082c59d2847bdd915d91f603fa3be17f90d4e"),("blockNo",Object (fromList [("unBlockNo",Number 8536417.0)])),("kind",String "DownloadedHeader"),("slot",Number 8.7647486e7)
                ]
                )
             ]
      ]
-}


---- nodepeers command ---------------------------------------------

-- TODO: support a sampling-node identifier at start of each.
-- TODO: update the datapoint so that a fromJSON will do!

cmd_nodepeers = go Map.empty
  where
  go map' = 
    do
    s <- B.getLine
    dpString <- readDataPoint "NodePeers" s
                `orFail` ("fail: readDataPoint: " <> s)
    NodePeers nps <- decodeStrict dpString
                     `orFail` ("fail decoding json: " <> dpString)
    entries <- mapM readPeerInfoPP nps
               `orFail` "fail readPeerInfoPP"
    let map'' = Data.List.foldr (uncurry Map.insert) map' entries
    render map''
    go map''

render :: Map.Map T.Text Int -> IO ()
render kas =
  do
  now' <- getCurrentTime -- :: UTCTime
  putStrLn (esc_clear ++ esc_toTL)
  print now'
  putStrLn ""
  let ps = sortBy
             (getComparison $ contramap snd defaultComparison)
             (Map.toList kas)
  forM_ ps $ \(h,slot) ->
             fprint (left 20 ' ' % "  " % commas % "\n") h slot
  putStrLn "\n"

  let slots = Data.List.map snd ps
      diffs = Data.List.maximum slots - Data.List.minimum slots
      alert = diffs > 20
      alertS :: String
      alertS = if alert then
                "SYNC STATUS: WARNING: (" ++ show diffs ++ ")  "
               else
                "SYNC STATUS: Good.                    " 
  putStrLn alertS

  where
  esc_clear = "\ESCc"     -- clear screen
  esc_toTL  = "\ESC[1;1H" -- to TopLeft
    

-- copied from Cardano.Node.Tracing.Peers:
type PeerInfoPP = T.Text -- The result of 'ppPeer' function


type HostName = T.Text
type SlotNo   = Int

readPeerInfoPP :: PeerInfoPP -> Maybe (HostName,SlotNo)
readPeerInfoPP s = case T.words s of
                     [ip,_,sn,_,_,_] -> Just ( ip
                                             , read (T.unpack sn) :: Int
                                             )
                     _               -> Nothing
  
readDataPoint :: B.ByteString
              -> B.ByteString -> Maybe B.ByteString
readDataPoint dpName cs = B.stripPrefix prefix cs
  where
  prefix = "DataPoint, name: " <> dpName <> ", raw value: "

{-
INPUT FORMAT:
DataPoint, name: NodePeers, raw value:

["18.130.53.86    ready    82091798    0      0       0"
,"18.224.78.212   ready    82091795    0      0       0"
]
-}

---- lib ---------------------------------------------------------------------

-- orFail - transform Maybe into Monad
orFail :: MonadFail m => Maybe a -> B.ByteString -> m a
m `orFail` s = case m of
                 Just x  -> return x
                 Nothing -> fail (B.toString s)

  -- Using "orFail", you can replace code on the left w/ that on the right:
  --
  -- case e of                       do
  --   Nothing -> fail s             x <- e `orFail` s
  --   Just x  -> E[x]               E[x]
