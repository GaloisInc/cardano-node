{-# LANGUAGE LambdaCase #-}

import Control.Arrow (first)
import Control.Monad (unless,when)
import Data.Fixed (Pico)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Cardano.Tracer.Test.Acceptor

main' :: IO ()
main' = do
  as <- getArgs
  localSock:ir:as' <- if length as >= 3 then
                        return as
                      else
                        usageFail "not enough arguments"
  ir' <- case ir of 
           "Initiator" -> return Initiator
           "Responder" -> return Responder
           _           -> usageFail ""

  dps' <- processDPs as'
  
  launchAcceptorsSimple ir' localSock dps'


-- FIXME: time to clone from demo-acceptor.

main :: IO ()
main = do
  as <- getArgs
  (ekgFreq,as') <- case as of
                     "-e":ekg:as' -> return (read ekg :: Pico, as')
                     _            -> return (10/1            , as )
                     
  (localSockets,as'') <- parseLocalSockets as'
  when (null localSockets) $
    usageFail "no sockets specified"
  dps' <- processDPs as''
  launchInitiators ekgFreq localSockets dps'


parseLocalSockets :: [String] -> IO ([String],[String])
parseLocalSockets ("-s":s:xs) = first (s:) <$> parseLocalSockets xs
parseLocalSockets ["-s"]      = usageFail "-s without path"
parseLocalSockets xs          = return ([],xs)


-- | create groups of DPs for multiple periods
processDPs :: [String] -> IO [(Double, [String])]
processDPs as' =
  do
  dps <- go (addPeriod dfltPeriod []) as'
  return
    $ reverse
    $ filter (\(_,ns)-> not(null ns)) dps
  where
  dfltPeriod = 1.0 :: Double
  
  go dps = \case
    []        -> return dps
    ["-p"]    -> usageFail "-p without 'period' argument"
    "-p":a:as -> go (addPeriod (read a :: Double) dps) as   -- FIXME
    a:as      -> go (addDP a dps)                      as

  addPeriod p xs      = (p,[]):xs
  
  addDP d ((p,ds):xs) = (p,ds++[d]):xs
  addDP _ []          = error "impossible"
  
  -- FIXME: input sanitization
  --  - no datapoints
  --  - `read a`
  --  - warn if a DP in multiple sets ?
  
usageFail :: String -> IO a
usageFail s = do
  unless (null s) $
    do
    putStr "Error:\n  "
    putStrLn s
  putStrLn "Usage: demo-acceptor [-e ekgfreq] [-s /path/to/local/sock]+ [Name.Of.DataPoint | -p seconds]+ "
  exitFailure

