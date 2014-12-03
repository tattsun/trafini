{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Data.Aeson                           hiding (json)
import qualified Data.ByteString.Char8                as B
import qualified Data.ByteString.Lazy.Char8           as BL
import           Data.Maybe
import qualified Data.Text                            as T
import           Network.Wai.Middleware.Cors
import qualified Network.Wai.Middleware.RequestLogger as Logger
import           Web.Scotty

--
import           Trafini.Command
import qualified Trafini.Persistent                   as Ps
import qualified Trafini.Task                         as Task


----------------------------------------------------------------------
-- *** useful funcs
toLazy :: B.ByteString -> BL.ByteString
toLazy = BL.fromChunks . (:[])

----------------------------------------------------------------------
-- *** main
main :: IO ()
main = undefined

type Port = Int
type ServerState = Ps.Persistent Task.Tasklist

run :: Port -> FilePath -> IO ()
run port file = do
  ps <- Ps.load file :: IO (Maybe (Ps.Persistent Task.Tasklist))
  start ps
  where
    start (Just st) = do
      forkIO $ saveRoutine file st
      server port st
    start Nothing = do
      st <- Ps.mkPersist Task.emptyList
      forkIO $ saveRoutine file st
      server port st

saveRoutine :: FilePath -> ServerState -> IO ()
saveRoutine file st = do
  Ps.save file st
  threadDelay (5 * 60 * 1000000) -- save per 5 min
  saveRoutine file st

server :: Port -> ServerState -> IO ()
server port ps = scotty port $ do
  middleware simpleCors
  middleware Logger.logStdout
  post "/" $ do
    q <- toLazy <$> param "q"
    let args = decode q :: (Maybe [T.Text])
    res <- liftIO $ exec (fromJust args) ps
    case res of
     Left err -> json err
     Right sth -> case sth of
       ID id -> json id
       Tasks xs -> json xs
       Task x -> json x
       NoRes -> json ("nores" :: String)

----------------------------------------------------------------------
-- *** for debug
debug = run 3005 "./test.json"
