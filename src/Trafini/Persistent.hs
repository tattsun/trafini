{-# LANGUAGE OverloadedStrings #-}
module Trafini.Persistent where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL

data Persistent a = Persistent { persistState :: MVar a
                               }

mkPersist :: (ToJSON a, FromJSON a) => a -> IO (Persistent a)
mkPersist def = do
  mvar <- newMVar def
  return $ Persistent { persistState = mvar
                      }

get :: (ToJSON a, FromJSON a) => Persistent a -> IO a
get ps = readMVar (persistState ps)

update :: (ToJSON a, FromJSON a) => Persistent a -> a -> IO a
update ps x = swapMVar (persistState ps) x

save :: (ToJSON a, FromJSON a) => FilePath -> Persistent a -> IO ()
save file ps = do
  content <- get ps
  BL.writeFile file (encode content)

load :: (ToJSON a, FromJSON a) => FilePath -> IO (Maybe (Persistent a))
load file = (do
  content <- BL.readFile file
  let obj = decode content
  maybe (return Nothing) (\o -> Just <$> mkPersist o) obj
  ) `catch` (\(SomeException e) -> return Nothing)

----------------------------------------------------------------------
-- *** for debug
test = do
  p <- mkPersist "foobar"
  get p >>= putStrLn
  update p "hogehoge"
  get p >>= putStrLn
