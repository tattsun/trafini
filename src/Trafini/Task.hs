{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Trafini.Task where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.TH
import           Data.List
import qualified Data.Text           as T
import           System.Random

----------------------------------------------------------------------
-- *** types

type TaskID = T.Text
type Priority = Integer
type Tag = T.Text
type Summary = T.Text
type Detail = T.Text
data Task = Task { taskId       :: TaskID
                 , taskPriority :: Priority
                 , taskTags     :: [Tag]
                 , taskSummary  :: Summary
                 , taskDetail   :: Detail
                 , taskFinished :: Bool
                 } deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''Task)
defaultTask :: Task
defaultTask = Task { taskId = ""
                   , taskPriority = 0
                   , taskTags = []
                   , taskSummary = ""
                   , taskDetail = ""
                   , taskFinished = False
                   }

newtype Tasklist = Tasklist { tasklist :: [Task] }
                   deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''Tasklist)
emptyList = Tasklist []

----------------------------------------------------------------------
-- *** useful funcs

contains :: (Eq a) => [a] -> [a] -> Bool
contains l r = and $ map (\x -> x `elem` l) r

alnum :: [Char]
alnum = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
int2alnum x= (!!) alnum $ (mod (abs x) 63)
randomStr len = do
   gen <- newStdGen
   let ns = randoms gen :: [Int]
   return $ map int2alnum (take len ns)

----------------------------------------------------------------------
-- *** operation tasks

newTask :: Tasklist -> IO (TaskID, Tasklist)
newTask tl = do
  id <- T.pack <$> randomStr 8
  let newtl = update tl (defaultTask {taskId = id})
  return (id, newtl)

findByIdOne :: Tasklist -> TaskID -> Maybe Task
findByIdOne tl id = case findById tl id of
                     (t:[]) -> Just t
                     otherwise -> Nothing

findById :: Tasklist -> TaskID -> [Task]
findById tl id = filter (\t -> id `T.isPrefixOf` taskId t) (tasklist tl)

findByTag :: Tasklist -> [Tag] -> [Task]
findByTag tl tags = filter (\t -> taskTags t `contains` tags) (tasklist tl)

update :: Tasklist -> Task -> Tasklist
update tl tnew = tl { tasklist = tnew:(filter (\t -> taskId t /= taskId tnew) (tasklist tl))
                    }

set :: Tasklist -> TaskID -> (Task -> Task) -> Maybe Tasklist
set tl id f = maybe Nothing set' (findByIdOne tl id)
  where
    set' t = Just $ update tl (f t)

setPriority :: Tasklist -> TaskID -> Priority -> Maybe Tasklist
setPriority tl id pr = set tl id (\t -> t { taskPriority = pr })

setTags :: Tasklist -> TaskID -> [Tag] -> Maybe Tasklist
setTags tl id tg = set tl id (\t -> t { taskTags = tg })

setSummary :: Tasklist -> TaskID -> Summary -> Maybe Tasklist
setSummary tl id sm = set tl id (\t -> t {taskSummary = sm})

setDetail :: Tasklist -> TaskID -> Detail -> Maybe Tasklist
setDetail tl id dt = set tl id (\t -> t {taskDetail = dt})

setFinish :: Tasklist -> TaskID -> Bool -> Maybe Tasklist
setFinish tl id b = set tl id (\t -> t {taskFinished = b})

----------------------------------------------------------------------
-- *** for debug
sample = Tasklist [defaultTask {taskId = "unko", taskTags = ["tattsun", "sgo"]}
                  ,defaultTask {taskId = "hoge", taskTags = ["tattsun"]}
                  ,defaultTask {taskId = "uppi", taskTags = ["sgo"]}]
