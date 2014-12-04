{-# LANGUAGE OverloadedStrings #-}
module Trafini.Command where

import           Control.Applicative
import           Data.Aeson
import           Data.List
import qualified Data.Text           as T

--
import qualified Trafini.Persistent  as Ps
import qualified Trafini.Task        as Task


data Res = NoRes
         | ID Task.TaskID
         | Tasks [Task.Task]
         | Task Task.Task

sortTask :: [Task.Task] -> [Task.Task]
sortTask = sortBy (\l r -> Task.taskPriority l `compare` Task.taskPriority r)

withoutFinished :: [Task.Task] -> [Task.Task]
withoutFinished = filter (\t -> not $ Task.taskFinished t)

toTags :: T.Text -> [Task.Tag]
toTags = T.splitOn ","

exec :: [T.Text] -> Ps.Persistent Task.Tasklist -> IO (Either String Res)
exec args ps = if null args
               then return (Left "args can't be null.")
               else go
  where
    cmd = head args
    opt = tail args
    success = return . Right
    fail = return . Left
    go = do
      tl <- Ps.get ps
      case cmd of
       "show" -> success $ Tasks (withoutFinished $ sortTask $ Task.findByTag tl (head $ map toTags opt))

       "detail" -> detail tl (Task.findByIdOne tl (head opt)) (head opt)
       "d" -> detail tl (Task.findByIdOne tl (head opt)) (head opt)

       "set" -> set tl (head opt) (tail opt)
       "s" -> set tl (head opt) (tail opt)

       "add" -> do (id, newtl) <- Task.newTask tl
                   set newtl id opt
                   success $ ID id
       "a" -> do (id, newtl) <- Task.newTask tl
                 set newtl id opt
                 success $ ID id

       "finish" -> maybe (candidates tl (head opt)) (\newtl -> Ps.update ps newtl >> success NoRes)
                   (Task.setFinish tl (head opt) True)

       "unfinish" -> maybe (candidates tl (head opt)) (\newtl -> Ps.update ps newtl >> success NoRes)
                     (Task.setFinish tl (head opt) False)

--       "delete" -> Task

       otherwise -> fail $ "unknown command"

    set tl id args = let newtl = execFuncs tl (parseSet id args)
                     in case newtl of
                         Just newtl' -> Ps.update ps newtl' >> success NoRes
                         otherwise -> candidates tl id

    execFuncs :: Task.Tasklist -> [Task.Tasklist -> Maybe Task.Tasklist] -> Maybe Task.Tasklist
    execFuncs tl funcs = foldr (\f acc -> case acc of
                                           Just tl' -> f tl'
                                           otherwise -> Nothing) (Just tl) funcs
    parseSet :: T.Text -> [T.Text] -> [Task.Tasklist -> Maybe Task.Tasklist]
    parseSet id xs = map (\(cmd:val:[]) -> toSetFunc id cmd val) . map (T.splitOn "=") $ xs
    toSetFunc id cmd val
      | cmd == "p" || cmd == "priority" = \l -> Task.setPriority l id (read $ T.unpack val)
      | cmd == "t" || cmd == "tags" = \l -> Task.setTags l id (T.splitOn "," val)
      | cmd == "s" || cmd == "summary" = \l -> Task.setSummary l id val
      | cmd == "d" || cmd == "detail" = \l -> Task.setDetail l id val

    detail tl (Just t) _ = success $ Task t
    detail tl Nothing id = candidates tl id

    candidates tl id = let ts = Task.findById tl id
                       in if null ts
                          then fail $ "unknown id"
                          else fail $ "there is two or more candidates: "
                               ++ (concat $ intersperse ", " $ map (T.unpack . Task.taskId) $ ts)
