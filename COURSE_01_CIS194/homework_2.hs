{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Parse an individual line of the log
-- eg
--  parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help" 
--  parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
--  parseMessage "This is not in the right format" == Unknown "This is not in the right format"

parseMessage :: String -> LogMessage
parseMessage message =
  let parts = words message in
    case parts of 
      "I" : ts : content         -> format Info ts content
      "W" : ts : content         -> format Warning ts content
      "E" : level : ts : content -> format (Error (read level)) ts content
      _                          -> Unknown message
  where
    format form ts content = LogMessage form (read ts) (unwords content)

-- Parse a whole log file

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Create tree of messages

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t    = t
insert m@(LogMessage _ mts _) (Node l n@(LogMessage _ nts _) r)  
  | mts < nts = (Node (insert m l) n r)
  | mts > nts = (Node l n (insert m r))
  | otherwise = (Node l n r)
insert m _ = Node Leaf m Leaf

build :: [LogMessage] -> MessageTree
build []     = Leaf
build (x:xs) = insert x (build xs) 

-- Order messages

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l v r) = (inOrder l) ++ [v] ++ (inOrder r)

-- Figure out what went wrong

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map extract) . (filter relevant) . inOrder . build 
  where relevant (LogMessage (Error n) _ _) | 50 <= n = True
        relevant _ = False
        extract (LogMessage _ _ c) = c
        extract (Unknown m)        = m
  


