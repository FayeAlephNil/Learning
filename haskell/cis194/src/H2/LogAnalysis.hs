{-# OPTIONS_GHC -Wall #-}
module H2.LogAnalysis where

import H2.Log

parseMessage :: String -> LogMessage
parseMessage a = msg
  where
    list = words a
    (msgType, newList) = case head list of
                           "E" -> (Error (read $ list !! 1), tail $ tail list)
                           "W" -> (Warning, tail list)
                           "I" -> (Info, tail list)
                           _ -> (Error 101, [[]])
    msg = case msgType of
            (Error 101) -> Unknown a
            _ -> LogMessage msgType (read $ head newList) (concat $ tail newList)

parse :: String -> [LogMessage]
parse a = map parseMessage (lines a)

insert :: LogMessage -> MessageTree -> MessageTree
insert l Leaf = Node Leaf l Leaf
insert l@(LogMessage _ t1 _) msgTree@(Node left m@(LogMessage _ t2 _) right)
  | t1 > t2 = Node left m (insert l right)
  | t1 < t2 = Node (insert l left) m right
  | t1 == t2 = msgTree
insert _ msgTree = msgTree

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf l right) = l : inOrder right
inOrder (Node left l right) = inOrder left ++ [l] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ls = map snd $ filter err50 $ map toLogMsg $ filter isError sorted
  where
    sorted = inOrder $ build ls
    isError (LogMessage (Error _) _ _)= True
    isError _ = False
    toLogMsg (LogMessage msg _ s) = (msg, s)
    toLogMsg (Unknown s) = (Error 101, s)
    err50 (Error s, _) = s >= 50
    err50 _ = False
