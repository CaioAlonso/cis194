{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import           Log

-- I 4992 MSG
parseMessage :: String -> LogMessage
parseMessage = parseMessage' . words

parseMessage' :: [String] -> LogMessage
parseMessage' ("E" : mType : time : str)
    | isNumeric mType && isNumeric time  = LogMessage (Error $ read mType) (read time) (unwords str)
parseMessage' ("I" : time : str)
    | isNumeric time                     = LogMessage Info (read time) (unwords str)
parseMessage' ("W" : time : str)
    | isNumeric time                     = LogMessage Warning (read time) (unwords str)
parseMessage' _                          = Unknown "This is not the right format"

isNumeric:: String -> Bool
isNumeric s = case reads s :: [(Int, String)] of
                  [(_, "")] -> True
                  _         -> False

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

insert :: LogMessage -> MessageTree -> MessageTree
insert m@(LogMessage _ time _) (Node left l@(LogMessage _ lTime _) right)
    | time < lTime = Node (insert m left) l right
    | time >= lTime = Node left l (insert m right)
insert m Leaf = Node Leaf m Leaf
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map printMessage (filter greaterThan50 (inOrder $ build msgs))

greaterThan50 :: LogMessage -> Bool
greaterThan50 (LogMessage (Error severity) _ _)
    | severity >= 50 = True
greaterThan50 _ = False

printMessage :: LogMessage -> String
printMessage (LogMessage _ _ msg) = show msg
printMessage _ = []
