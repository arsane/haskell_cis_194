{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1
--

-- TODO: deal with parse exception?
--
parseMessage :: String -> LogMessage
parseMessage str = case words str of
    ("I":t:s)   -> LogMessage Info    (read t) (unwords s)
    ("W":t:s)   -> LogMessage Warning (read t) (unwords s)
    ("E":i:t:s) -> LogMessage (Error (read i)) (read t) (unwords s)
    _           -> Unknown str


parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

-- Exercise 2
--
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) node = node
insert m Leaf           = Node Leaf m Leaf
insert m@(LogMessage _ t _) (Node l m'@(LogMessage _ t' _) r) 
    | t <= t' = Node (insert m l) m' r 
    | t >  t' = Node l m' (insert m r)
insert _ n = n -- to disable warning

-- Exercise 3
--
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf 

-- Exercise 4
--

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

-- fmap ((take 10) . inOrder . build) $ testParse parse 100 "error.log"

-- Exercise 5
--
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map (\(LogMessage _ _ s) -> s)) . inOrder . build . filter whatWentWrongFilter 
    where whatWentWrongFilter (LogMessage (Error e) _ _)  = e >= 50
          whatWentWrongFilter _ = False

-- fmap whatWentWrong $ testParse parse 10000 "error.log"
