{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- 解析一行日志信息
-- word 可以分词；read 将字符串转化为数字；unwords 把字符串数组拼接起来
parseMessage :: String -> LogMessage
parseMessage str =
  let wordList = words str
   in case wordList of
        ("I" : ts : msg) -> LogMessage Info (read ts) (unwords msg)
        ("W" : ts : msg) -> LogMessage Warning (read ts) (unwords msg)
        ("E" : lvl : ts : msg) -> LogMessage (Error (read lvl)) (read ts) (unwords msg)
        _ -> Unknown (unwords wordList)

-- 解析一整个日志文件
parse :: String -> [LogMessage]
parse str = map parseMessage (lines str)

-- 将 LogMessage 插入二叉搜索树
insert :: LogMessage -> MessageTree -> MessageTree
insert lmsg Leaf = Node Leaf lmsg Leaf
insert lmsg@(LogMessage _ ts1 _) (Node lchild lmsg2@(LogMessage _ ts2 _) rchild)
  | ts1 > ts2 = Node lchild lmsg2 (insert lmsg rchild)
  | otherwise = Node (insert lmsg lchild) lmsg2 rchild
insert _ tree = tree

-- 构造二叉搜索树
-- 等价于 (lmsg1 `insert` (lmsg2 `insert` (lmsg3 `insert` Leaf)))
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- 中序遍历二叉搜索树
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lchild lmsg rchild) = inOrder lchild ++ [lmsg] ++ inOrder rchild

-- 提取出错误等价大于 50 的错误信息
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lmsgList = [msg | (LogMessage (Error lvl) _ msg) <- inOrder (build lmsgList), lvl >= 50]
