{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module JoinList where

-- Size 就是整数的集合，是一个结合运算为 (+) 的幺半群，幺元是 0

import Buffer
import Scrabble
import Sized

-- 树状的数据结构，将 join 操作变成数据结构
-- 其中，m 是其 tag，或者注释；a 是节点中具体的值
data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

------------------------------------------------------------
-- Exercise 1
------------------------------------------------------------
-- 从 JoinList 中获得其 tag
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single t _) = t
tag (Append t _ _) = t

-- 将两个 JoinList 组合起来，m 来自两个字节点的 append 操作
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty jl = jl
(+++) jl Empty = jl
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

------------------------------------------------------------
-- Exercise 2
------------------------------------------------------------
-- 获得 JoinList 中的第 i 个元素，i 从 0 开始
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ | i < 0 = Nothing
indexJ i jl | i >= getSize (size $ tag jl) = Nothing
indexJ _ Empty = Nothing -- 这一句没有必要
indexJ _ (Single _ x) = Just x
indexJ i (Append _ jl1 jl2)
  | jl1_size > i = indexJ i jl1
  | otherwise = indexJ (i - jl1_size) jl2
  where
    jl1_size = getSize (size $ tag jl1)

-- 丢弃 JoinList 中的前 n 个元素
-- n <= 0 时，不丢弃；n >= length，全部丢弃
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0 = jl
dropJ n jl | n >= getSize (size $ tag jl) = Empty
dropJ _ Empty = Empty -- 这一句是没有必要的
dropJ _ (Single _ _) = Empty -- 这一句也没必要
dropJ n (Append _ jl1 jl2) = jl1' +++ jl2'
  where
    jl1' = dropJ n jl1
    jl2' = dropJ (n - jl1_size) jl2
    jl1_size = getSize (size $ tag jl1)

-- 从 JoinList 中取出前 n 个元素
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0 = Empty
takeJ n jl | n >= getSize (size $ tag jl) = jl
takeJ _ Empty = Empty -- 这一句是没有必要的
takeJ _ jl@(Single _ _) = jl -- 这一句是没有必要的
takeJ n (Append _ jl1 jl2) = jl1' +++ jl2'
  where
    jl1' = takeJ n jl1
    jl2' = takeJ (n - jl1_size) jl2
    jl1_size = getSize (size $ tag jl1)

-- 计算字符串的得分
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

------------------------------------------------------------
-- Exercise 4
------------------------------------------------------------
-- 将 JoinList a b 变为列表
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ jl1 jl2) = jlToList jl1 ++ jlToList jl2

-- 让 JoinList (Score, Size) String 成为 Buffer 的实例
instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList

  fromString = foldl (\jl s -> jl +++ jlOneLine s) Empty . lines
    where
      jlOneLine str = Single (scoreString str, Size 1) str

  line = indexJ

  replaceLine n str jl = takeJ n jl +++ fromString str +++ dropJ (n + 1) jl

  numLines = getSize . snd . tag

  value = getScore . fst . tag