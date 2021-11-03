{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module Scrabble where

import Data.Char

-- 定义 Score 这个类型，表示分数
newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score s) = s

-- append 操作位于 Semigroup 中
instance Semigroup Score where
  (<>) = (+)

-- 让 Score 成为 Monoid 的实例
instance Monoid Score where
  mempty = Score 0

-- 使用 Scrabble 游戏的规则，为每个字符计分
score :: Char -> Score
score c
  | c' `elem` "aeilnorstu" = Score 1
  | c' `elem` "dg" = Score 2
  | c' `elem` "bcmp" = Score 3
  | c' `elem` "fhvwy" = Score 4
  | c' `elem` "k" = Score 5
  | c' `elem` "jx" = Score 8
  | c' `elem` "qz" = Score 10
  | otherwise = Score 0
  where
    c' = toLower c

-- 使用 Scrabble 游戏的规则，为字符串计分
scoreString :: String -> Score
scoreString = foldr (mappend . score) (Score 0)
