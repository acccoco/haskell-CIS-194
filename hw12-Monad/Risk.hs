{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import GHC.Integer

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  -- random :: RandomGen g => g -> (a, g)
  random = first DV . randomR (1, 6)

  -- randomR :: RandomGen g => (a, a) -> g -> (a, g)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

-- StdGen 的 kind 是 *，是一个类型，是 RandomGen 这个 class 的实例
-- Rand g = RandT g Identity
--   Identity 的 kind 是 * -> *
--   上述的 g 是类型参数，因此 Rand 的 kind 是 * -> * -> *
--   Rand 需要接受两个类型参数，最终得到一个类型
--   RandT 的 kind 为 * -> (* -> *) -> * -> *
die :: Rand StdGen DieValue
-- getRandom :: Random a => m a
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield {attackers :: Army, defenders :: Army}

------------------------------------------------------------
-- Exercise 2
------------------------------------------------------------
-- 一次生成多个随机数，并且降序排序
rollDie :: Int -> Rand StdGen [DieValue]
rollDie n
  | n <= 0 = return []
  | otherwise =
    die >>= \d1 ->
      rollDie (n -1) >>= \ds ->
        return . reverse . sort $ (d1 : ds)

-- 生成进攻方的点数
attackRoll :: Army -> Rand StdGen [DieValue]
attackRoll n
  | n > 3 = rollDie 2
  | otherwise = rollDie (n - 1)

-- 生成防守方的点数
defendRoll :: Army -> Rand StdGen [DieValue]
defendRoll n
  | n > 2 = rollDie 2
  | otherwise = rollDie n

-- 比较两方的点数，进行结算。返回（攻方损失，守方损失）
settle :: [DieValue] -> [DieValue] -> (Int, Int)
settle att def = (length . filter (< DV 0) $ res, length . filter (> DV 0) $ res)
  where
    res = zipWith (\a b -> (if a > b then 1 else - 1)) att def

-- 模拟一次对局
battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield att def) =
  res >>= \r ->
    return (Battlefield (att - fst r) (def - snd r))
  where
    res =
      attackRoll att >>= \attLs ->
        defendRoll def >>= \defLs ->
          return (settle attLs defLs)

------------------------------------------------------------
-- Exercise 3
------------------------------------------------------------
-- 模拟多次对局，直到进攻方少于 2，或防守方少于 1
invade :: Battlefield -> Rand StdGen Battlefield
invade bat@(Battlefield att def) =
  if att < 2 || def < 1
    then return bat
    else battle bat >>= \bat' -> invade bat'

------------------------------------------------------------
-- Exercise 4
------------------------------------------------------------
-- 进行多次模拟，计算进攻方的胜率
successProb :: Battlefield -> Rand StdGen Double
successProb bat =
  replicateM 1000 (invade bat) >>= \xs ->
    return ((fromInteger . toInteger . length . filter (\(Battlefield _ def) -> def == 0) $ xs) / 1000)