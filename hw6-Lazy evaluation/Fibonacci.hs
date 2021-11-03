{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

--------------------------------------------------------------------------------
-- Exercies 1
--------------------------------------------------------------------------------
-- 斐波那契数列中某个元素的生成函数
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- 整个斐波那契数列
fibs1 :: [Integer]
fibs1 = map fib [0 ..]

--------------------------------------------------------------------------------
-- Exercies 2
--------------------------------------------------------------------------------
-- 更有效率的斐波那契数列计算方式
-- 通过列表推导式实现
fibs2 :: [Integer]
fibs2 =
  [ t | n <- [0 ..], let t
                           | n == 0 = 0
                           | n == 1 = 1
                           | otherwise = fibs2 !! (n -1) + fibs2 !! (n -2)
  ]

-- 通过 map 方法实现，这种更像是 for 循环
fibs2' :: [Integer]
fibs2' =
  map
    ( \n ->
        if n == 0
          then 0
          else if n == 1 then 1 else fibs2' !! (n - 1) + fibs2' !! (n - 2)
    )
    [0 ..]

--------------------------------------------------------------------------------
-- Exercies 3
--------------------------------------------------------------------------------
-- Stream 类型表示无限长的列表
data Stream a = Cons a (Stream a)

-- 将 Stream 类型转换为列表
streamToList :: Stream a -> [a]
streamToList (Cons e xs) = e : streamToList xs

-- 让 Stream 成为 Show 的实例，这样就可以对 Stream 类型使用 show 函数
instance (Show a) => Show (Stream a) where
  show s = "head 20: " ++ (show . take 20 . streamToList $ s)

-- 可以用这个简单测试下显示效果
-- gen n = Cons n (gen n)
-- gen 2

--------------------------------------------------------------------------------
-- Exercies 4
--------------------------------------------------------------------------------
-- 生成一个 Stream，每个元素都相同
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- Stream 版本的 map
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

-- 不断应用生成规则，给定第一个元素，生成一个无限长的 Stream
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f $ f x)

-- 测试一下 streamFromSeed
-- streamFromSeed ((+) 1) 0

--------------------------------------------------------------------------------
-- Exercies 5
--------------------------------------------------------------------------------
-- 自然数的 Stream
nats :: Stream Integer
nats = streamFromSeed (1 +) 0

-- 将两个 Stream 交错插入，得到一个 Stream
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons xa xas) (Cons xb xbs) =
  Cons xa . Cons xb $ interleaveStreams xas xbs

-- 取得 Stream 中的第 n 个元素，n 从 1 开始
streamAt :: Stream a -> Integer -> a
streamAt (Cons x _) 1 = x
streamAt (Cons _ xs) n = streamAt xs (n - 1)

-- 生成 ruler 数列，第 n 个元素（n 从 1 开始）的值为：它最大可以整除 2 的几次方
-- 0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,...
ruler :: Stream Integer
ruler =
  interleaveStreams (streamRepeat 0) $
    streamMap (\n -> 1 + ruler `streamAt` (n `div` 2)) (streamFromSeed (2 +) 2)

--------------------------------------------------------------------------------
-- Exercies 6
--------------------------------------------------------------------------------
-- 多项式为 a0 + a1*x + a2*x^2 + ... + anx^n + ...
-- 使用 Stream Integer 来储存多项式的系数
-- 当前系数的多项式表示 x
x :: Stream Integer
x = Cons 0 . Cons 1 $ streamRepeat 0

-- 让 Stream Integer 成为 Num 的实例，可以支持一些多项式的 (+) (*) 运算
instance Num (Stream Integer) where
  fromInteger i = Cons i $ streamRepeat 0
  negate (Cons x xs) = Cons (negate x) (negate xs)
  (+) (Cons x xs) (Cons y ys) = Cons (x + y) (xs + ys)
  (*) (Cons a0 a') b@(Cons b0 b') = Cons (a0 * b0) $ streamMap (a0 *) b' + (a' * b)

-- 可以用一下的式子来检验
-- x^4
-- (1 + x)^5
-- (x^2 + x + 3) * (x - 5)

-- 让 Stream Integer 成为 Fractional 的实例，支持 (/) 运算
instance Fractional (Stream Integer) where
  (/) (Cons a0 a') (Cons b0 b') = q
    where
      q = Cons (a0 `div` b0) $ streamMap (`div` b0) (a' - q * b')

-- 通过多项式来计算斐波那契数列
-- 以斐波那契数列为系数的多项式 F(x)，存在以下关系：
-- F(x) = x + xF(x) + x^2F(x)
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

--------------------------------------------------------------------------------
-- Exercies 7
--------------------------------------------------------------------------------
-- 通过矩阵乘法的方式来计算斐波那契数列

-- 2x2 矩阵类型
data Matrix22 a = Matrix22 a a a a

-- 让 Matrix22 成为 Num 的实例，支持 (*) 运算
instance Num (Matrix22 Integer) where
  (*)
    (Matrix22 a11 a12 a21 a22)
    (Matrix22 b11 b12 b21 b22) =
      Matrix22
        (a11 * b11 + a12 * b21)
        (a11 * b12 + a12 * b22)
        (a21 * b11 + a22 * b21)
        (a21 * b12 + a22 * b22)

-- 取得 2x2 矩阵的 a12 元素
getA12 :: Matrix22 Integer -> Integer
getA12 (Matrix22 _ x _ _) = x

-- 计算出第 n 个斐波那契数，n 从 0 开始
-- 这个算法的时间复杂度是对数级的
fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = getA12 $ Matrix22 1 1 1 0 ^ n