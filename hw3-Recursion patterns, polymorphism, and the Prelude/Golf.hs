module Golf where

-- 取出列表中每第 n 个元素，组成一个新列表
listN :: Int -> [t] -> [t]
listN n lst
  | n <= 0 = []
  | length lst >= n = lst !! (n - 1) : listN n (drop n lst)
  | otherwise = []

-- 根据列表生成列表的列表，第 n 个列表包括每第 n 个元素
skips :: [t] -> [[t]]
skips [] = [[]]
skips lst = [listN n lst | n <- [1 .. (length lst)]]

-- 找到列表中的局部最大值
localMaxima :: [Integer] -> [Integer]
localMaxima (x1 : x2 : x3 : xs)
  | x2 > x1 && x2 > x3 = x2 : localMaxima (x2 : x3 : xs)
  | otherwise = localMaxima (x2 : x3 : xs)
localMaxima _ = []

-- 统计列表中数字出现的次数
countTimes :: [Integer] -> [Int]
countTimes xs = map (\n -> length (filter (== n) xs)) [0 .. 9]

-- 绘制直方图的某一层，如果频次高于 n，就绘制
line :: [Int] -> Int -> String
line xs n = [if i >= n then '*' else ' ' | i <- xs]

-- 根据列表中数字出现的频率，绘制直方图
histogram :: [Integer] -> String
histogram xs = unlines (map (line ts) [m + 1, m .. 1]) ++ "==========\n0123456789\n"
  where
    ts = countTimes xs
    m = maximum ts
