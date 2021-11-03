-- 返回十进制每一位的逆序的列表
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- 返回十进制每一位的正序的列表
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- 从右边起，第偶数个数乘以2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x : (y : zlist))
  | odd (length zlist) = x : 2 * y : doubleEveryOther zlist
  | otherwise = (2 * x) : y : doubleEveryOther zlist

-- 列表求和，每个数字都要拆成单个数字
sumOneDigit :: Integer -> Integer
sumOneDigit n
  | n <= 0 = 0
  | n < 10 = n
  | otherwise = n `mod` 10 + sumOneDigit (n `div` 10)

sumDigits :: [Integer] -> Integer
sumDigits l = sum (map sumOneDigit l)

-- 检验银行卡号是否有效
validata :: Integer -> Bool
validata n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0