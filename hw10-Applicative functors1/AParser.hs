{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import Control.Applicative
import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing -- fail on the empty input
    f (x : xs) -- check if x satisfies the predicate
    -- if so, return x along with the remainder
    -- of the input (that is, xs)
      | p x = Just (x, xs)
      | otherwise = Nothing -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

------------------------------------------------------------
-- Exercise 1
------------------------------------------------------------
-- 将 (a -> b) 的函数转换为 ((a,c) -> (b,c)) 的函数
first :: (a -> b) -> (a, c) -> (b, c)
first f (p1, p2) = (f p1, p2)

-- 让 Parser 成为 Functor 的实例
--Parser f 是 Parser a 类型的，Parser f 是表达式，不是类型。
--   一个是构造函数，一个是类型，两者同名而已，实际上是两个东西
instance Functor Parser where
  fmap h (Parser f) = Parser (fmap (first h) . f)

------------------------------------------------------------
-- Exercise 2
------------------------------------------------------------
-- 让 Parser 成为 Applicative 的实例
instance Applicative Parser where
  pure a = Parser f
    where
      f str = Just (a, str)

  p1 <*> p2 = Parser f
    where
      f str = case runParser p1 str of
        Nothing -> Nothing
        Just (f', strRes) -> fmap (first f') (runParser p2 strRes)

------------------------------------------------------------
-- Exercise 3
------------------------------------------------------------
-- 这个 Parser 要求第一个字母是 'a'，第二个字母是 'b'
adParser :: Parser (Char, Char)
-- adParser = Parser f
--   where
--     f str = case runParser (char 'a') str of
--       Nothing -> Nothing
--       Just (_, strRes) -> case runParser (char 'b') strRes of
--         Nothing -> Nothing
--         Just (_, strRes') -> Just (('a', 'b'), strRes')
-- 下面的部分和上面的部分等价
adParser = (,) <$> char 'a' <*> char 'b'

-- 与上述的 Parser 类似，不过返回值不同
adParser_ :: Parser ()
adParser_ = const () <$> adParser

-- 这个 Parser 要求字符串符合 "整数 整数" 的模式
intPair :: Parser [Integer]
intPair = (\a _ b -> [a, b]) <$> posInt <*> char ' ' <*> posInt

------------------------------------------------------------
-- Exercise 4
------------------------------------------------------------
-- Applicative 是为了顺序匹配；Alternative 是为了选择匹配
instance Alternative Parser where
  -- 得到一个 Parser，这个 Parser 一定会失败
  empty = Parser (const Nothing)

  -- 两个 Parser 进行运算，得到的 Parser，只要有一个匹配通过就算通过
  p1 <|> p2 = Parser f
    where
      f str = runParser p1 str <|> runParser p2 str

------------------------------------------------------------
-- Exercise 5
------------------------------------------------------------
-- 可以匹配开头是大写字母或者整形
intOrUppercase :: Parser ()
intOrUppercase = (const () <$> posInt) <|> (const () <$> satisfy isUpper)
