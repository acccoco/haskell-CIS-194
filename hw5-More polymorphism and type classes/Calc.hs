{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import qualified Data.Map as M
import ExprT
import Parser
import StackVM

--------------------------------------------------------------------------------
-- Exercise 1
--------------------------------------------------------------------------------
-- 第一个版本的计算器
-- 计算 ExprT 表达式的值
eval :: ExprT -> Integer
eval (ExprT.Lit i) = i
eval (ExprT.Add e1 e2) = eval e1 + eval e2
eval (ExprT.Mul e1 e2) = eval e1 * eval e2

--------------------------------------------------------------------------------
-- Exercise 2
--------------------------------------------------------------------------------

-- 计算算数表达式的值
evalStr :: String -> Maybe Integer
evalStr = evalExp . parseExp ExprT.Lit ExprT.Add ExprT.Mul

evalExp :: Maybe ExprT -> Maybe Integer
evalExp Nothing = Nothing
evalExp (Just e) = Just (eval e)

--------------------------------------------------------------------------------
-- Exercise 3
--------------------------------------------------------------------------------

-- 抽象表达式的定义，这种抽象表达式可以被解释为各种具体的表达式
-- 例如：add (lit 2) (lit 3) :: ExprT 被解释为 Add (Lit 2) (Lit 3)
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit i = ExprT.Lit i
  add e1 e2 = ExprT.Add e1 e2
  mul e1 e2 = ExprT.Mul e1 e2

-- 这个函数可以将 Expr 解释为 ExprT
reify :: ExprT -> ExprT
reify = id

-- 注：如果直接执行 lit 4，无法得到确定的结果
-- 因为 Haskell 并不知道用户想要得到 type class Expr 的哪一个类型实例
-- 执行 reify $ lit 4 则没有问题，因为 Haskell 可以推断出用户想要得到 ExprT 这个类型实例

--------------------------------------------------------------------------------
-- Exercise 4
--------------------------------------------------------------------------------
-- 让 Integer，Bool，MinMax 和 Mod7 成为 Expr 的实例
instance Expr Integer where
  lit i = i
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit i = i > 0
  add a b = a || b
  mul a b = a && b

-- 重新包装 Integer 类型
newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit i = MinMax i
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

instance Expr Mod7 where
  lit i = Mod7 $ mod i 7
  add (Mod7 a) (Mod7 b) = Mod7 $ mod (a + b) 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ mod (a * b) 7

-- 将算数表达式转换为 Expr 类型的表达式
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

-- 分别用 Integer，Bool，MinMax，Mod7 的方式来解释 Expr 表达式
testInteger :: Maybe Integer
testInteger = testExp

testBool :: Maybe Bool
testBool = testExp

testMM :: Maybe MinMax
testMM = testExp

testSat :: Maybe Mod7
testSat = testExp

--------------------------------------------------------------------------------
-- Exercise 5
--------------------------------------------------------------------------------

-- 让 Program 类型成为 Expr 的实例
instance Expr StackVM.Program where
  lit i = [PushI i]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

testProg :: Maybe StackVM.Program
testProg = testExp

-- 将算数表达式编译为汇编代码
compile :: String -> Maybe Program
compile = parseExp lit add mul

--------------------------------------------------------------------------------
-- Exercise option
--------------------------------------------------------------------------------
-- 自己想写的，可以吧 Expr 的抽象表达式解释为 String
instance Expr String where
  lit i = show i
  add a b = a ++ " + " ++ b
  mul a b = "(" ++ a ++ ") * (" ++ b ++ ")"

--------------------------------------------------------------------------------
-- Exercise 6
--------------------------------------------------------------------------------

-- 其实例可以通过 var xx 的方法存储命名变量
class HasVars a where
  var :: String -> a

-- 表达式类型，支持命名变量，并成为 Expr 和 HasVars 的实例
-- 这个表达式类型，支持有变量的表达式
data VarExprT
  = VLit Integer
  | VAdd VarExprT VarExprT
  | VMul VarExprT VarExprT
  | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = Var

-- 让 {String, Integer} 的 Map 也符合 Expr 和 HasVars
varMap :: M.Map String Integer
varMap = M.empty

-- 让 var xxx 这个抽象表达式对应 Map 的查询操作
instance HasVars (M.Map String Integer -> Maybe Integer) where
  var str = flip (M.!?) str

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit i = \_ -> Just i
  add a b = \m -> addMaybe (a m) (b m)
  mul a b = \m -> mulMaybe (a m) (b m)

addMaybe :: Maybe Integer -> Maybe Integer -> Maybe Integer
addMaybe (Just a) (Just b) = Just (a + b)
addMaybe _ _ = Nothing

mulMaybe :: Maybe Integer -> Maybe Integer -> Maybe Integer
mulMaybe (Just a) (Just b) = Just (a * b)
mulMaybe _ _ = Nothing

-- 提供一个 {String, Integer} 的存放变量的 Map，对抽象的 Expr 表达式求值，得到 Integer
withVars ::
  [(String, Integer)] ->
  (M.Map String Integer -> Maybe Integer) ->
  Maybe Integer
withVars vs e = e $ M.fromList vs

-- 测试用例为：
-- withVars [("x", 6)] $ add (lit 3) (var "x")
-- == Just 9
-- withVars [("x", 6)] $ add (lit 3) (var "y")
-- == Nothing
-- withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
-- == Just 54