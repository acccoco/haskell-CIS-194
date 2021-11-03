module Party where

import Data.Tree
import Employee

------------------------------------------------------------
-- Exercise 1
------------------------------------------------------------
-- 将职员加入列表中，简单地求和，并且不判断是否已经在列表中
glCons :: Employee -> GuestList -> GuestList
glCons x@Emp {empFun = f} (GL xs fs) = GL (x : xs) (f + fs)

-- 将 GuestList 变为 Monoid 的实例
instance Semigroup GuestList where
  (<>) (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0

-- 判断哪个列表更快乐
moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2) = if f1 > f2 then gl1 else gl2

------------------------------------------------------------
-- Exercise 2
------------------------------------------------------------
-- 树的 fold 操作，也就是遍历操作
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node val subTrees) = f val $ map (treeFold f) subTrees

------------------------------------------------------------
-- Exercise 3
------------------------------------------------------------
-- 计算出最优的列表，由两项组成，第一项是包含 总Boss 的，第二项不包含
-- 第一个参数是 总Boss，第二个参数是每个部门的最佳邀请列表
--    其中：第一项是包含 子Boss 的，第二项是不包含 子Boss 的
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss@(Emp _ f) [] = (GL [boss] f, mempty)
nextLevel boss ls = (withBossLst, withoutBossLst)
  where
    withoutBossLst = mconcat (map fst ls)
    withBossLst = glCons boss $ mconcat (map snd ls)

------------------------------------------------------------
-- Exercise 4
------------------------------------------------------------
-- 输入公司架构，输出最佳的客人列表
maxFun :: Tree Employee -> GuestList
maxFun t = uncurry moreFun $ treeFold nextLevel t

------------------------------------------------------------
-- Exercise 5
------------------------------------------------------------
-- 将客人列表转化为字符串列表
glListToString :: GuestList -> [String]
glListToString (GL emps f) = ("Total fun: " ++ show f) : map showEmp emps
  where
    showEmp = \(Emp name _) -> name

-- 根据输入的架构字符串，得到客人字符串
calc :: String -> [String]
calc = glListToString . maxFun . read

-- 读取外部文件，得到最佳的客人列表
main :: IO ()
main =
  readFile "company.txt"
    >>= (foldl (\io str -> io >> putStrLn str) (putStrLn "") . calc)
