import Data.List

-------------------------------------------------
-- Exercise 1
-------------------------------------------------
fun1' :: [Integer] -> Integer
-- 可以这样实现
-- fun1' = foldl (\p x -> (x -2) * p) 1 . filter even
fun1' = product . map (\x -> x - 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum 
        . filter even 
        . takeWhile (>1) 
        . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

-------------------------------------------------
-- Exercise 2
-------------------------------------------------
-- 二叉树的定义，节点中存储的是树的高度，1 个节点高度为 0
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

-- 获得二叉树的高度
high :: Tree a -> Integer
high Leaf = 0
high (Node h _ _ _) = h

-- 向平衡二叉树中插入一个节点，得到的二叉树仍然平衡（插入高低较低的那个子树，最后重新计算高度）
insertTree :: Tree a -> a -> Tree a
insertTree Leaf value = Node 1 Leaf value Leaf
insertTree (Node _ lchild v1 rchild) v2 = Node (high h + high l') h v1 l'
    where
        (h, l) = if high lchild > high rchild then (lchild, rchild) else (rchild, lchild)
        l' = insertTree l v2

-- 构造一棵平衡的二叉树
foldTree :: [a] -> Tree a
foldTree = foldl insertTree Leaf

-------------------------------------------------
-- Exercise 3
-------------------------------------------------
-- 列表中 True 的数量为奇数时，返回 True，必须通过 fold 实现
-- foldr 中间的函数实际上是两个参数的异或操作
xor :: [Bool] -> Bool
xor = foldr (\x res -> not x && res || (x && not res)) False

-- 通过 fold 实现 map 函数
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x l -> f x : l) []

-- 通过 foldr 实现 foldl 函数
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

-------------------------------------------------
-- Exercise 4
-------------------------------------------------
-- 笛卡尔积
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- 利用 Sieve of Sundaram 算法筛选奇数的素数，直到 2n + 2
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = filter odd [1..2 * n + 2] \\ sieve
  where sieve = map (\(i, j) -> i + j + 2 * i * j)
                . filter (\(i, j) -> i + j + 2 * i * j <= n)
                $ cartProd [1..n][1..n]
