-- to run:		ghci haskell.hs
-- to exit:		:quit
module Haskell where

-- Calculates the nth Fibonacci number
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci n = fibonacciAux (n-1) 1 0

fibonacciAux :: Integer -> Integer -> Integer -> Integer
fibonacciAux 0 a _ = a
fibonacciAux n a b = fibonacciAux (n-1) (a+b) a

-- Factorial of X
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial(x-1)

-- Gets the position of an element in a list
elementPosition :: Int -> [a] -> a
elementPosition _ [] = error "element not found"
elementPosition x (y:list) =
  if (x == y) then 1
  else 1 + elementPosition x list

-- Gets first element of a list
car :: [a] -> a
car xs = case xs of
  [] -> error "empty list"
  (x:_) -> x

-- Calculates the dot product
dotProduct :: [Int] -> [Int] -> Int
dotProduct l1 l2 = sum (map (\(p,s) -> p*s) (zip l1 l2))

-- Counts the number of positive/negative numbers in a list [excluding 0's]
-- countSigns [-1,2,0,3] => (1,2)
countSigns :: [Int] -> (Int, Int)
countSigns l = countSignsAux l (0, 0)

countSignsAux :: [Int] -> (Int, Int) -> (Int, Int)
countSignsAux [] r = r
countSignsAux (x:xs) (neg, pos)
  | x < 0 = countSignsAux xs (neg+1, pos)
  | x > 0 = countSignsAux xs (neg, pos+1)
  | otherwise = countSignsAux xs (neg, pos)

-- Returns a list of results from applying binary functions from a list to a pair of integers
-- applyFuncs [(+),(-),(*)] 6 3 => [9,3,18]
applyFuncs :: [(Int -> Int -> Int)] -> Int -> Int -> [Int]
applyFuncs (x) a b = applyFuncsAux (x) a b []

applyFuncsAux :: [(Int -> Int -> Int)] -> Int -> Int -> [Int] -> [Int]
applyFuncsAux [] _ _ r = r
applyFuncsAux (x:xs) a b r = applyFuncsAux xs a b (r++[(x) a b])

-- Data type tree
data Tree a = Node (Tree a) a (Tree a) | EmptyTree deriving Show

-- Returns left node of a tree
leftNode :: Tree a -> Tree a
leftNode (Node l _ _) = l

-- Returns right node of a tree
rightNode :: Tree a -> Tree a
rightNode (Node _ _ r) = r

-- Returns the root node of a tree
root :: Tree a -> a
root (Node _ x _) = x

-- Returns the number of nodes in a tree
countNodes :: Tree a -> Int
countNodes EmptyTree = 0
countNodes (Node l _ r) = 1 + (countNodes l) + (countNodes r)

-- Prints the tree's elements in "in order" order
inOrder :: Tree a -> [a]
inOrder EmptyTree = []
inOrder (Node l x r) = (inOrder l)++[x]++(inOrder r)

-- Prints the tree's elements in "pre order" order
preOrder :: Tree a -> [a]
preOrder EmptyTree = []
preOrder (Node l x r) = [x]++(preOrder l)++(preOrder r)

-- Prints the tree's elements in "post order" order
postOrder :: Tree a -> [a]
postOrder EmptyTree = []
postOrder (Node l x r) = (postOrder l)++(postOrder r)++[x]

-- Generates new node with element X
newNode :: a -> Tree a
newNode x = (Node EmptyTree x EmptyTree)

-- Adds a new integer number into a tree 
addNode :: Tree Int -> Int -> Tree Int
addNode EmptyTree x = newNode x
addNode (Node l y r) x
  | x > y     = (Node l y (addNode r x))
  | otherwise = (Node (addNode l x) y r)