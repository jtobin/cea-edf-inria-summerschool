import qualified Data.Map as Map
import Data.Maybe                      (fromJust)
import Data.List                       (unfoldr, foldl', partition, delete)
import Control.Monad.State

-- 1 Basic evaluation

add :: Integer -> Integer -> Integer 
add x y = x + y

double :: Integer -> Integer
double x = x + x

first :: Integer -> Integer -> Integer
first x y = x

cond :: Bool -> Integer -> Integer -> Integer
cond x y z = if x then y else z

twice :: (Integer -> Integer) -> Integer -> Integer
twice f x = f $ f x

infinity :: Integer
infinity = infinity + 1

ex1 = first 42 (double (add 1 2))         -- 42 
ex2 = first 42 (double (add 1 infinity))  -- 42 
ex3 = first infinity (double (add 1 2))   -- infinity
ex4 = add (cond True 42 (1 + infinity)) 4 -- 46
ex5 = twice double (add 1 2)              -- 12
ex6 = twice (add 1) 0                     -- 2

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

-- 2 Basic definitions

square :: Num a => a -> a
square x = x ^ 2

quad :: Num a => a -> a
quad = square . square

larger :: Ord a => a -> a -> a
larger x y = case compare x y of
    LT -> y
    EQ -> x
    GT -> x

smaller :: Ord a => a -> a ->a
smaller x y = case compare x y of
    LT -> x
    EQ -> x
    GT -> y

area :: Double -> Double
area = (* pi) . square

(.&&) :: Bool -> Bool -> Bool
x .&& y = if  compare x y == EQ 
          then x 
          else False

(.||) :: Bool -> Bool -> Bool
x .|| y = if   compare x y == EQ 
          then x
          else True

(.&&.) :: Bool -> Bool -> Bool
x .&&. y
    | compare x y == EQ = x 
    | otherwise         = False

(.||.) :: Bool -> Bool -> Bool
x .||. y
    | compare x y == EQ = x
    | otherwise         = True

showDate :: Integer -> Integer -> Integer -> String
showDate d m y = show d ++ ext ++ fromJust (Map.lookup m month) ++ " " ++ show y
    where month = Map.fromList [(1, "January"), (2, "February"), (3, "March"), (4, "April"), (5, "May"), (6, "June"),
                                (7, "July"), (8, "August"), (9, "September"), (10, "October"), (11, "November"), (12, "December")] 
          ext   = case d of
                    1 -> "st "
                    2 -> "nd "
                    3 -> "rd "
                    _ -> "th "

-- 3 Types

-- [1, 2, 3]            :: Num a => [a]
-- [[1, 2], [3, 4]]     :: Num a => [[a]]
-- [(1, 2), (3, 4)]     :: Num a => [(a, a)]
-- [(1, 'a'), (2, 'b')] :: Num a => [(a, Char)]
-- ['a', 'b', 'c']      :: String
-- "abc"                :: String
-- ["abc", "xyz"]       :: [String]
-- []                   :: [a]
-- [[]]                 :: [[a]]
-- ([1, 2], ['a', 'b']) :: Num a => ([a], String)

prod :: [Int] -> Int
prod []     = 1
prod (x:xs) = x * prod xs

allTrue :: [Bool] -> Bool
allTrue []     = True
allTrue (x:xs) = x .&&. allTrue xs

allFalse :: [Bool] -> Bool
allFalse = not . allFalse0 
    where allFalse0 []     = False
          allFalse0 (x:xs) = x .||. allFalse0 xs

decAll :: [Int] -> [Int]
decAll []     = []
decAll (x:xs) = (x - 1) : decAll xs

convertIntBool :: [Int] -> [Bool]
convertIntBool []     = []
convertIntBool (x:xs) = (x /= 0) : convertIntBool xs

pairUp :: [a] -> [b] -> [(a, b)]
pairUp _  []         = []
pairUp [] _          = []
pairUp (x:xs) (y:ys) = (x, y) : pairUp xs ys

takePrefix :: Int -> [a] -> [a]
takePrefix 0 xs     = []
takePrefix n []     = []
takePrefix n (x:xs) = x : takePrefix (n - 1) xs

dropPrefix :: Int -> [a] -> [a]
dropPrefix 0 xs     = xs 
dropPrefix n []     = []
dropPrefix n (x:xs) = dropPrefix (n - 1) xs 

member :: Eq a => [a] -> a -> Bool
member [] mx     = False
member (x:xs) mx = (x == mx) .||. member xs mx

select :: [a] -> Int -> Maybe a
select []     _ = Nothing
select (x:xs) 0 = Just x
select (_:xs) n = select xs (n - 1)

largest :: [Int] -> Maybe Int
largest []         = Nothing
largest (x:xs)     = Just $ comparer (x:xs)
    where comparer (x:[]) = x
          comparer (x:xs) = larger x (comparer xs)

smallest :: [Int] -> Maybe Int
smallest []        = Nothing
smallest (x:xs)    = Just $ comparer (x:xs)
    where comparer (x:[]) = x
          comparer (x:xs) = smaller x (comparer xs)

-- Could use Either as a more informative error handler.

-- 4 Composition

loremIpsum :: String
loremIpsum = "here is a large block of text"

wordCount :: String -> Int
wordCount = length . words

lineCount :: String -> Int
lineCount = length . lines

sentenceCount :: String -> Int
sentenceCount = undefined -- FIXME

avgWordsPerLine :: String -> Double
avgWordsPerLine xs = (foldr (((+) . flip (/) n) . fromIntegral . length . words) 0 . lines) xs
    where n = fromIntegral . length . lines $ xs

avgWordsPerSentence :: String -> Double
avgWordsPerSentence = undefined -- FIXME

avgWordsPerParagraph :: String -> Double
avgWordsPerParagraph = undefined -- FIXME

avgCharsPerWord :: String -> Double
avgCharsPerWord xs = (foldr (((+) . flip (/) n) . fromIntegral . length) 0 . words) xs
    where n = fromIntegral . length . words $ xs

-- 5 Trees

data Tree a = Empty | Node (Tree a) a (Tree a) deriving Show

testTree0 = Node Empty 0 (Node Empty 1 (Node Empty 2 Empty)) :: Tree Int
testTree1 = Node (Node Empty 1 Empty) 0 (Node (Node Empty 3 Empty) 2 Empty) :: Tree Int

treeSize :: Tree a -> Integer
treeSize Empty        = 0
treeSize (Node l x r) = 1 + treeSize l + treeSize r

tree :: [a] -> Tree a
tree []     = Empty
tree (x:xs) = Node Empty x (tree xs)

memberTree :: Eq a => a -> Tree a -> Bool
memberTree m Empty        = False
memberTree m (Node l x r) 
    | m == x    = True
    | otherwise = memberTree m l || memberTree m r

searchTree :: Ord a => [a] -> Tree a
searchTree = foldr treeInsert Empty 
  where 
    treeInsert :: Ord a => a -> Tree a -> Tree a
    treeInsert x Empty        = Node Empty x Empty
    treeInsert x (Node l m r) = case compare x m of
        LT -> Node (treeInsert x l) m r
        EQ -> Node l x r
        GT -> Node l m (treeInsert x r)

memberS :: Ord a => a -> Tree a -> Bool
memberS m Empty        = False
memberS m (Node l x r) = case compare m x of
    LT -> memberS m l
    EQ -> True
    GT -> memberS m r

inOrder :: Tree a -> [a]
inOrder Empty                = []
inOrder (Node Empty x Empty) = [x] 
inOrder (Node l x r)         = inOrder l ++ [x] ++ inOrder r 

-- 6 Higher-order functions

prod1 :: [Int] -> Int
prod1 = foldr (*) 1

allTrue1 :: [Bool] -> Bool
allTrue1 = foldr (.&&.) True

allFalse1 :: [Bool] -> Bool
allFalse1 = not . foldr (.||.) False 

decAll1 :: [Int] -> [Int]
decAll1 = foldr ((:) . (+) (-1)) []

convertIntBool1 :: [Int] -> [Bool]
convertIntBool1 = foldr ((:) . (/= 0)) []

pairUp1 :: [a] -> [b] -> [(a, b)]
pairUp1 = foldr step (const [])
    where step x f []     = []
          step x f (y:ys) = (x, y) : f ys

takePrefix1 :: Int -> [b] -> [b]
takePrefix1 n xs = map (xs !!) [0..(n - 1)] 

dropPrefix1 :: Int -> [a] -> [a]
dropPrefix1 = undefined -- FIXME
 
member1 :: Eq a => [a] -> a -> Bool
member1 xs x = foldr ((.||.) . (== x)) False xs
 
select1 :: [a] -> Int -> Maybe a
select1 [] _ = Nothing 
select1 xs n = undefined -- FIXME 
 
largest1 :: [Int] -> Maybe Int
largest1 [] = Nothing
largest1 xs = Just $ foldr larger (minBound :: Int) xs
 
smallest1 :: [Int] -> Maybe Int
smallest1 [] = Nothing
smallest1 xs = Just $ foldr smaller (maxBound :: Int) xs

fib :: Int -> Int
fib n = fibs !! n
    where fibs = unfoldr (\(a, b) -> Just (a, (b, a + b))) (0, 1)

fac1 :: Integer -> Integer
fac1 n = foldr (*) 1 (unfoldr (\n -> if n == 0 then Nothing else Just (n, n - 1)) n)

-- 7 Typeclasses

instance Functor Tree where
    fmap f Empty        = Empty
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Eq a => Eq (Tree a) where
    Empty         == Empty         = True
    Empty         == Node{}    = False
    Node{}    == Empty         = False
    Node l0 x0 r0 == Node l1 x1 r1 = (l0 == l1) .&&. (x0 == x1) .&&. (r0 == r1)

instance Ord a => Ord (Tree a) where   
    -- Compares strictly on treeSize.
    compare Empty Empty    = EQ
    compare Empty (Node{}) = LT
    compare (Node{}) Empty = GT

    compare t0@(Node l0 x0 r0) t1@(Node l1 x1 r1) = compare (treeSize t0) (treeSize t1)
        -- For Trees with Num elements:
        --
        -- case compare (treeSize t0) (treeSize t1) of
        --       LT -> LT
        --       GT -> GT
        --       EQ -> compare (magnitude t0) (magnitude t1)
        --   where magnitude Empty = 0
        --         magnitude (Node l x r) = x + (magnitude l) + (magnitude r)

class Sizeable a where
    size :: a -> Integer
    
-- FIXME The following four instances should count memory used, or something like that.
instance Sizeable Int where 
    size x = 1 

instance Sizeable Char where
    size x = 1 

instance Sizeable Bool where
    size x = 1

instance Sizeable (Maybe a) where
    size Nothing  = 0
    size (Just x) = 1

instance Sizeable [a] where
    size [] = 0
    size xs = read . show $ length xs :: Integer

instance Sizeable (Tree a) where
    size = treeSize

-- 8 Monads

sumS :: Num a => [a] -> State a a
sumS []      = get 
sumS (x:xs)  = do
    acc <- get
    put $! x + acc 
    sumS xs

decorate :: Tree a -> State Int (Tree (Int, a))
decorate Empty = return Empty
decorate (Node l x r) = do
    acc  <- get
    l0   <- decorate l
    r0   <- decorate r
    put    $! acc + 1
    return $! Node l0 (acc, x) r0

-- Challenge question (not stateful, but..)

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) = foldl' (flip delete)        -- Strict version of the function from Data.List.

minfree :: [Int] -> Int            -- Bird, R. (2010) 'Pearls of Functional Algorithm Design', p. 1.
minfree xs = minfrom 1 (length xs, xs)
minfrom a (n, xs)                  
    | n == 0     = a
    | m == b - a = minfrom b (n - m, vs)
    | otherwise  = minfrom a (m, us)
      where (us, vs) = partition (< b) xs
            b        = a + 1 + n `div` 2
            m        = length us

