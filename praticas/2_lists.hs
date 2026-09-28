-- | This module contains the lists content.

import Data.Char
import Data.List
-- ===============================================================
-- Lists
-- ===============================================================
-- Lists are the sequence of values from the same type.
-- For example:
-- [1, 2, 3, 4] :: [Int]
-- [True] :: [Bool]
-- [(5,True),(7,True)] :: [(Int,Bool)]
-- 
-- Type synomyns:
-- type String = [Char]
-- 
-- [] is a list from any type
-- 
-- Some list characteristics:
--      1. The order of the elements is significant
--              [1, 2] /= [2, 1]
--      then: "sergio" /= "oigers"
--      2. The number of the elements is important too
--              [True, True] /= [True]
-- 
-- Some ways to write lists:
-- [5] is equal to 5:[]
-- [4,5] is equal to 4:(5:[])
-- [2,3,4,5] is equal to 2:3:4:5:[]
-- 
-- ':' is a polymorfic constructor:
-- (:) :: Int -> [Int] -> [Int]
-- (:) :: Bool -> [Bool] -> [Bool]
-- (:) :: t -> [t] -> [t]
-- 
-- Other ways to write lists:
-- [2..7] = [2,3,4,5,6,7]
-- [-1..3] = [-1,0,1,2,3]
-- [2.8..5.0] = [2.8,3.8,4.8]
-- [7,5..0] = [7,5,3,1]
-- [2.8,3.3..5.0] = [2.8,3.3,3.8,4.3,4.8]


-- | Returns the sum of the elements from a list
-- 
-- >>>[0..9]
-- 45
-- >>>[0, 2, 4]
-- 6
sumList :: [Int] -> Int
sumList [] = 0
sumList (a:as) = a + sumList as


-- | Returns a list with the double of the elements from a list
-- 
-- >>>double [2,4..12]
-- [4,8,12,16,20,24]
-- 
-- >>>double [2,5..20]
-- [4,10,16,22,28,34,40]
double :: [Int] -> [Int]
double [] = []
double (a:as) = 2*a:double as


-- | Checks if a element is in a list
-- 
-- >>>member [0..10] 10
-- True
-- 
-- >>>member [0..10] 90
-- False
member :: [Int] -> Int -> Bool
member [] _ = False
member (a:as) t
        | a == t = True
        | otherwise = member as t


-- | Filter the digits from a string
-- 
-- >>>digits "abcdef0a9f83"
-- "0983"
-- 
-- >>>digits "H3ll0"
-- 30
digits :: String -> String
digits [] = ""
digits (a:as)
        | isDigit a = [a] ++ (digits as)
        | otherwise = digits as


-- | Returns a list with the sum of each pair in a list of pairs
-- 
-- >>>sumPairs [(1,2), (3,5), (11,13)]
-- [3,8,24]
-- 
-- >>>sumPairs [(4,2), (7,5), (6,3), (9, 3)]
-- [6, 12, 9, 12]
sumPairs :: [(Int,Int)] -> [Int]
sumPairs [] =[]
sumPairs ((x,y):as) = [x+y] ++ sumPairs as


-- Sort functions:
-- | insertion sort (import Data.List)
-- Returns the list ordered
-- sort :: [Int] -> [Int]
-- 
-- >>> sort [6, 12, 9, 12]
-- [6, 9, 12, 12]


-- | insert an element in a list of integers (import Data.List)
-- If the integer is lower or equal the first element of the list,
-- it is inserted in the beggining of the list.
-- Otherwise, it is inserted in the end of the list.
-- insert :: Int -> [Int] -> [Int]
-- 
-- >>> insert 11 [10, 8..(-1)]
-- [10,8,6,4,2,0,11]
-- 
-- >>> insert 10 [10, 8..(-1)]
-- [10,10,8,6,4,2,0]


-- 'case' expression: allows pattern matching
-- in the scope of a function
-- | Returns the first digit from a string
-- 
-- >>> firstDigit "H3ll0"
-- '3'
-- 
-- >>> firstDigit "aaaa"
-- '\NUL'
firstDigit :: String -> Char
firstDigit st = case (digits st) of
                []      -> '\0'
                (a:as)  -> a


-- | Returns the length of a list
-- This function is native from Hakell, it comes from Prelude.
-- Then, it doesn't neet declaration neither importation.
-- 
-- >>> length []
-- 0
-- 
-- >>> length [1..10]
-- 10
-- 
-- length :: [t] -> Int
-- length [] = 0
-- length (a:as) = 1 + lenght as


-- | Concatenates two Lists from the same type
-- This function is native from Hakell, it comes from Prelude.
-- Then, it doesn't neet declaration neither importation.
-- 
-- >>> (++) "abc" "def"
-- "abcdef"
-- 
-- >>> [0..3] ++ [5..10]
-- [0,1,2,3,5,6,7,8,9,10]
-- 
-- (++) :: [t] -> [t] -> [t]
-- [] ++ y = y
-- (x:xs) ++ y = x : (xs ++ y)
-- 
-- Both functions above (length and (++)) are polymorfic!
-- 
-- ===============================================================
-- Polymorphism
-- ===============================================================
-- Caracterystics:
--      1. Function has a generic type;
--      2. Same definition used for many types;
--      3. Code reuse;
--      4. Usage of type variables.
-- 
-- These functions below are natural from haskell:


-- | Returns a list of pairs from 2 list;
-- which can be from different types.
-- 
-- >>> zip [0..3] "abcd"
-- [(0,'a'),(1,'b'),(2,'c'),(3,'d')]
-- 
-- zip :: [t] -> [u] -> [(t,u)]
-- zip (a:as) (b:bs) = (a,b):zip as bs
-- zip [] [] = []


-- | Returns the first element from a tuple
-- 
-- >>> fst (9, 0)
-- 9
-- 
-- fst :: (t,u) -> t
-- fst (x,y) = x


-- | Returns the second element from a tuple
-- 
-- >>> fst (9, 0)
-- 0
-- 
-- snd :: (t,u) -> u
-- snd (x,y) = y


-- | Returns the first element from a list.
-- WARNING: This is a partial function, which doesn't works for all the test cases.
-- In this function, it doesn't work for empty lists.
-- 
-- >>> head [9,3,7]
-- 9
-- 
-- >>> head []
-- *** Exception: Prelude.head: empty list
-- 
-- head :: [t] -> t
-- head (a:as) = a


-- | Returns a list all the elements from a list except the first one.
-- WARNING: This is a partial function, which doesn't works for all the test cases.
-- In this function, it doesn't work for empty lists.
-- 
-- >>> tail [9,3,7]
-- [3,7]
-- 
-- >>> tail []
-- *** Exception: Prelude.head: empty list
-- 
-- tail :: [t] -> [t]
-- tail (a:as) = as

-- | Works the same way as the tail function
-- Now, with all the test cases working correctly.
myTail :: [t] -> [t]
myTail [] = []
myTail (a:as) = as


-- ===============================================================
-- Example: Library
-- ===============================================================

type Person = String
type Book = String
type Database = [(Person, Book)]

-- Example of a database
exampleBase = 
        [("Alice","Postman Pat"),
         ("Anna","All Alone"),
         ("Alice","Spot"),
         ("Rory","Postman Pat")]


-- | Returns a list of books
books :: Database -> Person -> [Book]
books [] _ = []
books ((p, b):pbs) per
        | p==per = b : books pbs per
        | otherwise = books pbs per

-- | Returns a list of people
borrowers :: Database -> Book -> [Person]
borrowers [] _ = []
borrowers ((p, b):pbs) boo
        | boo==b = p : borrowers pbs boo
        | otherwise = borrowers pbs boo

-- | Checks if the book is borrowed
borrowed :: Database -> Book -> Bool
borrowed [] _ = False
borrowed ((p,b):pbs) boo
        | boo==b = True
        | otherwise = borrowed pbs boo

-- | Counts how many books are borrowed by one person
numBorrowed :: Database -> Person -> Int
numBorrowed [] _ = 0
numBorrowed ((p,b):pbs) per
        | per==p = 1 + numBorrowed pbs per
        | otherwise = numBorrowed pbs per

-- | Creates a loan in the database
makeLoan :: Database -> Person -> Book -> Database
makeLoan pbs per boo = (per,boo):pbs


-- | Erases a loan from the database, if exists
returnLoan :: Database -> Person -> Book -> Database
returnLoan [] _ _ = []
returnLoan ((p,b):pbs) per boo
        | (p,b) == (per,boo) = pbs
        | otherwise = (p,b): returnLoan pbs per boo