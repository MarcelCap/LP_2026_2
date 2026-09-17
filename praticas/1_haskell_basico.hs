-- | Basic arithmetic operations and comparisons
-- This module demonstrates fundamental Haskell concepts including
-- pattern matching, recursion, and higher-order functions.

import Data.Char

-- ============================================================================
-- Basic types
-- ============================================================================
-- Integers:
--  1, 2, 3, ... :: Int
--  +, *, -, div, mod :: Int -> Int -> Int
--  >, >=, ==, /=, <=, < :: Int -> Int -> Bool
--  Int: fixed lenght per implementation (8/16 bytes)
--  Integer: arbitrary lenght

-- Booleans:
--  True, Fase :: Bool
--  &&, || :: Bool -> Bool -> Bool
--  not :: Bool -> Bool


-- | Returns the square of an integer.
--
-- >>> square 5
-- 25
square :: Int -> Int
square x = x * x


-- | Checks if two integers are equal.
isEqual :: Int -> Int -> Bool
isEqual n m = n == m


-- | Checks if three integers are equal.
all3Equal :: Int -> Int -> Int -> Bool
all3Equal m n p = (m == n) && (n == p)


-- | Checks if four integers are equal.
all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal m n p q = (all3Equal m n p) && (m == q)


-- | Returns the maximum of two integers.
maxi :: Int -> Int -> Int
maxi n m
    | n >= m = n
    | otherwise = m


-- | Adds two integers and returns double the result.
--
-- >>> addD 3 4
-- 14
addD :: Int -> Int -> Int
addD a b = 2 * (a + b)


-- ============================================================================
-- Recursion
-- ============================================================================
-- Key concepts:
--   1. Base case: typically when n == 0
--   2. Recursive case: define how f(n) relates to f(n-1)
--   3. Ensure termination toward the base case


-- | Computes the factorial of a non-negative integer.
-- Returns 0 for negative inputs.
--
-- >>> fatorialfat 5
-- 120
-- >>> fatorialfat 0
-- 1
fatorialfat :: Int -> Int
fatorialfat a
    | a < 0 = 0
    | a == 0 = 1
    | otherwise = a * fatorialfat (a - 1)


-- | Counts how many of three integers are equal to each other.
-- Returns 3 if all equal, 2 if exactly two equal, 0 otherwise.
--
-- >>> howmanyequal 5 5 5
-- 3
-- >>> howmanyequal 5 5 3
-- 2
-- >>> howmanyequal 5 3 1
-- 0
howmanyequal :: Int -> Int -> Int -> Int
howmanyequal a b c
    | all3Equal a b c = 3
    | (a == b) || (b == c) || (a == c) = 2
    | otherwise = 0


-- | Placeholder function returning the input value.
-- Intended as a base case for sales data queries.
-- Future: extend to query arrays or databases.
sales :: Int -> Int
sales n = n


-- ============================================================================
-- Pattern Matching vs Guards
-- ============================================================================
-- Pattern matching: define separate clauses for distinct input patterns
-- Guards: test multiple conditions within a single definition
-- 
-- Pattern matching is preferred when cases are structurally distinct
-- (e.g., empty list vs non-empty, 0 vs n).
-- Guards work better for complex numerical conditions.


-- | Checks if an integer is zero using pattern matching.
isZero :: Int -> Bool
isZero 0 = True
isZero _ = False


-- | Finds the maximum sales value across weeks 0 to n.
-- Recursively compares sales at each week using pattern matching.
--
-- >>> maxSales 3
-- (depends on sales function)
maxSales :: Int -> Int
maxSales 0 = sales 0
maxSales n = maxi (maxSales (n - 1)) (sales n)


-- | Computes the sum of sales from week 0 to week n.
--
-- >>> totalSales 3
-- (depends on sales function)
totalSales :: Int -> Int
totalSales 0 = sales 0
totalSales n = sales n + totalSales (n - 1)


-- ============================================================================
-- Boolean Logic Functions
-- ============================================================================

-- | Logical NOT: inverts a boolean value.
--
-- >>> myNot True
-- False
myNot :: Bool -> Bool
myNot True = False
myNot False = True


-- | Logical AND: returns True only if both arguments are True.
--
-- >>> myAnd True True
-- True
-- >>> myAnd True False
-- False
myAnd :: Bool -> Bool -> Bool
myAnd True x = x
myAnd False _ = False


-- | Logical OR: returns True if at least one argument is True.
--
-- >>> myOr False False
-- False
-- >>> myOr False True
-- True
myOr :: Bool -> Bool -> Bool
myOr False x = x
myOr True _ = True


-- | Counts how many weeks (0 to n) had sales equal to a target value s.
--
-- >>> salesEqual 7 3
-- 1
-- >>> salesEqual 3 7
-- 0

salesEqual :: Int -> Int -> Int
salesEqual s 0  | isEqual s (sales 0) = 1
                | otherwise = 0
salesEqual s n  | n > 0 && isEqual s (sales n) = 1 + salesEqual s (n - 1)
                | n > 0 = salesEqual s (n - 1)


-- ============================================================================
-- Characters and Strings
-- ============================================================================
-- Haskell distinguishes between:
--   - Char: a single character, e.g. 'a', '1', '\n'
--   - String: a sequence of characters, e.g. "hello", ""
--
-- Special escape sequences:
--   - '\t' = tab
--   - '\n' = newline
--   - '\\' = backslash
--   - '\'' = single quote
--   - '\"' = double quote
--
-- Important: ' ', "", and " " are all different!
--   - ' ' is a space character (type Char)
--   - "" is an empty string (type String)
--   - " " is a string containing one space


-- | Converts a character to its Unicode code point.
--
-- Each character is represented internally by an integer. The ord function
-- retrieves this numeric value (used in ASCII/Unicode encoding).
--
-- >>> ord 'A'
-- 65
-- >>> ord 'a'
-- 97
-- >>> ord '0'
-- 48
ord :: Char -> Int


-- | Converts an integer code point back to its character.
--
-- The inverse of ord. Given a valid Unicode code point, returns the
-- corresponding character.
--
-- >>> chr 65
-- 'A'
-- >>> chr 97
-- 'a'
-- >>> chr 32
-- ' '
chr :: Int -> Char


-- | Concatenates two strings.
--
-- Joins two strings end-to-end into a single string.
--
-- >>> "abc" ++ "def"
-- "abcdef"
-- >>> "Hello" ++ " " ++ "World"
-- "Hello World"
(++) :: String -> String -> String


-- | Returns the next character in Unicode order.
--
-- Increments a character by one position in the character encoding.
-- Useful for generating sequences of characters.
--
-- >>> nextChar 'A'
-- 'B'
-- >>> nextChar '9'
-- ':'
-- >>> nextChar 'z'
-- '{'
nextChar :: Char -> Char
nextChar c = chr (ord c + 1)


-- | Offset between uppercase and lowercase letters in Unicode.
--
-- The difference in code points between 'A' (65) and 'a' (97) is 32.
-- This constant is used to convert between cases.
--
-- >>> offset
-- 32
offset :: Int
offset = ord 'A' - ord 'a'


-- | Converts a lowercase letter to its uppercase equivalent.
--
-- Uses the offset between ASCII uppercase and lowercase codes to perform
-- the conversion. Only works correctly for lowercase Latin letters.
--
-- >>> capitalize 'a'
-- 'A'
-- >>> capitalize 'm'
-- 'M'
-- >>> capitalize 'z'
-- 'Z'
capitalize :: Char -> Char
capitalize ch = chr (ord ch + offset)


-- | Checks if a character is a decimal digit ('0' through '9').
--
-- Returns True only for characters in the range [0-9], False otherwise.
-- This is a custom implementation; Data.Char.isDigit is the standard library
-- alternative.
--
-- >>> isDigit '5'
-- True
-- >>> isDigit 'a'
-- False
-- >>> isDigit '0'
-- True
isDigit :: Char -> Bool
isDigit c = (c >= '0') && (c <= '9')


-- | Generates a string of n spaces.
--
-- Uses recursion with pattern matching to build a string of the given length.
-- Base case: 0 spaces returns empty string. Recursive case: subtract 1 from n
-- and prepend one space.
--
-- >>> makeSpaces 3
-- "   "
-- >>> makeSpaces 0
-- ""
-- >>> length (makeSpaces 5)
-- 5
makeSpaces :: Int -> String
makeSpaces 0 = ""
makeSpaces a = " " ++ makeSpaces (a - 1)


-- | Pads a string with leading spaces (right-aligns text).
--
-- Prepends n spaces to a string, shifting it rightward. Useful for
-- formatting columns or creating indentation.
--
-- >>> pushRight 3 "hi"
-- "   hi"
-- >>> pushRight 0 "test"
-- "test"
-- >>> pushRight 5 "x"
-- "     x"
pushRight :: Int -> String -> String
pushRight n s = makeSpaces n ++ s
