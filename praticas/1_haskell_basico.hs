-- | Basic arithmetic operations and comparisons
-- This module demonstrates fundamental Haskell concepts including
-- pattern matching, recursion, and higher-order functions.


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