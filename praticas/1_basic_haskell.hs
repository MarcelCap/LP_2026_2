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
-- ord :: Char -> Int


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
-- chr :: Int -> Char


-- | Concatenates two strings.
--
-- Joins two strings end-to-end into a single string.
--
-- >>> "abc" ++ "def"
-- "abcdef"
-- >>> "Hello" ++ " " ++ "World"
-- "Hello World"
-- (++) :: String -> String -> String


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


-- ============================================================================
-- Floating Point Numbers
-- ============================================================================
-- Haskell provides two types for decimal numbers:
--
--   - Float: single precision (32 bits), less accurate but faster
--   - Double: double precision (64 bits), more accurate (default)
--
-- Examples:
--   - 22.3435 :: Float
--   - 3.14159 :: Double
--   - pi :: Floating a => a (works with Float or Double)
--
-- Arithmetic operations work with both types:
--   - (+), (-), (*), (/) :: Floating a => a -> a -> a
--
-- Key difference: Floating-point operations can have rounding errors.
-- For precise calculations (finance, etc.), consider using Rational or Decimal types.


-- | Rounds a floating-point number upward to the nearest integer.
--
-- Always rounds toward positive infinity. Returns an Int.
--
-- >>> ceiling 6.3
-- 7
-- >>> ceiling 6.0
-- 6
-- >>> ceiling (-2.5)
-- -2
-- ceiling :: Float -> Int


-- | Rounds a floating-point number downward to the nearest integer.
--
-- Always rounds toward negative infinity. Returns an Int.
--
-- >>> floor 5.7
-- 5
-- >>> floor 5.0
-- 5
-- >>> floor (-2.5)
-- -3
-- floor :: Float -> Int


-- | Rounds a floating-point number to the nearest integer.
--
-- Uses "round half to even" strategy: when exactly between two integers
-- (e.g., 6.5), rounds to the nearest even integer.
--
-- >>> round 6.5
-- 6
-- >>> round 6.8
-- 7
-- >>> round 3.3
-- 3
-- >>> round 2.5
-- 2
-- round :: Float -> Int


-- | Converts an integer to a floating-point number.
--
-- Bridges the gap between Int and Float/Double types. Useful when mixing
-- integer and floating-point operations (e.g., division).
--
-- >>> fromIntegral 5
-- 5.0
-- >>> fromIntegral 0
-- 0.0
-- >>> fromIntegral (-3)
-- -3.0
-- fromIntegral :: Int -> Float


-- | Calculates the average sales value from week 0 to week n.
--
-- Divides the total sales by the number of weeks (n+1, since we start
-- counting from 0). Uses fromIntegral to convert integers to floats for
-- accurate division.
--
-- Requires: totalSales function to be defined.
--
-- >>> averageSales 5
-- 3.0
-- >>> averageSales 0
-- 0.0
averageSales :: Int -> Float
averageSales 0 = fromIntegral (sales 0)
averageSales n = fromIntegral (totalSales n) / fromIntegral n


-- ============================================================================
-- Data Structures: Tuples
-- ============================================================================
-- Tuples are immutable collections of fixed size with heterogeneous types.
-- Each position can hold a different type.
--
-- Syntax: (value1, value2, ...)
-- Examples:
--   - (33, 43) :: (Int, Int)
--   - (True, 'x') :: (Bool, Char)
--   - (34, 22, 'b') :: (Int, Int, Char)
--
-- Pattern matching allows decomposition of tuples:
--   addPair (a, b) = a + b
--
-- Tuples are useful for grouping related values without defining a new type.


-- | A tuple containing two integers.
--
-- >>> intP
-- (33, 43)
intP :: (Int, Int)
intP = (33, 43)


-- | Adds the two integers in a tuple.
--
-- Demonstrates tuple pattern matching: deconstructs the tuple into its
-- individual components before performing the operation.
--
-- >>> addPair (5, 3)
-- 8
-- >>> addPair (10, 20)
-- 30
-- >>> addPair (0, 0)
-- 0
addPair :: (Int, Int) -> Int
addPair (a, b) = a + b


-- | Restructures a nested tuple from ((x, y), z) to (x, (y, z)).
--
-- Demonstrates pattern matching on nested tuples and tuple construction.
-- Useful for changing tuple association/grouping.
--
-- >>> shift ((1, 2), 3)
-- (1, (2, 3))
-- >>> shift ((10, 20), 30)
-- (10, (20, 30))
shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift ((x, y), z) = (x, (y, z))


-- ============================================================================
-- Type Synonyms
-- ============================================================================
-- Type synonyms create alternative names for existing types without defining
-- new types. Similar to 'typedef' in C or 'using' in C++.
--
-- Syntax: type NewName = ExistingType
--
-- Benefits:
--   - Improves code readability (Name is clearer than String)
--   - Makes refactoring easier (change all occurrences by changing the synonym)
--   - Documents intent (Person is more semantic than (String, Int, Int))
--
-- Note: Type synonyms are compile-time only; at runtime they are erased.


-- | A name represented as a String.
type Name = String

-- | An age represented as an Int.
type Age = Int

-- | A phone number represented as an Int.
type Phone = Int

-- | A person's information: name, age, and phone number.
type Person = (Name, Age, Phone)


-- | Extracts the name from a Person tuple.
--
-- >>> name ("Alice", 30, 123456)
-- "Alice"
name :: Person -> Name
name (n, a, p) = n


-- | Extracts the age from a Person tuple.
--
-- >>> age ("Alice", 30, 123456)
-- 30
age :: Person -> Age
age (n, a, p) = a


-- | Extracts the phone number from a Person tuple.
--
-- >>> phone ("Alice", 30, 123456)
-- 123456
phone :: Person -> Phone
phone (n, a, p) = p


-- ============================================================================
-- Local Definitions: Bottom-up vs Top-down Styles
-- ============================================================================
-- Haskell supports two complementary styles for organizing local definitions:
--
-- 1. BOTTOM-UP style (where clause):
--    - Main expression appears first, local definitions follow
--    - Visually emphasizes the final computation
--    - More commonly used in Haskell
--    - Scope: bindings visible to all expressions in the function
--    Example:
--      result where helper = computation
--
-- 2. TOP-DOWN style (let...in expression):
--    - Local definitions appear first, then the main expression
--    - Visually emphasizes helper variables before using them
--    - More intuitive for imperative programmers
--    - Scope: bindings available only to the 'in' expression
--    Example:
--      let helper = computation in result
--
-- Both styles are equivalent in functionality; choose based on preference
-- and readability. The following examples demonstrate all three approaches.


-- ============================================================================
-- Approach 1: Bottom-up with Simple where (Basic Pattern)
-- ============================================================================
-- | Calculates the sum of the squares of two integers.
--
-- This is the most straightforward bottom-up approach: compute the result
-- first, then define local helper variables using 'where' clause.
-- This approach is concise and emphasizes the main computation.
--
-- >>> sumSquares1 3 4
-- 25
-- >>> sumSquares1 5 12
-- 169
-- >>> sumSquares1 0 0
-- 0
sumSquares1 :: Int -> Int -> Int
sumSquares1 x y = sqX + sqY
  where
    sqX = x * x
    sqY = y * y


-- ============================================================================
-- Approach 2: Bottom-up with Helper Function (Functional Pattern)
-- ============================================================================
-- | Calculates the sum of the squares of two integers.
--
-- This bottom-up approach demonstrates defining a helper function (sq) instead
-- of just variables. The helper function can be reused and makes the intent
-- clearer: "square this value". This is a more functional style.
--
-- Benefits:
--   - Demonstrates abstraction (sq is a reusable operation)
--   - Shows how to define local functions within 'where'
--   - Cleaner for complex computations that need parameterization
--
-- >>> sumSquares2 3 4
-- 25
-- >>> sumSquares2 5 12
-- 169
-- >>> sumSquares2 0 0
-- 0
sumSquares2 :: Int -> Int -> Int
sumSquares2 x y = sq x + sq y
  where
    sq z = z * z


-- ============================================================================
-- Approach 3: Top-down with let...in (Imperative Pattern)
-- ============================================================================
-- | Calculates the sum of the squares of two integers.
--
-- This top-down approach uses 'let...in' expression. Local definitions appear
-- first, then the final computation follows after 'in'. This style may feel
-- more natural for programmers coming from imperative languages.
--
-- Benefits:
--   - Definitions appear before use (reads top-to-bottom)
--   - Bindings are scoped only to the 'in' expression
--   - Useful when definitions depend on each other sequentially
--
-- Note: Functionally identical to Approach 1, just different syntax/style.
--
-- >>> sumSquares3 3 4
-- 25
-- >>> sumSquares3 5 12
-- 169
-- >>> sumSquares3 0 0
-- 0
sumSquares3 :: Int -> Int -> Int
sumSquares3 x y = let sqX = x * x
                      sqY = y * y
                  in sqX + sqY


-- ============================================================================
-- Comparison Table
-- ============================================================================
-- | Comparison of the three approaches:
--
-- Approach 1 (where + variables):
--   Pros: Concise, emphasizes main computation, most common in Haskell
--   Cons: Variables only, no parameterization
--   Best for: Simple computations with a few local constants
--
-- Approach 2 (where + function):
--   Pros: Abstraction, reusability, functional style
--   Cons: Slightly more verbose
--   Best for: Complex operations that need parameterization or reuse
--
-- Approach 3 (let...in):
--   Pros: Definitions visible first, familiar to imperative programmers
--   Cons: Scope limited to 'in' expression only
--   Best for: Small scopes, sequential dependencies, one-off definitions
--
-- Recommendation: Use bottom-up (where) by default; use let...in when
-- the expression is small and self-contained, or when top-down readability
-- is significantly better.



-- | Returns the maximum value among three integers.
--
-- Uses the previously defined maxi function recursively.
--
-- >>> maxiThree 4 5 3
-- 5
-- >>> maxiThree 10 2 8
-- 10
-- >>> maxiThree 1 1 1
-- 1
maxiThree :: Int -> Int -> Int -> Int
maxiThree a b c = maxi a (maxi b c)


-- | Counts how many of four integers are equal to each other.
--
-- Uses previously defined helper functions (all4Equal, howmanyequal, maxi).
-- Checks if all four are equal first. Otherwise, computes equality counts
-- for all possible 3-element combinations and returns the maximum.
--
-- >>> equalCount 3 4 5 6
-- 0
-- >>> equalCount 3 4 5 5
-- 2
-- >>> equalCount 5 4 5 5
-- 3
-- >>> equalCount 5 5 5 5
-- 4
equalCount :: Int -> Int -> Int -> Int -> Int
equalCount a b c d
    | all4Equal a b c d = 4
    | otherwise = maxi (howmanyequal a b c)
                       (maxi (howmanyequal a c d)
                             (maxi (howmanyequal a b d)
                                   (howmanyequal b c d)))


-- | Finds the maximum value among three integers and counts its occurrences.
--
-- Returns a tuple containing:
--   1. The maximum value (using maxiThree)
--   2. The count of how many times it appears (using equalCount)
--
-- Demonstrates the use of 'where' clause for organizing local definitions
-- in a clear, readable manner.
--
-- >>> maxThreeOccurs 5 5 3
-- (5, 2)
-- >>> maxThreeOccurs 1 2 3
-- (3, 1)
-- >>> maxThreeOccurs 7 7 7
-- (7, 3)
maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
maxThreeOccurs m n p = (mx, eqCount)
  where
    mx = maxiThree m n p
    eqCount = equalCount mx m n p
