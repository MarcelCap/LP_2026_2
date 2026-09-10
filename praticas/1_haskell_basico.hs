square :: Int -> Int
square x = x * x

all3Equal :: Int -> Int -> Int -> Bool
all3Equal m n p = (m==n) && (n==p)

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal m n p q = (all3Equal m n p) && (m==q)

maxi :: Int -> Int -> Int
maxi n m
        | n >= m = n
        | otherwise = m

addD :: Int -> Int -> Int
addD a b = 2 *(a+b)

fatorialfat :: Int -> Int
fatorialfat a
        | a < 0 = 0 
        | a == 0 = 1
        | otherwise = a * fatorialfat (a-1)

howmanyequal :: Int -> Int -> Int -> Int
howmanyequal a b c 
        | all3Equal a b c == True = 3
        | (a==b) || (b==c) || (a==c) = 2
        | otherwise = 0
