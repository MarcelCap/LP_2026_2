double :: [Int] -> [Int]
double [] = []
double (a:as) = 2*a:double as

member :: [Int] -> Int -> Bool
member [] _ = False
member (a:as) t
        | a == t = True
        | otherwise = member as t

type Person = String
type Book = String
type Database = [(Person, Book)]

books :: Database -> Person -> [Book]
books [] _ = []
books ((p, b):pbs) per
        | p==per = b : books pbs per
        | otherwise = books pbs per
