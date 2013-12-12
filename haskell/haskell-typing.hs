module HaskellTyping where

-- slice returns a range of a list from index a to index b, inclusive
-- slice 5 8 [1..] => [5, 6, 7, 8]
-- Look up the "take" and "drop" functions
slice :: Int -> Int -> [a] -> [a]
slice a b = drop (a - 1) . take b



-- encode takes a string and run-length encoding, returning a list of tuples
-- with the element and the number of times it appears in sequence
-- encode "aaabbcaa" => [('a', 3), ('b', 2), ('c', 1), ('a', 2)]
-- "span " function

encode :: (Eq a) => [a] -> [(a, Int)]
encode [] = []
encode all@(p:xs) = 
    let first =  takeWhile (== p) all
        rest = dropWhile (== p) all
    in (p, length first) : encode rest


-- decode expands a run-length encoded list of tuples into a string
-- decode [('a', 3), ('b', 2), ('c', 1), ('a', 2)] => "aaabbcaa"
-- Look up the "replicate" and "concat" functions
decode :: [(a, Int)] -> [a]
decode [] = []
decode ((x, n):xs) = (replicate n x) ++ (decode xs)



-- dropEvery takes list and removes every nth element
-- dropEvery [1..13] 3 => [1, 2, 4, 5, 7, 8, 10, 11, 13]
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery lst n = 
    let first = take (n - 1) lst
        rest = drop n lst
    in first ++ (dropEvery rest n)


-- rpn evaluates a string of reverse polish notation operations
-- rpn "10 4 3 + 2 * -" => -4
-- Look up the "words" and "read" functions
rpn :: (Num a, Read a) => String -> a
rpn = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:xs) "+" = (x + y):xs
          foldingFunction (x:y:xs) "-" = (y - x):xs
          foldingFunction (x:y:xs) "*" = (x * y):xs
          foldingFunction xs num = (read num):xs

