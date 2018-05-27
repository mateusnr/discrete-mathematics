import Prelude hiding ((!!), lookup)
{- 
Exercise 1. Write a recursive function copy :: [a] -> [a] that copies its
list argument. For example, copy [2] ⇒[2].
-}

copy :: [a] -> [a]
copy [] = []
copy (x:xs) = x : copy xs

{-
Exercise 2. Write a function inverse that takes a list of pairs and swaps the
pair elements. For example,
inverse [(1,2),(3,4)] ==> [(2,1),(4,3)]
-}

inverse :: [(a, b)] -> [(b, a)]
inverse [] = []
inverse ((x, y):xs) = (y, x) : inverse xs

{-
Exercise 3. Write a function
merge :: Ord a => [a] -> [a] -> [a]
which takes two sorted lists and returns a sorted list containing the 
elements of each.
-}

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge x [] = x
merge [] x = x
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | otherwise = y : merge ys (x:xs)

{-
    Exercise 4. Write (!!), a function that takes a natural number n and a list
    and selects the nth element of the list. List elements are indexed from
    0, not 1, and since the type of the incoming number does not prevent it
    from being out of range, the result should be a Maybe type. For example,
    [1,2,3]!!0 ==> Just 1
    [1,2,3]!!2 ==> Just 3
    [1,2,3]!!5 ==> Nothing
-}

(!!) :: [a] -> Int -> Maybe a
(!!) [] _ = Nothing
(!!) (x:xs) n 
    | n == 0 = Just x
    | otherwise = (!!) xs (n-1)

{-
Exercise 5. Write a function lookup that takes a value and a list of pairs,
and returns the second element of the pair that has the value as its first
element. Use a Maybe type to indicate whether the lookup succeeded.
For example,
lookup 5 [(1,2),(5,3)] ==> Just 3
lookup 6 [(1,2),(5,3)] ==> Nothing
-}

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup n ((x, y):xs) = 
    if x == n then Just y else lookup n xs

{-
Exercise 6. Write a function that counts the number of times an element
appears in a list.

-}
count :: Eq a => a -> [a] -> Int
count _ [] = 0
count n (x:xs) = 
    let count' = count n xs
    in
        if x == n then 1 + count' else count'

{-
Exercise 7. Write a function that takes a value e and a list of values xs and
removes all occurrences of e from xs.
-}

removeValue :: Eq e => e -> [e] -> [e]
removeValue _ [] = []
removeValue n (x:xs) = 
    let remove' = removeValue n xs 
    in
    if n == x then remove' else x : remove'

{-
Exercise 8. Write a function
f :: [a] -> [a]
that removes alternating elements of its list argument, starting with the
first one. For examples, f [1,2,3,4,5,6,7] returns [2,4,6].
-}

removeAlternate :: [a] -> [a]
removeAlternate [_] = []
removeAlternate [_,y] = [y]
removeAlternate (x:y:xs) = removeAlternate [x,y] ++ removeAlternate xs

{-
Exercise 9. Write a function extract :: [Maybe a] -> [a] that takes a
list of Maybe values and returns the elements they contain. For example,
extract [Just 3, Nothing, Just 7] = [3, 7].
-}

extract :: [Maybe a] -> [a]
extract [] = []
extract (Nothing:xs) = extract xs
extract (Just x:xs)  = x : extract xs

{- anoter possible implementation -}
extract' :: [Maybe a] -> [a]
extract' [] = []
extract' xs = [ x | Just x <- xs ]

{-
Exercise 10. Write a function
f :: String -> String -> Maybe Int
that takes two strings. If the second string appears within the first, it
returns the index identifying where it starts. Indexes start from 0. For
example,
f "abcde" "bc" ==> Just 1
f "abcde" "fg" ==> Nothing
-}

isSubstring' :: String -> String -> Int -> Maybe Int
isSubstring' [] sub n = Nothing
isSubstring' (x:xs) sub n = 
    if length sub > length (x:xs) then Nothing
    else if take (length sub) (x:xs) == sub then Just n
    else isSubstring' xs sub (n+1)

isSubstring :: String -> String -> Maybe Int
isSubstring str substr = isSubstring' str substr 0
