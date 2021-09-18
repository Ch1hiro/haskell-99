--question01, Sep 15th 2021
myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs


--question02, Sep 15th 2021
myButLast :: [a] -> a
myButLast []        = error "myButLast needs more than 1 elements!!"
myButLast [x]       = error "myButLast needs more than 1 elements!!"
myButLast (x:y:[])  = x
myButLast (_:xs)    = myButLast xs

--question03,  Sep 15th 2021
elementAt :: [a] -> Int -> a
elementAt []     _ = error "no such elemet"
elementAt (x:_)  1 = x
elementAt (_:xs) k =
      if (k<2)
            then error "k value should be positive Int"
      else
            elementAt xs (k-1)

--question04, Sep 15th 2021
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + (myLength xs)

--q5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

--q6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome　x = x == (myReverse x)

--q7
data NestedList a = Elem a | List [NestedList a]
--q7はちょっと後にする。
--不安だけど。データ構造体ってのを理解してからもう一度やってみよう。


--q8
compress :: Eq a =>[a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
      | x == head xs    = compress xs
      | x /= head xs    = x : (compress xs)

--q9
{-
pack :: [a] -> [[a]]
pack []  = [[]]
pack [x] = [[x]]
pack (x:xs)
      | x == head xs
      | x /= head xs

listElemIsEq :: Eq a => [a] -> [Bool]
listElemIsEq [] = []
listElemIsEq [x] = []
listElemIsEq (x:xs)
      | x == head xs    = True : (listElemIsEq xs)
      | x /= head xs    = False : (listElemIsEq xs)
-}

split :: Eq a => [a] -> [[a]]
split [] =[[]]
split (x:xs)
      | (dropWhile (==x) (x:xs)) == []    = (takeWhile (== x) (x:xs)) :[]
      | otherwise                         = (takeWhile (== x) (x:xs)) : split ((dropWhile (== x) (x:xs)))


-- I can write the function pack like this;
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (takeWhile (== x) (x:xs)) : split ((dropWhile (== x) (x:xs)))

--q10
encode :: Eq a => [a] -> [(Int, a)]
encode x = zip (map length (pack x)) (map head (pack x))
