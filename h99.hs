 --question01
myLast :: [a] -> a
-- erroに対する例外処理がない, のでそれを追加
myLast [] = error "no end for empty lists!"
-- これは別の書き方もできる
-- myLast [x] = x
myLast (x:[]) = x
myLast (_:xs) = myLast xs
---------------------------------------------------
-- あいうえお
--question02
myButLast :: [a] -> a
myButLast [] = error "no end for empty lists!"
myButLast [x] = error "ちょっとこれは無理"
myButLast (x:y:[]) = x
myButLast (_:xs) = myButLast xs
---------------------------------------------------

--question03
elementAt :: [a] -> Int -> a
elementAt [] n = error "This is empty list!"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)
---------------------------------------------------

--question04
myLength :: [a] -> Int
myLength [] = 0
myLength xs = sumArray (map elementToOne xs)

sumArray :: [Int] -> Int
sumArray [] = 0
sumArray (x:xs) = x + sumArray xs

elementToOne :: a -> Int
elementToOne a = 1
---------------------------------------------------

--question05
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
---------------------------------------------------
{-
--question06
isPalindrome :: [a] -> Bool
isPalindrome xs = (==) xs (myReverse xs)
-}
