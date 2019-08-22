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
myLength xs = sumarray (map elementToOne xs)

sumarray :: [Int] -> Int
sumarray [] = 0
sumarray (x:xs) = x + sumarray xs

elementToOne :: a -> Int
elementToOne a = 1
---------------------------------------------------
