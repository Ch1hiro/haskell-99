myLast :: [a] -> a
-- erroに対する例外処理がない, のでそれを追加
myLast [] = error "no end for empty lists!"
-- これは別の書き方もできる
-- myLast [x] = x
myLast (x:[]) = x
myLast (_:xs) = myLast xs

-- あいうえお

myButLast :: [a] -> a
myButLast [] = error "no end for empty lists!"
myButLast [x] = error "ちょっとこれは無理"
myButLast (x:y:[]) = x
myButLast (_:xs) = myButLast xs
