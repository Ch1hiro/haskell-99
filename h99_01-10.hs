--question01, Sep 15th 2021
myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs


--question02, Sep 15th 2021
myButLast :: [a] -> a
myButLast []     = error "myButLast needs more than 1 elements!!"
myButLast [x]    = error "myButLast needs more than 1 elements!!"
myButLast [x:_]  = x
myButLast [_:xs] = myButLast xs
