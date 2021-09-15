--question01, Sep 15th 2021
myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs
