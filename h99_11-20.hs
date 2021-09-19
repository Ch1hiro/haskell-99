--Ploblem 11
-- I think I need to use encode at problem 10.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (takeWhile (== x) (x:xs)) : pack ((dropWhile (== x) (x:xs)))

--q10
encode :: Eq a => [a] -> [(Int, a)]
encode x = zip (map length (pack x)) (map head (pack x))

encodeModified_pre :: Show a => (Int, a) -> String
encodeModified_pre x
      | (fst x) == 1    = "Single " ++ (show $ snd x)
      | otherwise       = "Multiple " ++ (show $ fst x) ++ " " ++ (show $ snd x)

encodeModified :: (Show a, Eq a) => [a] -> [String]
encodeModified　x = map encodeModified_pre (encode x)
