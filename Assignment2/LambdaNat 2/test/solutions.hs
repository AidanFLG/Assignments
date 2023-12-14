module Main where

-- length function
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- sum function
sum' :: Num a => [a] -> a
sum' = foldl (+) 0

-- sort function using insertion sort
insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insert' x ys

sort' :: Ord a => [a] -> [a]
sort' = foldr insert' []

main :: IO ()
main = do
  let listLength = length' [1, 2, 3, 4]
      listSum = sum' [1, 2, 3, 4]
      listSort = sort' [5, 3, 4, 3, 1]
  
  putStrLn $ "Length of list: " ++ show listLength
  putStrLn $ "Sum of list: " ++ show listSum
  putStrLn $ "Sorted list: " ++ show listSort