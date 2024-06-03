qSort :: [Int] -> [Int]
qSort []     = []
qSort (x:xs) = qSort [y | y <- xs , y < x] ++ [x] ++ qSort [y | y <- xs, y >= x]

bSort :: [String] -> [String]
bSort []     = []
bSort (x:xs) = bSort [y | y <- xs , y < x] ++ [x] ++ bSort [y | y <- xs, y >= x]

main = do
       a <- getLine
       let result = bSort (read a :: [String])
       print result
