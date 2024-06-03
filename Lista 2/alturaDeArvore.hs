data Tree t = Node t (Tree t) (Tree t) 
              | Nilt
              deriving (Read)

alturaArvore :: Tree t -> Int
alturaArvore Nilt = 0
alturaArvore (Node value (tree1) (tree2)) = 1 + maxi (alturaArvore (tree1))(alturaArvore (tree2))

main = do
       a <- getLine
       let result = alturaArvore (read a::Tree Int)
       print result