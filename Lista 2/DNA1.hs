-- recebe um vetor de arvores, entao tem que transformar essas arvores realmente em um vetor
-- criar funcao que tira o mod dos nos transformando em letra e colocando tudo em um vetor de strings, cada string correspondendo a um elemento do vetor de arvores

-- recebe as arvores
-- tira o mod dos elementos
-- divide a string resultante a cada 8 letras

data Tree t = Node t (Tree t) (Tree t) | Nilt
  deriving (Read, Show)

dna1 :: Tree Int -> [String]
dna1 Nilt = []
dna1 arvore = dividirStr (transformarEmLetras arvore)

transformarEmLetras :: Tree Int -> String
transformarEmLetras Nilt = []   
transformarEmLetras (Node atual esq dir) | atual `mod` 5 == 0 = transformarEmLetras esq ++ "E" ++ transformarEmLetras dir
                                         | atual `mod` 5 == 1 = transformarEmLetras esq ++ "M" ++ transformarEmLetras dir
                                         | atual `mod` 5 == 2 = transformarEmLetras esq ++ "A" ++ transformarEmLetras dir
                                         | atual `mod` 5 == 3 = transformarEmLetras esq ++ "C" ++ transformarEmLetras dir
                                         | atual `mod` 5 == 4 = transformarEmLetras esq ++ "S" ++ transformarEmLetras dir
-- Node 432 (Node 5 Nilt (Node 378 Nilt Nilt)) (Node 698 Nilt Nilt)
{-    
       432 -- ele ta fazendo -> esquerda, direita, atual 
     /   \
    5    698
   / \    / \
 Nilt 378 Nilt Nilt
AECC 
-}
dividirStr :: String -> [String]
dividirStr []  = []
dividirStr str = take 8 str : dividirStr (drop 8 str) 
--Utiliza take 8 str para pegar os primeiros 8 caracteres da string e drop 8 str para remover os primeiros 8 caracteres, repetindo o processo recursivamente até que a string esteja vazia.

main :: IO ()
main = do
  input <- getLine
  let arvore = read input :: Tree Int
  let result = dna1 arvore
  print result