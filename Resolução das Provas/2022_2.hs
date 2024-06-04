--2022.2

{-1) (4.0) Run Length Encoding é um processo para comprimir caracteres quando
existe uma sequência longa de caracteres repetidos. O algoritmo funciona, por
exemplo, trocando zeros que se repetem em uma sequência (lista) de números por
zero seguido da quantidade de repetições:

Escreva uma função que comprime uma lista dessa forma, e outra que descomprime.
rlencode0 :: [Int] -> [Int]
rldecode0 :: [Int] -> [Int]
-- rlencode0 [17,8,54,0,0,0,97,5,16,0,45,23,0,0,0,0,0,3,67,0,0,8]
-- resultado: [17,8,54,0,3,97,5,16,0,1,45,23,0,5,3,67,0,2,8]
-- e rldecode0 faz o contrário
-}

rlencode0 :: [Int] -> [Int]
rlencode0 [] = []
rlencode0 list@(a:as) | a==0      = a : length((takeWhile (==0) list)) : rlencode0 (drop (length((takeWhile (==0) list))) list)
                      | otherwise = a : rlencode0 as

rldecode0 :: [Int] -> [Int]
rldecode0 [] = []
rldecode0 [a] = [a]
rldecode0 list@(a:b:as) | a==0      = replicate b 0 ++ rldecode0 as
                        | otherwise = a : rldecode0 (b:as)

{-2) (4.0) Escreva duas funções que que fazem esse mesmo processo para
qualquer sequência de letras. Assuma que a lista não possui números, apenas
letras, e que no máximo as letras se repetem 9 vezes. Se a letra não é seguida
por um número é porque ela não se repete naquele momento. Por exemplo:
rlencodeLetras :: String -> String
rldecodeLetras :: String -> String
-- rlencodeLetras "abaaacdaaaabbbccxyz"
-- resultado: ”aba3cda4b3c2xyz”
-}
rlencodeLetras :: String -> String
rlencodeLetras [] = []
rlencodeLetras [a] = [a]
rlencodeLetras str@(a:b:as) | a==b = [a] ++ show (length(takeWhile(==a) str)) ++ rlencodeLetras (drop (length(takeWhile(==a) str)) str)
                            | otherwise = a : rlencodeLetras (b:as)

isDigit :: Char -> Bool
isDigit x = x `elem` ['1'..'9'] 

toDigit :: Char -> Int
toDigit x = fromEnum x - fromEnum '0'

rldecodeLetras :: String -> String
rldecodeLetras [] = []
rldecodeLetras [a] = [a]
rldecodeLetras str@(a:b:as) | isDigit b = replicate (toDigit b) a ++ rldecodeLetras as
                            | otherwise = a : rldecodeLetras (b:as)

{-3) (2.0) Dado um tipo de dados que representa letras únicas ou letras
repetidas:
data Letra = Unica Char
| Repetida Char Int
deriving Show
Implemente as mesmas funções da questão anterior de forma que a String seja
codificada ou decodificada para este tipo ao invés de diretamente para uma
outra String. Exemplo:
rlencodeLetrasCodigo :: String -> [Letra]
rldecodeLetrasCodigo :: [Letra] -> String
-- rlencodeLetrasCodigo "abaaacdaaaabbbccxyz"
-- resultado: [Unica ‘a’, Unica ‘b’, Repetida ‘a’ 3, Unica ‘c’, Unica ‘d’,
-- Repetida ‘a’ 4, Repetida ‘b’ 3, Repetida ‘c’ 2, Unica ‘x’,
-- Unica ‘y’, Unica ‘z’]
-}
data Letra = Unica Char
            | Repetida Char Int
            deriving Show

rlencodeLetrasCodigo :: String -> [Letra]
rlencodeLetrasCodigo [] = []
rlencodeLetrasCodigo [a] = [Unica a]
rlencodeLetrasCodigo str@(a:b:as)| a /= b    = [Unica a] ++ rlencodeLetrasCodigo (b:as)
                                 | otherwise = [(Repetida a (length(takeWhile (==a)str)))] ++ rlencodeLetrasCodigo (drop (length(takeWhile (==a)str)) str)
