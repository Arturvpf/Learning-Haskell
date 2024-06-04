-- Sort
qSort :: [Double] -> [Double] 
qSort []     = []
qSort (x:xs) = qSort [y | y <- xs , y < x] ++ [x] ++ qSort [y | y <- xs, y >= x]

bSortdec :: [Int] -> [Int] -- ordem decrescente
bSortdec []     = []
bSortdec (x:xs) = bSortdec [y | y <- xs , y > x] ++ [x] ++ bSortdec [y | y <- xs, y <= x]

quicksort :: (a -> a -> Ordering) -> [a] -> [a]
quicksort _ [] = []
quicksort cmp (x:xs) = quicksort cmp [y | y <- xs, y `cmp` x == LT]
                        ++ [x] ++ quicksort cmp [y | y <- xs, y `cmp` x /= LT]

-- Em strings
take x str           -> pega os x primeiros elementos da string 
drop x str           -> apaga os x primeiros elementos da string
show x               -> transforma o int em string 
max a b              -> utiliza o que for maior entra a e b 
takeWhile (==x) list -> pega os elementos da string enquanto uma condição é satisfeita (dentro dos parenteses)
replicate a b        -> gera uma string que é o elemento a replicado b vezes

toLowerChar :: Char -> Char
toLowerChar c
    | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
    | otherwise            = c

-- Função para converter uma string para letras minúsculas
toLowerString :: String -> String
toLowerString = map toLowerChar

isDigit :: Char -> Bool
isDigit x = x `elem` ['1'..'9'] 

charToInt :: Char -> Int
charToInt ch = fromEnum ch - fromEnum '0'

-- Outras
fromIntegral tam -- transforma em double o int tam
(filter (\x -> x >= 0.1 && x <= 0.3) str) -- aplica o filtro pegando apenas os elementos x de str que atendem as condicoes
[expressao que usa variavel | variavel <- lista, condicao] -- compreensao de listas
