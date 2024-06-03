--import Data.Maybe (fromMaybe)

{-minMaxCartao :: String -> (Double, Double)
minMaxCartao []    = (0.0,0.0) 
minMaxCartao lista =  let valores = ehFloat lista -- mandando o resultado de ehfloat para valores
                          minMax = sort valores -- criando a variavel minmax para pegar o menor e o maior valor de valores
                        in (fromMaybe 0.0 (fst minMax), fromMaybe 0.0 (snd minMax)) -- retorna a tupla

conferir :: Char -> Bool --confere se o caractere faz parte de um float
conferir a = a `elem` ['0'..'9'] || a == '.'

ehFloat :: String -> [Double] --extrai os floats e retorna eles em uma lista
ehFloat [] = []
ehFloat lista = let (valores, resto) = span conferir lista -- let é usada para definir variaveis locais, aqui a funcao span ta dividindo a string em 2 partes, uma contendo valores numericos e outra o resto da string
                in case reads valores :: [(Double, String)] of -- reads tenta converter valores em uma lista de tuplas de floats e strings e reads interpreta a string como float retornando uma lista de tuplas com o valor lido e a string
                   [(d,"")] -> d : ehFloat resto -- se a conversao acontecer adicionamos o float f a lista resultante e ehfloat é chamado para processar o resto
                   _        -> ehFloat resto -- se tiver caracteres extras em valores processamos resto


sort :: [Double] -> (Maybe Double, Maybe Double)
sort []     = (Nothing, Nothing)
sort lista = (minimum maybeList, maximum maybeList) --calculo do menor e maior valor
  where
    maybeList = map Just lista :: [Maybe Double] --muda os numeros para maybe float


main = do
    a <- getLine
    let result = minMaxCartao a
    print result

-}

{-minMaxCartao :: String -> (Double, Double)
minMaxCartao [] = (0.0,0.0)
minMaxCartao str = case reads str :: [(Double, String)] of
                           [(d, "")] -> [] -- coloca o double na lista se for um
                           _         -> (0.0,0.0) -- coloca o vazio se n for -}


{-qSort :: [Double] -> [Double] 
qSort []     = []
qSort (x:xs) = qSort [y | y <- xs , y < x] ++ [x] ++ qSort [y | y <- xs, y >= x]

dividirString :: String -> [String] --funcionando certo
dividirString [] = [[]]
dividirString (a:as) | a == ';'  = verificarString "" : resto
                     | otherwise = (a : head resto) : tail resto -- adicionando o caractere a primeira substring
                    where
                        resto = dividirString as 

verificarString :: String -> [Double] -- funcao faz vetor de doubles
verificarString [] = []
verificarString str = case reads str :: [(Double, String)] of
                           [(d, "")] -> [d] -- coloca o double na lista se for um 
                           _         -> [] -- coloca o vazio se n for 

tuplaMaiorMenor :: [Double] -> (Double,Double) --chama a ordenacao e ja coloca na tupla
tuplaMaiorMenor []    = (0.0, 0.0)
tuplaMaiorMenor lista = (head ordenado, last ordenado)
    where 
        ordenado = qSort lista 

minMaxCartao :: String -> (Double,Double)
minMaxCartao [] = (0.0,0.0)
minMaxCartao str = tuplaMaiorMenor (concatMap verificarString (dividirString str)) -- primeiro entra dividirString, ai o resultado é passado para verificarString que tem o resultado passado para tuplaMaiorMenor, concat é usada pq dividir retorna uma lista de strings mas verifica so recebe uma

main = do
    a <- getLine
    let result = minMaxCartao a
    print result

-}


qSort :: [Double] -> [Double] 
qSort []     = []
qSort (x:xs) = qSort [y | y <- xs , y < x] ++ [x] ++ qSort [y | y <- xs, y >= x]

dividirString :: String -> [String] --funcionando certo
dividirString [] = [[]]
dividirString str = case span (/= ';') str of -- span divide str em duas partes se o caractere for diferente de ;
    (substring, resto) -> substring : dividirString (dropWhile (== ';') resto) -- substring é a parte antes do ; encontrado e o resto é depois, substring entao é adicionada a lista resultante e chamamos dnv dividirString e tiramos o ; do inicio da nova substring, isso acontece ate que nso tenha mais como dividir a string

verificarString :: String -> [Double] -- funcao faz vetor de doubles
verificarString [] = []
verificarString str = case reads str :: [(Double, String)] of
                           [(d, "")] -> [d] -- coloca o double na lista se for um 
                           _         -> [] -- coloca o vazio se n for 

tuplaMaiorMenor :: [Double] -> (Double,Double) --chama a ordenacao e ja coloca na tupla
tuplaMaiorMenor []    = (0.0, 0.0)
tuplaMaiorMenor lista = (head ordenado, last ordenado)
    where 
        ordenado = qSort lista 

minMaxCartao :: String -> (Double,Double)
minMaxCartao [] = (0.0,0.0)
minMaxCartao str = tuplaMaiorMenor (concatMap verificarString (dividirString str)) -- primeiro entra dividirString, ai o resultado (todas as substrings) é passado para verificarString (que dentro dela faz um vetor de doubles) que tem o resultado passado para tuplaMaiorMenor, concat é usada pq dividir retorna uma lista de strings mas verifica so recebe uma

main = do
    a <- getLine
    let result = minMaxCartao a
    print result

