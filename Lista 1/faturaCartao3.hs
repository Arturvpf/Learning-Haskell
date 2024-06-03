-- transformar tudo que é ponto e virgula em espaco ok
-- separar string por espaco ok
-- fazer uma tupla com a string e o double 
-- comparar a  sigla com a string ok
-- somar os doubles que forem iguais ok 

trocarCaractere :: String -> String --ok
trocarCaractere [] = []
trocarCaractere (a:as) | a == ';' = ' ' : trocarCaractere as
                       | otherwise = a : trocarCaractere as

dividirString2 :: String -> [String] --funcionando certo
dividirString2 []  = []
dividirString2 str = words str

criarTupla :: [String] -> [(String, Double)] -- cria uma lista de tuplas
criarTupla [] = []
criarTupla (a:b:c:d:xs) = (b, read d) : criarTupla xs

verificacao :: String -> [(String, Double)] -> [Double]
verificacao _ [] = []
verificacao sigla ((a,b):as) | a == sigla = b : verificacao sigla as
                             | otherwise  = verificacao sigla as

logMes :: String -> String -> Double
logMes sigla frase | null sigla || null frase = 0.0
                   | otherwise = foldl (+) 0 (verificacao sigla (criarTupla (dividirString2 (trocarCaractere frase))))

main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result