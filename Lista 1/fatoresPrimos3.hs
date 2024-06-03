-- Mandar o numero para uma funcao que divide o numero sempre pelo mesmo valor e manda esse valor para um vetor
-- Quando nao for possivel dividir mais por esse numero manda para uma funcao que faz a tupla com o valor + tam do vetor - (segundos elementos das outras tuplas)
-- manda de volta para a funcao 

fatPrime :: Int -> [(Int, Int)] -- tem que comecar enviando 2 e o numero para divisao
fatPrime 0 = []
fatPrime 1 = []
fatPrime x = criarTupla 2 (divisao x 2)

presenteNaLista :: Int -> [Int] -> Bool
presenteNaLista _ [] = False
presenteNaLista valor (a:as) | a == valor = True
                             | otherwise  = False

contador :: Int -> [Int] -> Int
contador _ [] = 0
contador valor (x:xs) | x == valor = 1 + contador valor xs
                      | otherwise  = contador valor xs

divisao :: Int -> Int -> [Int] -- numero a ser dividido -> primeiro elemento da tupla (inicio com 2) -> vetor com os divisores
divisao 1 _ = []
divisao 0 _ = []
divisao valor divisor | valor `mod` divisor == 0 = divisor : (divisao (valor `div` divisor) divisor)
                      | otherwise                = divisao valor (divisor + 1) -- indo para o proximo numero

criarTupla :: Int -> [Int] -> [(Int, Int)]
criarTupla _ [] = []
criarTupla valor vetor | presenteNaLista valor vetor == True = (valor, contador valor vetor) : criarTupla (valor + 1) (drop (contador valor vetor) vetor) -- tirando o elemento ja colocado na tupla do vetor
                       | presenteNaLista valor vetor == False = criarTupla (valor + 1) vetor

main = do
      a <- getLine
      let result = fatPrime (read a :: Int)
      print result