-- primeiro envia para a funcao o argumento 0 e o vetor
-- ai na recursividade da funcao manda o a e (b,c:as)
suaviza :: [Float] -> [Float]
suaviza []    = []
suaviza [a]   = [a]
suaviza [a,b] = [a,b]
suaviza vetor = transformacao 0 vetor

transformacao :: Float -> [Float] -> [Float]
transformacao _ [] = []
transformacao x (a:b:as) | x == 0    = a : transformacao a (b:as)
                         | as == []  = (a + b + x) / 3 : [b] -- aqui precisa retornar toda a lista antiga concatenada com o elemento [b] e concatenada com (a + b + x) / 3 
                         | otherwise = ((a+b+x) / 3) : transformacao a (b:as) 

main = do
        lista <- getLine
        print $ suaviza (read lista :: [Float])