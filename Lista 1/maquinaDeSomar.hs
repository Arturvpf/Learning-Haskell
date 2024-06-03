{-maquinaSomarAux :: [Int] -> Int --funcao que para a soma quando chega em 0,0
maquinaSomarAux []                                     = 0
maquinaSomarAux (a:as) let cabeca = head as
                         in |a == 0 && cabeca==0       = 0
                            |otherwise                 = a + maquinaSomarAux as -}

{-maquinaSomarAux :: [Int] -> Int --funcao que para a soma quando chega em 0,0
maquinaSomarAux [] = 0
maquinaSomarAux (a:as) | a == 0 && head as == 0 = 0
                       | otherwise              = a + maquinaSomarAux as -}

maquinaSomarAux :: [Int] -> Int
maquinaSomarAux [] = 0
maquinaSomarAux [a] = a  -- Caso em que 'as' tem apenas um elemento
maquinaSomarAux (a:as) | a == 0           = a
                       |as == []          = a 
                       | (head as) == 0   = a
                       | otherwise        = a + maquinaSomarAux (as)
                        
maquinaSomar :: [Int] -> [Int]
maquinaSomar []  = []
maquinaSomar [0] = []
maquinaSomar (a:as) | a==0 && as == []       = []
                    | a==0 && (head as == 0) = [] -- 2 zeros seguidos para a funcao
                    | a==0 && (head as /= 0) = [maquinaSomarAux as] ++ maquinaSomar (dropWhile (/=0) as) --concatena a ultima soma e inicia a soma dnv
                    | otherwise              = [maquinaSomarAux (a:as)] -- caso que nao comeca com o a sendo 0 

main = do
       lista <- getLine
       print $ maquinaSomar (read lista :: [Int])












soma :: [Int] -> Int
soma [] = 0
soma (a:as)
| (as == []) = a
| (head as == 0 || as == []) = a
| otherwise = a + soma(as)

lista :: [Int] -> [Int]
lista [] = []
lista (a:as)
| (as == []) = []
| (a == 0) && (head as == 0) = []
| (a == 0) = [soma(a:as)] ++ lista(as)
| otherwise = lista(as)