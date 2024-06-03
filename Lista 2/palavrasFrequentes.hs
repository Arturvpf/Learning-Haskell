palavrasFrequentes :: [String] -> [String]
palavrasFrequentes [] = []
palavrasFrequentes [a] = [a]
palavrasFrequentes lista = montarString(ordenacaoPelaString(ordenacaoPeloTam(chamarConta lista)))

apagarRep :: [String] -> [String]
apagarRep [] = []
apagarRep (x:xs) = x : apagarRep (filter (/=x)xs)

-- conta quantas vezes a palavra aconteceu e retorna a tupla com a palavra e num de ocorrencias
contaOcorrencias:: String -> [String] -> (String,Int) --palavra, num de rep, tam da palavra
contaOcorrencias palavra lista = (palavra, length (filter (== palavra) lista))

chamarConta :: [String] -> [(String,Int)] --palavra, num de rep, tam da palavra
chamarConta [] = []
chamarConta lista = [contaOcorrencias palavra lista | palavra <- apagarRep lista] 

bSortdec :: [Int] -> [Int] -- ordem decrescente
bSortdec []     = []
bSortdec (x:xs) = bSortdec [y | y <- xs , y > x] ++ [x] ++ bSortdec [y | y <- xs, y <= x]

bSort :: [Int] -> [Int]
bSort []     = []
bSort (x:xs) = bSort [y | y <- xs , y < x] ++ [x] ++ bSort [y | y <- xs, y >= x]

ordenacaoPelaString :: [(String,Int)] -> [(String,Int)]
ordenacaoPelaString [] = []
ordenacaoPelaString lista = quicksort (\(_, ocorr1) (_, ocorr2) -> compare ocorr2 ocorr1) lista

ordenacaoPeloTam :: [(String,Int)] -> [(String,Int)]
ordenacaoPeloTam [] = []
ordenacaoPeloTam lista = quicksort (\(palavra1, _) (palavra2, _) -> compare (length palavra1) (length palavra2)) lista

montarString :: [(String, Int)] -> [String]
montarString lista = [palavra | (palavra, _) <- take 3 lista] -- pega so os 3 primeiros

quicksort :: (a -> a -> Ordering) -> [a] -> [a]
quicksort _ [] = []
quicksort cmp (x:xs) = quicksort cmp [y | y <- xs, y `cmp` x == LT]
                        ++ [x] ++ quicksort cmp [y | y <- xs, y `cmp` x /= LT]

main = do
        lista <- getLine
        print $ palavrasFrequentes (read lista :: [String])


-- contar a ocorrencia de cada palavra OK
-- ordena primeiro as tuplas de acordo com o tam das palavras
-- depois ordena de acordo com o num de ocorrencias
-- ve quantas tuplas existem
-- se so existir uma imprime uma
-- se so existirem 2 imprime 2
-- se existirem 3 ou + imprime 3

