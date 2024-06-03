-- se você modelar cada envelope como uma lista de palavras então a primeira palavra do envelope0 corresponde ao DNA da primeira palavra do envelope1 e consequentemente a i-ésima-palavra do envelope 0 corresponde ao DNA da iésima-palavra do envelope 1.
--não há garantia que a j-ésima-palavra do envelope 0 é de fato igual ao da jésima-palavra do envelope 1, logo impossível de afirmar com certeza que para a jésima-palavra temos o DNA de um dos animais previamente mencionados.
--palavras no mesmo index não necessariamente tem o mesmo tamanho e ainda por cima um envelope pode conter mais amostras que outros, isto é numa das amostragens algumas amostras foram destrúidas.
--compute o produto escalar dos dois vetores e divida o resultado pelo tamanho do maior vetor. Assim você consegue obter um valor entre 0.0 e 1.0 e fazer o mapeamento como deseja.

-- primeiro recebe as duas listas
-- transforma elas em listas de fato com words, ja que recebe ela com espacos 
-- compara cada letra e fornece o resultado numerico 
-- se alguma lista for maior que a outra os elementos excedentes ficam com 0
-- compute o produto escalar dos dois vetores e divida o resultado pelo tamanho do maior vetor. Assim você consegue obter um valor entre 0.0 e 1.0 e fazer o mapeamento como deseja.

data Animal = Cisnal | Iguanoide | Narvale | Null
  deriving (Eq, Show)

toLowerChar :: Char -> Char
toLowerChar c
    | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
    | otherwise            = c

-- Função para converter uma string para letras minúsculas
toLowerString :: String -> String
toLowerString = map toLowerChar

dna2 :: [String] -> [String] -> [Int]
dna2 [] [] = [0,0,0]
--dna2 str1 str2 = contarAnimais(comparacao (map toLowerString str1) (map toLowerString str2))
dna2 str1 str2 = contarAnimais(comparacao str1 str2)



comparacao :: [String] -> [String] -> [Double] -- Aqui compara as strings e vai somando e envia o resultado para um vetor
comparacao [] [] = []
comparacao str1 [] = []
comparacao [] str2 = []
comparacao (a:as) (b:bs) = let tamanhoMaximo = max (length a) (length b)
                               resultado = comparacaoPalavra a b
                           in (calculo resultado tamanhoMaximo) : comparacao as bs

calculo :: Double -> Int -> Double
calculo 0.0 _ = 0.0
calculo x tam = x / fromIntegral tam

comparacaoPalavra :: String -> String -> Double
comparacaoPalavra [] [] = 0
comparacaoPalavra str1 [] = 0
comparacaoPalavra [] str2 = 0
comparacaoPalavra (a:as) (b:bs) | a == b    = 1.0 + comparacaoPalavra as bs
                                | otherwise = comparacaoPalavra as bs

contarAnimais :: [Double] -> [Int]
contarAnimais qtds = [
    length (filter (\x -> x >= 0.1 && x <= 0.3) qtds),
    length (filter (\x -> x >= 0.4 && x <= 0.7) qtds),
    length (filter (\x -> x >= 0.8) qtds)
  ]

main = do
  firstExtract <- words <$> getLine                       -- equivalente a (read firstExtract :: [String])
  secondExtract <- words <$> getLine
  let result = dna2 firstExtract secondExtract
  print result
