-- passar toda a string 1 e 2 para letras minusculas
-- separar a string por espaco
-- deixar nas strings apenas as palavras unicas
-- comparar a primeira palavra da string 1 com todas da string b, se ela for igual a alguma deletar a palavra das duas string
-- se for diferente de todas mandar para um vetor 
-- mandar as palavras que sobraram da string b para o vetor
-- ordenar o vetor resultante

toLowerChar :: Char -> Char
toLowerChar c
    | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
    | otherwise            = c

-- Função para converter uma string para letras minúsculas
toLowerString :: String -> String
toLowerString = map toLowerChar

bSort :: [String] -> [String]
bSort []     = []
bSort (x:xs) = bSort [y | y <- xs , y < x] ++ [x] ++ bSort [y | y <- xs, y >= x]

uncommonFromTwoSentences :: String -> String -> [String]
uncommonFromTwoSentences [][] = []
uncommonFromTwoSentences [] str2 = bSort (palavrasUnicas(words (toLowerString str2))) -- se a string 1 for vazia
uncommonFromTwoSentences str1 [] = bSort (palavrasUnicas(words (toLowerString str1))) -- se a string 2 for vazia 
uncommonFromTwoSentences str1 str2 = bSort (palavrasUnicas ((words (toLowerString str2)) ++ (words (toLowerString str1))) )
--uncommonFromTwoSentences str1 str2 = bSort (funcAux (palavrasUnicas(words (toLowerString str1))) (palavrasUnicas(words (toLowerString str2))))

{-funcAux :: [String] -> [String] -> [String] -- nao usei essa func
funcAux [] [] = []
funcAux [] str2 = str2 -- coloca oq sobrou da str2 no vet
funcAux str1 [] = str1 -- coloca oq sobrou da str1 no vet
funcAux str1@(a:as) str2@(b:bs) | a `elem` str2 = funcAux as (filter (/= a) str2) -- verifica se a palavra a está presente na lista str2, se a esta em str2 entao cria uma nova lista str2 em que todas as presencas de a sao removidas
                                | otherwise     = a : funcAux as str2 -- adiciona a ao vet resultado e continua ja que a é diferente de qualquer str de str2
-}

-- aqui filtra na frase as palavras que se repetem e ja tira da str
contaOcorrencias:: String -> [String] -> Int
contaOcorrencias palavra lista = length (filter (== palavra) lista)
-- aqui filtra na frase as palavras que se repetem e ja tira da str
palavrasUnicas :: [String] -> [String]
palavrasUnicas [] = []
palavrasUnicas lista = [palavra | palavra <- lista, contaOcorrencias palavra lista == 1] -- percorre cada elemento de x da lista xs e manda para o conta ocorrencias, se x for igual a qualquer xs o contador sera maior que 1 e ele nao sera retornado na lista 

main = do
sentence_1 <- getLine
sentence_2 <- getLine
let result = uncommonFromTwoSentences sentence_1 sentence_2
print result

-- elem é um operador infixo que verifica se um elemento esta presente na lista
-- a expressao a \`elem` str2 verifica se a esta na lista str2
-- filter (/= a) bs 
-- filter é uma func de ordem superior que filtra a lista retornando so os elementos de bs que sao diferentes de a 
-- precisa tambem verificar se a palavra aparece mais de uma vez na propria frase, se aparecer precisa ser deletada 