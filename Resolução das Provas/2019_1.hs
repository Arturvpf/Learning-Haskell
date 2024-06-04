-- 2019.1
{-1) ( 2.5) Escreva uma função que funcione como uma função "meio" ou "mid" do Excel: 
ela recebe 3 parâmetros, uma String e dois números ( digamos x e y), 
e retorna a substring que começa no primeiro índice ( o primeiro número, x )e contém y caracteres. 
A primeira posição na lista tem índice 1 ( um). 
Casos inválidos retornam a lista vazia (por exemplo, números negativos ou inicio fora do tamanho da lista).
Se a lista contiver menos caracteres que o solicitado deve ser retornado o que for possível.
Exemplos: meio "ver isso" 5 4 --------------> "isso"
          meio "ver isso tambem" 1 3 -------> "ver"
          meio "ver isso tambem" 13 10 -----> "bem"
          meio "ver isso tambem" 130 ( -4 )-> ""
meio :: String -> Int -> Int -> String
-}
meio :: String -> Int -> Int -> String
meio [] _ _ = []
meio str x y | y < 0           = []
             | x > length str  = []
             | otherwise       = take y (drop (x-1) str) 

{-2) (2.5) Escreva uma função localizar, que indica a posição  de início de uma String dentro de outra.
Caso não seja encontrada ou a String de busca seja vazia, deve ser retornado o valor 0 (zero). 
A contagem das posições começa no valor 1 para o primeiro caracter. 
Deve ser retornada a posicao da primeira ocorrencia, se houver mais de uma.
Exemplos: localizar "abc" "xyz12abrt" ----> 0
          localizar "abc" "aaabrsabcfr" --> 7
          localizar "aab" "aacrabceaabc" -> 9
          localizar "" "aacrabceaabc" ----> 0
localizar :: String -> String -> Int
-}
localizar :: String -> String -> Int
localizar [] _  = 0
localizar _ []  = 0
localizar str1 str2 = localizar2 str1 str2 (length str2)

localizar2 :: String -> String -> Int -> Int -- enviar o aux como sendo o tam da string
localizar2 [] _ _ = 0
localizar2 _ [] y = (-y)
localizar2 procurada str@(a:as) aux| take (length procurada) str == procurada = 1
                                   | take (length procurada) str /= procurada = 1 + localizar2 procurada as aux
                                   | otherwise                                = (-aux)

{-3) Uma fita infinita de papel pode ser representada por uma lista de caracteres. 
Esta fita possui uma cabeça de leitura/escrita que le ou escreve na posição atual da fita. 
Esta fita pode ser lida ou escrita (alterada) pelos seguintes comandos:
ParaFrente -- Que recebe um inteiro N como parâmetro e move a posição de leitura N posições para frente;
ParaTras -- que recebe um inteiro N como parâmetro e move a posição de leitura N posições para tras;
Escreva -- que recebe um caracter como parametro e escreve o caracter naquela posicao da fita, substituindo o caracter que estiver naquela posicao;
A fita comeca na posicao de numero 1.
Nao é preciso tratar o caso da fita se mover para uma posicao invalida (menor que 1).

data Comando = ParaFrente Int
             | ParaTras Int
             | Escreva Char
             deriving (Show, Eq)

3.1) (2.0) Esreva uma funcao que diga a posicao final da fita, apos receber uma lista de comandos.
Exemplos:
posicaofinal "abcdefghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1, Escreva 'y', ParaTras 1] ---> 6
posicaofinal "abcdefghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1, Escreva 'y']  --------------> 7
posicaofinal "abcdefghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1]  ---------------------------> 7
posicaofinal "abcdefghijklmno" [] -------------------------------------------------------------------> 1
posicaofinal :: String -> [Comando] -> Int 
-}
data Comando = ParaFrente Int
             | ParaTras Int
             | Escreva Char
             deriving (Show, Eq)

posicaofinal :: String -> [Comando] -> Int 
posicaofinal _ [] = 1
posicaofinal str (ParaFrente x:as) = x + posicaofinal str as
posicaofinal str (ParaTras x:as)   = (-x) + posicaofinal str as
posicaofinal str (Escreva x :as)   = posicaofinal (x:str) as

{-3.2) (2.0) Escreva uma funcao que, dada uma lista de comandos, retorne o caracter da posicao final da cabeca de leitura, apos comandos.
Exemplos:
interprete "abcdefghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1, Escreva 'y', ParaTras 1] ---> 'x'
interprete "abcdexfghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1, Escreva 'y']  --------------> 'y'
interprete "abcdefghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1]  ---------------------------> 'g'
interprete "abcdefghijklmno" [] -------------------------------------------------------------------> 'a'
interprete :: String -> [Comando] -> Char
-}
interprete :: String -> [Comando] -> Char
interprete str []= head str
interprete str comands = aux str comands 1

aux :: String -> [Comando] -> Int -> Char
aux str [] x = head (drop (x) str)
aux str (ParaFrente x:as) pos = aux str as (pos+x)
aux str (ParaTras x:as) pos   = aux str as (pos-x)
aux str (Escreva x :as) pos   = aux ((take (pos) str) ++ [x] ++ (drop (pos)str)) as pos


--3.3) (1.0) Escreva a mesma funcao anterior, mas que mostre toda a String apos a interpretacao
--estadofinal "abcdefghijklmno" [] -------------------------------------------------------------------> "abcdefghijklmno"
--estadofinal "abcdefghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1]  ---------------------------> "abcdexghijklmno"
-- estadofinal :: String -> [Comando] -> String

estadofinal :: String -> [Comando] -> String
estadofinal str [] = str
estadofinal str comands = aux2 str comands 1

aux2 :: String -> [Comando] -> Int -> String
aux2 str [] _ = str
aux2 str (ParaFrente x:as) pos = aux2 str as (pos+x)
aux2 str (ParaTras x:as) pos   = aux2 str as (pos-x)
aux2 str (Escreva x :as) pos   = aux2 ((take (pos-1) str) ++ [x] ++ (drop (pos)str)) as pos
