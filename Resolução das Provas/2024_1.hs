-- 2024.1

{-1. (2,5 pontos)  A sequência de Fibonacci é uma sequência de números que começa com os
números zero e um e os números seguintes são dados pela soma dos dois números
anteriores. Ela é infinita. Seus primeiros números são 0,1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89,
144, 233, 377, 610, 987, 1597, 2584, ...
Escreva o código necessário para gerar uma lista infinita contendo os números de Fibonacci,
a partir da lista de entrada contendo zero e um.
fibonacci :: [Int]
Para testar, selecione os 20 primeiros números da lista e confira com a lista acima.

1 - 0 + 0 = 0
2 - 0 + 1 = 1
3 - 0 + 1 = 1
4 - 1 + 1 = 2
5 - 1 + 2 = 3
6 - 2 + 3 = 5
sempre o ultima + o penultimo numero
-}

fib :: [Int] -> Int -> Int -> [Int] -- penultimo e ultimo numero
fib [] 0 0  = 0 : fib [0] 0 0
fib [0] 0 0 = 1 : fib [0] 0 1 
fib list pen ult = (pen + ult) : fib list ult (pen+ult)

fibonacci :: [Int]
fibonacci = fib [] 0 0 

{-2. (2,5 pontos) Escreva uma função que recebe duas listas já ordenadas e faz o merge
(combina) as duas listas, dando como resultado uma lista também ordenada. O algoritmo
deve levar em conta que as duas listas de entrada já estão ordenadas.
merge :: Ord t => [t] -> [t] -> [t]
Teste dando como entrada duas listas ordenadas de inteiros ou de letras.
-}

merge :: Ord t => [t] -> [t] -> [t]
merge [] [] = []
merge list1 [] = list1
merge [] list2 = list2
merge list1@(a:as) list2@(b:bs)| a<=b = a : (merge as list2)
                               | b<a  = b : (merge list1 bs)


{-3. (2,5 pontos) Uma estrutura de dados Pilha pode ser representada por uma lista onde a
cabeça da lista é o topo da pilha. Por exemplo, uma Pilha com dois elementos sendo o
elemento 3 no topo, e abaixo dele o elemento 5 seria representada como 3:5:[] (ou [3,5]).
Dado o tipo de dados Elemento abaixo, que representa valores (números inteiros) ou
operações aritméticas, escreva uma função que recebe uma Pilha (lista) de Elementos e
gere a String da expressão correspondente:
type Pilha t = [t]
exemploPilhaElem :: Pilha Elemento
exemploPilhaElem = [Valor 10, Valor 20, Soma, Valor 30, Multiplica]
gera_string :: Pilha Elemento -> String
-- exemplo de uso: gera_string exemploPilhaElem ——> "((10+20)*30)"
data Elemento = Valor Int | Soma | Multiplica deriving (Show)
-}

type Pilha t = [t]

exemploPilhaElem :: Pilha Elemento
exemploPilhaElem = [Valor 10, Valor 20, Soma, Valor 30, Multiplica]

data Elemento = Valor Int | Soma | Multiplica deriving (Show)

gera_string :: Pilha Elemento -> String
gera_string [] = []
gera_string pil = "(" ++ gera_string2 pil ++ ")"

gera_string2 :: Pilha Elemento -> String
gera_string2 [] = []
gera_string2 [Valor x] = show x
gera_string2 (Valor x: Valor y: Soma:as)       = "(" ++ show x ++ "+" ++ show y ++ ")" ++ gera_string2 as
gera_string2 (Valor x: Multiplica:as)          = "*" ++ show x ++ gera_string2 as
gera_string2 (Valor x: Valor y: Multiplica:as) = "(" ++ show x ++ "*" ++ show y ++ ")" ++ gera_string2 as
gera_string2 (Valor x: Soma:as)                = "+" ++ show x ++ gera_string2 as

{-4. (2,5 pontos) Escreva uma função que calcula o resultado da expressão representada em
uma Pilha de Elementos. Assuma que a Pilha só contém expressões válidas.
calcula :: Pilha Elemento -> Int
-- exemplo de uso: calcula exemploPilhaElem ——> 900
Sugestão: sempre que encontrar na Pilha (lista) dois valores seguidos de uma operação,
remova esses 3 Elementos e insira o Valor resultante do cálculo no topo da pilha. Repita
essa operação até que a pilha tenha apenas um Valor.
-}

calcula :: Pilha Elemento -> Int
calcula [] = 0
calcula [Valor x] = x
calcula (Valor x: Valor y: Soma: as)                = calcula (Valor (x+y) : as)
calcula (Valor x:Valor y: Multiplica: as)           = calcula (Valor(x*y) : as)
calcula (Valor x: Valor y: Valor z: Soma: as)       = calcula (Valor (x+y+z) : as)
calcula (Valor x:Valor y: Valor z : Multiplica: as) = calcula (Valor(x*y*z) : as)
