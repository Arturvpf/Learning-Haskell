-- precisa definir x inicialmente como sendo 0

type Comando = String
type Valor = Int

executa :: [(Comando, Valor)] -> Int
executa [] = 0
executa tupla = executando tupla 0 -- o 0 é o valor inicial do x

operacao :: Comando -> Int
operacao [] = 0
operacao nome | nome == "Soma"         = 1
              | nome == "Subtrai"      = 2
              | nome == "Multiplica"   = 3
              | nome == "Divide"       = 4
              | otherwise              = 0

executando :: [(Comando, Valor)] -> Int -> Int
executando [] x = x
executando ((a,b):as) x | (operacao a) == 0 = 0
                        | (operacao a) == 1 && as/= [] = executando as (x+b)
                        | (operacao a) == 2 && as/= [] = executando as (x-b)
                        | (operacao a) == 3 && as/= [] = executando as (x*b)
                        | (operacao a) == 4 && b == 0  = -666
                        | (operacao a) == 4 && as/= [] = executando as (x `div` b)
                        | (operacao a) == 1            = x+b
                        | (operacao a) == 2            = x-b
                        | (operacao a) == 3            = x*b
                        | (operacao a) == 4            = x `div` b

main = do
    a <- getLine
    let result = executa (read a)
    print result