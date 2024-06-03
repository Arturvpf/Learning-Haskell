htob :: String -> String
htob [] = []
htob (a:as) | a == '0' = "0000" ++ htob as
            | a == '1' = "0001" ++ htob as
            | a == '2' = "0010" ++ htob as
            | a == '3' = "0011" ++ htob as
            | a == '4' = "0100" ++ htob as
            | a == '5' = "0101" ++ htob as
            | a == '6' = "0110" ++ htob as
            | a == '7' = "0111" ++ htob as
            | a == '8' = "1000" ++ htob as
            | a == '9' = "1001" ++ htob as
            | a == 'A' = "1010" ++ htob as
            | a == 'B' = "1011" ++ htob as
            | a == 'C' = "1100" ++ htob as
            | a == 'D' = "1101" ++ htob as
            | a == 'E' = "1110" ++ htob as
            | a == 'F' = "1111" ++ htob as

main = do
    s <- getLine
    let result = htob s
    print result


   
