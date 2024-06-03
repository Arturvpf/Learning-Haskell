data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Eq, Read, Show)

frente :: Command -> Bool -- func auxiliar pra identificar se ta indo pra frente
frente (Forward _) = True
frente _           = False

tras :: Command -> Bool -- func auxiliar pra identificar se ta indo pra tras
tras (Backward _) = True
tras _            = False

faces :: Direction -> [Command] -> Direction
faces dir [] = dir
faces dir (a:as) | dir == North && a == TurnLeft    = faces West as 
                 | dir == North && a == TurnRight   = faces East as 
                 | dir == North && frente a         = faces North as
                 | dir == North && tras a           = faces North as
                 | dir == South && a == TurnLeft    = faces East as
                 | dir == South && a == TurnRight   = faces West as
                 | dir == South && frente a         = faces South as
                 | dir == South && tras a           = faces South as
                 | dir == East && a == TurnLeft     = faces North as
                 | dir == East && a == TurnRight    = faces South as
                 | dir == East && frente a          = faces East as
                 | dir == East && tras a            = faces East as
                 | dir == West && a == TurnLeft     = faces South as
                 | dir == West && a == TurnRight    = faces North as
                 | dir == West && frente a          = faces West as
                 | dir == West && tras a            = faces West as
                 | otherwise = dir

main = do
       a <- getLine
       b <- getLine
       let result = faces (read a) (read b)
       print result

-- se tiver olhando para norte e receber left olha pro oeste
-- se tiver olhando para norte e receber right olha pro leste
-- se tiver olhando para norte e receber forward continua olhando pra norte
-- se tiver olhando para norte e receber backward olha pro sul

-- se tiver olhando para o sul e receber left olha pro leste
-- se tiver olhando para o sul e receber right olha pro oeste
-- se tiver olhando para o sul e receber forward olha pro sul
-- se tiver olhando para o sul e receber backward olha pro norte

-- se tiver olhando para o leste e receber left olha pro norte
-- se tiver olhando para o leste e receber right olha pro sul
-- se tiver olhando para o leste e receber forward olha pro leste
-- se tiver olhando para o leste e receber backward olha pro oeste

-- se tiver olhando para o oeste e receber left olha pro sul
-- se tiver olhando para o oeste e receber right olha pro norte
-- se tiver olhando para o oeste e receber forward olha pro oeste
-- se tiver olhando para o oeste e receber backward olha pro leste