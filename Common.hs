module Common where

type Exp = [Op]
          
data Op = While Exp
        | IncP 
        | DecP
        | IncB
        | DecB
        | W
        | R
    deriving Show

