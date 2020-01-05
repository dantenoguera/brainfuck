module Common where
import Prelude hiding (repeat)
import Data.Int
import Data.Stream

type Exp = [Op]

data Machine a = Machine { tape :: Stream a
                         , pointer :: a
                         } deriving Show

newMachine :: Machine Int8
newMachine = Machine (repeat 0) 0



data Op = While Exp
        | IncP
        | DecP
        | IncB
        | DecB
        | W
        | R
    deriving Show
