module Common where
import Prelude
import Data.Int
import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)


type Prog = [Comm]

data Comm = While Prog
          | IncP
          | DecP
          | IncB
          | DecB
          | W
          | R
    deriving Show


data Machine a = Machine [a] a [a] [a] [a]

-- Para calmar GHC--
instance Functor ControlMachine where
    fmap = liftM
instance Applicative ControlMachine where
    pure = return
    (<*>) = ap
--------------------

data Error a = Raise String | Return a

newtype ControlMachine a = ControlMachine {runControlMachine :: Error a}

instance Monad ControlMachine where
    return x = ControlMachine (Return x)
    m >>= f  =  case runControlMachine m of
                    Raise e -> ControlMachine (Raise e)
                    Return x -> f x

runE, sizeE, readE :: ControlMachine a
runE = ControlMachine (Raise "Runtime Error")
sizeE = ControlMachine (Raise "Invalid Size")
readE = ControlMachine (Raise "Empty Stream")

mkMachine :: Int -> [Int8] -> Machine Int8
mkMachine size inp = Machine [] 0 (replicate (size - 1) 0) inp []
