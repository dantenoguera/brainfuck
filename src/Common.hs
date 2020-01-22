module Common where
import Data.Int
import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)
import Control.Monad.Trans.Class
import Data.Word

-- Para calmar GHC--
instance Monad m => Functor (OperateMachineT m) where
    fmap = liftM
instance Monad m => Applicative (OperateMachineT m) where
    pure = return
    (<*>) = ap
--------------------

type Prog = [Comm]

data Comm = While Prog
          | IncP
          | DecP
          | IncB
          | DecB
          | W
          | R
    deriving Show

data Machine a = Machine [a] a [a] deriving Show

data Error a = Raise String | Return a deriving Show

mkMachine :: Int -> Machine Word8
mkMachine size = Machine [] 0 (replicate (size - 1) 0)

newtype OperateMachineT m a = OperateMachineT { runOperateMachineT :: m (Error a) }

instance Monad m => Monad (OperateMachineT m) where
    return x = OperateMachineT (return (Return x))
    m >>= f  = OperateMachineT $ do errval <- runOperateMachineT m
                                    case errval of
                                        Raise e  -> return (Raise e)
                                        Return x -> runOperateMachineT (f x)

instance MonadTrans OperateMachineT where
    lift = OperateMachineT . (liftM Return)

pointE :: Monad m => OperateMachineT m a
pointE = OperateMachineT $ return (Raise "Pointer out of bounds")

sizeE :: IO (Error a)
sizeE = return (Raise "Invalid Size")
