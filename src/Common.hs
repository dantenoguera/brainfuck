module Common where
import Data.Int
import Data.Word
import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)
import Control.Monad.Trans.Class


-- Para calmar GHC--
instance Monad m => Functor (OperateMachine m) where
    fmap = liftM
instance Monad m => Applicative (OperateMachine m) where
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

mkMachine :: Machine Word8
mkMachine = Machine [] 0 (replicate 29999 0)

newtype OperateMachine m a = OperateMachine { runOperateMachine :: m (Error a) }

instance Monad m => Monad (OperateMachine m) where
    return = OperateMachine . return . Return
    m >>= f  = OperateMachine $ do errval <- runOperateMachine m
                                   case errval of
                                        Raise e  -> return $ Raise e
                                        Return x -> runOperateMachine $ f x

instance MonadTrans OperateMachine where
    lift = OperateMachine . (liftM  Return)

pointE, readE :: Monad m => OperateMachine m a
pointE = OperateMachine $ return $ Raise "Puntero fuera de límites"
readE = OperateMachine $ return $ Raise "Error de lectura"

