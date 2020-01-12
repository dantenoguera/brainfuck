{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Common where
import Prelude
import Data.Int
import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)
import Control.Monad.Trans.Class
import Data.Word

-- Para calmar GHC--
instance Functor OperateMachine where
    fmap = liftM
instance Applicative OperateMachine where
    pure = return
    (<*>) = ap
--------------------

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

data Machine a = Machine [a] a [a] [a] [a] deriving Show

data Error a = Raise String | Return a deriving Show

newtype OperateMachine a = OperateMachine { runOperateMachine :: Error a } deriving Show

instance Monad OperateMachine where
    return x = OperateMachine (Return x)
    m >>= f  =  case runOperateMachine m of
                    Raise e -> OperateMachine (Raise e)
                    Return x -> f x

pointE,readE :: OperateMachine a
pointE = OperateMachine (Raise "Pointer out of bounds")
readE = OperateMachine (Raise "Empty Stream")

sizeE :: Error a
sizeE = Raise "Invalid Size"

mkMachine :: Int -> [Word8] -> Machine Word8
mkMachine size inp = Machine [] 0 (replicate (size - 1) 0) inp []


-- Para la Maquina interactiva --

newtype OperateMachineT m a = OperateMachineT { runOperateMachineT :: m (Error a) }

instance Monad m => Monad (OperateMachineT m) where
    return x = OperateMachineT (return (Return x))
    m >>= f  = OperateMachineT $ do errval <- runOperateMachineT m
                                    case errval of
                                        Raise e  -> return (Raise e)
                                        Return x -> runOperateMachineT (f x)

instance MonadTrans OperateMachineT where
    lift = OperateMachineT . (liftM Return)

pointEI :: Monad m => OperateMachineT m a
pointEI = OperateMachineT $ return (Raise "Pointer out of bounds")

sizeEI :: IO (Error a)
sizeEI = return (Raise "Invalid Size")
