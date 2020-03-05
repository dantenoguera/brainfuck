module Eval where
import Common
import Data.Word
import Data.Char
import Control.Monad.Trans.Class
import Text.Read hiding (lift)

evalComm :: Comm -> Machine Word8 -> OperateMachine IO (Machine Word8)
evalComm (While p) m@(Machine _ c _) = if c == 0 then return m else evalProg' (p ++ [While p]) m
evalComm DecP (Machine [] _ _) = pointE
evalComm DecP (Machine  (l : ls) c rs) = return (Machine ls l (c : rs))
evalComm IncP (Machine _ _ []) = pointE
evalComm IncP (Machine ls c (r : rs)) = return (Machine (c : ls) r rs)
evalComm IncB (Machine ls c rs) = return (Machine ls (c + 1) rs)
evalComm DecB (Machine ls c rs) = return (Machine ls (c - 1) rs)
evalComm W (Machine ls c rs) = do lift $ putChar (chr (fromEnum c))
                                  return (Machine ls c rs)
evalComm R (Machine ls c rs) = do w <- lift getLine
                                  case w of
                                      "" -> return (Machine ls 0 rs)
                                      _  -> case readMaybe w of
                                                Nothing -> readE
                                                Just b  -> return (Machine ls b rs)

evalProg' :: Prog -> Machine Word8 -> OperateMachine IO (Machine Word8)
evalProg' p m = case p of
        [] -> return m
        (op : ops) -> do
            m' <- evalComm op m
            evalProg' ops m'

evalProg :: Prog -> IO (Error (Machine Word8))
evalProg p = runOperateMachine (evalProg' p mkMachine)
