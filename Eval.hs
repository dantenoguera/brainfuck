module Eval where
import Common
import Data.Word
import Control.Monad.Trans.Class

evalComm :: Comm -> Machine Word8 -> OperateMachine (Machine Word8)
evalComm (While p) m@(Machine _ c _ _ _) = if c == 0 then return m else evalProg' (p ++ [While p]) m
evalComm DecP (Machine [] _ _ _ _) = pointE
evalComm DecP (Machine  (l : ls) c rs is os) = return (Machine ls l (c : rs) is os)
evalComm IncP (Machine _ _ [] _ _) = pointE
evalComm IncP (Machine ls c (r : rs) is os) = return (Machine (c : ls) r rs is os)
evalComm IncB (Machine ls c rs is os) = return (Machine ls (c + 1) rs is os)
evalComm DecB (Machine ls c rs is os) = return (Machine ls (c - 1) rs is os)
evalComm W (Machine ls c rs is os) = return (Machine ls c rs is (c : os))
evalComm R (Machine _ _ _ [] _) = readE
evalComm R (Machine ls c rs (i : is) os) = return (Machine ls i rs is os)


evalProg :: Prog -> Int -> [Word8] ->  Error (Machine Word8)
evalProg p size inp = if size <= 0 then sizeE
    else runOperateMachine (evalProg' p (mkMachine size inp))

evalProg' :: Prog -> Machine Word8 -> OperateMachine (Machine Word8)
evalProg' p m = case p of
    [] -> return m
    (op : ops) -> do
        m' <- evalComm op m
        evalProg' ops m'



-- Maquina Interactiva --

evalCommI :: Comm -> Machine Word8 -> OperateMachineT IO (Machine Word8)
evalCommI (While p) m@(Machine _ c _ _ _) = if c == 0 then return m else evalProgI' (p ++ [While p]) m
evalCommI DecP (Machine [] _ _ _ _) = pointEI
evalCommI DecP (Machine  (l : ls) c rs _ _) = return (Machine ls l (c : rs) [] [])
evalCommI IncP (Machine _ _ [] _ _) = pointEI
evalCommI IncP (Machine ls c (r : rs) _ _) = return (Machine (c : ls) r rs [] [])
evalCommI IncB (Machine ls c rs _ _) = return (Machine ls (c + 1) rs [] [])
evalCommI DecB (Machine ls c rs _ _) = return (Machine ls (c - 1) rs [] [])
evalCommI W (Machine ls c rs _ _) = do lift (putStrLn (show c))
                                       return (Machine ls c rs [] [])
evalCommI R (Machine ls c rs _ _) = do w <- lift getLine
                                       return (Machine ls (read w) rs [] [])


evalProgI' :: Prog -> Machine Word8 -> OperateMachineT IO (Machine Word8)
evalProgI' p m = case p of
        [] -> return m
        (op : ops) -> do
            m' <- evalCommI op m
            evalProgI' ops m'

evalProgI :: Prog -> Int -> IO (Error (Machine Word8))
evalProgI p size = if size <= 0 then sizeEI
    else runOperateMachineT (evalProgI' p (mkMachine size []))
