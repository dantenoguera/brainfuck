module Eval where
import Common
import Data.Int



evalComm :: Comm -> Machine Int8 -> ControlMachine (Machine Int8)
evalComm (While p) m@(Machine _ c _ _ _) = if c == 0 then return m else evalProg' (p ++ [While p]) m
evalComm DecP (Machine [] _ _ _ _) = runE
evalComm DecP (Machine  (l : ls) c rs is os) = return (Machine ls l (c : rs) is os)
evalComm IncP (Machine _ _ [] _ _) = runE
evalComm IncP (Machine ls c (r : rs) is os) = return (Machine (c : ls) r rs is os)
evalComm IncB (Machine ls c rs is os) = return (Machine ls (c + 1) rs is os)
evalComm DecB (Machine ls c rs is os) = return (Machine ls (c - 1) rs is os)
evalComm W (Machine ls c rs is os) = return (Machine ls c rs is (c : os))
evalComm R (Machine _ _ _ [] _) = readE
evalComm R (Machine ls c rs is os) = return (Machine ls c rs is (c : os))


evalProg :: Prog -> Int -> [Int8] -> ControlMachine (Machine Int8)
evalProg p size inp = if size <= 0 then sizeE
    else evalProg' p (mkMachine size inp)

evalProg' :: Prog -> Machine Int8 -> ControlMachine (Machine Int8)
evalProg' p m = case p of
    [] -> return m
    (op : ops) -> do
        m' <- evalComm op m
        evalProg' ops m'
