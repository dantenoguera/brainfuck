{-# OPTIONS -XRecordWildCards #-}
module Main where
import Prelude hiding (print)
import System.Console.Readline
import Control.Exception (catch,IOException)
import Control.Monad.Except
import Data.List
import Data.Char
import System.IO hiding (print)
import System.Environment
import Data.Word
import Common
import Eval
import Parse


data Command = Compile CompileForm
             | Dump
             | Recompile
             | Quit
             | Help
             | Noop
             deriving Show

data CompileForm = CompileInteractive  String
                 | CompileFile         String
                 deriving Show

data State = S { inter :: Bool,       -- True, si estamos en modo interactivo.
                 lfile :: String,     -- Ultimo archivo cargado (para hacer "reload")
                 lmachine :: Machine Word8  -- Ultima maquina que se corrio
               }

data InteractiveCommand = Cmd [String] String (String -> Command) String

ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing

commands :: [InteractiveCommand]
commands =
    [ Cmd [":load"]        "<file>"  (Compile . CompileFile)
                                "Cargar un programa desde un archivo",
      Cmd [":reload"]      "<file>"  (const Recompile) "Volver a cargar el último archivo",
      Cmd [":dump"]       "" (const Dump) "Escribe en un archivo dump.txt la cinta de la última máquina que se corrió",
      Cmd [":help",":?"]   ""        (const Help)   "Mostrar esta lista de comandos",
      Cmd [":quit"]        ""        (const Quit)   "Salir del intérprete"]

main :: IO ()
main = do args <- getArgs
          readevalprint args (S True "" mkMachine )


readevalprint :: [String] -> State -> IO ()
readevalprint args state@(S {..}) =
    let rec st =
          do
            mx <- catch
                   (if inter
                    then readline "Bf> "
                    else fmap Just getLine)
                    ioExceptionCatcher
            case mx of
              Nothing   ->  return ()
              Just ""   ->  rec st
              Just x    ->
                do
                  when inter (addHistory x)
                  c   <- interpretCommand x
                  st' <- handleCommand st c
                  maybe (return ()) rec st'
    in
      do
        state' <- compileFiles args state
        when inter $ putStrLn ("Intérprete de " ++ "Brainfuck.\n" ++
                               "Escriba :? para recibir ayuda.")
        --  enter loop
        rec state' {inter=True}


interpretCommand :: String -> IO Command
interpretCommand str =
    if isPrefixOf ":" str then
        do let (cmd, t') = break isSpace str
               t             = dropWhile isSpace t'
           let  matching  =  filter (\ (Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
           case matching of
             []  ->  do  putStrLn ("Comando desconocido '" ++
                                   cmd                     ++
                                   "'. Escriba :? para recibir ayuda.")
                         return Noop
             [Cmd _ _ f _] ->  do  return (f t)
             _   ->  do  putStrLn ("Comando ambigüo, podría ser " ++
                                   concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
                         return Noop
     else
       return (Compile (CompileInteractive str))

handleCommand :: State -> Command -> IO (Maybe State)
handleCommand st@(S {..}) cmd =
    case cmd of
        Noop -> return (Just st)
        Quit -> return Nothing
        Help -> putStr (helpTxt commands) >> return (Just st)
        Compile c -> do st' <- case c of
                                CompileInteractive s -> compilePhrase s st
                                CompileFile f -> compileFile (st {lfile=f}) f
                        return (Just st')
        Recompile -> if null lfile
                      then putStrLn "No hay un archivo cargado.\n" >>
                           return (Just st)
                      else handleCommand st (Compile (CompileFile lfile))
        Dump -> dumpTape lmachine >> return (Just st)

compilePhrase :: String -> State -> IO State
compilePhrase s st = do
    maybep <- parseIO "<interactive>" parserProg s
    maybe (return st) (handleProg st) maybep


handleProg :: State -> Prog -> IO State
handleProg st@(S {..}) p = do r <- evalProg p 
                              case r of
                                 Raise str -> putStrLn str >> return st
                                 Return m@(Machine _ c _) -> putStrLn ("\nByte apuntado: " ++ show c) >>
                                                             return (st {lmachine=m})

compileFile :: State -> String -> IO State
compileFile st f = do
    putStrLn ("Abriendo "++f++"...")
    let f'= reverse(dropWhile isSpace (reverse f))
    catch (do x <- readFile f'
              maybep <- parseIO f' parserProg x
              maybe (return st) (handleProg st) maybep)
          (\e -> do let err = show (e :: IOException)
                    hPutStr stderr ("No se pudo abrir el archivo " ++ f' ++ ": " ++ err ++"\n")
                    return st)

compileFiles :: [String] -> State -> IO State
compileFiles [] s      = return s
compileFiles (x:xs) s  = do s' <- compileFile (s {lfile=x, inter=False}) x
                            compileFiles xs s'


helpTxt :: [InteractiveCommand] -> String
helpTxt cs
    =  "Lista de comandos: cualquier comando puede ser abreviado a :c donde\n" ++
       "c es el primer caracter del nombre completo.\n\n" ++
       unlines (map (\ (Cmd c a _ d) ->
                     let  ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
                     in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

parseIO :: String -> (String -> ParseResult a) -> String -> IO (Maybe a)
parseIO f p x = case p x of
                    Failed e  -> do putStrLn (f++": "++e)
                                    return Nothing
                    Ok r      -> return (Just r)



dumpTape :: Machine Word8 -> IO ()
dumpTape (Machine ls c rs) =
    let memtab = (mkTable ((reverse ls) ++
                                   [c]  ++
                                   rs) 0 12)
    in writeFile "dump.txt" ("Valor puntero: " ++
                        show (length ls) ++
                       "\n" ++ memtab)
mkTable _ 30000 _ = ""
mkTable tape dir n =
    let (chunk, rest) = splitAt n tape
        format a c = let a' = show a
                     in (replicate (c - length a') '0') ++ a' ++ " "
    in (format dir 4) ++ ":    " ++ concat (map (\v -> format v 3) chunk)
        ++ "\n" ++ mkTable rest (dir + 12) n
