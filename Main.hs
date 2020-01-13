{-# OPTIONS -XRecordWildCards #-}
{-# OPTIONS -XRecordWildCards #-}
module Main where

import Control.Exception (catch,IOException)
import Control.Monad.Except
import Data.Char
import Data.List
import Data.Maybe
import Prelude hiding (print)
import System.Console.Readline
import System.Environment
import System.IO hiding (print)
import Text.PrettyPrint.HughesPJ (render,text)

import Common
--import PrettyPrinter
import Eval
import Parse

data Command = Compile CompileForm
             | Print String
             | Recompile
             | Browse
             | Quit
             | Help
             | Noop

data CompileForm = CompileInteractive  String
                 | CompileFile         String


main :: IO ()
main = do args <- getArgs
          readevalprint args (S True "")


ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing

iname, iprompt :: String
iname = "Brainfuck"
iprompt = "BF> "

data State = S { inter :: Bool       -- True, si estamos en modo interactivo.
               , lfile :: String     -- Ultimo archivo cargado (para hacer "reload")
               }

readevalprint :: [String] -> State -> IO ()
readevalprint args state@(S {..}) =
    let rec st = do
         mx <- catch (if inter then readline iprompt
                  else fmap Just getLine)
                  ioExceptionCatcher
         case mx of
             Nothing -> return ()
             Just "" -> rec st
             Just x  -> do
                 when inter (addHistory x)
                 c <- interpretCommand x
                 st' <- handleCommand st c
                 maybe (return ()) rec st'
    in do
        state' <- compileFiles args state
        when inter $ putStrLn ("Intérprete de " ++ iname ++ ".\n" ++
                               "Escriba :? para recibir ayuda.")
        --  enter loop
        rec state' {inter=True}


interpretCommand :: String -> IO Command
interpretCommand x =
    if isPrefixOf ":" x then
        do let (cmd,t')  =  break isSpace x
               t         =  dropWhile isSpace t'
           let  matching  =  filter (\ (Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
           case matching of
               []  ->  do  putStrLn ("Comando desconocido `" ++
                                 cmd                         ++
                                 "'. Escriba :? para recibir ayuda.")
                           return Noop
               [Cmd _ _ f _]
                   ->  do  return (f t)
               _   ->  do  putStrLn ("Comando ambigüo, podría ser " ++
                                 concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
                           return Noop
    else
        return (Compile (CompileInteractive x))


handleCommand :: State -> Command -> IO (Maybe State)
handleCommand state@(S {..}) cmd
  =  case cmd of
       Quit       ->  when (not inter) (putStrLn "!@#$^&*") >> return Nothing
       Noop       ->  return (Just state)
       Help       ->  putStr (helpTxt commands) >> return (Just state)
       Compile c  ->  do  state' <- case c of
                                 CompileInteractive s -> compilePhrase state s
                                 CompileFile f        -> compileFile (state {lfile=f}) f
                          return (Just state')
       Print s    ->  let s' = reverse (dropWhile isSpace (reverse (dropWhile isSpace s)))
                      in printPhrase s' >> return (Just state)
       Recompile  -> if null lfile
                      then putStrLn "No hay un archivo cargado.\n" >>
                           return (Just state)
                      else handleCommand state (Compile (CompileFile lfile))


compileFiles :: [String] -> State -> IO State
compileFiles [] s = undefined


data InteractiveCommand = Cmd [String] String (String -> Command) String

commands :: [InteractiveCommand]
commands
  =  [ Cmd [":browse"]      ""        (const Browse) "Ver los nombres en scope",
       Cmd [":load"]        "<file>"  (Compile . CompileFile)
                                                     "Cargar un programa desde un archivo",
       Cmd [":reload"]      "<file>"  (const Recompile) "Volver a cargar el último archivo",
       Cmd [":quit"]        ""        (const Quit)   "Salir del intérprete",
       Cmd [":help",":?"]   ""        (const Help)   "Mostrar esta lista de comandos"]


helpTxt :: [InteractiveCommand] -> String
helpTxt cs =
    unlines (map (\ (Cmd c a _ d) ->
    let  ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
    in ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)


compilePhrase :: State -> String -> IO State
compilePhrase state x =
  do
    x' <- parseIO "<interactive>" parse x
    maybe (return state) handleProg x'


printPhrase :: String -> IO ()
printPhrase x =
  do
    x' <- parseIO "<interactive>" parse x
    maybe (return ()) (printStmt . fmap (\y -> (y, conversion y)) ) x'

parseIO :: String -> (String -> ParseResult a) -> String -> IO (Maybe a)
parseIO f p x = case p x of
                     Failed e  -> do putStrLn (f++": "++e)
                                     return Nothing
                     Ok r      -> return (Just r)

compileFile :: State -> String -> IO State
compileFile = undefined

handleProg :: Prog -> IO State
handleProg p = undefined

printStmt :: [(Comm, t0)] -> IO ()
printStmt = undefined

conversion :: Comm -> t0
conversion = undefined
