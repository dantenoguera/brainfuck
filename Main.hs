{-# OPTIONS -XRecordWildCards #-}
module Main where
import Prelude hiding (print)
import System.Console.Readline
import Data.List
import Data.Char
import System.IO hiding (print)
import System.Environment
import Common
--import PrettyPrinter
import Eval
import Parse
size = 10

data Command = Compile CompileForm
             | Print String
             | Recompile
             | Quit
             | Continue
             | Help
             | Noop
             deriving Show

data CompileForm = CompileInteractive  String
                 | CompileFile         String
                 deriving Show


data InteractiveCommand = Cmd [String] String (String -> Command) String

commands :: [InteractiveCommand]
commands =
    [ Cmd [":load"]        "<file>"  (Compile . CompileFile)
                                "Cargar un programa desde un archivo",
      Cmd [":reload"]      "<file>"  (const Recompile) "Volver a cargar el último archivo",
      Cmd [":quit"]        ""        (const Quit)   "Salir del intérprete",
      Cmd [":help",":?"]   ""        (const Help)   "Mostrar esta lista de comandos" ]

main :: IO ()
main = readEvalPrintLoop


readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
    maybeLine <- readline "Bf> "
    case maybeLine of
        Nothing     -> return () -- EOF / control-d
        Just line -> do addHistory line
                        cmd <- interpretCommand line
                        resp <- handleCommand cmd
                        case resp of
                            Quit -> return ()
                            _    -> do putStrLn $ show cmd
                                       readEvalPrintLoop

interpretCommand :: String -> IO Command
interpretCommand str =
    if isPrefixOf ":" str then
        do let (cmd, t') = break isSpace str
               t             = dropWhile isSpace t'
           let  matching  =  filter (\ (Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
           case matching of
             []  ->  do  putStrLn ("Comando desconocido `" ++
                                   cmd                     ++
                                   "'. Escriba :? para recibir ayuda.")
                         return Noop
             [Cmd _ _ f _] ->  do  return (f t)
             _   ->  do  putStrLn ("Comando ambigüo, podría ser " ++
                                   concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
                         return Noop

     else
       return (Compile (CompileInteractive str))

handleCommand :: Command -> IO Command
handleCommand cmd =
    case cmd of
        Quit -> return Quit
        Help -> putStr (helpTxt commands) >> return Continue
        Compile c -> case c of
            CompileInteractive s -> compilePhrase s >> return Continue
            _ -> return Continue

compilePhrase :: String -> IO ()
compilePhrase s = do
    maybep <- parseIO "<interactive>" parserProg s
    case maybep of
        Nothing -> return ()
        Just p -> do r <- evalProg p size
                     case r of
                        Raise str -> putStrLn str
                        Return m -> putStrLn $ show m



helpTxt :: [InteractiveCommand] -> String
helpTxt cs =
       unlines (map (\ (Cmd c a _ d) ->
                     let  ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
                     in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

parseIO :: String -> (String -> ParseResult a) -> String -> IO (Maybe a)
parseIO f p x = case p x of
                    Failed e  -> do putStrLn (f++": "++e)
                                    return Nothing
                    Ok r      -> return (Just r)
