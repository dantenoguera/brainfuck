{-# OPTIONS -XRecordWildCards #-}
module Main where
import System.Console.Readline
import Common
import Parse
import Eval

main :: IO ()
main = readevalprint


readevalprint :: IO ()
readevalprint = do
    maybeLine <- readline "Bf> "
    case maybeLine of
        Nothing     -> return () -- EOF / control-d
        Just "exit" -> return ()
        Just line -> do addHistory line
                        case bfparser line of
                            Failed e -> putStrLn e
                            Ok r -> putStrLn (show r)
                        readevalprint
