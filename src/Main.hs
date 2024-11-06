module Main where

import System.IO
import System.Exit
import System.Environment
import System.FilePath
import System.Process

import Grammar.ParLatte
import Grammar.AbsLatte
import Frontend.SemanticAnalyze
import Frontend.ParseProgram
import Backend.Compiler


printErrorAndExit :: String -> IO ()
printErrorAndExit err = do
    hPutStrLn stderr "ERROR"
    hPutStrLn stderr err
    exitFailure


generate :: FilePath -> Program -> IO ()
generate f program = do
    let compiledProgram = compile program
    let fileLL = replaceExtension f "ll"
    let fileBC = replaceExtension f "bc"
    writeFile fileLL compiledProgram
    readProcess "llvm-as" ["-o", fileBC, fileLL] ""
    readProcess "llvm-link" ["-o", fileBC, fileBC, "./lib/runtime.bc"] ""
    exitSuccess


analyze :: FilePath -> Program -> IO ()
analyze f program = do
    let parsedProgram = parseProgram program
    analyzeProgramResult <- semanticAnalyze parsedProgram
    case analyzeProgramResult of
        Left err -> printErrorAndExit $ show err
        Right _ -> do
            hPutStrLn stderr "OK"
            generate f parsedProgram


parseFile :: FilePath -> IO ()
parseFile f = do
    fileContent <- readFile f
    case pProgram (myLexer fileContent) of
        Left err -> printErrorAndExit $ show err
        Right program -> analyze f program


main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> parseFile f
        _ -> printErrorAndExit "Incorrect parameters."
