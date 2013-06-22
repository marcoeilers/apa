module Main where

import AST
import MyParser
--import PrettyPrint
--import Scanner
--import Text.PrettyPrint
--import Test.QuickCheck
import TypeEffect

-- test it
main :: IO ()
main = do 
        let fname = "examples/ex1.fun"
        input <- readFile fname
        let res = parseString input
        case res of
         Left errs -> putStrLn $ errs
         Right tree -> do 
           let (_, ltree) = lconvert 0 tree
               (ty, sub, constr) = runInfer ltree
           putStrLn "success"
           putStrLn "type:"
           putStrLn $ show ty
           putStrLn "constraints:"
           putStrLn $ show constr


