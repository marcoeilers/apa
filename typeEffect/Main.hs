module Main where

import AST
import MyParser
import ControlFlowAnalysis

-- test it
main :: IO ()
main = do 
        let fname = "examples/ex2.fun"
        input <- readFile fname
        let res = parseString input
        case res of
         Left errs -> putStrLn $ errs
         Right tree -> do 
           let (_, ltree) = lconvert 0 tree
               (ty, sub, constr) = runInfer ltree
               res = solveConstr constr
               resTy = applyConstr ty res
           putStrLn "success"
           putStrLn "type:"
           putStrLn $ show ty
           putStrLn "constraints:"
           putStrLn $ show constr
           putStrLn "type with constraints:"
           putStrLn $ show resTy


