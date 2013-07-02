module Main where

import AST
import MyParser
import ControlFlowAnalysis
import Data.Map as M
import System.Environment

-- test it
main :: IO ()
main = do 
        fname <- getArgs
        input <- readFile (head fname)
        let res = parseString input
        case res of
         Left errs -> putStrLn $ errs
         Right tree -> do 
           let (_, ltree) = lconvert 0 tree
               ((ty, sub, constr), map) = runInfer ltree
               res = solveConstr constr
               resTy = applyConstr ty res
           putStrLn "success"
           putStrLn "type of the entire expression:"
           putStrLn $ show ty
           putStrLn "constraints:"
           putStrLn $ show constr
           putStrLn "type with constraints:"
           putStrLn $ show resTy
           putStrLn "\n\ntypes for other labels:"
           printAll $ M.toList map

printAll :: [(Int, (LTerm, SType, TSubst, Constraint))] -> IO ()
printAll [] = return ()
printAll ((l, (tm, ty, sub, constr)):rest) = do
  let constrSolvd = solveConstr constr
      resTy = applyConstr ty constrSolvd
  putStrLn $ "Label " ++ (show l) ++ " is term " ++ (show tm) ++ ",\nhas type " ++ (show resTy)
  printAll rest


