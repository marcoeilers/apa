-- | Main.hs
--   Authors:
--   Marco Eilers
--   Bas in het Veld

module Main where

import AST
import FunParser
import ControlFlowAnalysis
import Data.Map as M
import System.Environment


main :: IO ()
main = do 
  args <- getArgs
  let fname = if (length args) > 0 then args !! 0 else error "Please provide a file name"
      level = if (length args) > 1 then read (args !! 1) else 2
  input <- readFile fname
  let res = parseString input
  case res of
    Left errs -> putStrLn $ "Parse error: " ++ errs
    Right tree -> do 
      let (_, ltree) = lconvert 0 tree
          ((ty, sub, constr), map) = runInfer level ltree
          res = solveConstr ty constr
          resTy = applyConstr ty res
      putStrLn "Success"
      printAll $ M.toList map

printAll :: [(Int, (LTerm, TEnv, SType, TSubst, Constraint))] -> IO ()
printAll [] = return ()
printAll ((l, (tm, tenv, ty, sub, constr)):rest) = do
  let constrSolvd = solveConstr ty constr
      resTy = applyConstr ty constrSolvd
  putStrLn $ "Label " ++ (show l) ++ " is term " ++ (show tm) ++ ",\nhas type " ++ (show resTy)
  printAll rest


