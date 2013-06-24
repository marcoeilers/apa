
module ControlFlowAnalysis where

import Data.Map as M
import Data.Set as S
import Data.Maybe
import Debug.Trace

import AST

import Control.Monad.State

-- We use the state monad to keep track of already used
-- type and annotation variable names.
-- First element of the pair denotes next type var, second
-- next annotation var.
-- Type vars are strings of format 'a, annotation vars '1 
type LS = State (Int, Int)

-- Gets a fresh annotation variable
getAV :: LS AVar
getAV = do
  (curTV, curAV) <- get
  put (curTV, curAV + 1)
  return $ AV ("'" ++ (show curAV))
  
-- Gets a fresh type variable
getTV :: LS TVar
getTV = do
  (curTV, curAV) <- get
  put (curTV + 1, curAV)
  let varString = getTVarString curTV
  return $ TV varString

-- Converts an integer to a string denoting a type var
getTVarString :: Int -> String
getTVarString i = if i < 26
                  then ("'" ++ [['a'..'z'] !! i])
                  else ("'" ++ getTVarString (i - 26))

-- Data type definitions follow NNH p. 306f

-- Simple types
data SType = Int
           | Bool
           | Func SType SType SAnn
           | PairT SType SType SAnn
           | ListT SType SAnn 
           | TVar TVar deriving (Eq, Show, Ord)

-- Type variables
data TVar = TV String deriving (Eq, Show, Ord)

data TScheme = ST SType
             | Scheme TVar TScheme

-- Annotation variables
data AVar = AV String deriving (Eq, Show, Ord)

-- Simple Annotations
data SAnn = EmptySet
          | Union SAnn SAnn
          | AVar AVar
          | Ann Int deriving (Eq, Show, Ord)

-- Type environment maps variables (strings) to types
type TEnv = M.Map Var TScheme 

-- Type substitution maps type vars to types  
-- and annotation vars to annotation vars
type TSubst = (M.Map TVar SType, M.Map AVar AVar)

-- A constraint set is a list of mappings from annotation vars
-- to annotations (ALWAYS Ann l, no vars or sets)
type Constraint = [(AVar, SAnn)]

generalise :: TEnv -> SType -> LS TScheme
generalise e t = do
  let free = findFreeVars e t
      freel = S.toList free
  generalise' t freel

generalise' :: SType -> [TVar] -> LS TScheme
generalise' t [] = return $ ST t
generalise' t (tvar : vars) = do
  a <- getTV
  ts <- generalise' t vars
  let s = (M.insert tvar (TVar a) M.empty, M.empty)
      res = substScheme ts s
  return $ Scheme a res

findFreeVars :: TEnv -> SType -> S.Set TVar
findFreeVars e (Func t1 t2 _) = S.union (findFreeVars e t1) (findFreeVars e t2)
findFreeVars e (PairT t1 t2 _) = S.union (findFreeVars e t1) (findFreeVars e t2)
findFreeVars e (ListT t _) = findFreeVars e t
findFreeVars e (TVar tv) = if envContains tv (M.toList e) then S.empty else S.singleton tv
findFreeVars _ _ = S.empty


envContains :: TVar -> [(Var, TScheme)] -> Bool
envContains tvar [] = False
envContains tvar ((v, (Scheme var ts)):maps) = if tvar == var 
                                              then envContains tvar maps
                                              else envContains tvar ((v, ts) : maps)
envContains tvar ((v, ST t) : maps) = (contains t tvar) || (envContains tvar maps)


instantiate :: TScheme -> LS SType
instantiate (Scheme tvar t) = do
  a <- getTV
  let s = (M.insert tvar (TVar a) (M.empty), M.empty)
  ti <- instantiate t
  return $ substT ti s
instantiate (ST t) = return t

-- Type substitution corresponding to the identity function
idSubst :: TSubst
idSubst = (M.empty, M.empty)

-- Applies a substitution to an environment
substEnv :: TEnv -> TSubst -> TEnv
substEnv e s = M.fromList $ Prelude.map (\(k, a) -> (k, substScheme a s)) (M.toList e)

substScheme :: TScheme -> TSubst -> TScheme
substScheme (Scheme tvar s) sub = Scheme tvar (substScheme s sub)
substScheme (ST t) sub = ST $ substT t sub 

-- Applies a substitution to a type
substT :: SType -> TSubst -> SType
substT (TVar v) (ts, _) = if M.member v ts 
                              then fromJust (M.lookup v ts) 
                              else (TVar v)
substT (Func t1 t2 a) ts = Func (substT t1 ts) (substT t2 ts) (substAnn a ts)
substT (PairT t1 t2 a) ts = PairT (substT t1 ts) (substT t2 ts) (substAnn a ts)
substT (ListT t a) ts = ListT (substT t ts) (substAnn a ts)
substT t _ = t

-- Applies a substitution to an annotation var
substA :: AVar -> TSubst -> AVar
substA v (_, as) = if M.member v as 
                   then fromJust (M.lookup v as)
                   else v
                        
-- Applies a substitution to a constraint set
substC :: Constraint -> TSubst -> Constraint
substC [] _ = []
substC ((b, p):cs) s = (substA b s, substAnn p s) : (substC cs s)
  
-- Applies a substitution to an annotation
substAnn :: SAnn -> TSubst -> SAnn
substAnn (Union a1 a2) s = Union (substAnn a1 s) (substAnn a2 s)
substAnn (AVar a) s = AVar $ substA a s
substAnn a _ = a

-- Combines two substitution into a new one
combine :: TSubst -> TSubst -> TSubst
combine (ts1, as1) (ts2, as2) = (M.union ts1 ts2, M.union as1 as2)

-- Checks if a type contains a given type variable
contains :: SType -> TVar -> Bool
contains (TVar v1) v2 = v1 == v2
contains (Func t1 t2 _) v = contains t1 v || contains t2 v
contains (PairT t1 t2 _) v = contains t1 v || contains t2 v
contains (ListT t _) v = contains t v
contains _ _ = False

-- Gets the type of both inputs to a given operator
argType :: Op -> SType
argType And = Bool
argType Or = Bool
argType _ = Int

-- Gets the type of the result of a given operator
resType :: Op -> SType
resType Plus = Int
resType Minus = Int
resType Times = Int
resType _ = Bool

-- Generates a type substitution which unifies two given types
-- Fails if impossible
-- Corresponds to U_CFA in NNH (p. 307) extended with cases for
-- lists and pairs
unify :: SType -> SType -> TSubst
unify Int Int = idSubst
unify Bool Bool = idSubst
unify (Func t1 t2 (AVar b1)) (Func t3 t4 (AVar b2)) = combine s2 (combine s1 s0)
  where s0 = (M.empty, M.insert b2 b1 M.empty)
        s1 = unify (substT t1 s0) (substT t3 s0)
        s2 = unify (substT (substT t2 s0) s1) (substT (substT t4 s0) s1)
unify (PairT t1 t2 (AVar b1)) (PairT t3 t4 (AVar b2)) = combine s2 (combine s1 s0)
  where s0 = (M.empty, M.insert b2 b1 M.empty) -- TODO
        s1 = unify (substT t1 s0) (substT t3 s0) -- TODO
        s2 = unify (substT (substT t2 s0) s1) (substT (substT t4 s0) s1) -- TODO
unify (ListT t1 (AVar b1)) (ListT t2 (AVar b2)) = combine s1 s0
  where s0 = (M.empty, M.insert b2 b1 M.empty) -- TODO
        s1 = unify (substT t1 s0) (substT t2 s0) -- TODO
unify (TVar v) t = if t == (TVar v) 
                   then result
                   else if not (contains t v) 
                        then result
                        else error "Error in unify: var t case"
  where result = (M.insert v t M.empty, M.empty)
unify t (TVar v) = unify (TVar v) t
unify t1 t2 = error $ "Error in unify: other case" ++ (show t1) ++ "  " ++ (show t2)
  

-- For a given term in a given environment,
-- infers its type, generated substitutions and constraints
-- Prints results
-- Corresponds to W_CFA in NNH (p. 310) extended with cases for
-- lists and pairs
infer :: TEnv -> LTerm -> LS (SType, TSubst, Constraint)
infer e t = do
  (ty, sub, constr) <- infer' e t
  let res = solveConstr constr
      finTy = applyConstr ty res
  return $ trace ("\n\nFor term " ++ (show t) ++ " get Type " ++ (show ty) ++ " and final type " ++ (show finTy) ++ " and substT " ++ (show sub) ++" and constr " ++ (show constr)) (ty, sub, constr)

infer' :: TEnv -> LTerm -> LS (SType, TSubst, Constraint)
infer' _ (LConst _ (CNum _)) = return (Int, idSubst, [])
infer' _ (LConst _ CTrue) = return (Bool, idSubst, [])
infer' _ (LConst _ CFalse) = return (Bool, idSubst, [])
infer' e (LIdent _ v) = 
  if M.member v e
  then do
    resT <- instantiate (fromJust (M.lookup v e))
    return (resT, idSubst, [])
  else error $ "Var not found in environment: " ++ (show v)

infer' e (LFn l v e0) = do
  ax <- getTV
  (t0, s0, c0) <- infer (M.insert v (ST (TVar ax)) e) e0
  b0 <- getAV
  let axs0 = substT (TVar ax) s0
      resT = Func axs0 t0 (AVar b0)
  return (resT, s0, (b0, Ann l) : c0 )

infer' e (LFun l f v e0) = do
  ax <- getTV
  a0 <- getTV
  b0 <- getAV
  let e' = M.insert v (ST (TVar ax)) e
      e'' = M.insert f (ST (Func (TVar ax) (TVar a0)  (AVar b0))) e'
  (t0, s0, c0) <- infer e'' e0
  let s1 = unify t0 (substT (TVar a0) s0)
      resT = Func (substT (substT (TVar ax) s0) s1) (substT t0 s1) (AVar (substA (substA b0 s0) s1))
      resC = (substA (substA b0 s0) s1, Ann l) : (substC c0 s1)
  return (resT, combine s1 s0, resC)

infer' e (LApp l e1 e2) = do
  (t1, s1, c1) <- infer e e1
  (t2, s2, c2) <- infer (substEnv e s1) e2
  a <- getTV
  b <- getAV
  let s3 = unify (substT t1 s2) (Func t2 (TVar a) (AVar b))
  return (substT (TVar a) s3, combine s3 (combine s2 s1), (substC (substC c1 s2) s3) ++ (substC c2 s3))

infer' e (LIf l e0 e1 e2) = do
  (t0, s0, c0) <- infer e e0
  (t1, s1, c1) <- infer (substEnv e s0) e1
  (t2, s2, c2) <- infer (substEnv (substEnv e s0) s1) e2
  let s3 = unify (substT (substT t0 s1) s2) Bool
      s4 = unify (substT t2 s3) (substT (substT t1 s2) s3)
      retS = combine s4 (combine s3 (combine s2 (combine s1 s0)))
      retC0 = substC (substC (substC (substC c0 s1) s2) s3) s4
      retC1 = substC (substC (substC c1 s2) s3) s4
      retC2 = substC (substC c2 s3) s4
  return (substT (substT t2 s3) s4, retS, retC0 ++ retC1 ++ retC2)

infer' e (LLet l v e1 e2) = do
  (t1, s1, c1) <- infer e e1
  generalised <- generalise e t1
  (t2, s2, c2) <- infer (M.insert v generalised (substEnv e s1)) e2
  return (t2, combine s2 s1, (substC c1 s2) ++ c2)

infer' e (LBinop l o e1 e2) = do
  (t1, s1, c1) <- infer e e1
  (t2, s2, c2) <- infer (substEnv e s1) e2
  let s3 = unify (substT t1 s2) (argType o)
      s4 = unify (substT t2 s3) (argType o)
      resS = combine s4 (combine s3 (combine s2 s1))
      resC1 = substC (substC (substC c1 s2) s3) s4 
      resC2 = substC (substC c2 s3) s4
  return (resType o, resS, resC1 ++ resC2)

-- Added: cases for lists and pairs
infer' e (LTPair l e1 e2) = do
  (t1, s1, c1) <- infer e e1
  (t2, s2, c2) <- infer (substEnv e s1) e2
  b <- getAV
  return (PairT t1 t2 (AVar b), combine s1 s2, (b, Ann l) : ((substC c1 s2) ++ c2)) -- TODO

infer' e (LPCase l e1 v1 v2 e2) = do
  (t1, s1, c1) <- infer e e1
  a1 <- getTV
  a2 <- getTV
  b <- getAV
  let e' = M.insert v1 (ST (TVar a1)) e
      e'' = M.insert v2 (ST (TVar a2)) e'
  (t2, s2, c2) <- infer (substEnv e'' s1) e2
  let s3 = unify (PairT (substT (substT (TVar a1) s2) s1) (substT (substT (TVar a2) s2) s1) (AVar b)) t1
  return (substT t2 s3, combine s3 (combine s2 s1), (substC (substC c1 s2) s3) ++ (substC c2 s3)) -- TODO

infer' e (LNil l) = do
  a <- getTV
  b <- getAV
  return (ListT (TVar a) (AVar b), idSubst, [(b, Ann l)]) -- TODO

infer' e (LCons l e1 e2) = do
  (t1, s1, c1) <- infer e e1
  (t2, s2, c2) <- infer (substEnv e s1) e2
  b <- getAV
  let s3 = unify (ListT t1 (AVar b)) t2
      resC = (b, Ann l) : ((substC (substC c1 s2) s3) ++ (substC c2 s3))
  return (substT t2 s3, combine s3 (combine s2 s1), resC) --TODO

infer' e (LListCase l e0 v1 v2 e1 e2) = do
  (t0, s0, c0) <- infer e e0
  av1 <- getTV
  bv1 <- getAV
  bv2 <- getAV
  let s4 = unify t0 (ListT (TVar av1) (AVar bv1))
      e' = M.insert v1 (ST (substT (TVar av1) s4)) (substEnv e s0)
      e'' = M.insert v2 (ST (ListT (substT (TVar av1) s4) (AVar bv2))) e'
  (t1, s1, c1) <- infer e'' e1
  (t2, s2, c2) <- infer (substEnv (substEnv (substEnv e s0) s1) s4) e2
  let s5 = unify t1 t2
      resS = combine s5 (combine s4 (combine s2 (combine s1 s0)))
      resC0 = (substC (substC (substC (substC c0 s1) s2) s4) s5)
      resC1 = (substC (substC (substC c1 s2) s4) s5)
      resC2 = (substC (substC c2 s4) s5)
  return (substT t2 s5, resS, resC0 ++ resC1 ++ resC2) --TODO
  
-- Calls infer with an empty environment  
runInfer :: LTerm -> (SType, TSubst, Constraint)
runInfer t = let res = infer (M.empty) t
             in evalState res (0, 0)


-- Translates a constraint set into a mapping from annotation vars
-- to annotations
solveConstr :: Constraint -> M.Map AVar SAnn
solveConstr cs = solveConstr' cs M.empty

solveConstr' :: Constraint -> M.Map AVar SAnn -> M.Map AVar SAnn
solveConstr' [] m = m
solveConstr' ((avar, Ann l) : cs) m = 
  case M.lookup avar m of
  Nothing -> solveConstr' cs (M.insert avar (Ann l) m)
  Just sann -> solveConstr' cs (M.insert avar (Union sann (Ann l)) m)

-- Applies a mapping generated by solveConstr to a type 
-- to get the final type
applyConstr :: SType -> M.Map AVar SAnn -> SType
applyConstr (Func t1 t2 (AVar a)) m = 
  case M.lookup a m of
  Nothing -> Func t1' t2' (AVar a)
  Just a' -> Func t1' t2' a'
  where t1' = applyConstr t1 m
        t2' = applyConstr t2 m
applyConstr (PairT t1 t2 (AVar a)) m =
  case M.lookup a m of
  Nothing -> PairT t1' t2' (AVar a)
  Just a' -> PairT t1' t2' a'
  where t1' = applyConstr t1 m
        t2' = applyConstr t2 m
applyConstr (ListT t1 (AVar a)) m =
  case M.lookup a m of
  Nothing -> ListT t1' (AVar a)
  Just a' -> ListT t1' a'
  where t1' = applyConstr t1 m
applyConstr t _ = t

