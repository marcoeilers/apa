
module ControlFlowAnalysis where

import Data.Map as M
import Data.Set as S
import Data.List as L
import Data.Maybe
import Debug.Trace

import AST

import Control.Monad.State

-- We use the state monad to keep track of already used
-- type and annotation variable names.
-- First element of the pair denotes next type var, second
-- next annotation var.
-- Type vars are strings of format 'a, annotation vars '1 
type LS = State (Int, Int, M.Map Int (LTerm, TEnv, SType, TSubst, Constraint))

-- Gets a fresh annotation variable
getAV :: LS AVar
getAV = do
  (curTV, curAV, m) <- get
  put (curTV, curAV + 1, m)
  return $ AV ("'" ++ (show curAV))
  
-- Gets a fresh type variable
getTV :: LS TVar
getTV = do
  (curTV, curAV, m) <- get
  put (curTV + 1, curAV, m)
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

data QualType = SType SType
              | ConstrType Constraint SType deriving (Eq, Show, Ord)

-- Type variables
data TVar = TV String deriving (Eq, Show, Ord)

data Scheme =  QT QualType
             | TScheme TVar Scheme
             | AScheme AVar Scheme deriving (Eq, Show, Ord)

-- Annotation variables
data AVar = AV String deriving (Eq, Show, Ord)

data SAnn = Set (S.Set SAnn)
          | AVar AVar
          | Ann Int deriving (Eq, Show, Ord)

unionSAnn :: SAnn -> SAnn -> SAnn
unionSAnn (Set s1) (Set s2) = Set $ S.union s1 s2
unionSAnn (Set s) a = Set $ S.insert a s
unionSAnn a (Set s) = Set $ S.insert a s
unionSAnn a1 a2 = Set $ S.insert a1 (S.singleton a2)

-- Type environment maps variables (strings) to types
type TEnv = M.Map Var Scheme 

-- Type substitution maps type vars to types  
-- and annotation vars to annotation vars
type TSubst = (M.Map TVar SType, M.Map AVar AVar)

-- A constraint set is a list of mappings from annotation vars
-- to annotations (ALWAYS Ann l, no vars or sets)
type Constraint = [(AVar, SAnn)]


-- Abstracts over all type & annotation vars in the type 
-- which are not bound in the environment
generalise :: Int -> TEnv -> SType -> SAnn -> Constraint -> LS Scheme
generalise level e t a c = do
  let freeT = findFreeTVars e t
      freeTl = if level > 0 then S.toList freeT else []
      notGen = extractAnnVars a
      freeA = findFreeAVars e t
      freeAl = if level > 1 then S.toList freeA else []
  genT <- generalise' t c freeTl
  generalise'' genT freeAl notGen
  
-- Extracts all annotation variables used in an annotation
extractAnnVars :: SAnn -> [AVar]
extractAnnVars (AVar a) = [a]
extractAnnVars (Ann _) = []
extractAnnVars (Set anns) = L.concat $ L.map extractAnnVars (S.toList anns)


-- Abstracts over a list of type vars
generalise' :: SType -> Constraint -> [TVar] -> LS Scheme
generalise' t [] [] = return $ QT (SType t)
generalise' t cs [] = return $ QT (ConstrType cs t)
generalise' t cs (tvar : vars) = do
  a <- getTV
  ts <- generalise' t cs vars
  let s = (M.insert tvar (TVar a) M.empty, M.empty)
      res = substScheme ts s
  return $ TScheme a res

-- Abstracts over a list of annotation vars
generalise'' :: Scheme -> [AVar] -> [AVar] -> LS Scheme
generalise'' s [] exempt = return s
generalise'' s (avar : vars) exempt = if L.elem avar exempt then return s else do
  b <- getAV
  ts <- generalise'' s vars exempt
  let s = (M.empty, M.insert avar b M.empty)
      res = substScheme ts s
  return $ AScheme b res


-- Finds the type vars in a type that are not bound in an environment
findFreeTVars :: TEnv -> SType -> S.Set TVar
findFreeTVars e (Func t1 t2 _) = S.union (findFreeTVars e t1) (findFreeTVars e t2)
findFreeTVars e (PairT t1 t2 _) = S.union (findFreeTVars e t1) (findFreeTVars e t2)
findFreeTVars e (ListT t _) = findFreeTVars e t
findFreeTVars e (TVar tv) = if envContains tv (M.toList e) then S.empty else S.singleton tv
findFreeTVars _ _ = S.empty

findFreeAVars :: TEnv -> SType -> S.Set AVar
findFreeAVars e (Func t1 t2 (AVar a)) = if envContainsA a (M.toList e) 
                                 then sub else S.insert a sub
  where sub = (S.union (findFreeAVars e t1) (findFreeAVars e t2))
findFreeAVars e (PairT t1 t2 (AVar a)) = if envContainsA a (M.toList e) 
                                  then sub else S.insert a sub
  where sub = (S.union (findFreeAVars e t1) (findFreeAVars e t2))
findFreeAVars e (ListT t1 (AVar a)) = if envContainsA a (M.toList e) 
                               then sub else S.insert a sub
  where sub = (findFreeAVars e t1)
findFreeAVars _ _ = S.empty

-- Checks if an environment contains a given type var
envContains :: TVar -> [(Var, Scheme)] -> Bool
envContains tvar [] = False
envContains tvar ((v, (TScheme var ts)):maps) = if tvar == var 
                                              then envContains tvar maps
                                              else envContains tvar ((v, ts) : maps)
envContains tvar ((v, (AScheme _ ts)):maps) = envContains tvar ((v, ts) : maps)
envContains tvar ((_, QT (SType t)) : maps) = (contains t tvar) || (envContains tvar maps)
envContains tvar ((_, QT (ConstrType _ t)) : maps) = (contains t tvar) || (envContains tvar maps)

-- Checks if an environment contains a given annotation var
envContainsA :: AVar -> [(Var, Scheme)] -> Bool
envContainsA avar [] = False
envContainsA avar ((v, (TScheme _ ts)):maps) = envContainsA avar ((v, ts) : maps)
envContainsA avar ((v, (AScheme var ts)):maps) = if avar == var
                                                 then envContainsA avar maps
                                                 else envContainsA avar ((v, ts) : maps)
envContainsA avar ((_, QT (SType t)) : maps) = (containsA t avar) || (envContainsA avar maps)
envContainsA avar ((_, QT (ConstrType c t)) : maps) = (containsA t avar) || (envContainsA avar maps) || (constrContainsA avar c)

constrContainsA :: AVar -> Constraint -> Bool
constrContainsA avar [] = False
constrContainsA avar ((a1, _) : rest) = (avar == a1) || (constrContainsA avar rest)

-- Instantiates a type scheme, i.e. replaces all quantified vars
-- with fresh type vars
instantiate :: Scheme -> LS (Constraint, SType)
instantiate (TScheme tvar t) = do
  a <- getTV
  let s = (M.insert tvar (TVar a) (M.empty), M.empty)
  (ct, ti) <- instantiate t
  return $ (substC ct s, substT ti s)
instantiate (AScheme avar t) = do
  b <- getAV
  let s = (M.empty, M.insert avar b M.empty)
  (ct, ti) <- instantiate t
  return $ (substC ct s, substT ti s)
instantiate (QT (SType t)) = return ([], t)
instantiate (QT (ConstrType c t)) = return (c, t)

-- Type substitution corresponding to the identity function
idSubst :: TSubst
idSubst = (M.empty, M.empty)

-- Applies a substitution to an environment
substEnv :: TEnv -> TSubst -> TEnv
substEnv e s = M.fromList $ Prelude.map (\(k, a) -> (k, substScheme a s)) (M.toList e)

-- Applies a substitution to a type scheme
substScheme :: Scheme -> TSubst -> Scheme
substScheme (TScheme tvar s) sub = TScheme tvar (substScheme s sub)
substScheme (AScheme avar s) sub = AScheme avar (substScheme s sub)
substScheme (QT (SType t)) sub = QT $ SType $ substT t sub 
substScheme (QT (ConstrType c t)) sub = QT $ ConstrType (substC c sub) (substT t sub) 

-- Applies a substitution to a type
substT :: SType -> TSubst -> SType
substT (TVar v) (ts, _) = if M.member v ts 
                              then fromJust (M.lookup v ts) 
                              else (TVar v)
substT (Func t1 t2 a) ts = Func (substT t1 ts) (substT t2 ts) (substAnn ts a)
substT (PairT t1 t2 a) ts = PairT (substT t1 ts) (substT t2 ts) (substAnn ts a)
substT (ListT t a) ts = ListT (substT t ts) (substAnn ts a)
substT t _ = t

-- Applies a substitution to an annotation var
substA :: AVar -> TSubst -> AVar
substA v (_, as) = if M.member v as 
                   then fromJust (M.lookup v as)
                   else v
                        
-- Applies a substitution to a constraint set
substC :: Constraint -> TSubst -> Constraint
substC [] _ = []
substC ((b, p):cs) s = (substA b s, substAnn s p) : (substC cs s)
  
-- Applies a substitution to an annotation
substAnn :: TSubst -> SAnn -> SAnn
substAnn s (AVar a) = AVar $ (substA a s)
substAnn s (Set anns) = Set $ S.map (substAnn s) anns
substAnn s ann = ann


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

-- Checks if a type contains a given annotation variable
containsA :: SType -> AVar -> Bool
containsA (Func t1 t2 (AVar a)) v = (a == v) || (containsA t1 v) || (containsA t2 v)
containsA (PairT t1 t2 (AVar a)) v = (a == v) || (containsA t1 v) || (containsA t2 v)
containsA (ListT t1 (AVar a)) v = (a == v) || (containsA t1 v)
containsA _ _ = False

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

-- Gets the top level annotation variable of a type, if any
getTopAnn :: SType -> SAnn
getTopAnn (Func _ _ a) = case a of
  (AVar av) -> AVar av
  _ -> Set S.empty
getTopAnn (PairT _ _ a) = Set S.empty
getTopAnn (ListT _ a) = Set S.empty
getTopAnn _ = Set S.empty


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
  where s0 = (M.empty, M.insert b2 b1 M.empty) 
        s1 = unify (substT t1 s0) (substT t3 s0) 
        s2 = unify (substT (substT t2 s0) s1) (substT (substT t4 s0) s1)
unify (ListT t1 (AVar b1)) (ListT t2 (AVar b2)) = combine s1 s0
  where s0 = (M.empty, M.insert b2 b1 M.empty) 
        s1 = unify (substT t1 s0) (substT t2 s0) 
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
infer :: Int -> TEnv -> LTerm -> LS (SType, TSubst, Constraint)
infer lv e t = do
  (ty, sub, constr) <- infer' lv e t
  (n, m, map) <- get
  put (n, m, M.insert (getLabel t) (t, e, ty, sub, constr) map)
  return $ (ty, sub, constr)

infer' :: Int -> TEnv -> LTerm -> LS (SType, TSubst, Constraint)
infer' _ _ (LConst _ (CNum _)) = return (Int, idSubst, [])
infer' _ _ (LConst _ CTrue) = return (Bool, idSubst, [])
infer' _ _ (LConst _ CFalse) = return (Bool, idSubst, [])
infer' _ e (LIdent _ v) = 
  if M.member v e
  then do
    (c, resT) <- instantiate (fromJust (M.lookup v e))
    return (resT, idSubst, c)
  else error $ "Var not found in environment: " ++ (show v)

infer' lv e (LFn l v e0) = do
  ax <- getTV
  (t0, s0, c0) <- infer lv (M.insert v (QT (SType (TVar ax))) e) e0
  b0 <- getAV
  let axs0 = substT (TVar ax) s0
      resT = Func axs0 t0 (AVar b0)
  return (resT, s0, (b0, Ann l) : c0 )

infer' lv e (LFun l f v e0) = do
  ax <- getTV
  a0 <- getTV
  b0 <- getAV
  let e' = M.insert v (QT (SType (TVar ax))) e
      e'' = M.insert f (QT (SType (Func (TVar ax) (TVar a0)  (AVar b0)))) e'
  (t0, s0, c0) <- infer lv e'' e0
  let s1 = unify t0 (substT (TVar a0) s0)
      resT = Func (substT (substT (TVar ax) s0) s1) (substT t0 s1) (AVar (substA (substA b0 s0) s1))
      resC = (substA (substA b0 s0) s1, Ann l) : (substC c0 s1)
  return (resT, combine s1 s0, resC)

infer' lv e (LApp l e1 e2) = do
  (t1, s1, c1) <- infer lv e e1
  (t2, s2, c2) <- infer lv (substEnv e s1) e2
  a <- getTV
  b <- getAV
  let s3 = unify (substT t1 s2) (Func t2 (TVar a) (AVar b))
  return (substT (TVar a) s3, combine s3 (combine s2 s1), (substC (substC c1 s2) s3) ++ (substC c2 s3))

infer' lv e (LIf l e0 e1 e2) = do
  (t0, s0, c0) <- infer lv e e0
  (t1, s1, c1) <- infer lv (substEnv e s0) e1
  (t2, s2, c2) <- infer lv (substEnv (substEnv e s0) s1) e2
  let s3 = unify (substT (substT t0 s1) s2) Bool
      s4 = unify (substT t2 s3) (substT (substT t1 s2) s3)
      retS = combine s4 (combine s3 (combine s2 (combine s1 s0)))
      retC0 = substC (substC (substC (substC c0 s1) s2) s3) (trace ("-------------!" ++ (show s4)) s4)
      retC1 = substC (substC (substC c1 s2) s3) s4
      retC2 = substC (substC c2 s3) s4
  return (substT (substT t2 s3) s4, retS, retC0 ++ retC1 ++ retC2)

infer' lv e (LLet l v e1 e2) = do
  (t1, s1, c1) <- infer lv e e1
  generalised <- generalise lv e t1 (getTopAnn t1) c1
  (t2, s2, c2) <- infer lv (M.insert v generalised (substEnv e s1)) e2
  return (t2, combine s2 s1, (substC c1 s2) ++ c2)

infer' lv e (LBinop l o e1 e2) = do
  (t1, s1, c1) <- infer lv e e1
  (t2, s2, c2) <- infer lv (substEnv e s1) e2
  let s3 = unify (substT t1 s2) (argType o)
      s4 = unify (substT t2 s3) (argType o)
      resS = combine s4 (combine s3 (combine s2 s1))
      resC1 = substC (substC (substC c1 s2) s3) s4 
      resC2 = substC (substC c2 s3) s4
  return (resType o, resS, resC1 ++ resC2)

-- Added: cases for lists and pairs
infer' lv e (LTPair l e1 e2) = do
  (t1, s1, c1) <- infer lv e e1
  (t2, s2, c2) <- infer lv (substEnv e s1) e2
  b <- getAV
  return (PairT t1 t2 (AVar b), combine s1 s2, (b, Ann l) : ((substC c1 s2) ++ c2)) 

infer' lv e (LPCase l e1 v1 v2 e2) = do
  (t1, s1, c1) <- infer lv e e1
  a1 <- getTV
  a2 <- getTV
  b <- getAV
  let e' = M.insert v1 (QT (SType (TVar a1))) e
      e'' = M.insert v2 (QT (SType (TVar a2))) e'
  (t2, s2, c2) <- infer lv (substEnv e'' s1) e2
  let s3 = unify (PairT (substT (substT (TVar a1) s2) s1) (substT (substT (TVar a2) s2) s1) (AVar b)) t1
  return (substT t2 s3, combine s3 (combine s2 s1), (substC (substC c1 s2) s3) ++ (substC c2 s3)) 

infer' lv e (LNil l) = do
  a <- getTV
  b <- getAV
  return (ListT (TVar a) (AVar b), idSubst, [(b, Ann l)]) 

infer' lv e (LCons l e1 e2) = do
  (t1, s1, c1) <- infer lv e e1
  (t2, s2, c2) <- infer lv (substEnv e s1) e2
  b <- getAV
  let s3 = unify (ListT t1 (AVar b)) t2
      resC = (b, Ann l) : ((substC (substC c1 s2) s3) ++ (substC c2 s3))
  return (ListT (substT t1 s3) (AVar b), combine s3 (combine s2 s1), resC) 

infer' lv e (LListCase l e0 v1 v2 e1 e2) = do
  (t0, s0, c0) <- infer lv e e0
  av1 <- getTV
  bv1 <- getAV
  bv2 <- getAV
  let s4 = unify t0 (ListT (TVar av1) (AVar bv1))
      e' = M.insert v1 (QT (SType (substT (TVar av1) s4))) (substEnv e s0)
      e'' = M.insert v2 (QT (SType (ListT (substT (TVar av1) s4) (AVar bv2)))) e'
  (t1, s1, c1) <- infer lv e'' e1
  (t2, s2, c2) <- infer lv (substEnv (substEnv (substEnv e s0) s1) s4) e2
  let s5 = unify t1 t2
      resS = combine s5 (combine s4 (combine s2 (combine s1 s0)))
      resC0 = (substC (substC (substC (substC c0 s1) s2) s4) s5)
      resC1 = (substC (substC (substC c1 s2) s4) s5)
      resC2 = (substC (substC c2 s4) s5)
  return (substT t2 s5, resS, resC0 ++ resC1 ++ resC2) 
  
-- Calls infer with an empty environment  
runInfer :: Int -> LTerm -> ((SType, TSubst, Constraint), M.Map Int (LTerm, TEnv, SType, TSubst, Constraint))
runInfer lv t = (last, all)
  where res = infer lv (M.empty) t
        (last, (_, _, all)) = runState res (0, 0, M.empty)
        


-- Translates a constraint set into a mapping from annotation vars
-- to annotations
solveConstr :: Constraint -> M.Map AVar SAnn
solveConstr cs = solveConstr' cs M.empty

solveConstr' :: Constraint -> M.Map AVar SAnn -> M.Map AVar SAnn
solveConstr' [] m = m
solveConstr' ((avar, Ann l) : cs) m = 
  case M.lookup avar m of
  Nothing -> solveConstr' cs (M.insert avar (Ann l) m)
  Just sann -> solveConstr' cs (M.insert avar (unionSAnn sann (Ann l)) m)



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

