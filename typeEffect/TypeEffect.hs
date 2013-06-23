
module TypeEffect where

import Data.Map as M
import Data.Set
import Data.Maybe
import Debug.Trace

import AST
import Parser

import Control.Monad.State

type LS = State Int

data SType = Int
           | Bool
           | Func SType SType SAnn
           | PairT SType SType SAnn
           | ListT SType SAnn 
           | TVar TVar deriving (Eq, Show, Ord)

data TVar = TV Int deriving (Eq, Show, Ord)

data AVar = AV Int deriving (Eq, Show, Ord)

data SAnn = EmptySet
          | Union SAnn SAnn
          | AVar AVar
          | Ann Int deriving (Eq, Show, Ord)

type STEnv = M.Map Var SType 

type TSubst = (M.Map TVar SType, M.Map AVar AVar)

type Constraint = [(AVar, SAnn)]

idSubst :: TSubst
idSubst = (M.empty, M.empty)

substEnv :: STEnv -> TSubst -> STEnv
substEnv e s = M.fromList $ Prelude.map (\(k, a) -> (k, subst a s)) (M.toList e)

subst :: SType -> TSubst -> SType
subst (TVar v) (ts, _) = if M.member v ts 
                              then fromJust (M.lookup v ts) 
                              else (TVar v)
subst (Func t1 t2 a) ts = Func (subst t1 ts) (subst t2 ts) a
subst (PairT t1 t2 a) ts = PairT (subst t1 ts) (subst t2 ts) a
subst (ListT t a) ts = ListT (subst t ts) a
subst t _ = t

substA :: AVar -> TSubst -> AVar
substA v (_, as) = if M.member v as 
                   then fromJust (M.lookup v as)
                   else v
                        
substC :: Constraint -> TSubst -> Constraint
substC [] _ = []
substC ((b, p):cs) s = (substA b s, substAnn p s) : (substC cs s)
  
substAnn :: SAnn -> TSubst -> SAnn
substAnn (Union a1 a2) s = Union (substAnn a1 s) (substAnn a2 s)
substAnn (AVar a) s = AVar $ substA a s
substAnn a _ = a

combine :: TSubst -> TSubst -> TSubst
combine (ts1, as1) (ts2, as2) = (M.union ts1 ts2, M.union as1 as2)

contains :: SType -> TVar -> Bool
contains (TVar v1) v2 = v1 == v2
contains (Func t1 t2 _) v = contains t1 v || contains t2 v
contains (PairT t1 t2 _) v = contains t1 v || contains t2 v
contains (ListT t _) v = contains t v
contains _ _ = False

argType :: Op -> SType
argType And = Bool
argType Or = Bool
argType _ = Int

resType :: Op -> SType
resType Plus = Int
resType Minus = Int
resType Times = Int
resType _ = Bool

unify :: SType -> SType -> TSubst
unify Int Int = idSubst
unify Bool Bool = idSubst
unify (Func t1 t2 (AVar b1)) (Func t3 t4 (AVar b2)) = combine s2 (combine s1 s0)
  where s0 = (M.empty, M.insert b2 b1 M.empty)
        s1 = unify (subst t1 s0) (subst t3 s0)
        s2 = unify (subst (subst t2 s0) s1) (subst (subst t4 s0) s1)
unify (PairT t1 t2 (AVar b1)) (PairT t3 t4 (AVar b2)) = combine s2 (combine s1 s0)
  where s0 = (M.empty, M.insert b2 b1 M.empty) -- TODO
        s1 = unify (subst t1 s0) (subst t3 s0) -- TODO
        s2 = unify (subst (subst t2 s0) s1) (subst (subst t4 s0) s1) -- TODO
unify (ListT t1 (AVar b1)) (ListT t2 (AVar b2)) = combine s1 s0
  where s0 = (M.empty, M.insert b2 b1 M.empty) -- TODO
        s1 = unify (subst t1 s0) (subst t2 s0) -- TODO
unify (TVar v) t = if t == (TVar v) 
                   then result
                   else if not (contains t v) 
                        then result
                        else error "Error in unify: var t case"
  where result = (M.insert v t M.empty, M.empty)
unify t (TVar v) = unify (TVar v) t
unify t1 t2 = error $ "Error in unify: other case" ++ (show t1) ++ "  " ++ (show t2)

getAV :: LS AVar
getAV = do
  current <- get
  put (current + 1)
  return $ AV current
  
getTV :: LS TVar
getTV = do
  current <- get
  put (current + 1)
  return $ TV current
  
insertTypeMap :: TSubst -> TVar -> SType -> TSubst
insertTypeMap (ts, as) v t = (M.insert v t ts, as)

infer :: STEnv -> LTerm -> LS (SType, TSubst, Constraint)
infer e t = do
  (ty, sub, constr) <- infer' e t
  let res = solveConstr constr
      finTy = applyConstr ty res
  return $ trace ("\n\nFor term " ++ (show t) ++ " get Type " ++ (show ty) ++ " and final type " ++ (show finTy) ++ " and subst " ++ (show sub) ++" and constr " ++ (show constr)) (ty, sub, constr)

infer' _ (LConst _ (CNum _)) = return (Int, idSubst, [])
infer' _ (LConst _ CTrue) = return (Bool, idSubst, [])
infer' _ (LConst _ CFalse) = return (Bool, idSubst, [])
infer' e (LIdent _ v) = if M.member v e
                        then return (fromJust (M.lookup v e), idSubst, [])
                        else error $ "Var not found in environment: " ++ (show v)
infer' e (LFn l v e0) = do
  ax <- getTV
  (t0, s0, c0) <- infer (M.insert v (TVar ax) e) e0
  b0 <- getAV
  let axs0 = subst (TVar ax) s0
      resT = Func axs0 t0 (AVar b0)
  return (resT, s0, (b0, Ann l) : c0 )
infer' e (LFun l f v e0) = do
  ax <- getTV
  a0 <- getTV
  b0 <- getAV
  let e' = M.insert v (TVar ax) e
      e'' = M.insert f (Func (TVar ax) (TVar a0)  (AVar b0)) e'
  (t0, s0, c0) <- infer e'' e0
  let s1 = unify t0 (subst (TVar a0) s0)
      resT = Func (subst (subst (TVar ax) s0) s1) (subst t0 s1) (AVar (substA (substA b0 s0) s1))
      resC = (substA (substA b0 s0) s1, Ann l) : (substC c0 s1)
  return (resT, combine s1 s0, resC)
infer' e (LApp l e1 e2) = do
  (t1, s1, c1) <- infer e e1
  (t2, s2, c2) <- infer (substEnv e s1) e2
  a <- getTV
  b <- getAV
  let s3 = unify (subst t1 s2) (Func t2 (TVar a) (AVar b))
  return (subst (TVar a) s3, combine s3 (combine s2 s1), (substC (substC c1 s2) s3) ++ (substC c2 s3))
infer' e (LIf l e0 e1 e2) = do
  (t0, s0, c0) <- infer e e0
  (t1, s1, c1) <- infer (substEnv e s0) e1
  (t2, s2, c2) <- infer (substEnv (substEnv e s0) s1) e2
  let s3 = unify (subst (subst t0 s1) s2) Bool
      s4 = unify (subst t2 s3) (subst (subst t1 s2) s3)
      retS = combine s4 (combine s3 (combine s2 (combine s1 s0)))
      retC0 = substC (substC (substC (substC c0 s1) s2) s3) s4
      retC1 = substC (substC (substC c1 s2) s3) s4
      retC2 = substC (substC c2 s3) s4
  return (subst (subst t2 s3) s4, retS, retC0 ++ retC1 ++ retC2)
infer' e (LLet l v e1 e2) = do
  (t1, s1, c1) <- infer e e1
  (t2, s2, c2) <- infer (M.insert v t1 (substEnv e s1)) e2
  return (t2, combine s2 s1, (substC c1 s2) ++ c2)
infer' e (LBinop l o e1 e2) = do
  (t1, s1, c1) <- infer e e1
  (t2, s2, c2) <- infer (substEnv e s1) e2
  let s3 = unify (subst t1 s2) (argType o)
      s4 = unify (subst t2 s3) (argType o)
      resS = combine s4 (combine s3 (combine s2 s1))
      resC1 = substC (substC (substC c1 s2) s3) s4 
      resC2 = substC (substC c2 s3) s4
  return (resType o, resS, resC1 ++ resC2)
infer' e (LTPair l e1 e2) = do
  (t1, s1, c1) <- infer e e1
  (t2, s2, c2) <- infer (substEnv e s1) e2
  b <- getAV
  return (PairT t1 t2 (AVar b), combine s1 s2, (substC c1 s2) ++ c2) -- TODO
infer' e (LPCase l e1 v1 v2 e2) = do
  (t1, s1, c1) <- infer e e1
  a1 <- getTV
  a2 <- getTV
  b <- getAV
  let e' = M.insert v1 (TVar a1) e
      e'' = M.insert v2 (TVar a2) e'
  (t2, s2, c2) <- infer (substEnv e'' s1) e2
  let s3 = unify t1 (PairT (subst (subst (TVar a1) s2) s1) (subst (subst (TVar a2) s2) s1) (AVar b))
  return (subst t2 s3, combine s3 (combine s2 s1), (substC (substC c1 s2) s3) ++ (substC c2 s3)) -- TODO
infer' e (LNil l) = do
  a <- getTV
  b <- getAV
  return (ListT (TVar a) (AVar b), idSubst, [(b, Ann l)]) -- TODO
infer' e (LCons l e1 e2) = do
  (t1, s1, c1) <- infer e e1
  (t2, s2, c2) <- infer (substEnv e s1) e2
  b <- getAV
  let s3 = unify (ListT t1 (AVar b)) t2
  return (subst t2 s3, combine s3 (combine s2 s1), (substC (substC c1 s2) s3) ++ (substC c2 s3)) --TODO
infer' e (LListCase l e0 v1 v2 e1 e2) = do
  (t0, s0, c0) <- infer e e0
  av1 <- getTV
  bv1 <- getAV
  bv2 <- getAV
  let s4 = unify t0 (ListT (TVar av1) (AVar bv1))
      e' = M.insert v1 (subst (TVar av1) s4) (substEnv e s0)
      e'' = M.insert v2 (ListT (subst (TVar av1) s4) (AVar bv2)) e'
  (t1, s1, c1) <- infer e'' e1
  (t2, s2, c2) <- infer (substEnv (substEnv (substEnv e s0) s1) s4) e2
  let s5 = unify t1 t2
  return (subst t2 s5, combine s5 (combine s4 (combine s2 (combine s1 s0))), (substC (substC (substC (substC c0 s1) s2) s4) s5) ++ (substC (substC (substC c1 s2) s4) s5) ++ (substC (substC c2 s4) s5)) --TODO
  
  
runInfer :: LTerm -> (SType, TSubst, Constraint)
runInfer t = let res = infer (M.empty) t
             in evalState res 0


solveConstr :: Constraint -> M.Map AVar SAnn
solveConstr cs = solveConstr' cs M.empty

solveConstr' :: Constraint -> M.Map AVar SAnn -> M.Map AVar SAnn
solveConstr' [] m = m
solveConstr' ((avar, Ann l) : cs) m = 
  case M.lookup avar m of
  Nothing -> solveConstr' cs (M.insert avar (Ann l) m)
  Just sann -> solveConstr' cs (M.insert avar (Union sann (Ann l)) m)

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

