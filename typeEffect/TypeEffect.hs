
module TypeEffect where

import Data.Map as M
import Data.Set
import Data.Maybe

import AST

import Control.Monad.State

type LS = State Int

data SType = Int
           | Bool
           | Func SType SType AVar
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
unify (Func t1 t2 b1) (Func t3 t4 b2) = combine s2 (combine s1 s0)
  where s0 = (M.empty, M.insert b2 b1 M.empty)
        s1 = unify (subst t1 s0) (subst t3 s1)
        s2 = unify (subst (subst t2 s0) s1) (subst (subst t4 s0) s1)
unify (TVar v) t = if t == (TVar v) 
                   then result
                   else if not (contains t v) 
                        then result
                        else error "Error"
  where result = (M.insert v t M.empty, M.empty)
unify t (TVar v) = unify (TVar v) t
unify _ _ = error "Error"

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
infer _ (LConst _ (CNum _)) = return (Int, idSubst, [])
infer _ (LConst _ CTrue) = return (Bool, idSubst, [])
infer _ (LConst _ CFalse) = return (Bool, idSubst, [])
infer e (LFn l v t) = do
  ax <- getTV
  (t0, s0, c0) <- infer (M.insert v (TVar ax) e) t
  b0 <- getAV
  let axs0 = subst (TVar ax) s0
  return (Func axs0 t0 b0, s0, (b0, Ann l) : c0 )
infer e (LFun l v1 v2 t) = do
  ax <- getTV
  a0 <- getTV
  b0 <- getAV
  let e' = M.insert v2 (TVar ax) e
      e'' = M.insert v1 (Func (TVar ax) (TVar a0)  b0) e'
  (t0, s0, c0) <- infer e'' t
  let s1 = unify t0 (subst (TVar a0) s0)
      resT = Func (subst (subst (TVar ax) s0) s1) (subst t0 s1) (substA (substA b0 s0) s1)
      resC = (substA (substA b0 s0) s1, Ann l) : (substC c0 s1)
  return (resT, combine s1 s0, resC)
infer e (LApp l e1 e2) = do
  (t1, s1, c1) <- infer e e1
  (t2, s2, c2) <- infer (substEnv e s1) e2
  a <- getTV
  b <- getAV
  let s3 = unify (subst t1 s2) (Func t2 (TVar a) b)
  return (subst (TVar a) s3, combine s3 (combine s2 s1), (substC (substC c1 s2) s3) ++ (substC c2 s3))
infer e (LIf l e0 e1 e2) = do
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
infer e (LLet l v e1 e2) = do
  (t1, s1, c1) <- infer e e1
  (t2, s2, c2) <- infer (M.insert v t1 (substEnv e s1)) e2
  return (t2, combine s2 s1, (substC c1 s2) ++ c2)
infer e (LBinop l o e1 e2) = do
  (t1, s1, c1) <- infer e e1
  (t2, s2, c2) <- infer (substEnv e s1) e2
  let s3 = unify (subst t1 s2) (argType o)
      s4 = unify (subst t2 s3) (argType o)
      resS = combine s4 (combine s3 (combine s2 s1))
      resC1 = substC (substC (substC c1 s2) s3) s4 
      resC2 = substC (substC c2 s3) s4
  return (resType o, resS, resC1 ++ resC2)



{-
data AnnVar = AnnV Int deriving (Show, Eq, Ord)
data TypeVar = TypeV Int deriving (Show, Eq, Ord)

data Type =  Nat 
          | Bool 
          | Func SimpleType SimpleType AnnVar

data SimpleType = SimpleType Type
                | TypeVar TypeVar
                  
data TypeScheme = Type SimpleType 
                | NewVar String TypeScheme
                  
type TypeEnv = Map Var TypeScheme

type TypeSubst = Map TypeVar Type

type Phi = Int

data Constraint = EmptySet 
                | LessOrEqual AnnVar Phi 
                | Union Constraint Constraint 
                  
generalise :: TypeEnv -> SimpleType -> TypeScheme
generalise = undefined

instantiate :: TypeScheme -> Type
instantiate = undefined

subst :: SimpleType -> TypeSubst -> SimpleType
subst (TypeVar v) ts = if M.member v ts 
                            then SimpleType (fromJust (M.lookup v ts))
                            else TypeVar v
subst (SimpleType (Func t1 t2 a)) ts = SimpleType (Func (subst t1 ts) (subst t2 ts) a)
subst t _ = t

unify :: SimpleType -> SimpleType -> TypeSubst
unify (SimpleType Nat) (SimpleType Nat) = M.empty
unify (SimpleType Bool) (SimpleType Bool) = M.empty
--unify (SimpleType (Func t1 t2 b1)) (SimpleType (Func t3 t4 b2)) =
--  where s0 = 


infer :: TypeEnv -> LTerm -> (SimpleType, TypeSubst, Constraint)
infer _ (LConst _ (CNum i)) = (SimpleType Nat, M.empty, EmptySet)
infer _ (LConst _ CTrue) = (SimpleType Bool, M.empty, EmptySet)
infer _ (LConst _ CFalse) = (SimpleType Bool, M.empty, EmptySet)
infer e (LIdent _ v) = (SimpleType (instantiate (fromJust (M.lookup v e))), M.empty, EmptySet)
infer e (LFn l v t) = (SimpleType (Func a1s t2 b), p, Union c1 (LessOrEqual b l))
  where a1 = (TypeVar (TypeV l))
        (t2, p, c1) = infer (M.insert v (Type a1) e) t
        b = AnnV l
        a1s = subst a1 p 

-}