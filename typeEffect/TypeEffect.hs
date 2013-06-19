
module TypeEffect where

import Data.Map
import Data.Set

import AST

type AnnVar = String
type TypeVar = String

data Type =  Nat 
          | Bool 
          | Func SimpleType SimpleType AnnVar

data SimpleType = SimpleType Type
                | TypeVar TypeVar
                  
data TypeScheme = Type SimpleType 
                | NewVar String TypeScheme
                  
type TypeEnv = Map AnnVar TypeScheme

type TypeSubst = Map TypeVar Type

type Phi = String

data Constraint = EmptySet 
                | LessOrEqual AnnVar Phi 
                | Union Constraint Constraint 
                  
generalise :: TypeEnv -> SimpleType -> TypeScheme
generalise = undefined

instantiate :: TypeScheme -> Type
instantiate = undefined

unify :: SimpleType -> SimpleType -> TypeSubst
unify = undefined

infer :: TypeEnv -> Term -> (Type, TypeSubst)
infer = undefined