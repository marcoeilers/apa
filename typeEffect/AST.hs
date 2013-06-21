module AST where

import Test.QuickCheck
import Control.Monad
import Debug.Trace

-- define an AST tree
type Var = String
type Lab = Int

data Exp  = Exp Lab Term deriving (Show, Eq)
data Term 
  = Const Const
  | Ident Var
  | Fn Var Term
  | Fun Var Var Term
  | App Term Term
  | If Term Term Term
  | Let Var Term Term
  | Binop Op Term Term deriving (Show, Eq)

data LTerm =
    LConst Lab Const
  | LIdent Lab Var
  | LFn Lab Var LTerm
  | LFun Lab Var Var LTerm
  | LApp Lab LTerm LTerm
  | LIf Lab LTerm LTerm LTerm
  | LLet Lab Var LTerm LTerm
  | LBinop Lab Op LTerm LTerm deriving (Show, Eq)

lconvert :: Int -> Term -> (Int, LTerm)
lconvert i t = trace ("Label "++ (show i) ++ " : "++ (show t)) (lconvert' i t)

lconvert' i (Const c) = (i+1, LConst i c)
lconvert' i (Ident v) = (i+1, LIdent i v)
lconvert' i (Fn v t) = (ni, LFn i v lt)
  where (ni, lt) = lconvert (i+1) t
lconvert' i (Fun v1 v2 t) = (ni, LFun i v1 v2 lt)
  where (ni, lt) = lconvert (i+1) t
lconvert' i (App t1 t2) = (ni2, LApp i lt1 lt2)
  where (ni1, lt1) = lconvert (i+1) t1
        (ni2, lt2) = lconvert ni1 t2 
lconvert' i (If t1 t2 t3) = (ni3, LIf i lt1 lt2 lt3)
  where (ni1, lt1) = lconvert (i+1) t1
        (ni2, lt2) = lconvert ni1 t2
        (ni3, lt3) = lconvert ni2 t3
lconvert' i (Let v t1 t2) = (ni2, LLet i v lt1 lt2)
  where (ni1, lt1) = lconvert (i+1) t1
        (ni2, lt2) = lconvert ni1 t2
lconvert' i (Binop o t1 t2) = (ni2, LBinop i o lt1 lt2)
  where (ni1, lt1) = lconvert (i+1) t1
        (ni2, lt2) = lconvert ni1 t2

data Const
  = CNum Int
  | CTrue
  | CFalse deriving (Show, Eq)

data Op 
  = Plus 
  | Times 
  | Minus 
  | And 
  | Or 
  | Ls 
  | Gt deriving (Show, Eq, Enum, Bounded)

instance Arbitrary Op where
  arbitrary = oneof (map return [minBound .. maxBound])
--  coarbitrary = undefined

instance Arbitrary Const where
  arbitrary = oneof [ fmap CNum arbitrary
                    , return CTrue
                    , return CFalse ]
--  coarbitrary = undefined

instance Arbitrary Term where
  arbitrary = sized arbTerm 
--  coarbitrary = undefined

arbTerm 0 = return $  Ident "x"
arbTerm n =
  oneof [ liftM Const arbitrary
        , return $ Ident "y"
        , liftM (Fn "f") (arbTerm (n-1))
        , liftM (Fun "f" "x") (arbTerm (n-1))
        , liftM2 App (arbTerm (n `div` 2)) (arbTerm (n `div` 2))
        , liftM3 If (arbTerm (n `div` 2)) (arbTerm (n `div` 2)) (arbTerm (n `div` 2))
        , liftM2 (Let "z") (arbTerm (n `div` 2)) (arbTerm (n `div` 2))
        , liftM3 Binop arbitrary (arbTerm (n `div` 2)) (arbTerm (n `div` 2))
        ]


depth :: Term -> Int
depth t = case t of
    Const _ -> 1
    Ident _ -> 1
    Fn s e -> 1 + depth e
    Fun f x e -> 1 + depth e
    App e1 e2 -> 1 + max (depth e1) (depth e2)
    If e1 e2 e3 -> 1 + max (depth e1) (max (depth e2) (depth e3))
    Let x e1 e2 -> 1 + max (depth e1) (depth e2)
    Binop op e1 e2 -> 1 + max (depth e1) (depth e2)
