module PrettyPrint where

-- import the the library functions from uulib
import Text.PrettyPrint
import AST

-- pretty printing  of AST

data Priority =  PApp  | PFactor | PSum | PComp | PAnd | POr | PIf | PLet | PFn | PRoot deriving (Eq,Ord)
{-- 
prettyExp :: Exp -> Prior -> Doc
 = Exp Term Lab deriving (Show, Eq)
--}
prettyTerm :: Priority -> Bool -> Term -> Doc 
prettyTerm p isRight t
 = case t of 
    Const c -> prettyConst c 
    Ident v -> text v
    Fn x e -> wrap p isRight PFn (text "fn" <+> text x <+> text "=>" <+> prettyTerm PFn False e)
    Fun f x e -> wrap p isRight PFn (text "fun" <+> text f <+> text x <+> text "=>" <+> prettyTerm PFn False e)
    App e1 e2 -> wrap p isRight PApp (prettyTerm PApp False e1 <+> prettyTerm PApp True e2)
    If e1 e2 e3 -> wrap p isRight PIf (text "if" <+> prettyTerm PRoot False e1 <+> text "then" <+> prettyTerm PRoot False e2 <+> text "else" <+> prettyTerm PIf False e3)
    Let x e1 e2 -> wrap p isRight PLet (text "let" <+> text x <+> text "=" <+> prettyTerm PRoot False e1 $$ nest 1 (text "in" <+> prettyTerm PLet False e2))
    Binop op e1 e2 -> let p' = prio op in wrap p isRight p' (prettyTerm p' False e1 <+> prettyOp op <+> prettyTerm p' True e2)

prio op =
   case op of
      Plus -> PSum
      Minus -> PSum
      Times -> PFactor
      And -> PAnd 
      Or -> POr
      Ls -> PComp
      Gt -> PComp


wrap :: Priority -> Bool -> Priority -> Doc -> Doc
wrap p r m d = if p < m  || (p == m && r)
                 then parens d
                 else d

prettyConst :: Const -> Doc
prettyConst c = 
   case c of
     CTrue -> text "true"
     CFalse -> text "false"
     CNum i -> int i

prettyOp :: Op -> Doc
prettyOp op =
   case op of
      Plus -> text "+"
      Minus -> text "-"
      Times -> text "*"
      And -> text "&&" 
      Or -> text "||"
      Ls -> text "<"
      Gt -> text ">"


