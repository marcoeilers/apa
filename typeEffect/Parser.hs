module Parser where

-- import the the library functions from uulib
import UU.Parsing
import UU.Scanner

-- import our custom made Alex-scanner
import Scanner
import AST

import Data.List as List

-- Some boilerplate code to use the parser
-- Give `parse tokens' to your parser and a list of tokens, returned by the `scanTokens'
-- function exported by the Scanner module, then you get either a list of strings with error
-- messages in case of a parse error, or the parse tree.
type TokenParser a = Parser Token a

parseTokens :: TokenParser a -> [Token] -> Either [String] a
parseTokens p tks
  = if List.null msgs
    then final `seq` Right v
    else Left (List.map show msgs)
  where
    steps = parse p tks
    msgs  = getMsgs steps
    (Pair v final) = evalSteps steps

-- code for the parser
-- Note: * make sure that the parser is not left recursive
--       * make sure that the parser is not ambiguous
--           (by introducing priority levels for the operators)


-- Expr -> Factor | Factor + Expr
pTerm  
  =   pLet
  <|> Fn <$ pKey "fn" <*> pVarid <* pKey "=>" <*> pTerm
  <|> Fun <$ pKey "fun" <*> pVarid <*> pVarid <* pKey "=>" <*> pTerm
  <|> PCase <$ pKey "case" <*> pTerm <* pKey "Pair(" <*> pVarid <* pKey "," <*> pVarid <* pKey ")" <* pKey "=>" <*> pTerm
  <|> Cons <$ pKey "Cons(" <*> pTerm <* pKey "," <*> pTerm <* pKey ")"
  <|> Nil <$ pKey "Nil"
  <|> ListCase <$ pKey "case" <*> pTerm <* pKey "of" <* pKey "Cons(" <*> pVarid <* pKey "," <*> pVarid <* pKey ")" <* pKey "=>" <*> pTerm <* pKey "or" <*> pTerm <* pKey ")"

pLet 
  =   pIf
  <|> Let <$ pKey "let" <*> pVarid <* pKey "=" <*> pTerm <* pKey "in" <*> pLet

pIf
  =   pOr
  <|> If <$ pKey "if" <*> pTerm <* pKey "then" <*> pTerm <* pKey "else" <*> pIf
  


pOr :: TokenParser Term
pOr
   =  pChainl (Binop Or <$ pKey "||") pAnd 

pAnd :: TokenParser Term
pAnd
   =  pChainl (Binop And <$ pKey "&&") pComp 

pComp :: TokenParser Term
pComp
   =  pChainl (Binop Ls <$ pKey "<" <|> Binop Gt <$ pKey ">") pSum

pSum :: TokenParser Term
pSum
   =  pChainl (Binop Plus <$ pKey "+" <|> Binop Minus <$ pKey "-") pFactor 

-- Factor -> Term | Term * Factor
pFactor :: TokenParser Term
pFactor
  =  pChainl (Binop Times <$ pKey "*") pApp 

pApp 
  =  pChainl (pSucceed App) pSingle
  

-- Term -> Var
-- Term -> Int
pSingle :: TokenParser Term
pSingle
  =  Const <$> pConst  
  <|> Ident <$> pVarid
  <|> pParens pTerm 
  

pConst :: TokenParser Const
pConst 
  =   (CNum . read) <$> pInteger16
  <|> CTrue <$ pKey "true"
  <|> CFalse <$ pKey "false"


