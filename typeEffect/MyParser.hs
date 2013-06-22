module MyParser where

import Text.ParserCombinators.ReadP
import Data.Char(isSpace,isLower,isUpper,isDigit,isAlpha,isAlphaNum)
import Control.Applicative((<$>),(<*>))
import AST
import SimpleReadP

parseString :: String               
            -> Either String Term 
parseString s = case res of
    [] -> Left $ "Could not parse"
    [(p, _)]   -> Right p
    (r:rs)     -> Left $ "Ambiguous parsing result; this should never happen\n\n" ++ (show (r) ++ "\n\n" ++ (show (head rs)))
    where res = parseEof pTerm s
          (r:rs) = res

reservedWords :: [String]
reservedWords = [ "let"
                 ,"fn"
                 ,"=>"
                 ,"fun"
                 ,"case"
                 ,"Cons"
                 ,"of"
                 ,"if"
                 ,"then"
                 ,"else"
                 ,"or"
                 ,"in"
                 ,"&&"
                 ,"||"
                 ,"Nil"
                 ,"true"
                 ,"false"]

pVar :: Parser Var
pVar = token $ do
    c <- satisfy isAlpha
    cs <- munch ((||) <$> isAlphaNum <*> (==) '_')
    if (c:cs) `elem` reservedWords
        then pfail 
        else return $ (c:cs)

pIdent :: Parser Term
pIdent = do
  v <- pVar
  return $ Ident v

pInt :: Parser Int
pInt = token $ do
  cs <- munch1 isDigit
  return $ read cs

pTerm :: Parser Term
pTerm = simpleTerm 
    <|> pApp

pParens = do
  symbol "("
  t <- pTerm
  symbol ")"
  return t

simpleTerm :: Parser Term
simpleTerm = 
        pFn
    <|> pFun
    <|> pIf
    <|> pLet
    <|> pBinop
    <|> pPair
    <|> pPCase
    <|> pCons
    <|> pLCase

pSingle = pConst
      <|> pIdent
      <|> pParens

pConst = pTrue
     <|> pFalse
     <|> pNum

pTrue = do
  symbol "true"
  return $ Const CTrue

pFalse = do
  symbol "false"
  return $ Const CFalse

pNum = do
  i <- pInt
  return $ Const (CNum i)

pFn = do
  symbol "fn"
  v <- pVar
  symbol "=>"
  t <- simpleTerm
  return $ Fn v t

pFun = do
  symbol "fun"
  v1 <- pVar
  v2 <- pVar
  symbol "=>"
  t <- simpleTerm
  return $ Fun v1 v2 t

pIf = do
  symbol "if"
  t0 <- simpleTerm
  symbol "then"
  t1 <- simpleTerm
  symbol "else"
  t2 <- simpleTerm
  return $ If t0 t1 t2

pLet = do
  symbol "let"
  v <- pVar 
  symbol "="
  t1 <- simpleTerm
  symbol "in"
  t2 <- simpleTerm
  return $ Let v t1 t2

pApp :: Parser Term
pApp = do
  t1 <- simpleTerm
  t2 <- simpleTerm
  return $ App t1 t2 --chainl1 simpleTerm (symbol "" >> return App)

pPair = do
  symbol "Pair"
  symbol "("
  t1 <- simpleTerm
  symbol ","
  t2 <- simpleTerm
  symbol ")"
  return $ TPair t1 t2

pPCase = do
  symbol "case"
  t0 <- simpleTerm
  symbol "of"
  symbol "Pair"
  symbol "("
  v1 <- pVar
  symbol ","
  v2 <- pVar
  symbol ")"
  symbol "=>"
  t1 <- simpleTerm
  return $ PCase t0 v1 v2 t1

pCons = do
  symbol "Cons"
  symbol "("
  t1 <- simpleTerm
  symbol ","
  t2 <- simpleTerm
  symbol ")"
  return $ Cons t1 t2

pNil = do
  symbol "Nil"
  return Nil

pLCase = do
  symbol "case"
  t0 <- simpleTerm
  symbol "of"
  symbol "Cons" 
  symbol "("
  v1 <- pVar
  symbol ","
  v2 <- pVar
  symbol ")" 
  symbol "=>"
  t1 <- simpleTerm
  symbol "or"
  t2 <- simpleTerm
  return $ ListCase t0 v1 v2 t1 t2

pBinop = pOr

pOr = (chainl1 pAnd (symbol "||" >> return (Binop Or)))

pAnd = (chainl1 pComp (symbol "&&" >> return (Binop And)))

pComp = (chainl1 pPlus ((symbol ">" <|> symbol "<") >> return (Binop Gt)))


pPlus = (chainl1 pTimes ((symbol "+" <|> symbol "-") >> return (Binop Plus))) 


pTimes = (chainl1 pSingle (symbol "*" >> return (Binop Times)))




  
  
  


