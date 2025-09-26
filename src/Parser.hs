module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until", "case"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","

                        , "++"
                        , ":"
                        , "{"
                        , "}"
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
-- Parser para expresiones enteras con precedencia correcta
intexp :: Parser (Exp Int)
intexp = additive

-- Nivel más bajo de precedencia: + y -
additive :: Parser (Exp Int)
additive = chainl1 multiplicative plusminus
  where
    plusminus = do reservedOp lis "+"
                   return Plus
            <|> do reservedOp lis "-"
                   return Minus

-- Nivel medio de precedencia: * y /
multiplicative :: Parser (Exp Int)
multiplicative = chainl1 unary timesdiv
  where
    timesdiv = do reservedOp lis "*"
                  return Times
           <|> do reservedOp lis "/"
                  return Div

-- Nivel más alto de precedencia: unarios y términos básicos
unary :: Parser (Exp Int)
unary = uminus <|> primary

uminus :: Parser (Exp Int)
uminus = do reservedOp lis "-"
            expr <- unary  -- Recursión sobre unary, no intexp
            return $ UMinus expr

-- Términos básicos (números, variables, expresiones entre paréntesis)
primary :: Parser (Exp Int)
primary = nat 
      <|> varInc
      <|> var 
      <|> parens lis intexp  -- Expresiones entre paréntesis

-- Parser para números naturales
nat :: Parser (Exp Int)
nat = do n <- integer lis
         return (Const (fromIntegral n))  -- Conversión explícita si es necesaria

varInc :: Parser (Exp Int)
varInc = try $ do  
  v <- identifier lis
  reservedOp lis "++"
  return (VarInc v)

-- Parser para variables
var :: Parser (Exp Int)  
var = do v <- identifier lis
         return (Var v)


------------------------------------
--- Parser de expresiones booleanas
-----------------------------------
boolexp :: Parser (Exp Bool)
boolexp = orExpr

orExpr :: Parser (Exp Bool)
orExpr = chainl1 andExpr  orOp
  where orOp = do reservedOp lis "||"
                  return Or

andExpr :: Parser (Exp Bool)
andExpr = chainl1 compExpr andOp
  where andOp = do reservedOp lis "&&"
                   return And

-- acá va la comparación numérica y NOT
compExpr :: Parser (Exp Bool)
compExpr = notExpr

notExpr :: Parser (Exp Bool)
notExpr =  notOp <|> compTerm
  where notOp = do reservedOp lis "!"
                   expr <- compTerm
                   return (Not expr)

numComp :: Parser (Exp Bool)
numComp = do
   n1 <- intexp
   op <- opNumComp
   n2 <- intexp
   return (op n1 n2)

compTerm :: Parser (Exp Bool)
compTerm = try numComp
       <|> boolVal
       <|> parens lis boolexp

-- Operador que uso para comparar
opNumComp :: Parser (Exp Int -> Exp Int -> Exp Bool)
opNumComp = do reservedOp lis "=="
               return Eq
        <|> do reservedOp lis "!="
               return NEq
        <|> do reservedOp lis "<"
               return Lt
        <|> do reservedOp lis ">"
               return Gt

-- Valores Booleanos
boolVal :: Parser (Exp Bool)
boolVal = do reserved lis "true"
             return BTrue
      <|> do reserved lis "false"
             return BFalse
          
-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 simpleComm seqOp 

simpleComm :: Parser Comm
simpleComm = parens lis comm 
         <|> pSkip  
         <|> pAssign 
         <|> pIf 
         <|> pRepeat
         <|> pCase

seqOp :: Parser (Comm -> Comm -> Comm)
seqOp = do
  reservedOp lis ";"
  return Seq

pSkip :: Parser Comm
pSkip = do
  reserved lis "skip"
  return Skip

pAssign :: Parser Comm
pAssign = do
  x <- identifier lis
  reservedOp lis "="
  exp <- intexp
  return (Let x exp)

pIf :: Parser Comm
pIf = do
  reserved lis "if"
  b <- boolexp
  reservedOp lis "{"
  c <- comm
  reservedOp lis "}"
  rest b c <|> return (IfThenElse b c Skip)

rest :: (Exp Bool) -> Comm -> Parser Comm
rest b c = do
  reserved lis "else"
  reservedOp lis "{"
  c1 <- comm
  reservedOp lis "}"
  return (IfThenElse b c c1)

pRepeat :: Parser Comm
pRepeat = do
  reserved lis "repeat"
  reservedOp lis "{"
  c <- comm
  reservedOp lis "}"
  reserved lis "until"
  b <- boolexp
  return (RepeatUntil c b)

-- Parser para comando case usando Pattern Synonym
pCase :: Parser Comm
pCase = do
  reserved lis "case"
  reservedOp lis "{"
  alternatives <- many caseAlternative
  reservedOp lis "}"
  return (Case alternatives)  -- Usar el Pattern Synonym del módulo AST
  where
    caseAlternative = do
      condition <- boolexp
      reservedOp lis ":"
      reservedOp lis "{"
      command <- comm
      reservedOp lis "}"
      return (condition, command)

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)


-- Funciones auxiliares para testing
testComm :: String -> Either ParseError Comm
testComm s = parseComm "" s

-- Ejemplos de uso:
-- testComm "skip"
-- testComm "x = 5; y = 10"
-- testComm "if (x > 5) { x = 10 } else { x = 0 }"
-- testComm "repeat { x = x + 1 } until (x > 10)"
-- testComm "case { x == 5 : { x = 10 } x < 7 : { x = 100 } }"

