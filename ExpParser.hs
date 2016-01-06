{--Token Parser-}
module ExpParser where

import           Control.Applicative  hiding (optional, many, (<|>))
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language (haskellStyle, emptyDef)
import qualified Text.Parsec.Token    as P
import           Text.Parsec.String   (Parser)

data Exp =
      Hor Exp Exp
    | Ver Exp Exp
    | Id Exp
    | Val String
    deriving (Show, Eq, Read, Ord)

languageDef =
  haskellStyle  { P.commentStart    = "{-"
           , P.commentEnd      = "-}"
           , P.commentLine     = "--"
           , P.identStart      = letter
           , P.identLetter     = letter <|> digit <|> (char '_') <|> (char '\'')
           , P.reservedNames   = [ "cat"
                                 , "func"
                                 , "nat"
                                 , "diag"								 
                                 ]
           , P.opStart = oneOf "*.:->id"
           , P.opLetter = oneOf "*.:->id"
           , P.reservedOpNames = ["*", ".", ":", "->", "id"
                                     ]
           }
lexer = P.makeTokenParser languageDef          
parens = P.parens lexer
identifier = P.identifier lexer
reservedOp = P.reservedOp lexer
reserved = P.reserved lexer
semiSep1 = P.semiSep1 lexer
comma = P.comma lexer
whiteSpace = P.whiteSpace lexer  
lexeme = P.lexeme lexer
                      
strng :: Parser String
strng    = do
        a <- identifier
        return $ a

term =  parens expr
    <|> Val <$> strng
    <?> "term"

table = [ [ binary "*" Hor AssocLeft,
            binary "." Ver AssocLeft, 
            unary "id" Id ]]
binary  name fun = Infix   (do { reservedOp name; return fun })
unary  name fun = Prefix   (do { reservedOp name; return fun })

expr :: Parser Exp
expr = buildExpressionParser table term <?> "expression"

parseExp :: String -> Either ParseError Exp
parseExp = parse expr ""