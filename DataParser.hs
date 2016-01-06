module DataParser where

import ExpParser
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char (endOfLine)
import Data.List  (sort, group)

type Cat = String
data Func = Func { nameF :: String
                     , codomain :: Cat
                     , domain :: Cat
                     }
            | HorF [Func] 
            | IdF Cat
            deriving (Show, Eq, Read,Ord)
                            
data Nat = Nat { nameN :: String
                    , originN :: Func
                    , target :: Func
                   } 
            | HorN [Nat]
            | VerN [Nat]
            | IdN Func
            deriving (Show, Eq, Read, Ord)
data ExpSD = Na Nat
            | Fun Func
            | CatD Cat
            deriving (Show, Eq, Read, Ord)
data DiagSD = DiagSD { nameD :: String
                     , diag :: ExpSD
                     }deriving Show
data EqSD = EqSD [ExpSD] deriving (Show, Eq, Read, Ord) 

data NewObj = NewObj { typ :: String
                     , elm :: [String]
                     } 
                     deriving Show
data Value = Value { name :: String
                   , arg1 :: Exp
                   , arg2 :: Exp
                   }
                   deriving Show
data Dgrm = Dgrm {dname :: String
                 , arg :: Exp} deriving Show

objectsParser :: Parsec String () String
objectsParser = do
        skipMany comment
        p <- many1 (letter <|> digit <|> char '\'' <|> char '_')
        skipMany commentC
        optional (char ',')
        skipMany commentC
        return $  p
        
decLine :: Parser NewObj
decLine = do      
        skipMany commentCE 
        typ <- try (string "cat") <|> try (string "func") <|> try (string "nat")
        skipMany comment
        many1 (char ' ')
        elem <- try $ many objectsParser        
        skipMany commentCE
        return $ NewObj typ elem

assgnLine :: Parser Value
assgnLine = do    
        name <- try identifier
        skipMany commentC        
        reservedOp ":" 
        skipMany commentC   
        arg1 <- expr 
        skipMany (char ' ')
        reservedOp "->"  
        skipMany (char ' ')      
        arg2 <- expr 
        skipMany commentCE    
        return $ Value name arg1 arg2
        
diagLine :: Parser Dgrm
diagLine = do 
        string "diag"
        skipMany comment
        many1 (char ' ')
        skipMany comment
        name <-identifier
        skipMany commentC       
        reservedOp ":"
        skipMany commentC        
        arg <- expr 
        skipMany commentCE     
        return $ Dgrm name arg
        
mainParser :: Parser ([NewObj],[Value],[Dgrm])
mainParser = do
        a <- many1 $ try decLine
        b <- many $ try assgnLine
        c <- many1 diagLine
        return $ (a,b,c)

commentC :: GenParser Char st () 
commentC =
    (char ' ' >> return ()) <|> 
    comment 
commentE :: GenParser Char st () 
commentE =
    (endOfLine >> return ()) <|> 
    comment 
commentCE :: GenParser Char st () 
commentCE =
    (char ' ' >> return ()) <|>
    (endOfLine >> return ()) <|> 
    comment     
comment :: GenParser Char st () 
comment =
    (string "--" >> many (noneOf "\r\n") >> return ()) <|>
    (string "{-" >> manyTill anyChar ((try (string "-}") >> return ()) <|> eof) >> return ())       
