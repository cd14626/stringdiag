module DataConstructor where
import ExpParser
import DataParser
import Text.Parsec
import Text.Parsec.String
import Data.List  (sort, group)

buildData :: ([NewObj],[Value],[Dgrm]) -> [DiagSD]
buildData (obj,val,diag) = diags
        where
            newObj = getNams obj
            values = setVal (newObj,val)
            diags  = [getDgrm (newObj,values,x)| x <- diag]

getDgrm :: (([String],[String],[String]),([Func],[Nat]),Dgrm) -> DiagSD
getDgrm ((c_n,f_n,n_n),(f,n),d) = DiagSD name d'
        where
            name = dname d
            p = arg d
            types = getType ((c_n,f_n,n_n),p)
            d' = case types of
                 "cat" -> CatD (head $ fst (valuesOf p))
                 "fun" -> Fun (cnvrtF (f,p))
                 "nat" -> Na (cnvrtN (f,n,p))
                 "null"-> CatD "Error"
                  
getType :: (([String],[String],[String]),Exp) -> String
getType ((c_n,f_n,n_n),p) = eval
        where
            vals = valuesOf p
            eval = if (all (\x -> elem x c_n) (fst vals)) && ((fst vals)/=[])  then "cat"
                        else if (all (\x -> elem x f_n) (fst vals)) && (all (\x -> elem x c_n) (snd vals)) then "fun"
                            else if (all (\x -> elem x n_n) (fst vals)) && (all (\x -> elem x f_n) (snd vals))then "nat"
                                else "null"

valuesOf :: Exp -> ([String],[String])
valuesOf (Hor x1 x2) = (f,c)
        where
            z = [valuesOf x1,valuesOf x2] 
            f = concat [fst x | x <- z] 
            c = concat [snd x | x <- z]
valuesOf (Ver x1 x2) = (f,c)
        where
            z = [valuesOf x1,valuesOf x2] 
            f = concat [fst x | x <- z] 
            c = concat [snd x | x <- z]
valuesOf (Id (Val x)) = ([],[x])
valuesOf (Val x) = ([x],[])

valueOf :: Exp -> String
valueOf (Val x) = x
valueOf cplx = " "
 
getNams :: [NewObj] -> ([String],[String],[String])
getNams obj = (newCat,newFunc,newNat)
        where
            newCat = concat [[x | x <- (if (typ y == "cat") then (elm y) else [])] | y <- obj] 
            newFunc = concat [[x | x <- (if (typ y == "func") then (elm y) else [])] | y <- obj] 
            newNat = concat [[x | x <- (if (typ y == "nat") then (elm y) else [])] | y <- obj] 

setVal :: (([String],[String],[String]),[Value]) -> ([Func],[Nat])
setVal ((c,f,n),val) = (a,b)
        where
            a = concat [getFunc (c,f,x)| x <- val]
            b = concat [getNat  (c,a,n,y) | y <- val]
            
getFunc :: ([String],[String],Value) -> [Func]
getFunc (c,f,v) = funct
        where
            a1 = valueOf (arg1 v)
            a2 = valueOf (arg2 v)
            funct = if(elem (name v) f && elem a1 c && elem a2 c)
                        then [Func (name v) a1 a2]
                        else []

getNat :: ([String],[Func],[String],Value) -> [Nat]
getNat (c,f,n,v) = nt
        where
            a1 = valuesOf (arg1 v)
            a2 = valuesOf (arg2 v)
            fLst = [nameF n | n <- f]
            f1 = (all (\x -> elem x fLst) (fst a1)) && (all (\x -> elem x c) (snd a1))
            f2 = (all (\x -> elem x fLst) (fst a2)) && (all (\x -> elem x c) (snd a2))
            nt = if(elem (name v) n && f1 && f2) 
                    then [Nat (name v)  (cnvrtF (f,arg2 v))(cnvrtF (f,arg1 v))]
                    else []

cnvrtF :: ([Func],Exp) -> Func
cnvrtF (f,v) = case v of
    Hor (Hor x1 x2) (Hor y1 y2) -> HorF[cnvrtF (f,x1),cnvrtF (f,x2),cnvrtF (f,y1),cnvrtF (f,y2)]
    Hor (Hor x1 x2) y -> HorF[cnvrtF (f,x1),cnvrtF (f,x2),cnvrtF (f,y)]
    Hor x (Hor y1 y2) -> HorF[cnvrtF (f,x),cnvrtF (f,y1),cnvrtF (f,y2)]
    Hor x y -> HorF[cnvrtF (f,x),cnvrtF (f,y)]
    Val fn -> case ((filter (\x -> nameF x==fn) f)) of
        [] -> IdF "Error"
        x -> head x
    Id (Val cat) -> IdF cat
cnvrtN :: ([Func],[Nat],Exp) -> Nat
cnvrtN (f,n,v) = case v of
    Hor (Hor x1 x2) (Hor y1 y2) -> HorN[cnvrtN (f,n,x1),cnvrtN (f,n,x2),cnvrtN (f,n,y1),cnvrtN (f,n,y2)]
    Hor (Ver x1 x2) (Ver y1 y2) -> HorN[cnvrtN (f,n,(Ver x1 x2)),cnvrtN (f,n,(Ver y1 y2))]
    Hor (Ver x1 x2) (Hor y1 y2) -> HorN[cnvrtN (f,n,(Ver x1 x2)),cnvrtN (f,n,y1),cnvrtN (f,n,y2)]
    Ver (Ver x1 x2) (Ver y1 y2) -> VerN[cnvrtN (f,n,x1),cnvrtN (f,n,x2),cnvrtN (f,n,y1),cnvrtN (f,n,y2)]
    Ver (Hor x1 x2) (Hor y1 y2) -> VerN[cnvrtN (f,n,(Hor x1 x2)),cnvrtN (f,n,(Hor y1 y2))]
    Ver (Hor x1 x2) (Ver y1 y2) -> VerN[cnvrtN (f,n,(Hor x1 x2)),cnvrtN (f,n,y1),cnvrtN (f,n,y2)]
    Hor (Hor x1 x2) (Ver y1 y2) -> HorN[cnvrtN (f,n,x1),cnvrtN (f,n,x2),cnvrtN (f,n,(Ver y1 y2))]
    Ver (Ver y1 y2) (Hor x1 x2) -> VerN[cnvrtN (f,n,y1),cnvrtN (f,n,y2),cnvrtN (f,n,(Hor x1 x2))]
    Hor (Hor x1 x2) y -> HorN[cnvrtN (f,n,x1),cnvrtN (f,n,x2),cnvrtN (f,n,y)]
    Hor x (Hor y1 y2) -> HorN[cnvrtN (f,n,x),cnvrtN (f,n,y1),cnvrtN (f,n,y2)]
    Hor x (Ver y1 y2) -> HorN[cnvrtN (f,n,x),cnvrtN (f,n,(Ver y1 y2))]
    Hor (Ver x1 x2) x -> HorN[cnvrtN (f,n,(Ver x1 x2)),cnvrtN (f,n,x)]
    Ver (Ver x1 x2) y -> VerN[cnvrtN (f,n,x1),cnvrtN (f,n,x2),cnvrtN (f,n,y)]
    Ver x (Ver y1 y2) -> VerN[cnvrtN (f,n,x),cnvrtN (f,n,y1),cnvrtN (f,n,y2)]
    Ver x (Hor x1 x2) -> VerN[cnvrtN (f,n,x),cnvrtN (f,n,(Hor x1 x2))]
    Ver (Hor x1 x2) x -> VerN[cnvrtN (f,n,(Hor x1 x2)),cnvrtN (f,n,x)]
    Ver x y -> VerN[cnvrtN (f,n,x),cnvrtN (f,n,y)]
    Hor x y -> HorN[cnvrtN (f,n,x),cnvrtN (f,n,y)]
    Id (Val f01) -> IdN $ case (filter (\x -> nameF x==f01) f) of
        [] -> IdF "Error"
        x -> head x
    Id (Id (Val cat)) -> IdN (IdF cat) 
    Val fn -> case (filter (\x -> nameN x==fn) n) of
        [] -> IdN (IdF "Error")
        x -> head x
    
zero :: String -> [DiagSD]
zero str = list
        where
            expression = case (parse mainParser "" str) of
                            Left x -> ([NewObj "E" ["E"]],[Value "E" (Val "E") (Val "E")],[Dgrm "E" (Val "E")])
                            Right ex -> ex
            list = buildData expression
            
file = "cat C,D,E;\nfunc F,G,H,id_C;\nnat Ω;\nF : C -> D;\nG : D -> E;\nH : C -> E;\nΩ : (F * G)  -> H;\nid_C : C -> C;\nN2 : (F * G) -> G;\n{diag functorFG : F * G;}\n{diag categoryC : id C;}\n{diag naturalΩ : Ω;}"
test = "cat C,D\nfunc F,G\nnat α,α'\nF : C -> D\nG : C -> D\nα : F -> G\nα' : id C -> id C\ndiag idC : id C\ndiag C : C\ndiag idF : id F\ndiag F : F\ndiag alpha : α\ndiag alphap : α'"
objTst = (["C","D","E"],["F","G","H","id_C"],["N"])
--https://github.com/kseo/calculator/blob/master/src/Calculator/Interpreter.hs
--http://kwangyulseo.com/2014/01/07/parsing-arithmetic-expressions-with-parsec/
--https://wiki.haskell.org/Parsing_a_simple_imperative_language#Lexer
--https://hackage.haskell.org/package/parsec-3.1.9/docs/Text-Parsec-Combinator.html
--https://hackage.haskell.org/package/parsec-3.1.9/docs/Text-ParserCombinators-Parsec-Token.html

