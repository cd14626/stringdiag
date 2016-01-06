module DataDefinition08 where 
{-# LANGUAGE NoMonomorphismRestriction #-}
import DataConstructor
import DataParser
import Data.List  (sort, group, nub)
import qualified Data.Text as T
import qualified Lucid.Svg as LST
import Diagrams.TwoD.Types
import Diagrams.TwoD.Text
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Size (mkWidth)
import Data.List (transpose,delete, intercalate)
                     
plot :: DiagSD -> IO()
plot (n) = plot2 (nameD n,diag n);

plot2 :: (String,ExpSD) -> IO()
plot2 (a,Na n) = plotN a n
plot2 (a,CatD c)= plotC a c
plot2 (a,Fun f)= plotF a f

sequ :: [IO a] -> IO ()
sequ [] = return ()
sequ (x:xs) = do 
              x
              sequ xs
                      
plotAll :: [DiagSD] -> [IO()]
plotAll dgs = salida
      where
        salida = [plot x | x <- dgs]
dgNames :: String -> [String]
dgNames dgs = salida
      where
        salida = [concat [nameD x,".svg"] | x <- b]
        b = DataConstructor.zero dgs
bigTest :: String -> IO()
bigTest file = do
      a <- sequ $ plotAll $ DataConstructor.zero file
      return $ a

plotC :: String -> Cat -> IO()
plotC n c = renderSVG (outputFile n) (sizing) (diagramC c)  

plotF :: String -> Func -> IO()
plotF n f = var
    where
--        var = if (checkF f)
        var = if (True)
              then  renderSVG (outputFile n) (sizing) (diagramF f)
              else plotC n ("Error " ++ n)
plotN :: String -> Nat -> IO()
plotN n nat = var
    where
--        var = if (checkN nat)
        var = if (True)
              then  renderSVG (outputFile n) (sizing) (diagramN nat)
              else plotC n ("Error " ++ n)


diagramC :: Cat -> Diagram B
diagramC c = (title01 === bodyC c === title02) #center
    where 
    title01 = (rectsF "") # center
    title02 = (rectsF "") # center
    
bodyC :: Cat -> Diagram B
bodyC c = (text c <> square 2 # dashingN [0.03,0.03] 0 ) # fontSize (normalized 0.1) # font "freeserif"

diagramF :: Func -> Diagram B
diagramF (HorF f) = (titleF === bodyF=== title02) #center
    where
        funcs = f
        titleF = (titles funcs)# center     
        title02 = (rectsF "") # center
        bodyF = (bodiesF funcs) #center

diagramF (IdF f) = (title01 === bodyF === title02) #center
    where
        title01 = (rectsF ("id" ++ f)) # center 
        title02 = (rectsF "") # center
        bodyF01 = ( rectsC f ||| rectsC f) # center 
        bodyF = (lineID <> lineID01 <> bodyF01) # center 
        lineID01 = (vrule 2 ) # center # lc white # lw 10
        lineID = (vrule 2 ) # dashingN [0.03,0.03] 0  #center
diagramF f =  (title01 === bodyF === title02) #center
    where
        title01 = titles [f] # center 
        title02 = (rectsF "") # center
        bodyF = bodiesF [f] # center 

----------------------------------------------
getFNames' :: Nat -> ([String],[String])
getFNames' (HorN nat) = (o1,t2)
    where
        nats = [getFNames' x | x <- (nat)]
        o1 = concat [fst n | n <-nats]
        t2 = concat [snd n | n <- nats]
getFNames' (IdN nat) = ([nameF nat],[nameF nat])
getFNames' nat = (getFN' (originN nat),getFN' (target nat))
getFN' :: Func -> [String]
getFN' (HorF funct) = concat [getFN' f | f <- (funct)]
getFN' (IdF c) = ["id" ++ c]
getFN' funct = [nameF funct]

--
getFNames :: Nat -> ([Func],[Func])
getFNames (HorN nat) = (o1,t2)
    where
        nats = [getFNames x | x <- (nat)]
        o1 = concat [fst n | n <-nats]
        t2 = concat [snd n | n <- nats]
getFNames (IdN nat) = ([nat],[ nat])
getFNames nat = (getFN (originN nat),getFN (target nat))
getFN :: Func -> [Func]
getFN (HorF funct) = concat [getFN f | f <- (funct)]
getFN (IdF c) = [IdF c]
getFN funct = [funct]

getFNV :: [Nat] -> [[Func]]
getFNV nat01 = fn
        where
        {--
            totalNats = length nat01
            title01 = nub $ concat [fst (head nats), t1]
            title02 = nub $ concat [snd (last nats), t2]
        --}            
            nat = reverse nat01
            nats = [getFNames x | x <- nat]  -- [([o1],[t1]), ... ([on],[tn])]
            t2 = [ c | c <- ts, c `notElem` os] -- all targets of the lower border
            t1 = [ c | c <- os, c `notElem` ts] -- all origin of the upper border
            fs = concat [os,ts] -- all functors in the composition
            levels = transpose [tOgs,inTa]
            lvln = [(head ors)] ++ [nub $ concat x | x <- levels] ++ [(last tas)]
            orsLen = length ors
            t1F = addF (reverse ors) t1 ((length ors) -1)
            t2F = addF tas t2 ((length tas) -1)
            lvln1 = [nub $ concat x | x <- transpose [lvln, t1F]]
            lvln2 = reverse [nub $ concat x | x <- transpose [(reverse lvln1), t2F]]
            fn = [orderFuncs x | x<-lvln2]  ---- Functors ordered in every level 
            tOgs = tail ors
            inTa = init tas
            ors = [fst n | n <- nats] -- all the functors origins [[os1],[os2],[os3],...]
            tas = [snd n | n <- nats] -- all the functors target [[ts1],[ts2],[ts3],...]
            os = concat ors -- [o1,o2,...]
            ts = concat tas --[t1,t2,...]
addF :: [[Func]] -> [Func] -> Int -> [[Func]]
addF ors t1 l = [res1,concat res2]
    where
        res1 = [x | x <-t1,x `notElem` (ors !! l)]
        res2 = if (l == 0) then [] else (addF ors res1 (l-1))

orderFuncs :: [Func] -> [Func]      
orderFuncs f
    | (length f) > 1 = pp
    | otherwise = f
    where 
        ll = orF f 0 0
        pp = if((snd ll)>0)
            then orderFuncs (fst ll)
            else fst ll     
    
orF :: [Func] -> Int -> Int -> ([Func], Int)
orF f it n
    | it < ((length f)-1) = t
    | otherwise = (f,n)
    where
        h = take it f
        b = ordrF (([f !! it] ++ [f !! (it +1)]),n)
        t = orF (h ++ (fst b) ++(drop (it +2) f)) (it+1)  (snd b)

ordrF :: ([Func], Int) -> ([Func], Int)
ordrF (f ,cnt) 
    | (length  f ) > 1 = hh
    | otherwise = ([],cnt)
    where
        hh = if((snd(orderFunc (head f))) == (fst(orderFunc(last f))))
            then  (f ,cnt)
            else ((reverse f),cnt+1)
            
orderFunc :: Func -> (String, String)   
orderFunc (IdF f) = (dom, cod)
    where
        dom = f;
        cod = f;
orderFunc f = (dom, cod)
    where
        dom = domain f;
        cod = codomain f;
      
testV = VerN [Nat "γ" (Func "N" "F" "A")(HorF [ Func "J" "E" "A", Func "K" "F" "E"]) ,
              Nat "β" (HorF [Func "G" "B" "A" , Func "M" "D" "B"]) (HorF [Func "N" "F" "A" , Func "L" "D" "F"]) ,
              Nat "α" (HorF [Func "H" "C" "B" , Func "I" "D" "C"]) (Func "M" "D" "B") ]

pointsVN :: Nat -> [[Func]]           
pointsVN (VerN nat01)
    | (length lvls) > 2 = nlvls
    | otherwise = lvls
    where
        lvls = getFNV nat01
        h = [head lvls]
        l = [last lvls]
        b = tail (init lvls)
        nlvls = h ++ concat [[x] ++ [x] |x <-b] ++ l   

----------------------------------------------      
        
diagramN :: Nat -> Diagram B  
diagramN (VerN nat01) = (diagramH h) === var
    where
    var = if ((length nat01) > 2)
            then vcat [diagramB n | n <- n02] === (diagramL l)
            else (diagramL l)
    nat = reverse nat01
    h = head nat
    n01 = tail nat
    n02 = init nat
    l= last nat
diagramN (HorN nat) = hcat[diagramN n | n <-(nat)]
diagramN (IdN (IdF x)) = circlesN <> (title01 === bodyN === title02) # center 
    where
        circlesN = (circleNatI ("id" ++ x) # center # dashingN [0.01,0.01] 0 )
        title01 = (rectsF ("id" ++ x)) # center
        title02 = (rectsF ("id" ++ x)) # center
        bodyN = (lineID <> bodyC "") # center
        lineID = (vrule 2 ) # dashingN [0.03,0.03] 0  #center        
diagramN (IdN nat) =  circlesN <> (title01 === bodyN === title02) # center 
    where
        circlesN = (circleNatI ("id" ++ (nameF nat)) # center # dashingN [0.01,0.01] 0 )
        title01 = (rectsF (nameF nat)) # center
        title02 = (rectsF (nameF nat)) # center
        bodyN = (bodiesF [nat]) # center        
diagramN nat =  (title01  === bN === title02 ) # center
    where
        title01 = (tNaturals (tF + 1) ts) # center
        title02 = (tNaturals (tF + 1) os) # center
        bN = (circlesN <> ((bT === bO ) # center )) # center
        circlesN = circleNat (nameN nat) # center
        bT = (bodyT 1 (tF + 1) 1.5 (target nat)) # center
        bO = (bodyT (-1) (tF + 1) 1.5 (originN nat)) # center
        tF = max (length ts) (length os)
        ts= fNames (target nat)
        os= fNames (originN nat)
        
diagramH (HorN nat) = hcat[diagramH n | n <-(nat)]
diagramH (IdN nat) = (title01 === bN) # center
    where   
        bN  = (nn <> ((bT === bO) # center )) # center
        nn= juxtapose (- unitY) (square 0) (circlesN)       
        circlesN = (circleNat ("id" ++ (nameF nat)) # center # dashingN [0.01,0.01] 0 )
        title01 = (tNaturals (2) t)# center
        t= fNames (nat)
        bT = (bodyT 1 2 1.5 nat) # center
        bO = (bodyT (-1) 2 0.75 nat) # center
diagramH nat =  (title01 === bN) # center
    where   
        bN =(nn <> ((bT === bO) # center )) # center
        nn= juxtapose (- unitY) (square 0) (circlesN)
        circlesN = circleNat (nameN nat) # center
        tF = max (length ts) (length os)
        title01 = (tNaturals (tF + 1) ts) # center
        ts= fNames (target nat)
        os= fNames (originN nat)
        bT = (bodyT 1 (tF + 1) 1.5 (target nat)) # center
        bO = (bodyT (-1) (tF + 1) 0.75 (originN nat)) # center

diagramL (HorN nat) = hcat[diagramL n | n <-(nat)]
diagramL (IdN nat) = (bN === title02) # center
    where   
        bN  = (nn <> ((bT === bO) # center )) # center
        nn= juxtapose unitY (square 0) (circlesN)       
        circlesN = (circleNat ("id" ++ (nameF nat)) # center # dashingN [0.01,0.01] 0 )
        title02 = (tNaturals (2) t)# center
        t= fNames (nat)
        bT = (bodyT 1 2 0.75 nat) # center
        bO = (bodyT (-1) 2 1.5 nat) # center
diagramL nat = (bN === title02) # center
    where
        bN = (nn <> ((bT === bO ) # center )) #center
        nn= juxtapose unitY (square 0) (circlesN)
        circlesN = circleNat (nameN nat) # center
        tF = max (length ts) (length os)
        title02 = (tNaturals (tF + 1) os) #center
        ts= fNames (target nat)
        os= fNames (originN nat)
        bT = (bodyT 1 (tF + 1) 0.75 (target nat)) # center
        bO = (bodyT (-1) (tF + 1) 1.5 (originN nat)) # center
---
diagramB (HorN nat) = hcat[diagramB n | n <-(nat)]
diagramB (IdN nat) = (bN) # center
    where   
        bN  = (nn <> ((bT === bO) # center )) # center
        nn= juxtapose (- unitY) (square 0.375) (circlesN)       
        circlesN = (circleNat ("id" ++ (nameF nat)) # center # dashingN [0.01,0.01] 0 )
        title02 = (tNaturals (2) t)# center
        t= fNames (nat)
        bT = (bodyT 1 2 0.75 nat) # center
        bO = (bodyT (-1) 2 0.75 nat) # center        
diagramB nat =  bN # center
    where       
        bN= (circlesN <> ((bT === bO) # center ))# center
        circlesN = circleNat (nameN nat) # center
        tF = max (length ts) (length os)
        ts= fNames (target nat)
        os= fNames (originN nat)
        bT = (bodyT 1 (tF + 1) 0.75 (target nat)) # center
        bO = (bodyT (-1) (tF + 1) 0.75 (originN nat)) # center      
        
isIdF :: Func -> Bool 
isIdF (IdF  n) = False
isIdF n = True
        
bodyT :: Double -> Int -> Double -> Func -> Diagram B 
bodyT o w h (HorF f) = cc <> baseSq 
    where
        l = length (f) 
        widF =   fromIntegral(w) / (fromIntegral(l) *2)
        xs2 = [((widF * fromIntegral(n)) - (fromIntegral(w)/2)) | n <- [1,3..(l*2)]]
        baseSq = (hcat $(replicate (w) c1)) # center
        fTypes = [isIdF n | n <- (f)]
        c1 = rect 1 h # opacity 0
        cc = (mconcat $ [(ccurve (fTypes !! n) 0 0 (xs2 !! n) (h*o)) | n <- [0..(l-1)]]) # center   
bodyT o w h (IdF f) = cc <> baseSq 
    where
        l = 1
        widF =   fromIntegral(w) / (fromIntegral(l) *2)
        xs2 = [(widF * fromIntegral(n)) - (fromIntegral(w)/2) | n <- [1,3..l]]
        baseSq = (hcat $(replicate (w) c1)) # center
        c1 = rect 1 h # opacity 0
        cc = (mconcat $ [(ccurve False 0 0 n (h*o)) | n <- xs2]) # center
bodyT o w h f = cc <> baseSq 
    where
        l = 1
        widF =   fromIntegral(w) / (fromIntegral(l) *2)
        xs2 = [(widF * fromIntegral(n)) - (fromIntegral(w)/2) | n <- [1,3..l]]
        baseSq = (hcat $(replicate (w) c1)) # center
        c1 = rect 1 h # opacity 0
        cc = (mconcat $ [(ccurve True 0 0 n (h*o)) | n <- xs2]) # center
        
ccurve :: Bool -> Double -> Double ->  Double -> Double -> Diagram B    
ccurve True x1 y1 x2 y2 =  strokeLocT  mm 
    where
        mm :: Located (Trail V2 Double)
        mm = fromSegments [bézier3 c1 c2 z2]  `at` (0 ^& (-(y2/2)))
        z2 = r2 (x2,y2) :: V2 Double     -- endpoint
        [c1,c2] = map r2 [(x2,y1), (x2, (y2/2))]     -- control points
ccurve False x1 y1 x2 y2 =  strokeLocT  mm # dashingN [0.03,0.03] 0 
    where
        mm :: Located (Trail V2 Double)
        mm = fromSegments [bézier3 c1 c2 z2]  `at` (0 ^& (-(y2/2)))
        z2 = r2 (x2,y2) :: V2 Double     -- endpoint
        [c1,c2] = map r2 [(x2,y1), (x2, (y2/2))]     -- control points      
        
tNaturals :: Int -> [String] -> Diagram B
tNaturals sze tas = tiT <> baseSq 
    where
        fT = fromIntegral(length tas)
        widF =   fromIntegral(sze) / fT 
        baseSq = (hcat $(replicate (sze) c1)) # center
        tiT = (hcat $[rectsTFN n widF | n <- tas]) # center
        c1 = rect 1 0.5 # opacity 0
        
rectsTFN :: String -> Double -> Diagram B
rectsTFN t widF = textsF t <> r
    where
        r = rect widF 0.5 # opacity 0
    
fNames  :: Func -> [String]   
fNames (HorF f) = (concat [fNames n | n <- f])
fNames (IdF f) = [("id" ++ f)]
fNames f = [nameF f]

circleNatI :: String -> Diagram B        
circleNatI n = ((textsN n) <> c) # fontSize (local 0.3)
    where
        c=circle 0.23 #fc white
         
circleNat :: String -> Diagram B        
circleNat n = ((textsN n) <> c) # fontSize (local 0.3)
    where
        c=circle 0.3 #fc white

bodiesF :: [Func] -> Diagram B
bodiesF  f = hcat rects
    where
        rects =  firstF : [((vrule 2 ) ||| rectsC n)| n <- codF]
        firstF = rectsC (domain (head f))
        codF = [codomain n | n <- f]
rectsC :: String -> Diagram B
rectsC t = textsF t <> r
    where
        r = rect 1 2 # dashingN [0.03,0.03] 0   
        
titles :: [Func] -> Diagram B
titles f =  cat' (r2 (1,0)) (with & catMethod .~ Distrib & sep .~ 1 ) (rects)
    where
        rects = [rectsF n | n <- namesF]
        namesF = [nameF n | n <- f]
        
textsF :: String -> Diagram B
textsF t = text t # fontSize (normalized 0.1)# font "freeserif"

textsN :: String -> Diagram B
textsN t = text t # font "freeserif"

rectsF :: String -> Diagram B
rectsF t = textsF t <> r
    where
        r = rect 2 0.5 # opacity 0

outputFile :: String -> FilePath
outputFile name = "static/img/" ++ name ++".svg"

sizing ::  Num n => SizeSpec V2 n
sizing = mkSizeSpec $ V2 (Just 1100) (Just 700)      

------------------------------------------------------------------------------------------------
checkN :: Nat -> Bool
checkN n = (checkNamesN n) && (checkCoherenceN n) && (checkRelN n)

checkF :: Func -> Bool 
checkF f = (checkNamesF f) && (checkCoherenceF f) && (checkRelF f)

-- ------------CHECK NATURAL TRANSFORMATIONS RELATIONS---------------------------------------
checkRelN :: Nat -> Bool
checkRelN (HorN nat) = res
    where
        res = i && j
        i = all (==True) ([checkRelN x | x <- nat])
        j = checkNatLinks nat
checkRelN (VerN nat) = res
    where
        res = i && j
        i = all (==True) ([checkRelN x | x <- nat])
        j = verifyVLinks nat
checkRelN (IdN nat) = True
checkRelN nat = all (==True) [i,j,k]
    where
        i = checkRelF (originN nat)
        j = checkRelF (target nat)
        k = (getBorders (originN nat) == getBorders (target nat))

verifyVLinks :: [Nat] -> Bool
verifyVLinks [] = True
verifyVLinks arr = ans
    where
        ans = aux  && nextN
        tailN = tail arr
        aux = if(tailN/=[]) then (any (`elem` (origins (head arr))) (targets (head tailN))) else True
        nextN = verifyVLinks tailN

targets :: Nat -> [Func]
targets (HorN n) = concat [targets x | x <- n]
targets (VerN n) = concat [targets x | x <- n]
targets (IdN n) = [n]
targets n = getFunctors (target n)
    
origins :: Nat -> [Func]
origins (HorN n) = concat [origins x | x <- n]
origins (VerN n) = concat [origins x | x <- n]
origins (IdN n) = [n]
origins n = getFunctors (originN n)
        
-- Verify links between natural transformations 
checkNatLinks :: [Nat] -> Bool
checkNatLinks [natL] = True
checkNatLinks natL = res
    where
        tailL = tail natL
        res = link && nextL
        link = (natDomain (head natL) == natCodomain (head tailL))
        nextL = checkNatLinks tailL
-- Verify relations between functor compositions
checkRelF :: Func -> Bool
checkRelF (HorF funct) = (checkLinks funct)
checkRelF funct = True
-- Get external domain and codomain of natural transformations
natCodomain :: Nat -> String
natCodomain (HorN natL) = codomainF
    where
        codomainF = natCodomain (head natL)
natCodomain (VerN natL) = codomainF
    where
        codomainF = natCodomain (head natL)
natCodomain (IdN funct) = getFunctCodomain funct
natCodomain nat = codomainF
    where
        codomainF = getFunctCodomain (target nat)

natDomain :: Nat -> String
natDomain (HorN natL) = domainF
    where 
        domainF = natDomain (last natL)
natDomain (VerN natL) = domainF
    where
        domainF = natDomain (head natL)
natDomain (IdN funct) = getFunctDomain funct
natDomain nat = domainF
    where
        domainF = getFunctDomain (originN nat)  

getFunctDomain :: Func -> String
getFunctDomain (HorF funcL) = getFunctDomain (last funcL)
getFunctDomain (IdF a) = a  
getFunctDomain funct = domain funct

getFunctCodomain :: Func -> String
getFunctCodomain (HorF funcL) = getFunctCodomain (head funcL)
getFunctCodomain (IdF a) = a
getFunctCodomain funct = codomain funct
-- Check links between functors 
checkLinks :: [Func] -> Bool
checkLinks [] = True
checkLinks [a] = True
checkLinks listF = res
    where
        tailL = tail listF
        res = link && nextL
        link = (getFunctDomain (head listF) == getFunctCodomain (head tailL))
        nextL = checkLinks tailL
        
getBorders :: Func -> [String]
getBorders (HorF funct) = [firstF,lastF]
    where
        firstF = getFunctDomain  (last funct)
        lastF  = getFunctCodomain(head funct)
getBorders funct = [firstC,lastC]
    where
        firstC = getFunctDomain funct
        lastC  = getFunctCodomain funct

-------- CHECK EXPRESION COHERENCE  --------------------------------------- 
checkCoherenceN :: Nat -> Bool
checkCoherenceN input = checked
        where
            elements = getMembers input
            naturals = fst elements
            functors = snd elements
            resNat = [(any (/=nat) (filter (\x -> nameN x==nameN nat) (naturals))) | nat<-naturals]
            resFunc = [(any (/=func) (filter (\x -> nameF x==nameF func) (functors))) | func<-functors]
            checked = all (==False) (resNat++resFunc)

getMembers :: Nat -> ([Nat],[Func])   
getMembers (HorN nat)
    | True = (i,j)
    where
        y = [getMembers(x) | x <- nat]
        i = concat [fst w | w <- y]
        j = concat [snd w | w <- y]
getMembers (VerN nat)
    | True = (i,j)
    where
        y = [getMembers(x) | x <- nat]
        i = concat [fst w | w <- y]
        j = concat [snd w | w <- y]
getMembers (IdN funct)
    | True = ([],j)
    where
        j = getFunctors funct
getMembers nat
    | True = ( [nat], originFunctors ++ targetFunctors)
    where
        originFunctors = getFunctors (originN nat)
        targetFunctors = getFunctors (target nat)

checkCoherenceF :: Func -> Bool
checkCoherenceF input = checked
        where
            functors = getFunctors input
            resFunc = [(any (/=func) (filter (\x -> nameF x==nameF func) (functors))) | func<-functors]
            checked = all (==False) (resFunc)
        
getFunctors :: Func -> [Func]
getFunctors (HorF funct)
    | True = j
    where
        j = concat [(getFunctors w) | w <- funct]
getFunctors (IdF cat) = []
getFunctors funct = [funct]

-------------CHECK EXPRESION NAMES------------------------------
checkNamesN :: Nat -> Bool
checkNamesN input   
    | any (\nat -> (elem nat functors)) (naturals) = False
    | any (\nat -> (elem nat categories)) (naturals) = False
    | any (\funct -> (elem funct categories)) (functors) = False
    | otherwise = True
    where
        names = getNames input
        naturals = fst_3Tuple names
        functors = snd_3Tuple names
        categories = trd_3Tuple names
            
getNames :: Nat -> ([String],[String],[String])
getNames (HorN nat) 
    | True = (i,j,k)
    where
        y = [getNames(x) | x <- nat]
        i = concat [fst_3Tuple(w) | w <- y]
        j = concat [snd_3Tuple(w) | w <- y]
        k = concat [trd_3Tuple(w) | w <- y]
getNames (VerN nat)
    | True = (i,j,k)
    where
        y = [getNames(x) | x <- nat]
        i = concat [fst_3Tuple(w) | w <- y]
        j = concat [snd_3Tuple(w) | w <- y]
        k = concat [trd_3Tuple(w) | w <- y]
getNames (IdN nat)
    | True = (i,j,k)
    where
        y = getFuncNames(nat)
        i = fst_3Tuple(y)
        j = snd_3Tuple(y)
        k = trd_3Tuple(y)
getNames nat
    | True = ([nameNat], originFunctors ++ targetFunctors, originCats ++ targetCats )
    where
        originF = (originN nat)
        targetF = (target nat)
        nameNat = [n | n <- (nameN nat)]
        originData = getFuncNames(originN nat)
        targetData = getFuncNames(target nat)
        originFunctors = snd_3Tuple(originData)
        originCats = trd_3Tuple(originData)
        targetFunctors = snd_3Tuple(targetData)
        targetCats = trd_3Tuple(targetData)

checkNamesF :: Func -> Bool
checkNamesF input   
    | any (\funct -> (elem funct categories)) (functors) = False
    | otherwise = True
    where
        names = getFuncNames input
        functors = snd_3Tuple names
        categories = trd_3Tuple names       
        
getFuncNames :: Func -> ([String],[String],[String])
getFuncNames (HorF funct) 
    | True = (i , j, k)
    where
        functors = [getFuncNames(x) | x <- funct]
        i = concat [fst_3Tuple(w) | w <- functors]
        j = concat [snd_3Tuple(w) | w <- functors]
        k = concat [trd_3Tuple(w) | w <- functors]
getFuncNames (IdF cat)
    | True = ([],[],[cat])
getFuncNames funct 
    | True = ([], [nameFunct] , [domainFunc] ++ [codomainFunct])
    where
        nameFunct = (nameF funct)
        domainFunc = (domain funct)
        codomainFunct = (codomain funct)
        
----------------get Triples elements------------        

fst_3Tuple :: ([String],[String],[String]) -> [String]  
fst_3Tuple (a,_,_) = a
    
snd_3Tuple :: ([String],[String],[String]) -> [String]  
snd_3Tuple (_,a,_) = a  

trd_3Tuple :: ([String],[String],[String]) -> [String]  
trd_3Tuple (_,_,a) = a                       
