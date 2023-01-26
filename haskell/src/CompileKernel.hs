module CompileKernel where

-- assume b p are common accross all kernels, this allows a common w

import NTT
import Data.Maybe
import Data.List
import qualified Data.Map as Map (fromList,empty,insert,Map,member,lookup,mapWithKey,mapAccum,mapAccumWithKey)
 
--- [Kernel] --> C file

type Path=[Kernel]
type PCC = Map.Map Kernel (Int,String)


-- name -> path -> code
compilePath :: Path -> PCC -> String
compilePath p pcc = let compKers = fmap (\ker -> compileKernel 0 ker pcc) p in
  let steps = foldr (++) [] (fmap maybeToList compKers) in
    "\n  //Computation\n" ++ foldl (\x y -> (x++swap)++y) (head steps) (tail steps)

--  fmap (Kernel -> String) [Kernel]
-- Maybe (Integer,String) >>= (String -> Maybe String)

--compilePath p km = let steps = foldr (++) [] (fmap maybeToList (fmap (\ker -> compileKernel 0 ker (Map.lookup km) p)) in

showTuple :: Show a => [a] -> String
showTuple t = let st = fmap show t in foldl (\x y -> (x++","++y)) (head st) (tail st)

compileKernel :: Int -> Kernel -> PCC -> Maybe String
--compileKernel = undefined
compileKernel o (Phi n k d b p) pcc = Just ("  Phi("++(showTuple [n,k,d,b,p])++","++(endXY o)++","++(endW (Phi n k d b p) pcc)++");\n")
compileKernel o (KL n k) pcc = Just ("  LPerm("++(showTuple [n,k])++","++(endXY o)++");\n")
compileKernel o (KId n) pcc = Nothing
compileKernel o (Gamma n d b p) pcc = Just ("  Gamma("++(showTuple [n,d,b,p])++","++(endXY o)++","++(endW (Gamma n d b p) pcc)++");\n")
compileKernel o (Kernel_Repeat n k ker) pcc = Just (foldr (++) "" (foldr (++) [] (fmap maybeToList [compileKernel (o+(div n k)*i) ker pcc | i<-[0..k-1]])))
compileKernel o (Kernel_Extend n k f) pcc = Just (foldr (++) "" (foldr (++) [] (fmap maybeToList [(f i) >>= (\ker -> compileKernel (o+(div n k)*i) ker pcc) | i<-[0..k-1]]))) 

endW :: Kernel -> PCC -> String
endW ker pcc = squashMaybeString (do { mtup <- Map.lookup ker pcc;
                                          (\x -> Just (snd x)) mtup }) "<Error: couldn't find kernel in PCC Map>"

swap :: String
swap = "  swap(X,Y);\n"

endXY :: Int -> String
endXY o = "*X+"++show o++",*Y+"++show o

squashMaybeString :: Maybe String -> String -> String
squashMaybeString (Just str) msg = str
squashMaybeString Nothing msg = msg
---

squashMaybeBool :: Maybe a -> (a -> Bool) -> Bool
squashMaybeBool (Just t) f = f t
squashMaybeBool Nothing _ = False

reqPCC :: Kernel -> Bool
reqPCC (Phi _ _ _ _ _) = True
reqPCC (KL _ _) = False
reqPCC (KId _) = False
reqPCC (Gamma _ _ _ _) = True
reqPCC (Kernel_Repeat _ _ k) = reqPCC k
reqPCC (Kernel_Extend _ _ f) = squashMaybeBool (f 0) reqPCC

---

listLeafs :: Kernel -> [Kernel]
listLeafs (Phi n k d b p) = [Phi n k d b p]
listLeafs (KL _ _) = []
listLeafs (KId _) = []
listLeafs (Gamma n d b p) = [Gamma n d b p]
listLeafs (Kernel_Repeat n k ker) = listLeafs ker
listLeafs (Kernel_Extend n k f) = let maybeKernels = [f i >>= (\ker -> Just (listLeafs ker)) | i<-[0..k-1]] in
  foldl (++) [] (foldl (++) [] (fmap maybeToList maybeKernels))

precompute :: Kernel -> String -> String
precompute (Phi n k d b p) pcc_name = "  Phi_W(w,"++showTuple [d,b,k,p]++","++pcc_name++");\n"
precompute (Gamma n d b p) pcc_name = "  Gamma_W(w,"++showTuple [n,d,b,p]++","++pcc_name++");\n"
--precompute (Kernel_Repeat n k ker) pcc_name = precompute ker pcc_name
--precompute (Kernel_Extend n k f) pcc_name = foldr (++) "" [precompute (f i) pcc_name | i<-[0..k]]
--
--
----
associateKernels :: Path -> PCC
associateKernels p = let rp = nub (filter reqPCC (foldr (++) [] (fmap listLeafs p))) in
  Map.fromList [(rp!!i, associateKernel i (rp!!i)) | i<-[0..(length rp)-1]]
--
associateKernel :: Int -> Kernel -> (Int,String)
associateKernel c (Phi n k d b p) = (n, ("W"++show c))
associateKernel c (Gamma n d b p) = (n, ("W"++show c))
associateKernel c _ = (0,"Error: Kernel does not need PCC")

----
--

initializePCC :: PCC -> String
initializePCC pcc = let allocs =fst (Map.mapAccum (\pred (size,name) -> (pred++"  int* "++name++" = malloc("++show size++"*sizeof(int));\n",0)) "" pcc) in
  let assigns = fst (Map.mapAccumWithKey (\pred ker (size,name) -> (pred++(precompute ker name),0)) "" pcc) in
    "\n  //Pre-Compute Constants\n"++allocs ++ assigns
    
  --  let assigns = Map.mapWithKey (\ker (size,name) -> precompute ker name) pcc in
--    (Map.mapAccum (\x y -> (x++y,y)) allocs) ++ (Map.mapAccum (\x y -> (x++y,y)) assigns)
    
destroyPCC :: PCC -> String
destroyPCC pcc = "\n  //free Pre-Computed Constants\n" ++ fst (Map.mapAccum (\pred (size,name) -> (pred++"  free("++name++");\n",0)) "" pcc)

--

initialize_w :: (Integer,Integer) -> String
initialize_w (b,p) = "  int w = Nth_root("++show p++",generator("++show p++"),"++show b++");\n"
--

compile :: (Integer,Integer) -> String -> Path -> String
compile bp name path = let iw = initialize_w bp in
  let pcc = associateKernels path in
    let ip = initializePCC pcc in
      let cp = compilePath path pcc in
        let dp = destroyPCC pcc in
          "void "++name++"(int** X,int** Y){\n"++iw++ip++cp++dp++"}\n"
--
