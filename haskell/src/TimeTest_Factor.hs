module TimeTest_Factor where

import CodeGen
import FField
import NTT
import CompileKernel
import Search
import Fourier
import PolyRings
import PolyMult
import Data.List


--timeSize :: Ring -> IO [(Int,Int,Float)]
--timeSize r = let n = get_size r in
--  let power=round (log n / log 2) in if 2**power/=n then error show n++" is not power of 2" else 
--timeFactorPath :: Ring -> Int -> Int -> Int -> IO Float
factorPath r k t l = let path = turtles r (Factor k) in
  let npath=path>>= (\x -> (Just (takePath t x))) >>= (\x -> turtlesExtend x (Factor l)) in
      npath

listFactorPaths :: Integral a => Int -> [(a,a,a)]
listFactorPaths n = let fn = fromIntegral n in
  let l=foldr (++) [] [[(i,j) | j <-[0..(fn/2/i)]] | i<-[1..fn/2]] :: [(Float,Float)] in
    fmap (\(i,j) -> ( round (2**i),round j,round (((2**i)**j)*(2**(fromIntegral (mod n (round i))))))) l


timeFactorPaths logn p = let n=round (2**(fromIntegral logn)) in (fmap (\(k,t,l) -> pure (\x -> (k,l,x)) <*> (timePath (factorPath (Base n 0 n p) k (-t) l) "Gen")) (listFactorPaths logn))
