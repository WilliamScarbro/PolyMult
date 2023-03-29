module TimeTest_Factor where

import CodeGen
import FField
import NTT
import CompileKernel
import Search
import Fourier
import PolyRings
import Data.List


--timeSize :: Ring -> IO [(Int,Int,Float)]
--timeSize r = let n = get_size r in
--  let power=round (log n / log 2) in if 2**power/=n then error show n++" is not power of 2" else 
--timeFactorPath :: Ring -> Int -> Int -> Int -> IO Float
factorPath :: Ring -> Int ->  Int -> Int -> IO Path
factorPath r k t l = do { path <- turtles r (Factor k);
                          short_path <- return (takePath t path);
                          turtlesExtend short_path (Factor l) }

listFactorPaths :: Integral a => Int -> [(a,a,a)]
listFactorPaths n = let fn = fromIntegral n in
  let l=foldr (++) [] [[(i,j) | j <-[0..(fn/2/i)]] | i<-[1..fn/2]] :: [(Float,Float)] in
    fmap (\(i,j) -> ( round (2**i),round j,round (((2**i)**j)*(2**(fromIntegral (mod n (round i))))))) l


timeFactorPaths logn p = let n=round (2**(fromIntegral logn)) in do { -- []
  (k,t,l) <- listFactorPaths logn;
  return (pure (\x -> (k,l,x)) <*>( (factorPath (Base n 0 n p) k (-t) l) >>= (\p -> timePath p "Gen"))); }
