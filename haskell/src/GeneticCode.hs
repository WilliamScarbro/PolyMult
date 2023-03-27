{-# LANGUAGE FlexibleInstances #-}

module GeneticCode where

import FField
import NTT
import CompileKernel
import Search
import Fourier
import PolyRings
import PolyMult
import Genetic
import CodeGen
import Logger

instance Species Path where
  --sample :: RandomGen b => a -> Std -> Maybe (a,StdGen)
  sample specimen rand = randomPathGen (path_get_start specimen) rand
  
  --combine :: RandomGen b => StdGen -> a -> a -> Maybe (a,StdGeb)
  combine rand spec1 spec2 = combinePaths2 rand spec1 spec2
  
  --fitness :: a -> IO Float
  fitness specimen = (logObj "Timing: " specimen) >> fmap (\x -> -1*x) (timePath (Just specimen) "Genetic")

