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

instance Species Path where
  --sample :: RandomGen b => a -> b -> Maybe (a,b)
  sample specimen rand = randomPathGen (path_get_start specimen) rand
  
  --combine :: RandomGen b => b -> a -> a -> Maybe (a,b)
  combine rand spec1 spec2 = Just (randomChoice [spec1,spec2] rand)
  
  --fitness :: a -> Float
  fitness specimen = timePath (Just specimen) "Genetic"

