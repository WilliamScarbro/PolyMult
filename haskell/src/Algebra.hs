module SymbolicRingAlgebra where

import FField
import NTT
import PolyRings
import Fourier

-- add morph and inv-morph later
data RingExpr = Add RingExpr RingExpr | Mult RingExpr RingExpr | Term
--  Neg NegExpr | Morph Morphism RingExpr | InvMorph Morphism RingExpr

-- 
data OperMap a = OM Int Int ([a]->[a])

instance Show (OperMap a) where
  show (OM i o f) = "OM "++show i++" "++show o

oper_size_in :: OperMap a -> Int
oper_size_in (OM n m f) = n
oper_size_out :: OperMap a -> Int
oper_size_out (OM n m f) = m
oper_func :: OperMap a -> [a] -> [a]
oper_func (OM n m f) = f

extendOM :: Int -> (Int -> OperMap a) -> OperMap a
extendOM k f = let oms=[f i | i<-[0..k-1]] in
  OM (foldr (+) 0 (fmap oper_size_in oms)) (foldr (+) 0 (fmap oper_size_out oms)) (eom_help k f)
  
eom_help :: Int -> (Int -> OperMap a) -> [a] -> [a]
eom_help k f x | k>0 = ((oper_func (f 0)) (take (oper_size_in (f 0)) x)) ++ (eom_help (k-1) (\i -> f (i+1)) (drop (oper_size_in (f 0)) x))
               | k<=0 = []

re_to_fe :: RingExpr -> Ring -> OperMap FF
re_to_fe re (Prod n k f) = OM 0 0 id
