--{-# LANGUAGE TypeSynonymInstances #-}
--{-# LANGUAGE FlexibleInstances #-}
-- 
module :FField where


import qualified Data.Set as Set

--import AbstractAlgebra.Fields

class Residue a where
  reduce :: a -> a
  comparable :: a -> a -> Bool
  
class ResidueRing a where
  add :: a -> a -> Maybe a
  neg :: a -> a
  sub :: a -> a -> Maybe a
  sub x y = add x (neg y) 
  mult :: a -> a -> Maybe a
  one :: a
  zero :: a

data ResInt = Res Integer Integer deriving (Show)

instance Residue ResInt where
  reduce (Res x 0) = Res x 0
  reduce (Res x p) = Res (mod x p) p
  comparable (Res x p) (Res y q) = p==q
  
instance Eq ResInt where
  (==) = res_cmp (==)
  --x == y = get_rep x == get_rep y && comparable x y --get_mod x == get_mod y
  
instance Ord ResInt where
  (<=) = res_cmp (<=)
  --x <= y = get_rep x < get_rep y && comparable x y --get_mod x == get_mod y

instance ResidueRing ResInt where
  add = res_op (+)
  neg (Res x p) = Res (0-x) p
  mult = res_op (*)
  one = Res 1 0
  zero = Res 0 0

get_rep :: ResInt -> Integer
get_rep res = let Res y p = reduce res in y

get_mod :: ResInt -> Integer
get_mod ( Res x p ) = p

set_rep :: Integer -> Integer -> ResInt
set_rep p x = Res x p

res_op :: (Integer -> Integer -> Integer) -> (ResInt -> ResInt -> Maybe ResInt)
res_op op (Res x p) (Res y q) | (p /= q) && (p /= 0) && (q /= 0) = Nothing
                              | (p == 0) && (q /= 0) = res_op op (Res x q) (Res y q)
                              | (p /= 0) && (q == 0) = res_op op (Res x p) (Res y p)
                              | (p == q) = Just (Res (op x y) p)

res_cmp :: (Integer -> Integer -> Bool) -> (ResInt -> ResInt -> Bool)
res_cmp op (Res i p) (Res j q) | (p /= q) && (p /= 0) && (q /= 0) = False
                               | (p == 0) && (q /= 0) = res_cmp op (Res i q) (Res j q)
                               | (p /= 0) && (q == 0) = res_cmp op (Res i p) (Res j p)
                               | (p == q) = (op (mod i p) (mod j p))

is_prime :: (Integral a) => a -> Bool
is_prime x = length (divisors x) == 2
divisors :: (Integral a) => a -> [a]
divisors x = [1] ++ (filter (\z -> mod x z == 0) [2..floor . sqrt . fromIntegral $ x]) ++ [x]

power_set :: ResInt -> Set.Set ResInt
power_set (Res x p) = Set.map (set_rep p) (power_set_help x x p p)

power_set_help ::  (Integral a) => a -> a -> a -> a -> Set.Set a
power_set_help y z p 0  = Set.empty
power_set_help y z p q =  if q > 0 then Set.insert y ( power_set_help (mod (z*y) p) z p (q-1) ) else Set.empty
--
is_generator :: ResInt -> Bool
is_generator (Res x p) = ( toInteger . Set.size $ power_set (Res x p) ) == p-1
--
---- Utility functions
ff_generators :: Integer -> [ResInt]
ff_generators p | is_prime p = map (set_rep p) $ filter (\x -> is_generator (Res x p) ) [1..p-1]
                | otherwise = []
ff_generator :: Integer -> ResInt
ff_generator p = head (ff_generators p)
--
ff_inv :: ResInt -> Maybe ResInt
ff_inv x | x==zero = Nothing
         | otherwise = let p=get_mod x in Just (fst ( head ( filter (\(y,z) -> z==Just one) [(y,x `mult` y) | y<-[Res i p| i<-[1..p-1]]] ) ) )
  
class ResidueRing a => FiniteField a where
  inv :: a -> Maybe a
  divi :: a -> a -> Maybe a
  divi x y = ( inv y ) >>= mult x

instance FiniteField ResInt where
  inv = ff_inv
 
--  add ( Res i n  ) ( Res j m ) = if n == m then Just mod ( Res (i+j) n ) n else Nothing
--  mult ( Res i n  ) ( Res j m ) = if n == m then Just mod ( Res (i*j) n ) n else Nothing
--  sub ( Res i n  ) ( Res j m ) = if n == m then Just mod ( Res (i-j) n ) n else Nothing
--  zero = 
--
