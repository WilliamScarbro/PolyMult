{-# LANGUAGE FlexibleInstances #-}


module PolyRings where

instance Show (Int -> Maybe Ring) where
  show f = show (f 0)

instance Eq (Int -> Maybe Ring) where
  (==) f1 f2 = (f1 0) == (f2 0)

instance Ord (Int -> Maybe Ring) where
  (<=) f1 f2 = (f1 0) <= (f2 0)
  
data Ring = Base Int Int Int Int | Prod Int Int (Int -> Maybe Ring) | Quo Int Int Int Ring deriving (Show,Eq,Ord)

get_root :: Ring -> Int
get_root (Base _ _ b _) = b
get_root (Prod _ _ f) = squashMaybeInt (f 0) get_root
get_root (Quo _ _ _ r) = get_root r

get_prime :: Ring -> Int
get_prime (Base _ _ _ p) = p
get_prime (Prod _ _ f) = squashMaybeInt (f 0) get_prime
get_prime (Quo _ _ _ r) = get_prime r

get_size :: Ring -> Int
get_size (Base n _ _ _) = n
get_size (Prod _ _ f) = squashMaybeInt (f 0) get_size
get_size (Quo _ _ _ r) = get_size r


--phi
factor :: Int -> Ring -> Maybe Ring
factor k (Base n d b p) = Just (Prod n k (\i -> Just (Base (n `div` k) ((d `div` k)+i*(b `div` k)) b p)))
factor k r = Nothing

--xi
label :: Int -> Ring ->  Maybe Ring
label k (Base n d b p) = Just (Quo n k 0 (Base (n `div` k) d b p))
label k r = Nothing

--gamma
norm :: Ring -> Maybe Ring
norm (Base n d b p) = Just (Quo n 1 (div d n) (Base n 0 b p))
norm r = Nothing

--psi
define :: Ring -> Maybe Ring
define (Quo n k d0 (Base 1 d b p)) = Just (Base k (d0+d) b p)
define r = Nothing

--zeta
pushin :: Ring -> Maybe Ring
pushin (Quo nq kq d0 (Prod np kp f)) = let nf = div np kp in
  let new_nq = nf*kq in
  Just (Prod (new_nq*kp) kp (\i -> (f i) >>= (\g -> Just (Quo new_nq kq d0 g))))
pushin r = Nothing

-- a bit hacky
squashMaybeInt :: Maybe a -> (a -> Int) -> Int
squashMaybeInt (Just a) f = f a
squashMaybeInt Nothing f = 0