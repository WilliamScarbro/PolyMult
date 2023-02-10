--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Fourier where

import FField
import PolyRings
import NTT
import Data.List
import Data.Maybe
import qualified Data.Map as Map (empty,insert,Map,member)

---

instance Show (Int -> Morphism) where
  show f = show (f 0)

instance Eq (Int -> Morphism) where
  (==) f1 f2 = (f1 0) == (f2 0)
  
data Morphism = Inverse Morphism | Extend Int Morphism | Repeat Int Morphism | Factor Int | Label Int | Norm | Define | Pushin | IdR deriving (Show,Eq)

--instance Show Morphism where
--  show Compose m1 m2 = show m1++" . "++shw m2
--  show Extend k f = "Extend "++show k++" "++show (f 0)
--  show Repeat k m = "Repeat "++show k++" "++show m
--  show Factor k = "Factor "++show k++

apply :: Morphism -> Ring -> Maybe Ring
apply (Label k) x = (label k) x
apply (Factor k) x = (factor k) x
apply (Norm) x = norm x
apply Define x = define x
apply Pushin x = pushin x
--apply (Compose m1 m2) x = (apply m2 x) >>= (\y -> apply m1 y)
apply (Extend k0 m) (Prod n k f) | k0==k = Just (Prod n k (\i -> f i >>= (\g -> apply m g)))  --- ( int -> Maybe R ) >>= (Ring -> Maybe Ring)  :: Int -> Maybe Ring
                                 | otherwise = Nothing
apply (Repeat i m) (Quo n j k x) | i==j = (apply m x) >>= (\y -> Just (Quo n j k y))
                                 | otherwise = Nothing
apply IdR x = Just x

---

data Match = Match (Ring -> [Morphism])

match :: Match -> Ring -> [Morphism]
match (Match m) r = m r

matchAdd :: Match -> Match -> Match
matchAdd (Match m1) (Match m2) = Match (\r -> (m1 r) ++ (m2 r))

infixl 2 <+>
(<+>)=matchAdd
  
matchMorphism :: Match
matchMorphism = (Match matchExtend) <+> (Match matchRepeat) <+> (Match matchFactor) <+> (Match matchLabel) <+> (Match matchDefine) <+> (Match matchPushin) -- <+> (Match matchId) <+> (Match matchNorm)

-- 

-- insures that any matched morphism can be applied to entire domain of f 
matchExtend :: Ring -> [Morphism]
matchExtend (Prod n k f) = 
--let morphs=[(maybeToList (f i)) >>= (\r -> (match matchMorphism r)++(match (Match matchNormExtend) r)) | i<-[0..k-1]]
  let morphs=[(maybeToList (f i)) >>= (\r -> match matchMorphism r) | i<-[0..k-1]]
  in fmap (\x -> Extend k x) (foldr intersect (head morphs) morphs)
matchExtend r = []

matchRepeat :: Ring -> [Morphism]
matchRepeat (Quo n k d r) = fmap (\x -> Repeat k x) (match matchMorphism r)
matchRepeat r = []

matchFactor :: Ring -> [Morphism]
matchFactor (Base n d b p) = [Factor k | k <- non_triv_factors(n) ]
matchFactor r = []

matchLabel :: Ring -> [Morphism]
matchLabel (Base n d b p) = [Label k | k <- (filter (\x -> x /= n) (non_triv_factors n)) ]
matchLabel r = []

matchNorm :: Ring -> [Morphism]
matchNorm (Base n d b p) | d /= 0 && n /= 1 = [Norm]
                         | otherwise = []
matchNorm r = []

matchNormExtend :: Ring -> [Morphism]
matchNormExtend (Base n d b p) | n /= 1 = [Norm]
                               | otherwise = []
matchNormExtend r = []

matchDefine :: Ring -> [Morphism]
matchDefine (Quo n k d0 (Base 1 d b p)) = [Define]
matchDefine r = []

matchPushin :: Ring -> [Morphism]
matchPushin (Quo n kq d0 (Prod n2 kp f)) = [Pushin]
matchPushin r = []

matchId :: Ring -> [Morphism]
matchId r = [IdR]

---


---

define_morphism :: Morphism -> Ring -> Maybe (LinearOp FF)

define_morphism (Factor k) (Base n d b p) = Just (phi n k d b p)
define_morphism (Factor k) r = Nothing
define_morphism (Label k) (Base n d b p) = Just (mL n k)
define_morphism (Label k) r = Nothing
define_morphism Norm (Base n d b p) = Just (gamma n d b p)
define_morphism Norm r = Nothing
define_morphism Define (Quo n k d0 (Base 1 d b p)) = Just (mId n)
define_morphism Define r = Nothing
define_morphism Pushin (Quo n kq d0 (Prod n2 kp f)) = Just (mL n kq) -- check
define_morphism Pushin r = Nothing
define_morphism (Repeat k0 m) (Quo n k1 d r) = if k0 == k1 then (define_morphism m r) >>= (\lo -> (repeatLO n lo)) else Nothing
define_morphism (Repeat k0 m) r = Nothing
define_morphism (Extend k0 m) (Prod n k f) = extendLO n k0 (\i -> (f i) >>= (\r -> define_morphism m r))
define_morphism (Extend k0 m) r = Nothing

morphism_to_kernel :: Morphism -> Ring -> Maybe Kernel
morphism_to_kernel (Factor k) (Base n d b p) = Just (Phi n k d b p)
morphism_to_kernel (Factor k) r = Nothing
morphism_to_kernel (Label k) (Base n d b p) = Just (KL n (div n k))
morphism_to_kernel (Label k) r = Nothing
morphism_to_kernel Norm (Base n d b p) = Just (Gamma n d b p)
morphism_to_kernel Norm r = Nothing
morphism_to_kernel Define (Quo n k d0 (Base 1 d b p)) = Just (KId n)
morphism_to_kernel Define r = Nothing
morphism_to_kernel Pushin (Quo n kq d0 (Prod n2 kp f)) = Just (KT n kq (div n2 kp)) -- check
morphism_to_kernel Pushin r = Nothing
morphism_to_kernel (Repeat k0 m) (Quo n k1 d r) = if k0 == k1 then (morphism_to_kernel m r) >>= (\lo -> Just (Kernel_Repeat n k0 lo)) else Nothing
morphism_to_kernel (Repeat k0 m) r = Nothing
morphism_to_kernel (Extend k0 m) (Prod n k f) = Just (Kernel_Extend n k0 (\i -> (f i) >>= (\r -> morphism_to_kernel m r)))
morphism_to_kernel (Extend k0 m) r = Nothing 

is_par_morph :: Morphism -> Morphism -> Bool
is_par_morph base (Repeat k m) = base == (Repeat k m) || is_par_morph base m
is_par_morph base (Extend k m) = base == (Extend k m) || is_par_morph base m 
is_par_morph base m = base == m

patternMatchMorphism :: (Morphism -> Bool) -> Morphism -> Bool
patternMatchMorphism f (Repeat k m) = if f (Repeat k m) then True else patternMatchMorphism f m
patternMatchMorphism f (Extend k m) = if f (Extend k m) then True else patternMatchMorphism f m
patternMatchMorphism f m = f m
