--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Fourier where

import FField
import NTT
import Data.List
import Data.Maybe

instance Show (Int -> Maybe Ring) where
  show f = show (f 0)

instance Eq (Int -> Maybe Ring) where
  (==) f1 f2 = (f1 0) == (f2 0)

data Ring = Base Int Int Int Int | Prod Int Int (Int -> Maybe Ring) | Quo Int Int Int Ring deriving (Show,Eq)
--
--instance Show Ring where
--  show (Base n d b p) = "Base "++show n++" "++show d++" "++show b++" "++show p
--  show (Prod n k f) = "Prod "++show n++" "++show k++" "++show (f 0)
--  show (Quo n d r) = "Quo "++show n++" "++show d++" "++show r
--
--instance Eq Ring where
--  (==) (Base n1 d1 b1 p1) (Base n2 d2 b2 p2) = n1 == n2 && d1==d2 && b1==b2 && p1==p2
--  (==) (Prod n1 k1 f1) (Prod n2 k2 f2) = n1==n2 && k1==k2 && (f1 0)==(f2 0)
--  (==) (Quo n1 d1 r1) (Quo n2 d2 r2) = n1==n2 && d1==d2 && r1==r2
--  (==) _ _ = False
--
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
pushin (Quo n kq d0 (Prod n2 kp f)) = Just (Prod n kp (\i -> (f i) >>= (\g -> Just (Quo n2 kq d0 g))))
pushin r = Nothing

---

instance Show (Int -> Morphism) where
  show f = show (f 0)

instance Eq (Int -> Morphism) where
  (==) f1 f2 = (f1 0) == (f2 0)
  
data Morphism = Compose Morphism Morphism | Extend Int Morphism | Repeat Int Morphism | Factor Int | Label Int | Norm | Define | Pushin | IdR deriving (Show,Eq)

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
apply (Compose m1 m2) x = (apply m2 x) >>= (\y -> apply m1 y)
apply (Extend k0 m) (Prod n k f) | k0==k = Just (Prod n k (\i -> f i >>= (\g -> apply m g)))  --- ( int -> Maybe R ) >>= (Ring -> Maybe Ring)  :: Int -> Maybe Ring
                                 | otherwise = Nothing
apply (Repeat i m) (Quo n j k x) | i==j = (apply m x) >>= (\y -> Just (Quo n j k y))
                                 | otherwise = Nothing
apply IdR x = Just x

---
--- ( a -> M b ) -> (( a -> b ) -> M c) -> M c
--- ( a -> M b ) -> M ( a -> b )
--- (>>=) :: M a -> (a -> b) -> M b
--- (<*>) :: f ( a -> b ) -> f a -> f b
-- it is very difficult to insure all patterns get matched (with extend and repeat)
--- sol use parse pattern?



-- assume n|d
--match :: Ring -> [Morphism]
--match (Base n d b p) = [Factor i | i<-non_triv_factors(n)]
--  ++ [Label i | i<-non_triv_factors(n)]
--  ++ if d /= 0 then [Norm] else []
--match (Quo k d0 (Base n d b p)

data Match = Match (Ring -> [Morphism])

match :: Match -> Ring -> [Morphism]
match (Match m) r = m r

matchAdd :: Match -> Match -> Match
matchAdd (Match m1) (Match m2) = Match (\r -> (m1 r) ++ (m2 r))

infixl 2 <+>
(<+>)=matchAdd
  
matchMorphism :: Match
matchMorphism = (Match matchExtend) <+> (Match matchRepeat) <+> (Match matchFactor) <+> (Match matchLabel)
  <+> (Match matchDefine) <+> (Match matchPushin)  <+> (Match matchNorm) <+> (Match matchId)

matchExtend :: Ring -> [Morphism]
matchExtend (Prod n k f) = 
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
matchNorm (Base n d b p) | n /= 1 = [Norm]
                         | otherwise = []
matchNorm r = []

matchDefine :: Ring -> [Morphism]
matchDefine (Quo n k d0 (Base 1 d b p)) = [Define]
matchDefine r = []

matchPushin :: Ring -> [Morphism]
matchPushin (Quo n kq d0 (Prod n2 kp f)) = [Pushin]
matchPushin r = []

matchId :: Ring -> [Morphism]
matchId r = [IdR]

---

expand :: Maybe Ring -> [Maybe Ring]
expand (Just r) = nub (fmap (\m -> apply m r) (match matchMorphism r))
expand Nothing = []

rec_expand :: [Maybe Ring] -> [Maybe Ring]
rec_expand lmr = nub (foldr (++) [] [[x] >>= expand | x <- lmr])

n_expand :: Int -> [Maybe Ring] -> [Maybe Ring]
n_expand n lmr | n>0 = n_expand (n-1) (rec_expand lmr)
               | n<=0 = lmr

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



                                                
--class Domain a b where
--  isin :: a -> b -> Bool
--
--class Morphism a b where
--  source :: Domain c b => a -> b
--  target :: Domain c b => a -> b
--
----apply :: (Domain c, Morphism a) => a -> b -> Maybe c
----apply a b = if isin (source a) b then Just (target a) else Nothing
--
----class Category a where
----  morphisms :: a -> b -> [Morphism c]
----
----compose :: Morphism a -> Morphism b -> c -> Maybe d
--
-----
--
--data Object a = O a
--data RedD=RedD
----data BlueD=Blue
----data GreenD=Green
---- 
----data Red2Blue = Red2Blue
----data Blue2Green = Blue2Green
----data Green2Red = Green2Red
----
--instance Domain RedD (Object a) where
--  isin (RedD) (O RedD) = True
--  isin (RedD) x = False
----
--instance Domain BlueD where
--  isin (Blue) Blue = True
--  isin (Blue) x = False
--  
--instance Domain GreenD where
--  isin (Green) Green = True
--  isin (Green) x = False
--  
--instance Morphism Red2Blue where
--  source Red2Blue = Red
--  target Red2Blue = Blue
--  
--instance Morphism Blue2Green where
--  source Blue2Green = Blue
--  target Blue2Green = Green
--
--
--matchApply :: Morphism a => [a] -> b -> [c]
--matchApply z b = filter (\x -> x /= Nothing) (fmap (\x -> apply x b) z) 
--
