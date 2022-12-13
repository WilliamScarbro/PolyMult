{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module NTT where

import FField

import Data.Matrix 
--import qualified Data.Vector as Vector ( Vector, (!), generate)

--apply :: LinearOperato ResInt -> Vector ResInt -> Maybe (Vector (Maybe ResInt))
--apply (LOP n1 x) (VEC n2 y) | n1 /= n2 = Nothing
--                            | otherwise = Just (VEC n1 [foldr (\x -> \y -> do { ux <- x; uy <- y; add ux uy }) (Just zero) (zipWith mult xj y) | xj<-x])
--mat_ctor :: Integer -> (Integer -> Integer -> Maybe a) -> LinearOperator a
--mat_ctor :: 

--mv_mult :: Num a => Matrix a -> Vector a -> Vector a
--mv_mult m v = vectorize (m * (vertical v))



data LinearOp a = LO (Matrix a)
data Vector a = Vec (Matrix a)

ffVec :: Integral a =>  a -> a -> (Int -> Int) -> Vector FF
ffVec n p f = Vec (matrix (fromIntegral n) 1 (\(x,y) -> Just (Res (toInteger (f x)) (toInteger p))))

linearOp :: Integral a => a -> ((Int,Int)->b) -> LinearOp b
linearOp n f = LO (matrix (fromIntegral n) (fromIntegral n) (\(x,y) -> f ((x-1),(y-1))))

----

lo_op :: (Matrix a -> Matrix a -> Matrix a) -> (LinearOp a -> LinearOp a -> LinearOp a)
lo_op op (LO x) (LO y) = LO (op x y)

lo_cmp :: (Matrix a -> Matrix a -> Bool) -> (LinearOp a -> LinearOp a -> Bool)
lo_cmp op (LO x) (LO y) = (op x y)


instance Show (LinearOp FF) where
  show (LO m) = show m

instance Show (Vector FF) where
  show (Vec v) = show v

instance Functor LinearOp where
  -- (a->b) -> (f a->f b) = 
  fmap f (LO m) = LO (fmap f m)
  
instance Num (LinearOp FF) where
  (+) = lo_op (+)
  negate = fmap negate
  (-) = lo_op (-)
  (*) = lo_op (*)
  abs = id
  signum x = 1
  fromInteger x = linearOp 1 (\(z,y) -> fromInteger x :: FF) 

instance Eq (LinearOp FF) where
  (==) = lo_cmp (==)

---

mm :: LinearOp FF -> LinearOp FF -> Maybe (LinearOp FF)
mm (LO m) (LO m2) | ncols m /= nrows m2 = Nothing
                  | otherwise = Just (LO (m*m2))
                  
mv :: LinearOp FF -> Vector FF -> Maybe (Vector FF)
mv (LO m) (Vec v) | ncols m /= nrows v = Nothing
                  | otherwise = Just (Vec (m*v))

vm :: Vector FF -> LinearOp FF -> Maybe (Vector FF)
vm (Vec v) (LO m) | ncols v /= nrows m = Nothing
                  | otherwise = Just (Vec (v*m))

vv :: Vector FF -> Vector FF -> Maybe (Vector FF)
vv (Vec v) (Vec v2) | ncols v /= nrows v2 = Nothing
                    | otherwise = Just (Vec (v*v2))

-- haskell matrices are 1 indexed :(
get_el :: Int -> Int -> Matrix a -> a
get_el x y m = getElem (x+1) (y+1) m

tensor :: LinearOp FF -> LinearOp FF -> LinearOp FF
tensor (LO m1) (LO m2) = let n1=nrows m1 in let n2=nrows m2 in
  linearOp (n1 * n2) (\(x,y) -> (get_el (div x n2) (div y n2) m1) * (get_el (mod x n2) (mod y n2) m2))
                   
-------


mId :: Integral a => a -> LinearOp FF
mId n = linearOp n (\(i,j) -> if i==j then Just one else Just FField.zero)

-- assumes (Int -> Int) is a bijection
perm :: Integral a => a -> (Int -> Int) -> LinearOp FF
perm n f = linearOp n (\(i,j) -> if f(j)==i then Just one else Just FField.zero)

mL :: Integral a => a -> a -> LinearOp FF
mL n k = let m = fromIntegral (div n k) in let ik = fromIntegral k in perm n (\x -> (div x ik) + m * (mod x ik))

mNTT :: Integral a =>  a -> a -> LinearOp FF
mNTT n p = let w=nth_root n p in (linearOp n (\(i,j) -> w >>= (\x -> pow x (i*j) )))

mNTT_inv :: Integral a => a -> a -> LinearOp FF
mNTT_inv n p =
  let w_inv=(nth_root (2*n) p) in
    let n_inv=inv (Res (toInteger n) (toInteger p)) in
      (linearOp n (\(i,j) -> n_inv * (gen_inv >>= (\x -> pow x (i*j)))))

--phi n k d b p
phi :: Int -> Int -> Int -> Int -> Int -> LinearOp FF
-- x,y -> let z=x//m , j=x%m, i=y//m, j'=y%m in if j==j' then w_b^(d+z*b)*i/k
phi n k d b p = 
  let w=nth_root b p in
    let m=div n k in
      linearOp n (\(x,y) -> if mod x m == mod y m then w >>= (\z -> pow z (div ((d+(div x m)*b)*(div y m)) k)) else 0)
phi_inv :: Int -> Int -> Int -> Int -> Int LinearOp FF
phi_inv n k d b p =
  let w_inv=inv (nth
gamma :: n d b p =
  
