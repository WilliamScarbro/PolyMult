{-# LANGUAGE FlexibleContexts #-}

module NTT where

import FField

import qualified Data.Matrix as Matrix ( Matrix, matrix, getElem, nrows )
import qualified Data.Vector as Vector ( Vector, (!), generate)

type Matrix=Matrix.Matrix
type Vector=Vector.Vector
--apply :: LinearOperato ResInt -> Vector ResInt -> Maybe (Vector (Maybe ResInt))
--apply (LOP n1 x) (VEC n2 y) | n1 /= n2 = Nothing
--                            | otherwise = Just (VEC n1 [foldr (\x -> \y -> do { ux <- x; uy <- y; add ux uy }) (Just zero) (zipWith mult xj y) | xj<-x])
--mat_ctor :: Integer -> (Integer -> Integer -> Maybe a) -> LinearOperator a
--mat_ctor :: 

--mv_mult :: Num a => Matrix a -> Vector a -> Vector a
--mv_mult m v = vectorize (m * (vertical v))
  
ffVec :: Integral a =>  a -> a -> (Int -> Int) -> Matrix FF
ffVec n p f = Vector.generate (fromIntegral n) (\x -> Just (Res (toInteger (f x)) (toInteger p)))

linearOp :: Integral a => a -> ((Int,Int)->b) -> Matrix b
linearOp n f = Matrix.matrix (fromIntegral n) (fromIntegral n) (\(x,y) -> f ((x-1),(y-1)))

vertical :: Vector a -> Matrix a
vertical v = Matrix.matrix (length v) 1 (\(x,y) -> v Vector.! x)

diag :: a -> Vector a -> Matrix a
diag def v = linearOp (length v) (\(x,y) -> if x==y then v Vector.! x else def)

fNTT :: Integral a =>  a -> a -> Matrix FF
fNTT n p = let gen=ff_generator p in (linearOp n (\(i,j) -> pow gen (i*j) ))

--apply :: LinearOperator -> Vector a -> Vector a
--apply LONTT (Vec n x:y) = mv (fNTT n p) 
