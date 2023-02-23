{-# LANGUAGE FlexibleContexts #-}

module Genetic where

import System.Random
import System.Random.Shuffle

import Data.List

class Species a where
  sample :: RandomGen b => a -> b -> Maybe (a,b)
  combine :: RandomGen b => b -> a -> a -> Maybe (a,b)
  fitness :: a -> IO Float

type Population a = [(a,Float)]

--initializePopulation :: (Species a,RandomGen b) => b -> Int -> (b,Population a)
--initializePopulation size rand = let expand pred rand count = let (new_ind,new_rand)=sample rand in
--                                       if count > 0 then expand (pred++[new_ind]) new_rand (count-1)
--                                       else pred in
--                                   expand [] rand size
--
-- params
fitnessFilter = 0.5

-- creates population, sets fitness to 0
initializePopulation :: (Species a,RandomGen b) => a -> b -> Int -> Maybe (IO (Population a,b))
initializePopulation specimen rand size = let inter = do { list <- traverse id (scanl ip_scanf (sample specimen rand) [0..size]);
                                                           pop <- Just (popFitness (fmap (\x -> (x,0)) (fmap fst list)));
                                                           newRand <- Just (snd( last( list )));
                                                           return (pop,newRand); } in
                                            let maybeOverIO = fmap (\(p,r) -> (return (\z -> (z,r))) <*> p ) inter in
                                              maybeOverIO
  
ip_scanf :: (Species a, RandomGen b) => Maybe (a,b) -> Int -> Maybe (a,b)
ip_scanf (Just (x,rand)) y = sample x rand
ip_scanf Nothing _ = Nothing

-- computes fitness for each individual
popFitness :: (Species a) => Population a -> IO (Population a)
popFitness pop = traverse id (fmap (\(x,f) -> return (\y -> (x,y)) <*> fitness x) pop)

--generates new individuals from fit individuals
nextGenPopulation :: (Species a,RandomGen g) => g -> Population a -> Maybe (IO (Population a,g))
nextGenPopulation rand pop = let sortedPop = sortBy (\(x1,x2) (y1,y2) -> compare (-x2) (-y2)) pop in
  let numSurvive = (floor ((fromIntegral (length sortedPop))*fitnessFilter)) in
    let filteredPop = take numSurvive (sortedPop) in
      let shuffledFP = shuffle' filteredPop numSurvive rand in
        let zipped = zip filteredPop shuffledFP in
          let inter = do { nextGen <- traverse id (scanl (ngp_scanf filteredPop shuffledFP) (Just (fst (head filteredPop),rand)) [0..(length filteredPop)-1]);
                           nextPop <- return ((popFitness (fmap (\x -> (x,0)) (fmap fst nextGen))) >>= (\x -> return (x ++ filteredPop)));
                           return (nextPop,snd (last nextGen)); } in
            fmap (\(p,r) -> (return (\z -> (z,r))) <*> p ) inter
            
                
ngp_scanf :: (Species a,RandomGen g) => [(a,Float)] -> [(a,Float)] -> Maybe (a,g) -> Int -> Maybe (a,g)
ngp_scanf l1 l2 (Just (x,rand2)) ind = combine rand2 (fst (l1!!ind)) (fst (l2!!ind))
ngp_scanf _ _ Nothing _ = Nothing

--

--nthGeneration :: (Species a, RandomGen b) => a -> b -> Int -> Int -> IO (Maybe (Population a,b))
--nthGeneration specimen rand size n = let ip = initializePopulation specimen rand size

unwrapMaybeOverIO :: Show a => Maybe (IO a) -> IO (Maybe a)
unwrapMaybeOverIO (Just x) = x >>= (\y -> return (Just y))
unwrapMaybeOverIO Nothing = return Nothing


-------

instance Species Integer where
  sample x rand = Just (randomR (0,100) rand)
  combine rand x y = Just (gcd x y,rand)
  fitness x = return (fromIntegral (length (filter (\y -> mod x y == 0) [2..x-1]))) -- counts factors 

initInt :: [(Integer,StdGen)]
initInt = scanl (\(x,rand) y -> randomR (0,100) rand) (0,mkStdGen 10) [0,1,2]

randomChoice :: RandomGen g => [a] -> g -> (a,g)
randomChoice list rand = let (ind,rand2)=randomR (0,(length list)-1) rand in (list!!ind,rand2)

instance Species Bool where
  sample x rand = Just (randomChoice [True,False] rand)
  combine rand x y = Just (randomChoice [x,y] rand)
  fitness x = return (if x then 1 else 0)

