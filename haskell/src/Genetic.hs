{-# LANGUAGE FlexibleContexts #-}

module Genetic where

import System.Random
import System.Random.Shuffle

import Data.List
import Control.Monad
import Data.Maybe

import Search
import Logger

class Species a where
  sample :: a -> StdGen -> Maybe (a,StdGen)
  combine :: StdGen -> a -> a -> Maybe (a,StdGen)
  fitness :: a -> IO Float

type Population a = [(a,Float)]

--initializePopulation :: (Species a,RandomGen b) => b -> Int -> (b,Population a)
--initializePopulation size rand = let expand pred rand count = let (new_ind,new_rand)=sample rand in
--                                       if count > 0 then expand (pred++[new_ind]) new_rand (count-1)
--                                       else pred in
--                                   expand [] rand size
--
-- params
fitnessFilter = 0.6


failedState = return ([] :: Population a,mkStdGen 10)

getFitness :: IO (Population a, StdGen) -> IO [Float]
getFitness i_pop_rand = do {
  (pop,rand) <- i_pop_rand;
  return (fmap snd pop) }
  
-- creates population, sets fitness to 0
initializePopulation :: (Species a, Show a) => a -> StdGen -> Int -> IO (Population a,StdGen)
initializePopulation specimen rand size = let inter = do { list <- traverse id (scanl ip_scanf (sample specimen rand) [0..(size-2)]); -- Maybe
                                                           pop <- Just (popFitness (fmap (\x -> (x,0)) (fmap fst list)));
                                                           newRand <- Just (snd( last( list )));
                                                           pop_logged <- Just ((logObj "InitializePopulation" list) >> pop); -- log initial population
                                                           return (pop_logged,newRand); } in
                                            let maybeOverIO = fmap (\(p,r) -> (return (\z -> (z,r))) <*> p ) inter in
                                              fromMaybe failedState maybeOverIO
--  fromMaybe [] (traverse id (scanl ip_scanf (sample specimen rand)) [0..(size-2)]) in
--  let popInitFit = fmap (\x -> (x,0)) initial in
--    let 
ip_scanf :: (Species a) => Maybe (a,StdGen) -> Int -> Maybe (a,StdGen)
ip_scanf (Just (x,rand)) y = sample x rand
ip_scanf Nothing _ = Nothing

-- computes fitness for each individual
popFitness :: (Species a) => Population a -> IO (Population a)
popFitness pop = traverse id (fmap (\(x,f) -> return (\y -> (x,y)) <*> fitness x) pop)

--generates new individuals from fit individuals
nextGenPopulation :: (Species a,Show a,Eq a) => Population a -> StdGen -> Int -> IO (Population a,StdGen)
nextGenPopulation pop rand size = let uniquePop = nub pop in
  let sortedPop = sortBy (\(x1,x2) (y1,y2) -> compare (-x2) (-y2)) uniquePop in
    let numSurvive = (floor ((fromIntegral size)*fitnessFilter)) in
      let filteredPop = take numSurvive (sortedPop) in
        let shuffledFP = shuffle' filteredPop (length filteredPop) rand in
          let zipped = zip filteredPop shuffledFP in
            let inter = do { -- Maybe
                  nextGen <- traverse id (scanl (ngp_scanf filteredPop shuffledFP) (Just (fst (head filteredPop),rand)) [0..(length filteredPop)-1]); -- [(a,StdGen)]
                  nextGenwf <- return (fmap (\x -> (x,0)) (fmap fst nextGen)); -- [(a,0)]
                  nextGenUniq <- return (insureUnique filteredPop nextGenwf); -- [(a,0)]
                  nextGenFinal <- return (take (size-numSurvive) nextGenUniq); -- [(a,0)]
                  nextPop <- return ((popFitness nextGenFinal) >>= (\x -> return (x ++ filteredPop))); -- [(a,Float)]
                  --nextPop <- return ((popFitness (fmap (\x -> (x,0)) (fmap fst nextGen))) >>= (\x -> return (x ++ filteredPop)));
                  nextPop_logged <- return ((logObj "nextGen" "" ) >> nextPop); -- IO (Population a)
                  return (nextPop_logged,snd (last nextGen)); } in -- Maybe (IO Population a, StdGen)
              let inter2 = fmap (\(p,r) -> (return (\z -> (z,r))) <*> p ) inter in -- Maybe IO (Population a, StdGen)
                fromMaybe failedState inter2
              where
                insureUnique :: Eq a => Population a -> Population a -> Population a
                insureUnique oldp newp = deleteFirstsBy (\x y -> fst x == fst y) (nub newp) oldp
              
                
ngp_scanf :: (Species a) => [(a,Float)] -> [(a,Float)] -> Maybe (a,StdGen) -> Int -> Maybe (a,StdGen)
ngp_scanf l1 l2 (Just (x,rand2)) ind = combine rand2 (fst (l1!!ind)) (fst (l2!!ind))
ngp_scanf _ _ Nothing _ = Nothing

sortPop :: (Species a) => IO (Population a,StdGen) -> IO (Population a, StdGen)
sortPop pop_rand = fmap (\x -> (sortBy (\(x1,x2) (y1,y2) -> compare (-x2) (-y2)) (fst x),snd x)) pop_rand
--

nthGeneration :: (Species a, Show a, Eq a) => (IO (Population a,StdGen)) -> Int -> Int -> (IO (Population a,StdGen))
--nthGeneration specimen rand size n = let ip = initializePopulation specimen rand size in ngCount ip size n
nthGeneration i_pop size n = ngCount i_pop size n 
  where
     ngCount :: (Species a, Show a, Eq a) => IO (Population a,StdGen) -> Int -> Int -> IO (Population a,StdGen)
     ngCount pop_rand size count = if count <= 0 then pop_rand else
       let next_pop_rand = fmap (\x -> nextGenPopulation (fst x) (snd x) size) pop_rand in
         let joined_next_pop_rand = join next_pop_rand in
           let logged_next_pop_rand = logIO ("NthGeneration "++show count) joined_next_pop_rand >> joined_next_pop_rand in
             ngCount logged_next_pop_rand size (count-1)

----

unwrapMaybeOverIO :: Show a => Maybe (IO a) -> IO (Maybe a)
unwrapMaybeOverIO (Just x) = x >>= (\y -> return (Just y))
unwrapMaybeOverIO Nothing = return Nothing


-------

instance Species Integer where
  sample x rand = Just (randomR (0,100) rand)
  combine rand x y = Just (gcd x y,rand)
  fitness x = logObj "factoring " x >> return (fromIntegral (length (filter (\y -> mod x y == 0) [2..x-1]))) -- counts factors 

initInt :: [(Integer,StdGen)]
initInt = scanl (\(x,rand) y -> randomR (0,100) rand) (0,mkStdGen 10) [0,1,2]

instance Species Bool where
  sample x rand = Just (randomChoice [True,False] rand)
  combine rand x y = Just (randomChoice [x,y] rand)
  fitness x = return (if x then 1 else 0)

