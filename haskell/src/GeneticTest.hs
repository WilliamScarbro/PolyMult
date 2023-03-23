module GeneticTest where

import Genetic
import GeneticCode
import Search
import PolyRings
import FField

import System.Random
import Control.Monad
import Data.Maybe


new_hope_params=(1024,12289) :: (Int,Int)
kyber_params=(256,7681) :: (Int,Int)

testPathPop :: Int -> IO ()
testPathPop size = let pop = do { -- Maybe
                    path <- randomPath (Base 16 0 16 17) 10; -- Path
                    ip <- return (initializePopulation path (mkStdGen 10) size); -- IO (Population Path,StdGen)
                    sorted_ip <- return (sortPop ip);
                    return ((join . (fmap (putStr . show))) sorted_ip)} in -- Maybe ( IO () )
                fromMaybe (putStr "Fail") pop

-- KernelSize -> PopSize -> Generations
testNthGen :: (Int,Int) -> Int -> Int -> IO ()
testNthGen (ksize,prime) psize gens = 
    let mngen = do { -- Maybe
          path <- randomPath (Base ksize 0 ksize prime) 10; -- Path
            ip <- return (initializePopulation path (mkStdGen 10) psize); -- IO (Population Path,StdGen)
            ngen <- return (nthGeneration ip psize gens); -- IO (Population Path,StdGen)
            sorted_ngen <- return (sortPop ngen); -- IO (Population Path,StdGen)
            fitness <- return (getFitness sorted_ngen); -- IO [Float]
            return ((join . (fmap (putStr . (\x -> show x ++ "\n")))) fitness)} in -- Maybe ( IO () )
      fromMaybe (putStr "Fail") mngen



