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
                    (ip,rand) <- return (initializePopulation path (mkStdGen 10) size); -- (IO (Population Path),StdGen)
                    sorted_ip <- return (sortPop ip);
                    return ((join . (fmap (putStr . show))) sorted_ip)} in -- Maybe ( IO () )
                fromMaybe (putStr "Fail") pop

-- KernelSize -> PopSize -> Generations
testNthGen :: (Int,Int) -> Int -> Int -> IO ()
testNthGen (ksize,prime) psize gens = 
    let mngen = do { -- Maybe
          path <- randomPath (Base ksize 0 ksize prime) 10; -- Path
            (ip,rand) <- return (initializePopulation path (mkStdGen 10) psize); -- (IO (Population Path),StdGen)
            (ngen,nrand) <- return (nthGeneration (ip,rand) psize gens); -- (IO (Population Path),StdGen)
            sorted_ngen <- return (sortPop ngen); -- IO (Population Path)
            --fitness <- return (getFitness sorted_ngen); -- IO [Float]
            out <- return ( do { -- IO
               io_pop <- sorted_ngen; -- Population Path
               return (fmap snd io_pop,fst (head (io_pop))); }); -- ([Float],Path)
            return ((join . (fmap (putStr . (\x -> show x ++ "\n")))) out)} in -- Maybe ( IO () )
      fromMaybe (putStr "Fail") mngen



