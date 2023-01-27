module Main where

import NTT
import CompileKernel
import Search
import Fourier
import PolyRings
import qualified Data.Map as Map (empty,insert,Map,member)
import Data.Tree

path = [(Phi 4 2 0 4 5), Kernel_Repeat 4 2 (Phi 2 2 0 4 5), Kernel_Extend 4 2 (\i -> Just (Phi 2 2 (2*i) 4 5))]

pathTree :: Ring -> IO()
pathTree r = putStrLn (drawForest (fmap (fmap show) (buildPathForest r)))

ringTree :: Ring -> IO()
ringTree r = putStrLn (drawTree (fmap show (buildRingTree r)))

main :: IO ()
main = putStrLn (drawForest (fmap (fmap show) (buildPathForest (Base 4 0 4 5))))
--main = putStrLn $ drawTree $ fmap $ (fmap show) $ buildTree $ (Base 4 0 4 5)
--main = let graph = search [Base 4 0 4 5] expand Map.empty in putStrLn (show (terminal graph))
--main = putStrLn (show (search_morphs [Base 4 0 4 5] Map.empty))
--main = putStrLn (compile (4,5) "Generated4" path)
