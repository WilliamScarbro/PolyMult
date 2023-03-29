module PolyMult where

import NTT
import CompileKernel
import Search
import Fourier
import PolyRings
import qualified Data.Map as Map (empty,insert,Map,member)
import Data.Tree
import System.Random


--interface functions
pathTree :: Ring -> IO()
pathTree r = putStrLn (drawForest (fmap (fmap show) (buildPathForest r)))

ringTree :: Ring -> IO()
ringTree r = putStrLn (drawTree (fmap show (buildRingTree r)))


samplePath :: Ring -> Int -> IO ()
samplePath start seed = 
samplePath :: Ring -> Int -> IO()
samplePath start seed = let pf = buildPathForest start in
  let m_walk = fst (randomWalk pf (mkStdGen seed)) in
    let code = squashMaybeString (m_walk >>= (\mw -> Just (compile (get_size start,get_root start,get_prime start) "Sampled" mw))) "Error: compilation failure" in
        let s_walk = squashMaybeString (m_walk >>= (\ms -> Just (show ms))) "Error: exploration failure" in
            putStrLn ("Path:\n  "++s_walk++"\n---\nCode:\n"++code) 

sampleCode :: Ring -> Int -> String
sampleCode start seed = let pf = buildPathForest start in
  let m_walk = fst (randomWalk pf (mkStdGen seed)) in
     squashMaybeString (m_walk >>= (\mw -> Just (compile (get_size start,get_root start,get_prime start) "Sampled" mw))) "Error: compilation failure"
