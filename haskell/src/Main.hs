module Main where

import NTT
import CompileKernel
import Search
import Fourier
import PolyRings
import qualified Data.Map as Map (empty,insert,Map,member)
import Data.Tree
import System.Random

import System.Environment
import System.Exit

 
 --test vars
path = [(Phi 4 2 0 4 5), Kernel_Repeat 4 2 (Phi 2 2 0 4 5), Kernel_Extend 4 2 (\i -> Just (Phi 2 2 (2*i) 4 5))]
 
 --interface functions
pathTree :: Ring -> IO()
pathTree r = putStrLn (drawForest (fmap (fmap show) (buildPathForest r)))
 
ringTree :: Ring -> IO()
ringTree r = putStrLn (drawTree (fmap show (buildRingTree r)))
 
samplePath :: Ring -> Int -> IO()
samplePath start seed = let pf = buildPathForest start in
   let m_walk = fst (randomWalk pf (mkStdGen seed)) in
     let code = squashMaybeString (m_walk >>= (\mw -> Just (compile (get_size start,get_root start,get_prime start) "Sampled" mw))) "Error: compilation failure" in
         let s_walk = squashMaybeString (m_walk >>= (\ms -> Just (show ms))) "Error: exploration failure" in
             putStrLn ("Path:\n  "++s_walk++"\n---\nCode:\n"++code) 
 
main :: IO ()
 --main = putStrLn (drawForest (fmap (fmap show) (buildPathForest (Base 4 0 4 5))))
 --main = putStrLn $ drawTree $ fmap $ (fmap show) $ buildTree $ (Base 4 0 4 5)
 --main = let graph = search [Base 4 0 4 5] expand Map.empty in putStrLn (show (terminal graph))
 --main = putStrLn (show (search_morphs [Base 4 0 4 5] Map.empty))
 --main = putStrLn (compile (4,5) "Generated4" path)
 --main = putStrLn (show (fst (randomWalk (buildPathForest (Base 4 0 4 5)) (mkStdGen 100))))
main = putStrLn (squashMaybeString ((fst (randomWalk (buildPathForest (Base 4 0 4 5)) (mkStdGen 100))) >>= (\mw -> Just (compile (4,4,5) "Sampled4" mw)))  "Error: compilation failed")
 --  
 --  code <- squashMaybeString m_code "Error: compilation failed";
 --  putStrLn (show code);
 --  }
 --
 


-- CLI
--main = getArgs >>= parse >>= putStr
--
--codeGen :: (String,Ring,Int) -> String
--codeGen = samplePath
--
--toInt :: String -> Int
--toInt s = read s :: Int
--
--parse :: [String] -> IO (String,Ring)
--parse [name,n,d,b,p,seed] = codeGen (name,Base (toInt n) (toInt d) (toInt b) (toInt p),toInt seed)
--parse ["-h"] = usage >> exit
--parse x = usage >> exit
--
--exit = exitWith ExitSuccess
--usage = putStrLn "Usage: PolyMult [flags]\n  --name : kernel name\n  --ring : start ring"
--
