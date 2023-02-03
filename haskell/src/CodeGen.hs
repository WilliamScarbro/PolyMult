module CodeGen where

import NTT
import CompileKernel
import Search
import Fourier
import PolyRings
import PolyMult
import Data.List

import System.Process

kt_home = "/home/scarbro/CSU/PolyMult/kernel-timer/"
writeSample :: Ring -> Int -> FilePath -> IO ()
writeSample start seed fname = writeFile (kt_home++"src/"++fname++".c") (sampleCode start seed)

timeCode :: FilePath -> IO String
timeCode fname = readProcess "bash" [kt_home++"timer.sh",kt_home++"bin/"++fname] ""

testCode :: FilePath -> IO String
testCode fname = readProcess "bash" [kt_home++"tester.sh",kt_home++"bin/"++fname] ""

timeSample start seed fname = writeSample start seed fname >> timeCode fname

testSample start seed fname = writeSample start seed fname >> testCode fname

find_bad start fname range = fmap (filter (\x -> (isInfixOf "fail" (x!!1)))) (traverse id [traverse id [pure (show i),timeSample start i fname] | i <- range])

--

-- check correct
expandTerminal :: Ring -> Maybe [Int]
expandTerminal (Prod n k f) = fmap (foldr (++) []) (traverse id [(f i) >>= expandTerminal | i<- [0..k-1]])
expandTerminal (Base n d b p) = Just [d]

correctPermutation :: Ring -> Int 
