module CodeGen where

import FField
import NTT
import CompileKernel
import Search
import Fourier
import PolyRings
import PolyMult
import Data.List
import Text.Regex.Posix

import System.Process

kt_home = "/home/scarbro/CSU/PolyMult/kernel-timer/"
writeSample :: Ring -> Int -> FilePath -> IO ()
writeSample start seed fname = writeFile (kt_home++"src/gen/"++fname++".c") (sampleCode start seed)

timeCode :: FilePath -> IO String
timeCode fname = readProcess "bash" [kt_home++"timer.sh",kt_home++"bin/"++fname] ""

extractTimes :: String -> [Float]
extractTimes s = let (before,match,after,_) = s =~ "[0-9]+\\.[0-9]*" :: (String,String,String,[String]) in
  if match == "" then [] else [read match :: Float] ++ (extractTimes after) 

timeCodeAvg :: FilePath -> IO Float
timeCodeAvg fname = 
      do { str_res <- timeCode fname;
           res <- return (extractTimes str_res);
           sum <- return (foldr (+) 0 res);
           total <- return (length res);
           return (sum/(fromIntegral total)) }

 -- let str_res = timeCode fname in -- IO String
 -- let res = str_res >>= extractTimes in -- IO [Float] 
 --   let sum=fmap (foldr (+) 0 res) in -- IO Float
 --     sum >>= (\s -> s/(fmap length res))
 
testCode :: FilePath -> IO String
testCode fname = readProcess "bash" [kt_home++"tester.sh",kt_home++"bin/"++fname] ""

--

timeSample start seed fname = writeSample start seed fname >> timeCode fname

testSample start seed fname = writeSample start seed fname >> testCode fname

find_bad start fname range = fmap (filter (\x -> (isInfixOf "fail" (x!!1)))) (traverse id [traverse id [pure (show i),timeSample start i fname] | i <- range])

---

pathCode :: Path -> String
pathCode path = let pstart = path_get_start path in
  let sn = get_size pstart in
    let sb = get_root pstart in
      let sp = get_prime pstart in
        let msteps = path_get_steps path in
          squashMaybeString (msteps >>= (\steps -> Just (compile (sn,sb,sp) "Gen" steps) ) ) "Compilation Error"
        
writePath :: Maybe Path -> FilePath -> IO ()
writePath (Just path) fname = let pcode = pathCode path in
  writeFile (kt_home++"src/gen/"++fname++".c") pcode
writePath Nothing fname = writeFile (kt_home++"src/gen/"++fname++".c") "Compilation Error"

timePath :: Maybe Path -> FilePath -> IO Float
timePath path fname = writePath path fname >> timeCodeAvg fname

--

-- check correct
expandTerminal :: Ring -> Maybe [Int]
expandTerminal (Prod n k f) = fmap (foldr (++) []) (traverse id [(f i) >>= expandTerminal | i<- [0..k-1]])
expandTerminal (Base n d b p) = Just [d]

terminalToPerm :: Int -> Int -> Int -> [Int] -> [Int]
terminalToPerm n d b term = fmap (\x -> (x-(d `div` n)) `div` (b `div` n)) term

correctResult :: Ring -> Maybe [Int]
correctResult (Base n d b p) = let vec = ffVec n p id in
  let phi_lop = phi n n d b p in
    (mv phi_lop vec) >>= toIntList


extractResult :: FilePath -> IO [Int]
extractResult fname = do { result <- testCode fname; (\x -> pure (fmap (\num -> read num :: Int) (tail (words x)))) result }

applyPerm :: [Int] -> [Int] -> [Int]
applyPerm l perm = [l!!p | p <- perm]


checkCorrect :: Maybe Path -> FilePath -> IO (String)
checkCorrect (Just path) fname = let written = writePath (Just path) fname in
  let result = extractResult fname in
    let start = path_get_start path in
      written >> result >>= (\res -> return (squashMaybeString (do { end <- path_get_end path;
                                                          cor <- correctResult start;
                                                         term <- expandTerminal end;
                                                         perm <- return (terminalToPerm (get_size start) (get_root_power start) (get_root start) term);
                                                         permCor <- return (applyPerm cor perm);
                                                         Just (show (res==permCor)) } ) "Test error")
                                                         --Just ("Result: "++show res++"  Correct: "++show cor++"  Term:"++show term++"  Perm: "++show perm++"  PermCor: "++show permCor ) }) "Test error")
                 )
checkCorrect Nothing fname = return "Test error"
