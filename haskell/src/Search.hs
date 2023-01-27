module Search where

import FField
import PolyRings
import NTT
import Fourier
import Data.List
import Data.Maybe
import qualified Data.Map as Map (empty,insert,Map,member,mapAccumWithKey)
import Data.Tree
import System.Random

---

expand :: Ring -> [Ring]
expand r = let mrl=nub (fmap (\m -> apply m r) (match matchMorphism r)) in foldr (++) [] (fmap maybeToList mrl)

---

search_morphs :: [Ring] -> Map.Map Ring [Morphism] -> Map.Map Ring [Morphism]
search_morphs (current:z) hmap = if Map.member current hmap then search_morphs z hmap else
  let morphs = match matchMorphism current in
    let neighbors = foldl (++) [] (fmap maybeToList (fmap (\m -> apply m current) morphs)) in
      let hmap_new = Map.insert current morphs hmap in
        search_morphs (nub (z++neighbors)) hmap_new
search_morphs [] hmap = hmap

terminal :: Eq b => Map.Map a [b] -> [a]
terminal hmap = fst (Map.mapAccumWithKey (\accum key val -> (if val==[] then accum++[key] else accum,0)) [] hmap)


--- 

buildPathForest :: Ring -> [Tree (Maybe Kernel)]
buildPathForest start = unfoldForest buildPathForest_branch [(Just start,mi) | mi <- match matchMorphism start]
 
buildPathForest_branch :: (Maybe Ring,Morphism) -> (Maybe Kernel, [(Maybe Ring, Morphism)])
buildPathForest_branch (r,m) = let r2 = r >>= apply m in
  (r >>= morphism_to_kernel m, [(r2,mi) | mi <- buildPathForest_help1 r2])

buildPathForest_help1 :: Maybe Ring -> [Morphism]
buildPathForest_help1 (Just r) = match matchMorphism r
buildPathForest_help1 Nothing = []


--


buildRingTree :: Ring -> Tree Ring
buildRingTree start = unfoldTree buildRingTree_branch start

buildRingTree_branch :: Ring -> (Ring,[Ring])
buildRingTree_branch r = (r,foldl (++) [] (fmap maybeToList (fmap (\m -> apply m r) (match matchMorphism r))))

--

randomWalk :: RandomGen b => [Tree (Maybe Kernel)] -> b -> Maybe [Kernel]
randomWalk forest g = let (index,new_g) = randomR (0,(length forest)-1) g in
  let tree = forest !! index in
    traverse id (fst (randomWalk_help ([],new_g) tree))
                   
randomWalk_help :: (RandomGen b, Eq a) => ([a],b) -> Tree a -> ([a],b)
randomWalk_help (walk,g) (Node c rest) = if rest == [] then (walk++[c],g) else
  let (index,new_g) = randomR (0,(length rest)-1) g in
    let new_rest = rest !! index  in -- this may be a bad idea
        randomWalk_help (walk++[c],new_g) new_rest

strTree :: Show a => Tree a -> String
strTree (Node x rest) = show x ++ (foldr (++) "" [ (strTree r) | r <- rest] )

--

turtlesAWD :: Ring -> Morphism -> Maybe [Kernel] -> Maybe [Kernel]
turtlesAWD cur turtle path = let morphs = (filter (\m -> is_par_morph turtle m) (match matchMorphism cur)) in
  if morphs == [] then path else
    let my_morph = head morphs in do {
      new_ring <- apply my_morph cur;
      uw_path <- path;
      uw_ker <- traverse id [(morphism_to_kernel my_morph cur)];
      turtlesAWD new_ring turtle (Just (uw_path ++ uw_ker))
      }
      
is_par_morph :: Morphism -> Morphism -> Bool
is_par_morph base (Repeat k m) = base == (Repeat k m) || is_par_morph base m
is_par_morph base (Extend k m) = base == (Extend k m) || is_par_morph base m 
is_par_morph base m = base == m

--allPaths :: Ring -> [Kernel]
--allPaths start = let pf = buildPathForest start in
--  foldr 

--buildTree_help :: Ring -> [Kernel]
--buildTree_help r = 


--search_paths :: Ring -> Map.Map Ring [Morphism] -> [[Kernel]]
--search_paths cur hmap = let mmorphs = Map.lookup cur hmap in
--  let m_ker_neighbors = fmap (\m -> (morphism_to_kernel cur m , apply m cur) 
--  let neighbors = foldl (++) [] (fmap maybToList (fmap (\m -> apply m current) (Map.lookup cur hmap))) in
--  let paths = fmap search_paths 
--1

rec_expand :: [Ring] -> [Ring]
rec_expand lmr = nub (foldr (++) [] [[x] >>= expand | x <- lmr])

--
n_expand :: Int -> [Ring] -> [Ring]
n_expand n lr | n>0 = n_expand (n-1) (rec_expand lr)
             | n<=0 = lr

--
