module Search where

import FField
import PolyRings
import NTT
import Fourier
import Data.List
import Data.Maybe
import qualified Data.Map as Map (empty,insert,Map,member,mapAccumWithKey)
import Data.Tree

---


search_morphs :: [Ring] -> Map.Map Ring [Morphism] -> Map.Map Ring [Morphism]
search_morphs (current:z) hmap = if Map.member current hmap then search_morphs z hmap else
  let morphs = match matchMorphism current in
    let neighbors = foldl (++) [] (fmap maybeToList (fmap (\m -> apply m current) morphs)) in
      let hmap_new = Map.insert current morphs hmap in
        search_morphs (nub (z++neighbors)) hmap_new
        
--search_morphs (current:z) f hmap = if Map.member current hmap then search z f hmap else
--  let neighbors = filter (\x -> x /= current) (f current) in
--    let hmap_new = Map.insert current neighbors hmap in
--      search (nub (z++neighbors)) f hmap_new
search_morphs [] hmap = hmap

--

terminal :: Eq b => Map.Map a [b] -> [a]
terminal hmap = fst (Map.mapAccumWithKey (\accum key val -> (if val==[] then accum++[key] else accum,0)) [] hmap)


--- 

buildTree :: Ring -> [Tree (Maybe Kernel)]
buildTree start = unfoldForest buildTree_branch [(Just start,mi) | mi <- match matchMorphism start]
 
buildTree_branch :: (Maybe Ring,Morphism) -> (Maybe Kernel, [(Maybe Ring, Morphism)])
buildTree_branch (r,m) = let r2 = r >>= apply m in
  (r >>= morphism_to_kernel m, [(r2,mi) | mi <- buildTree_help1 r2])

buildTree_help1 :: Maybe Ring -> [Morphism]
buildTree_help1 (Just r) = match matchMorphism r
buildTree_help1 Nothing = []


--


buildRingTree :: Ring -> Tree Ring
buildRingTree start = unfoldTree buildRingTree_branch start

buildRingTree_branch :: Ring -> (Ring,[Ring])
buildRingTree_branch r = (r,foldl (++) [] (fmap maybeToList (fmap (\m -> apply m r) (match matchMorphism r))))



--buildTree_help :: Ring -> [Kernel]
--buildTree_help r = 


--search_paths :: Ring -> Map.Map Ring [Morphism] -> [[Kernel]]
--search_paths cur hmap = let mmorphs = Map.lookup cur hmap in
--  let m_ker_neighbors = fmap (\m -> (morphism_to_kernel cur m , apply m cur) 
--  let neighbors = foldl (++) [] (fmap maybToList (fmap (\m -> apply m current) (Map.lookup cur hmap))) in
--  let paths = fmap search_paths 
--1

expand :: Ring -> [Ring]
expand r = let mrl=nub (fmap (\m -> apply m r) (match matchMorphism r)) in foldr (++) [] (fmap maybeToList mrl)


rec_expand :: [Ring] -> [Ring]
rec_expand lmr = nub (foldr (++) [] [[x] >>= expand | x <- lmr])

--
n_expand :: Int -> [Ring] -> [Ring]
n_expand n lr | n>0 = n_expand (n-1) (rec_expand lr)
             | n<=0 = lr

--
