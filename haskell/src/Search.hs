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

data Path = Path Ring [Morphism] deriving Show
path_get_start :: Path -> Ring
path_get_start (Path start morphs) = start

path_get_steps :: Path -> Maybe [Kernel] 
path_get_steps (Path r (m:morphs)) = let new_r = apply m r in
  let m_new_steps = (new_r >>= (\m_r -> path_get_steps (Path m_r morphs))) in
    let m_ker_list = traverse id ([morphism_to_kernel m r]) in
      m_ker_list >>= (\ker_list -> pure (ker_list ++) <*> m_new_steps)
path_get_steps (Path start []) = Just []

path_get_morphs :: Path -> [Morphism]
path_get_morphs (Path r morphs) = morphs

path_get_end :: Path -> Maybe Ring
path_get_end (Path start (m:morphs)) = let new_start = apply m start in
  new_start >>= (\r -> (path_get_end (Path r morphs)))
path_get_end (Path cur []) = Just cur

buildPath :: Ring -> (Ring -> Maybe Morphism) -> Maybe Path
buildPath start f = buildPath_help start f start []
buildPath_help :: Ring -> (Ring -> Maybe Morphism) -> Ring -> [Morphism] -> Maybe Path
buildPath_help start f cur build = let morph = f cur in
  if morph == Nothing then Just (Path start build) else do {
    m <- morph;
    new_cur <- apply m cur;
    buildPath_help start f new_cur (build++(maybeToList morph)) }

appendPath :: Path -> Path -> Maybe Path
appendPath lhs rhs = do {
  lend <- path_get_end lhs;
  if lend == path_get_start rhs then Just (Path (path_get_start lhs) ((path_get_morphs lhs)++(path_get_morphs rhs))) else Nothing }

takePath :: Int -> Path -> Path
takePath i (Path s morphs) = let j=mod i (length morphs) in if j==0 then Path s morphs else Path s (take j morphs)
---

buildForestPath :: Ring -> [Tree (Morphism)]
buildForestPath start = unfoldForest buildForestPath_branch [(Just start,mi) | mi <- match matchMorphism start]

buildForestPath_branch :: (Maybe Ring,Morphism) -> (Morphism, [(Maybe Ring, Morphism)])
buildForestPath_branch (r,m) = let r2 = r >>= apply m in
  (m,[(r2,mi) | mi <- buildForestPath_help r2])

buildForestPath_help :: Maybe Ring -> [Morphism]
buildForestPath_help (Just r) = match matchMorphism r
buildForestPath_help Nothing = []

randomPath :: Ring -> Int -> Maybe Path
randomPath start seed = let morphs = fst (randomWalk (fmap (fmap (\x -> Just x)) (buildForestPath start)) (mkStdGen seed)) in
  morphs >>= (\ms -> Just (Path start ms))

randomPathGen :: RandomGen g => Ring -> g -> Maybe (Path,g)
randomPathGen start rand = let (morphs,rand2) = (randomWalk (fmap (fmap (\x -> Just x)) (buildForestPath start)) rand) in
                             Just (\x -> (Path start x,rand2)) <*> morphs

-- Maybe Ring -> [Morphism]


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

--randomWalk :: RandomGen b => [Tree (Maybe Kernel)] -> b -> (Maybe [Kernel],b)
randomWalk :: (RandomGen b, Eq a) => [Tree (Maybe a)] -> b -> (Maybe [a],b)
randomWalk forest g = let (index,new_g) = randomR (0,(length forest)-1) g in
  let tree = forest !! index in
    let (walk,new_new_g) = randomWalk_help ([],new_g) tree in
      (traverse id walk,new_new_g)
--    (traverse id (fst (randomWalk_help ([],new_g) tree))
                   
randomWalk_help :: (RandomGen b, Eq a) => ([a],b) -> Tree a -> ([a],b)
randomWalk_help (walk,g) (Node c rest) = if rest == [] then (walk++[c],g) else
  let (index,new_g) = randomR (0,(length rest)-1) g in
    let new_rest = rest !! index  in -- this may be a bad idea
        randomWalk_help (walk++[c],new_g) new_rest

strTree :: Show a => Tree a -> String
strTree (Node x rest) = show x ++ (foldr (++) "" [ (strTree r) | r <- rest] )

--

-- rewrite turtles to work with paths
turtles :: Ring -> Morphism -> Maybe Path
turtles start turtle = let findTurtle ring = let morphs = (filter (\m -> is_par_morph turtle m) (match matchMorphism ring)) in
                             if morphs == [] then Nothing else Just (head morphs) in
                         buildPath start findTurtle

turtlesExtend :: Path -> Morphism -> Maybe Path
turtlesExtend p1 turtle = do { p1_end <- path_get_end p1;
                               p2 <- turtles p1_end turtle;
                               appendPath p1 p2 }
  
turtlesAWD :: Ring -> Morphism -> Maybe [Kernel] -> Maybe [Kernel]
turtlesAWD cur turtle path = let morphs = (filter (\m -> is_par_morph turtle m) (match matchMorphism cur)) in
  if morphs == [] then path else
    let my_morph = head morphs in do {
      new_ring <- apply my_morph cur;
      uw_path <- path;
      uw_ker <- traverse id [(morphism_to_kernel my_morph cur)];
      turtlesAWD new_ring turtle (Just (uw_path ++ uw_ker))
      }
      

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
