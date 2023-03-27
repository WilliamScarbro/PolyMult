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

data Path = Path Ring [Morphism] deriving (Show,Eq)
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

path_get_states :: Path -> Maybe [Ring]
path_get_states (Path start morphs) = traverse id (scanl (\prev_state m -> prev_state >>= apply m) (Just start) morphs)
                                                            
buildPath :: Ring -> (Ring -> Maybe Morphism) -> Maybe Path
buildPath start f = buildPath_help start f start []
buildPath_help :: Ring -> (Ring -> Maybe Morphism) -> Ring -> [Morphism] -> Maybe Path
buildPath_help start f cur build = let morph = f cur in
  if morph == Nothing then Just (Path start build) else do {
    m <- morph;
    new_cur <- apply m cur;
    buildPath_help start f new_cur (build++(maybeToList morph)) }

buildPath_random :: Ring -> StdGen -> (Ring -> StdGen -> Maybe (Morphism,StdGen)) -> Maybe (Path,StdGen)
buildPath_random start rand f = buildPath_random_help start rand f start []
buildPath_random_help :: Ring -> StdGen -> (Ring -> StdGen -> Maybe (Morphism, StdGen)) -> Ring -> [Morphism] -> Maybe (Path,StdGen)
buildPath_random_help start rand f cur build = let morph_rand = f cur rand in
  if morph_rand == Nothing then Just (Path start build,rand) else do {
    (morph,nrand) <- morph_rand;
    new_cur <- apply morph cur;
    buildPath_random_help start nrand f new_cur (build++[morph]) }


appendPath :: Path -> Path -> Maybe Path
appendPath lhs rhs = do {
  lend <- path_get_end lhs;
  if lend == path_get_start rhs then Just (Path (path_get_start lhs) ((path_get_morphs lhs)++(path_get_morphs rhs))) else Nothing }

takePath :: Int -> Path -> Path
takePath i (Path s morphs) = let j=mod i (length morphs) in if j==0 then Path s morphs else Path s (take j morphs)

uniquePathChoices :: Path -> Path -> Int
uniquePathChoices p1 p2 = let common = commonStates p1 p2 in let uniq = do {
  choices <- foldl choiceFold (Just [(p1,p2)]) common; -- [(Path,Path)]
  uniqueC <- return (foldl (\prev (x,y) -> prev + (if x==y then 0 else 1)) 0 choices);
  return uniqueC } in
    fromMaybe (-1) uniq
  
combinePaths :: StdGen -> Path -> Path -> Maybe (Path,StdGen)
combinePaths rand p1 p2 = let common = commonStates p1 p2 in -- [Ring]
  do { -- Maybe
    choices <- foldl choiceFold (Just [(p1,p2)]) common; -- [(Path,Path)]
    r_choices <- return ( reverse choices ) ; -- [(Path,Path)]
    nPath <- return (foldl npFold (Just (Path (path_get_start p1) [],rand)) r_choices);
    nPath }
  where
    npFold :: Maybe (Path,StdGen) -> (Path,Path) -> Maybe (Path,StdGen)
    npFold (Just (prev,old_rand)) ctup = let (choice,new_rand) = randomChoice [fst ctup,snd ctup] old_rand in
      appendPath prev choice >>= (\x -> Just (x,new_rand))
    npFold Nothing _ = Nothing
    
choiceFold :: Maybe [(Path,Path)] ->  Ring -> Maybe [(Path,Path)]
choiceFold prev cur = do
   m_prev <- prev
   prev_head <- return . head $ m_prev
   q1 <- return . fst $ prev_head
   q2 <- return . snd $ prev_head
   (before_q1,after_q1) <- splitPathOnState q1 cur
   (before_q2,after_q2) <- splitPathOnState q2 cur
   return ([(after_q1,after_q2),(before_q1,before_q2)] ++ (tail m_prev))
                              

randomPath2 :: Ring -> StdGen -> Maybe (Path,StdGen)
randomPath2 start rand = buildPath_random start rand builder
  where
    builder :: Ring -> StdGen -> Maybe (Morphism,StdGen)
    builder cur rand = let morphs = match matchMorphism cur in
      if morphs == [] then Nothing else Just (randomChoice morphs rand)

combinePaths2 :: StdGen -> Path -> Path -> Maybe (Path,StdGen)
combinePaths2 rand p1 p2 = let (r1,r2) = split rand in
   let combinedMorphs  = (path_get_morphs p1) ++ (path_get_morphs p2) in
     --let shuffledMorphs = shuffle' combinedMorphs (length combinedMorphs) r1 in
     buildPath_random (path_get_start p1) rand (pathBuilder combinedMorphs)
     where
       pathBuilder :: [Morphism] -> Ring -> StdGen -> Maybe (Morphism,StdGen)
       pathBuilder oldMorphs ring rand = let morphs = match matchMorphism ring in 
         let overlapping = intersectBy (\m1 m2 -> is_par_morph (morph_get_inner m1) m2) morphs oldMorphs in
           let choices = if overlapping == [] then morphs else overlapping in
             if choices == [] then Nothing else Just (randomChoice choices rand)

--      return nPath
--      where


commonStates :: Path -> Path -> [Ring]
commonStates p1 p2 = let p1_states = path_get_states p1 in
  let p2_states = path_get_states p2 in
    fromMaybe [] (traverse id [p1_states,p2_states] >>= (\x -> Just (intersect (head x) (last x))))

splitPathOnState :: Path -> Ring -> Maybe (Path,Path)
splitPathOnState (Path start morphs) split = helper start [] start morphs split
  where
    helper :: Ring -> [Morphism] -> Ring -> [Morphism] -> Ring -> Maybe (Path,Path)
    helper start before current [] split = if current == split then Just (Path start before, Path split []) else Nothing
    helper start before current after split = if current == split then Just (Path start before,Path split after) else
      let m = head after in
        (apply m current) >>= (\next -> helper start (before++[m]) next (tail after) split)
                                                
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
      

randomChoice :: RandomGen g => [a] -> g -> (a,g)
randomChoice list rand = let (ind,rand2)=randomR (0,(length list)-1) rand in (list!!ind,rand2)

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
