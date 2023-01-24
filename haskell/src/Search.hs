module Search where

import FField
import PolyRings
import NTT
import CompileKernel
import Fourier
import Data.List
import Data.Maybe
import qualified Data.Map as Map (empty,insert,Map,member)

---

search :: (Eq a, Ord a) => [a] -> (a->[a]) -> Map.Map a [a] -> Map.Map a [a]
search (current:z) f hmap = if Map.member current hmap then search z f hmap else
  let neighbors = filter (\x -> x /= current) (f current) in
    let hmap_new = Map.insert current neighbors hmap in
      search (nub (z++neighbors)) f hmap_new
search [] f hmap = hmap
