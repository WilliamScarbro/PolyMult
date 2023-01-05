module PolyRings where

-- objects

-- n d b p
data Base = Base Integer Integer Integer Integer deriving (Show)
-- k
data Product a = Prod Integer a deriving (Show)
-- k d
data Quotient a = Quo Integer Integer a deriving (Show)

instance Functor Quotient where
  fmap f (Quo k d r) = (Quo k d (f r))

instance Functor Product where
  fmap f (Prod k r) = Prod k (f r)


-- morphisms

--phi
factor :: Integer -> Base -> Product Base
factor k (Base n d b p) = Prod k (Base (n `div` k) (d `div` k) b p)

--xi
label :: Integer -> Base ->  Quotient Base
label k (Base n d b p) = Quo k 0 (Base (n `div` k) d b p)

--gamma
norm :: Base -> Quotient Base
norm (Base n d b p) = Quo 1 (div d n) (Base n 0 b p)  

--psi
define :: Quotient Base -> Base
define (Quo k d0 (Base 1 d b p)) = Base k (d0+d) b p

--zeta
pushin :: Quotient (Product Base) -> Product (Quotient Base)
pushin (Quo kq d0 (Prod kp (Base n d b p))) = Prod kp (Quo kq d0 (Base n d b p))


-- object-morphisms
--
data Morphism = Compose Morphism Morphism | Extend Integer Morphism | Repeat Integer Morphism | Factor Integer | Label Integer | Norm | Define | Pushin

apply :: Morphism -> a -> a
apply (Label i) x = (label i) x
--
-- a -> b
--P k r) = P k (apply m r)
--Q k r) = Q k (apply m r)
--

-- tasks:
--   build a list of possible morphisms from a given type
--   given a morphism and a type, produce a new type
--   given a morphism return corresponding function which acts on objects of domain (and returns objects in codomain)

--class Morphism a where
--  domain :: a -> b
--  apply :: 
--class Morphism a b c where
--  apply :: a -> b -> c
--
--data Factor = Factor Integer
--instance Morphism Factor Base (Product Base) where
--  function (Factor k) = factor k
--
--data Label = Label Integer
--instance Morphism Label Base (Quotient Base) where
--  function (Label k) = label k
--
--data Norm
--instance Morphism Norm Base (Quotient Base) where
--  function Norm = norm
--
--data Define
--instance Morphism Define where
--  function Define = define
--    
--data Pushin
--instance Morphism Pushin where
--  function Pushin = pushin
--
--data Repeat a
--instance Morphism (Repeat Morphism) (Quotient a) (Quotient b)  where
--  apply (Repeat (Factor k)) (Base n d b p) = fmap (function m)
--
--data Extend = Extend Morphism where
--  function (Extend (Factor k)) = fmap (function m)
-- 
--- meta-morphisms

