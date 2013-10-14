{-------------------------------------------------------------------------------
 MorphGrammar.Hofm.Transf
 Higher-order functions morphology: transformations and condition functions
 v1.0

 Jan Snajder <jan.snajder@fer.hr>

 (c) 2008 TakeLab
 University of Zagreb
 Faculty of Electrical Engineering and Computing
-------------------------------------------------------------------------------}

module MorphGrammar.Hofm.Transf (
  Transf (..),
  InvTransf (..),
  OptTransf (..),
  AnalyzableTransf (..),
  sfx, 
  pfx, 
  dsfx, 
  dpfx, 
  difx, 
  asfx, 
  apfx, 
  aifx, 
  try, 
  opt,
  Cond (..),
  FCond,
  ends, 
  starts, 
  nends) where

import Data.Maybe
import Data.List
import Data.Char
import Control.Monad
import Data.Ord (comparing)

--------------------------------------------------------------------------------
-- String transformation class
--------------------------------------------------------------------------------

class Transf t where 
  ($$) :: (MonadPlus m) => t -> String -> m String
  (&)  :: t -> t -> t
  rsfx :: String -> String -> t
  rpfx :: String -> String -> t
  rifx :: String -> String -> t
  nul  :: t
  fail :: t -- mislim da ovo ne treba eksplicitno. ili?
            -- mora vrijediti: forall s. fails $$ s == mzero

class (Transf t) => InvTransf t where  -- invertible transf
  inv :: t -> t

class (Transf t) => OptTransf t where   
  (.||.) :: t -> t -> t   -- "orelse": do right ONLY IF left fails
  (.|.)  :: t -> t -> t   -- "or": do both transformations
  
class (Transf t, Eq t) => AnalyzableTransf t where
  functional :: t -> Bool
  injective  :: t -> Bool
  consistent :: t -> Bool

  consistent t = t /= MorphGrammar.Hofm.Transf.fail

infixr 9 &
infixr 6 $$

--------------------------------------------------------------------------------
-- Wrappers for common string transformations
--------------------------------------------------------------------------------

sfx :: (Transf t) => String -> t
sfx s = rsfx "" s

pfx :: (Transf t) => String -> t
pfx s = rpfx "" s

-- NB: 'ifx' operation is not well-defined and thus omitted

dsfx :: (Transf t) => String -> t
dsfx s = rsfx s ""

dpfx :: (Transf t) => String -> t
dpfx p = rpfx p ""

difx :: (Transf t) => String -> t
difx i = rifx i ""

-- generic afix alternation
afx :: (OptTransf t) => (String -> String -> t) -> [(String,String)] -> t 
afx r [] = MorphGrammar.Hofm.Transf.fail
afx r as = foldl1 (.|.) $ map (uncurry r) as

asfx :: (OptTransf t) => [(String,String)] -> t
asfx = afx rsfx

apfx :: (OptTransf t) => [(String,String)] -> t
apfx = afx rpfx

aifx :: (OptTransf t) => [(String,String)] -> t
aifx = afx rifx
  
try :: (OptTransf t) => t -> t  -- transform if applicable (do if you can)
try = (.||. nul)

opt :: (OptTransf t) => t -> t  -- optional transform
opt = (.|. nul)

{-- Examples:
> sfx "i" & alt sibil & inv(sfx "a") $$ "slika" :: Maybe String
  Just "slici"
> inv(sfx "i" & alt sibil & inv(sfx "a")) $$ "slici" :: [String]
  ["slika"]
--}

--------------------------------------------------------------------------------
-- Condition testing
--------------------------------------------------------------------------------

class Cond c where
  test :: c -> String -> Bool
  land :: c -> c -> c
  lor  :: c -> c -> c
  neg  :: c -> c
  always :: c
  never :: c

  c1 `land` c2 = neg (neg c1 `lor` neg c2)
  c1 `lor`  c2 = neg (neg c1 `land` neg c2)
  never = neg $ always

data FCond = FCond (String -> Bool)

instance Cond FCond where
  test (FCond c) = c
  land (FCond c1) (FCond c2) = FCond $ \s -> c1 s && c2 s
  lor  (FCond c1) (FCond c2) = FCond $ \s -> c1 s || c2 s
  neg  (FCond c) = FCond $ not . c
  always = FCond $ const True

instance Show FCond where
  show c = "<fcond>"

--------------------------------------------------------------------------------
-- Common condition functions
--------------------------------------------------------------------------------

nends :: [String] -> FCond
nends = neg . ends

ends :: [String] -> FCond
ends ss = FCond $ \s -> any (`isSuffixOf` s) ss

starts :: [String] -> FCond
starts ss = FCond $ \s -> any (`isPrefixOf` s) ss

