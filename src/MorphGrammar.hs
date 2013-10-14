{-------------------------------------------------------------------------------
 MorphGrammar
 A generic morphology model.

 Jan Snajder <jan.snajder@fer.hr>

 (c) 2009 TakeLab
 University of Zagreb
 Faculty of Electrical Engineering and Computing
-------------------------------------------------------------------------------}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module MorphGrammar (

 -- type classes:
 IP (..), 
 IPR (..),
 DP (..),
 Labeled (..),

 -- functions:
 inflectionOf,
 iRelated,
 lWfs,
 lWfsMsd,
 wLemma,
 wLemmaMsd,
 lm,
 lm',
 lmMsd,
 lmMsd',
 sApp,
 lApp,
 lInflect, --inflectMsd,
 --lDerive,
 --derivationOf,dRelated,
 getPattern) where

import Data.List
import Control.Monad

--type Msd = String --TMP (should be a class)
-- TODO: ne uspijevam Msd ugraditi kao dodatni parametar klase
-- ... probaj jednog dana

--------------------------------------------------------------------------------
-- Inflectional pattern classes
--------------------------------------------------------------------------------

class (Eq ip, Ord x, Labeled ip) => IP ip x | ip -> x where
  sWfs    :: ip -> String -> [String]
  sWfsMsd :: ip -> String -> [(String,x)]
  sLemma  :: (MonadPlus m) => ip -> String -> m String  -- stem's lemma
  msds    :: ip -> [x]
--  inflect :: (MonadPlus m) => r -> Msd -> String -> m String

  -- minimal complete definition: sLemma, sWfsMsd, msds
  -- default definitions:
  sWfs f       = nub . map fst . sWfsMsd f

class (IP ip x) => IPR ip x where
  lStem     :: (MonadPlus m) => ip -> String -> m String   -- lemma's stem
  wStem     :: (MonadPlus m) => ip -> String -> m String
  wStemMsd  :: (MonadPlus m) => ip -> String -> m (String,x)

  -- minimal complete definition: lStem, wStemMsd
  -- default definitions:
  wStem ip = mnub . map fst . wStemMsd ip
  
--  wStemMsd r = 
--    msum . liftM (\(l,d) -> do s <- lStem r l; return (s,d)) . wLemmaMsd r

-- other functions:

mnub :: (MonadPlus m, Eq a) => [a] -> m a
mnub = msum . map return . nub

lWfsMsd :: (IPR ip x) => ip -> String -> [(String,x)]
lWfsMsd ip = nub . concatMap (sWfsMsd ip) . lStem ip

lWfs :: (IPR ip x) => ip -> String -> [String]
lWfs ip = nub . concatMap (sWfs ip) . lStem ip

wLemmaMsd :: (MonadPlus m, IPR ip x) => ip -> String -> m (String,x)
wLemmaMsd ip = 
  msum . liftM (\(s,x) -> do l <- sLemma ip s; return (l,x)) . wStemMsd ip

wLemma :: (MonadPlus m, IPR ip x) => ip -> String -> m String
wLemma ip = mnub . map fst . wLemmaMsd ip

sInflect :: (MonadPlus m, IP ip x) => ip -> x -> String -> m String
sInflect ip x = mnub . map fst . filter ((==x).snd) . sWfsMsd ip

lInflect :: (MonadPlus m, IPR ip x) => ip -> x -> String -> m String
lInflect ip x = mnub . map fst . filter ((==x).snd) . lWfsMsd ip

sApp :: (IP ip x) => ip -> String -> Bool
sApp ip = not . null . sWfs ip

lApp :: (IPR ip x) => ip -> String -> Bool
lApp ip = not . null . lWfs ip

-- groups together the inner homographs
--lWfsMsd' :: (IPR ip x) => ip -> String -> [(String,[x])]
--lWfsMsd' ip l = [(w,nub . sort $ map snd wds) | 
--  wds@((w,_):_) <- eqClasses (equating fst) (lWfsMsd ip l)]

-- lemmatises a wordform according to a list of inflectional patterns
-- returns possible lemma-Pattern pairs wrapped in a MonadPlus instance
lm :: (IPR ip x, MonadPlus m) => [ip] -> String -> m (String,ip)
lm ips w = mnub [(l,ip) | ip <- ips, l <- wLemma ip w]
  
-- E.g.
-- > lemmatize [n01,n03] "izvorom"
-- [("izvorom",N01),("izvor",N01)]

lm' :: (IPR ip x, MonadPlus m) => [ip] -> String -> m (String,String)
lm' ips w = mnub [(l,label ip) | ip <- ips, l <- wLemma ip w]
  
-- same as above but associates a msd label to each lemma-pattern pair
lmMsd :: (IPR ip x, MonadPlus m) => [ip] -> String -> m (String,x,ip)
lmMsd ips w = mnub [(l,x,ip) | ip <- ips, (l,x) <- wLemmaMsd ip w]

-- E.g.
-- > lemmatizeMsd "izvorom" [n01,n03]
-- [("izvorom","N=msn",N01),("izvorom","N=msa",N01),("izvor","N=msi",N01)]

-- same as above but associates a msd label to each lemma-pattern pair
lmMsd' :: (IPR ip x, MonadPlus m) => [ip] -> String -> m (String,x,String)
lmMsd' ips w = mnub [(l,x,label ip) | ip <- ips, (l,x) <- wLemmaMsd ip w]

{- TODO: mnub
-- generates all wordforms of a lemma accoring to a list of
-- inflectional Patterns
generate :: (IPattern r) => [r] -> String -> [String]
generate rs s = concatMap 
  (\r -> do l <- lm r s; w <- wfs r l; return w) rs

-- TODO: mnub
-- same as above but also attaches an Msd label
generateMsd :: (IPattern f x) => [f] -> String -> [(String,x)]
generateMsd rs s = concatMap 
  (\r -> do l <- sLemma r s; (w,d) <- wfsMsd r l; return (w,d)) rs
-}

{- needed? -}
--wMsd :: (IPatternW r,MonadPlus m) => r -> String -> m Msd
--wMsd r = mnub . map snd . wLemmaMsd r

-- relation of inflectional relatedness between a lemma and a wordform
-- not symmetric, reflexive,.... <--- TODO
inflectionOf :: (IPR ip x) => ip -> String -> String -> Bool
inflectionOf ip l w = w `elem` lWfs ip l

-- relation of inflectional relatedness between two wordforms
-- (worforms are related by Pattern 'r' if they share the same lemma
-- according to Pattern 'r')
iRelated :: (IPR ip x) => ip -> String -> String -> Bool
iRelated ip w1 w2 = ls1 `intersect` ls2 /= []
  where ls1 = wLemma ip w1
        ls2 = wLemma ip w2
  
--------------------------------------------------------------------------------
-- Derivational pattern class
--------------------------------------------------------------------------------

-- imamo 4 mogućnosti:
-- cilj/odredište: sr-pair ili lr-pair
-- transformacija: lemma-lemma ili stem-stem <-- ovo nije stvar API-ja

-- važno: bez functional dependency "d -> r" neće raditi "invert"
--class (Labeled d,IPattern f) => DPattern d f | d -> f where 
class (Labeled dp, IPR ip x) => DP dp ip x y where 
  lDerive :: (MonadPlus m) => dp -> (String,ip) -> m (String,y)
--  lDerive :: (MonadPlus m) => d -> (String,f) -> m (String,[f])

-- other functions:
{-
lDerive :: (MonadPlus m, IPR f x, DP d f x) => 
  d -> (String,f) -> m (String,[f])
lDerive d (l1,f1) = mnub [(l2,fs2) | 
  sf <- sfs, (s2,fs2) <- sDerive d sf, f2 <- fs2, l2 <- sLemma f2 s2]
  where sfs = [(s,f1) | s <- lStem f1 l1]
-}

-- relation of derivational relatedness
--derivationOf :: (DP d r) => d -> LR r -> LR r -> Bool
--derivationOf d lr1 lr2 = lr2 `elem` derive d lr1

-- symmetric and reflexive (but not transitive) closure of the above relation
--dRelated :: (DP d r) => d -> (String, r) -> (String, r) -> Bool
--dRelated d lr1 lr2 = 
--  derivationOf d lr1 lr2 || derivationOf d lr2 lr1 || lr1==lr2

{-
-- applies 'derive' to a list of derivational Patterns
deriveAll :: (DP d r) => [d] -> (String,r) -> [(String,r)]
deriveAll ds lr1 = nub . concat $ [derive d lr1 | d <- ds]
-}

--------------------------------------------------------------------------------
-- Auxiliary classes
--------------------------------------------------------------------------------

class Labeled x where
  label :: x -> String

--- rest:

getPattern :: (Labeled p) => [p] -> String -> Maybe p
getPattern ps l = find ((==l).label) ps

