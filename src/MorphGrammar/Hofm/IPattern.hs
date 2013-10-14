{-------------------------------------------------------------------------------
 MorphGrammar.Hofm.Inflection
 Higher-order functions morphology: inflectional pattern instance

 Jan Snajder <jan.snajder@fer.hr>

 (c) 2009 TakeLab
 University of Zagreb
 Faculty of Electrical Engineering and Computing
-------------------------------------------------------------------------------}

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module MorphGrammar.Hofm.IPattern (
  IPattern, 
  iPattern, 
  (<&), 
  (#), 
  (<#)) where

import MorphGrammar
import MorphGrammar.Hofm.Transf hiding (consistent,functional,injective)
import qualified MorphGrammar.Hofm.Transf as Transf 
  (consistent,functional,injective)
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Data.Char (isDigit)
import Control.Monad

--------------------------------------------------------------------------------
-- MSD class
--------------------------------------------------------------------------------

class Ord x => Msd x where
  combine :: x -> x -> x

instance Msd String where

-- combines two MSD descriptors
-- elements of the second descriptor are inserted over the symbol "." in the
-- first descriptor whereas extra elements are concatenated to the right
  combine [] ys = ys
  combine xs [] = xs
  combine (x:xs) (y:ys) | x == '.'  = y : combine xs ys
                        | otherwise = x : combine xs (y:ys)

{-- Example:
> combine "Afp...y=" "msa"
  "Afpmsay="
--}

--------------------------------------------------------------------------------
-- Inflectional rule instance
--------------------------------------------------------------------------------

type Label = String    -- unique identifier
type TransfMsd t x = (t,[x])
data IPattern t c x = IPattern Label c [TransfMsd t x]
--  deriving Show

instance Eq (IPattern t c x) where
  ip1 == ip2 = label ip1 == label ip2

instance Ord (IPattern t c x) where
  compare = comparing label

instance Show (IPattern t c x) where
  show (IPattern label _ rs) = label
  
instance Labeled (IPattern t c x) where
  label (IPattern l _ _) = l

instance (Transf t, Cond c, Msd x) => IP (IPattern t c x) x where

  -- transforms the stem into the lemma
  -- sLemma :: (MonadPlus m, IP ip) => ip -> String -> m String
  sLemma (IPattern _ _ ((t0,_):_)) = (t0 $$)

  -- wordform generation from the lemma
  -- sWfsMsd :: IP ip => ip -> String -> [(String, Msd)]
  sWfsMsd (IPattern _ c tds) s | app       = nub wds
                               | otherwise = []
    where (ts@(t0:_),dss) = unzip tds
          app = test c s && all (not . null) wss
          wss = map ($$ s) ts
          wds = [(w,d) | (ws,ds) <- zip wss dss, w <- ws, t <- ts, d <- ds]
 
  --msds :: IRule t Msd -> [Msd]
  msds (IPattern _ _ tds) = nub $ concatMap snd tds
         
instance (InvTransf t, Cond c, Msd x) => IPR (IPattern t c x) x where

  -- lStem :: (MonadPlus m, IP ip) => ip -> String -> m String
  lStem (IPattern _ _ ((t0,_):_)) = (inv t0 $$)

  -- may return several distinct stems if:
  -- a) transformation that is applied is nonninjective, thus its
  --    inverse is nonfunctional
  -- b) several transformations apply to the wordform (as is the case
  --    when one transformations is the nul transformation)
  --    (e.g. "izvorom" and rule n01)
  -- wStemMsd :: (MonadPlus m, IP ip) => ip -> String -> m (String, Msd)
  wStemMsd f@(IPattern _ c txs) w = 
    msum . map return . nub $ sxs
    where (ts@(t0:_),xss) = unzip txs
          sss  = map (($$ w) . inv) ts
          sxs = [(s,x) | (ss,xs) <- zip sss xss, s <- ss, x <- xs, f `sApp` s]

--------------------------------------------------------------------------------
-- Helpers for rule definitions
--------------------------------------------------------------------------------

-- infix operator for associating MSD labels to transformations
-- fixity is 7 thus it binds weakly than .&. whose fixity is 9
infix 7 #
(#) :: (Transf t) => t -> [x] -> TransfMsd t x
t # msd = (t, msd)

-- infix operator for combining MSD labels to MSD-labeled transformations
-- the new MSD labels are combined to the left of the existing labels
infixl 7 <#
(<#) :: (Transf t,Msd x) => [TransfMsd t x] -> [x] -> [TransfMsd t x]
ts <# msds = 
    [(t,[combine msd1 msd2 | msd1<-msds, msd2<-msds']) | (t,msds') <- ts]

-- distributes a transformation over a list of transformations 
-- (and the associated msds)
-- the transformation that is distributed will be applied first
-- (i.e. it is composed to the right)
-- fixity is 9 thus it binds more strongly than ## whose fixity is 7
infixl 9 <&
(<&) :: (Transf t,Msd x) => [TransfMsd t x] -> t -> [TransfMsd t x]
ts <& t = map (\(t',msds)->(t' & t,msds)) ts

iPattern :: (Transf t, Cond c, Msd x) =>
  Label -> x -> c -> [TransfMsd t x] -> IPattern t c x
iPattern label msd c ts = 
  IPattern label c (map (\(t,msds) -> (t,map (combine msd) msds)) ts)
  --IPattern label (const True) (map (\(t,msds)->(t,map (combine msd) msds)) ts)
                    -- ^^^^^ TMP (19.9.2012.) Jan

{-- Example:
n01 = InflRule "N01" "N=m"   -- izvor
      always
      [nul         # ["sn","sa"],
       sfx "a"     # ["sg","pg"], 
       sfx "u"     # ["sd","sl"], 
       sfx "e"     # ["sv","pa"], 
       sfx "om"    # ["si"], 
       sfx "i"     # ["pn","pv"], 
       sfx "ima"   # ["pd","pl","pi"]]
--}

consistent :: (AnalyzableTransf t) => IPattern t c msd -> Bool
consistent (IPattern _ _ txs) = all (Transf.consistent . fst) txs

functional :: (AnalyzableTransf t) => IPattern t c msd -> Bool
functional (IPattern _ _ txs) = all (Transf.functional . fst) txs

injective :: (AnalyzableTransf t) => IPattern t c msd -> Bool
injective (IPattern _ _ txs) = all (Transf.injective . fst) txs

-- > map label . filter (not . Hofm.IRule.consistent) $ (iRules :: [IRule CTransf Msd])

