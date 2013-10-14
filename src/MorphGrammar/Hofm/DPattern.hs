{-------------------------------------------------------------------------------
 MorphGrammar.Hofm.Derivation
 Higher-order functions morphology: derivational pattern instance
 v1.0

 Jan Snajder <jan.snajder@fer.hr>

 (c) 2008 TakeLab
 University of Zagreb
 Faculty of Electrical Engineering and Computing
-------------------------------------------------------------------------------}

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module MorphGrammar.Hofm.DPattern (
    DPattern (DPatternSS, DPatternSL, DPatternLL) -- TMP constructor export
    ) where

import MorphGrammar
import MorphGrammar.Hofm.Transf
import MorphGrammar.Hofm.IPattern
import Data.List
import Control.Monad
import Data.Maybe 
  
--------------------------------------------------------------------------------
-- Derivational pattern instance
--------------------------------------------------------------------------------

type Label = String
data DPattern t ip = 
    DPatternSS Label t [ip] [ip]  -- derivation from a stem to stem
  | DPatternSL Label t [ip] [ip]  -- derivation from a stem to lemma
  | DPatternLL Label t [ip] [ip]  -- derivation from a lemma to lemma
  deriving Show

instance Eq (DPattern t ip) where
  dp1 == dp2 = label dp1 == label dp2

instance Ord (DPattern t ip) where
  compare dp1 dp2 = compare (label dp1) (label dp2)

--instance Show t => Show (DPattern t ip) where
--  show (DPattern t ip) = "DPattern " ++ show t ++ " " ++ show ip
--  show = label

instance Labeled (DPattern t ip) where  
  label (DPatternSS l _ _ _) = l 
  label (DPatternSL l _ _ _) = l 

-- DRule applies the deriving transformation to the lemma,
-- and additionally checks whether the obtained lemma-rule pair is
-- valid (i.e. that the infl. rule is applicable to the lemma)
instance (Transf t, IPR ip x) => DP (DPattern t ip) ip x [ip] where
 
  -- deriving transformation applies to stem and derives a lemma
  -- derive :: d -> (String,r) -> [(String,r)]
  lDerive (DPatternSL _ t ips1 ips2) (l1,ip1)
    | ip1 `elem` ips1 = msum . map return . groupSnd $ 
                        [(l2,ip2) | s1 <- lStem ip1 l1, 
                         l2 <- t $$ s1, ip2 <- ips2, ip2 `lApp` l2]
    | otherwise       = mzero
  lDerive (DPatternSS _ t ips1 ips2) (l1,ip1)
    | ip1 `elem` ips1 = msum . map return . groupSnd $ 
                        [(l2,ip2) | s1 <- lStem ip1 l1, 
                         s2 <- t $$ s1, ip2 <- ips2, l2 <- sLemma ip2 s2]
    | otherwise       = mzero
  lDerive (DPatternLL _ t ips1 ips2) (l1,ip1)
    | ip1 `elem` ips1 = msum . map return . groupSnd $ 
                        [(l2,ip2) |
                         l2 <- t $$ l1, ip2 <- ips2, ip2 `lApp` l2]
    | otherwise       = mzero

groupSnd :: Eq a => [(a, b)] -> [(a, [b])]
groupSnd xs = [ (y, map snd ys) | ys@((y,_):_) <- eqClasses (equating fst) xs]

eqClasses :: (a -> a -> Bool) -> [a] -> [[a]]
eqClasses _  []     = []
eqClasses eq (x:xs) = (x:qs) : eqClasses eq rest
  where (qs,rest) = partition (eq x) xs

equating :: Eq a => (b -> a) -> b -> b -> Bool
equating p x y = p x == p y
  

  {-
  sDerive (DPattern _ t fs1 fs2) (s1,f1)
    | f1 `elem` fs1 = msum . map return $ 
        [(s2,filter (`app` s2) fs2) | l2 <- t $$ s1] 
    | otherwise     = mzero
  -}
  -- deriveValid :: d -> (String,r) -> [(String,r)]
--  deriveValid d = filter (\(l,r) -> r `app` l) . derive d
 
  -- deriveValid :: d -> (String,r) -> [(String,r)]
--  deriveValid d = filter (\(l,r) -> r `app` l) . derive d

{-
  -- NB: we can provide a version in which deriving transf. is applied
  -- to the stem, however such a derivation would be more ambiguous (when
  -- transforming from the lemma, we also constrain the set of possible
  -- suffixes that the lemma can have)
  derive (DRule _ t rs1 rs2) (l1,r1)
    | r1 `elem` rs1 = [(l2,r2) | s1 <- lStem r1 l1, s2 <- t $$ s1, 
                                 r2 <- rs2, r2 `sApp` s2, l2 <- sLemma r2 s2] 
    | otherwise     = []
-}


--------------------------------------------------------------------------------
-- Derivational relation
--------------------------------------------------------------------------------

-- funkcija koja generira "deriv nest" krenuvši od nekog (l,r),
-- uz uvjet da za svaki generirani vrijedi neki predikat
-- (l,r) -> Bool
-- (koji može biti provjera je li r `app` l, ili/i provjera je li
-- lr-par postoji u lekikonu)
-- i omogućiti "preskakanja"?
-- da li to sve ovdje definirati ili ipak negdje drugdje?
--



