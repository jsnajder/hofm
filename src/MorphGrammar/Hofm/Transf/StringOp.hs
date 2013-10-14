{-------------------------------------------------------------------------------
 MorphGrammar.Hofm.Transf.StringOp
 String operations

 Jan Snajder <jan.snajder@fer.hr>

 (c) 2008 TakeLab
 University of Zagreb
 Faculty of Electrical Engineering and Computing
-------------------------------------------------------------------------------}

module MorphGrammar.Hofm.Transf.StringOp (
  replacePrefix,
  replacePrefixCS,
  replaceSuffix,
  replaceInfix,
  replaceInfixes,
  replaceInfixI) where

import Data.List
import Data.Char
import Control.Monad

dropPrefix :: (MonadPlus m,Eq a) => [a] -> [a] -> m [a]
dropPrefix [] [] = return []
dropPrefix [] rs = return rs
dropPrefix _  [] = mzero
dropPrefix (s:ss) (r:rs) 
  | s == r     = dropPrefix ss rs
  | otherwise  = mzero

dropSuffix :: (MonadPlus m,Eq a) => [a] -> [a] -> m [a]
dropSuffix s = liftM reverse . dropPrefix (reverse s) . reverse

replaceSuffix :: (MonadPlus m,Eq a) => [a] -> [a] -> [a] -> m [a]
replaceSuffix s1 s2 = liftM (++s2) . dropSuffix s1

replacePrefix :: (MonadPlus m,Eq a) => [a] -> [a] -> [a] -> m [a]
replacePrefix p1 p2 = liftM (p2++) . dropPrefix p1
  
replacePrefixCS :: (MonadPlus m) => String -> String -> String -> m String
replacePrefixCS p1 p2 s = replacePrefix p1 p2' s'
  where (p2',s') = case (p2,s) of
          (p:ps,s:ss) -> if isUpper s then (toUpper p:ps,toLower s:ss)
                         else (p:ps,s:ss)
          (ps,ss)     -> (ps,ss) 

replaceInfix :: (MonadPlus m,Eq a) => [a] -> [a] -> [a] -> m [a]
replaceInfix [] [] s = return s
replaceInfix [] _  _ = mzero
replaceInfix i1 i2 s = replace i1 s s []
  where replace [] rs _ qs = return (reverse qs ++ i2 ++ rs)
        replace (s:ss) (r:rs) (p:ps) qs 
          | s == r    = replace ss rs (p:ps) qs 
          | otherwise = replace i1 ps ps (p:qs)
        replace _ [] _ _ = mzero

-- zamjenjuje infikas na više mjesta, ako je to moguće
-- vrlo ružna izvedba...
replaceInfixes :: (MonadPlus m, Eq a) => [a] -> [a] -> [a] -> m [a]
replaceInfixes i1 i2 xs = msum . map return . init $ replace xs
  where replace [] = [[]]
        replace xs@(x:zs) = case replacePrefix i1 i2 xs of
          Just ys -> ys : (map (x:) (replace zs))
          Nothing -> map (x:) (replace zs)

-- invertable infix replacement, scanning from left to right
-- if x should be replaced by y, then y must not appear to the left of x,
-- otherwise operation is not injective (Nothing is returned in this case)
-- TODO: pogledaj malo ovo, sigurno je nepotrebno komplicirano!!!
-- infix replacement (_,"") is not allowed (unlike for replaceInfix
-- above)
replaceInfixI :: (MonadPlus m,Eq a) => [a] -> [a] -> [a] -> m [a]
replaceInfixI [] [] s = return s
replaceInfixI [] _  _ = mzero
replaceInfixI _ []  _ = mzero
replaceInfixI s1 s2 s = replace s1 s s []
    where
      replace [] rs _ qs | injective = return (rqs ++ s2 ++ rs)
                         | otherwise = mzero
          where 
            rqs = reverse qs
            left = rqs ++ (init s2)
            injective = not (or [isPrefixOf s2 s | s <- tails left])
      replace (s:ss) (r:rs) (p:ps) qs | s == r = replace ss rs (p:ps) qs 
                                      | otherwise = replace s1 ps ps (p:qs)
      replace _ [] _ _ = mzero

