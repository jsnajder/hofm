{-------------------------------------------------------------------------------
 MorphGrammar.Hofm.Transf.TTransf
 Transformation function represented as a Haskell functions

 Jan Snajder <jan.snajder@fer.hr>

 (c) 2009 TakeLab
 University of Zagreb
 Faculty of Electrical Engineering and Computing
-------------------------------------------------------------------------------}

module MorphGrammar.Hofm.Transf.FTransf (FTransf) where

import MorphGrammar.Hofm.Transf
import MorphGrammar.Hofm.Transf.StringOp
import Control.Monad

data FTransf = FTransf (String -> [String])

instance Transf FTransf where

  ($$) (FTransf t) = msum . map return . t

  (FTransf t1) & (FTransf t2) = FTransf (\s -> concatMap t1 (t2 s))

  nul = FTransf (:[])

  rsfx s1 s2 = FTransf (\s -> replaceSuffix s1 s2 s)
  
  rpfx p1 p2 = FTransf (\s -> replacePrefix p1 p2 s)
  
  rifx i1 i2 = FTransf (\s -> replaceInfix i1 i2 s)

  fail = FTransf (const [])

instance OptTransf FTransf where

  (FTransf t1) .|. (FTransf t2) = FTransf (\s -> (t1 s) ++ (t2 s))

  (FTransf t1) .||. (FTransf t2) = 
    FTransf (\s -> case t1 s of
                     [] -> t2 s
                     ss -> ss)

