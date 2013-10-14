{-------------------------------------------------------------------------------
 MorphGrammar.Hofm.Transf.TTransf
 Transformation function represented as a tree of replacement operators

 Jan Snajder <jan.snajder@fer.hr>

 (c) 2009 TakeLab
 University of Zagreb
 Faculty of Electrical Engineering and Computing
-------------------------------------------------------------------------------}

module MorphGrammar.Hofm.Transf.TTransf (TTransf) where

import MorphGrammar.Hofm.Transf
import MorphGrammar.Hofm.Transf.StringOp
import Data.Maybe
import Data.List
import Control.Monad
import Data.Ord (comparing)

--------------------------------------------------------------------------------
-- TTransf instance: Transformation in tree-form
--------------------------------------------------------------------------------

data Op = 
  RP  String String |
  RS  String String |
  RI  String String |
  NO
  deriving (Eq,Show)

data TTransf = 
  Else TTransf TTransf |
  Ilse TTransf TTransf |
  Or   TTransf TTransf |
  Comp TTransf TTransf |
  Tip  Op |
  Fail 
  deriving (Eq,Show)

applyOp :: (MonadPlus m) => Op -> String -> m String
applyOp (RP p1 p2) = replacePrefixCS p1 p2
applyOp (RS s1 s2) = replaceSuffix s1 s2
applyOp (RI i1 i2) = replaceInfixes i1 i2
applyOp NO         = return

invOp :: Op -> Op
invOp (RP p1 p2) = RP p2 p1
invOp (RS s1 s2) = RS s2 s1
invOp (RI i1 i2) = RI i2 i1
invOp NO         = NO

apply :: (MonadPlus m) => TTransf -> String -> m String
apply t x = msum . map return $ app t
  where app Fail = mzero
        app (Tip op) = applyOp op x
        app (Else t1 t2) = case app t1 of
          [] -> app t2
          ys -> ys
        app (Or t1 t2) = app t1 `mplus` app t2
        app (Comp t1 t2) = apply t2 x >>= apply t1
        app (Ilse t1 t2) =
          app t1 `mplus` [r | r <- apply t2 x, apply (inv t1) x == []]
          -- NB: t1 i t2 s već invertirani
          
-- (t1||t2)^-1(s) = t1^-1(s) U t2^-1(s) ako (t1 . t2^-1)(s) = []
--                = t1^-1(s)            inače
-- tj. ako bi se na t2^-1(s) mogao primijeniti t1, onda bi se sigurno
-- primjenio, pa je dakle u tom slucaju t2 zasjenjen s t1.
-- ali ako to nije slucaj, onda je i t2^-1(s) moguci inverz
--
-- npr:
-- > let t = rsfx "a" "x" .||. nul :: TTransf
-- > t $$ "bra" :: [] String
-- ["brx"]
-- > t $$ "brx" :: [] String
-- ["brx"]
-- > t $$ "bry" :: [] String
-- ["bry"]
--
-- > (inv t) $$ "bra" :: [] String
-- []
-- *Hofm.Transf.TTransf> (inv t) $$ "brx" :: [] String
-- ["bra","brx"]
-- *Hofm.Transf.TTransf> (inv t) $$ "bry" :: [] String
-- ["bry"]

compose :: TTransf -> TTransf -> TTransf
compose (Tip NO) t2       = t2
compose t1       (Tip NO) = t1
compose t1       t2       = t1 `Comp` t2

orelse ::  TTransf -> TTransf -> TTransf
orelse t1 t2 = t1 `Else` t2

inverse :: TTransf -> TTransf
inverse Fail         = Fail
inverse (Tip op)     = Tip $ invOp op
inverse (Else t1 t2) = Ilse (inverse t1) (inverse t2) -- !!!
inverse (Ilse t1 t2) = Else (inverse t1) (inverse t2)
inverse (Or t1 t2)   = Or (inverse t1) (inverse t2)
inverse (Comp t1 t2) = Comp (inverse t2) (inverse t1)
  
-- instance definitions

instance Transf TTransf where

  t $$ s = apply t s

  (&) = Comp

  rsfx s1 s2 = Tip $ RS s1 s2

  rpfx p1 p2 = Tip $ RP p1 p2

  rifx "" _  = Fail
  rifx _ ""  = Fail
  rifx i1 i2 = Tip $ RI i1 i2

  nul  = Tip NO

  fail  = Fail

instance InvTransf TTransf where

  inv  = inverse

instance OptTransf TTransf where

  (.||.) = Else

  (.|.) = Or

