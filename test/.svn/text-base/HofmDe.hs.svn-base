{-------------------------------------------------------------------------------
 HofmDe test file

 (c) 2012 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

import MorphGrammar.Hofm hiding (($$))
import qualified MorphGrammar.Hofm (($$))
import MorphGrammar.Hofm.Language.German hiding (iPatterns,dPatterns)
import qualified MorphGrammar.Hofm.Language.German as G (iPatterns,dPatterns)

-- type specializations
($$) = (MorphGrammar.Hofm.$$) :: TransfDefault -> String -> [String]
iPatterns = G.iPatterns :: [IPatternDefault]
dPatterns = G.dPatterns :: [DPatternDefault]

t1 = pfx "ge" & rifx "i" "a" & dsfx "en" :: TransfDefault

testDPattern :: 
  DPatternDefault -> (String,IPatternDefault) -> [String]
testDPattern d (l1,ip1) = [l2 ++ "_" ++ label (ip2::IPatternDefault) | 
  (l2,ips) <- lDerive d (l1,ip1), ip2 <- ips]

-- > testDPattern d1 ("singen",verb)
-- ["gesang_Nm"] 

testDPatterns :: (String,IPatternDefault) -> [(String,String)]
testDPatterns lp1 = [(l2,label d) | d <- dPatterns, l2 <- testDPattern d lp1 ]

-- > testDPatterns ("singen",verb)
-- [("gesang_Nm","dVN01"),("gesinge_Nn","dVN02"),("unsingbar_A","dVA01")]

