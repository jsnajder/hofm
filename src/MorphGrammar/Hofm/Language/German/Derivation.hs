{-------------------------------------------------------------------------------
 MorphGrammar.Hofm.Language.German.Derivation
 HOFM German Derivation Grammar

 (c) 2012
 Britta Zeller <zeller@cl.uni-heidelberg.de>
 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

module MorphGrammar.Hofm.Language.German.Derivation where

import MorphGrammar.Hofm.Transf
import MorphGrammar.Hofm.IPattern
import MorphGrammar.Hofm.DPattern
import MorphGrammar.Hofm.Language.German.Transf
import MorphGrammar.Hofm.Language.German.Inflection

-------------------------------------------------------------------------------
-- Derivational patterns
-------------------------------------------------------------------------------

dPattern = DPatternSL -- default derivation is from a stem to lemma

dPatterns = [
  dVN01,dVN02,dVN03,
  dVA01]

-------------------------------------------------------------------------------
-- 1. NOMENABLEITUNG
-------------------------------------------------------------------------------

-- 1.1 NOMEN ZU NOMEN

-- 1.2 ADJEKTIV ZU NOMEN

-- 1.3 VERB ZU NOMEN

-- singen -> gesang
dVN01 = dPattern "dVN01" 
  (pfx "ge" & rifx "i" "a") verbs mNouns
-- reden -> gerede
dVN02 = dPattern "dVN02"
  (pfx "ge" & sfx "e") verbs nNouns
-- tanzen -> tänzer
dVN03 = dPattern "dVN03"
  (sfx "er" & uml) verbs nNouns

-------------------------------------------------------------------------------
-- 2. ADJEKTIVABLEITUNG
-------------------------------------------------------------------------------

-- 2.1 NOMEN ZU ADJEKTIV

-- 2.2 ADJEKTIV ZU ADJEKTIV

-- 2.3 VERB ZU ADJEKTIV

-- sagen -> unsangbar
dVA01 = dPattern "dVA01"
  (pfx "un" & sfx "bar") verbs adjectives

-------------------------------------------------------------------------------
-- 3. VERBABLEITUNG
-------------------------------------------------------------------------------

-- 3.1 NOMEN ZU VERB

-- 3.2 ADJEKTIV ZU VERB

-- 3.3 VERB ZU VERB

