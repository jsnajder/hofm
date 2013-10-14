{-------------------------------------------------------------------------------
 MorphGrammar.Hofm.Language.Croatian
 Higher-order functions morphology application to Croatian language (Hr)
 v1.0

 Jan Snajder <jan.snajder@fer.hr>

 (c) 2008 TakeLab
 University of Zagreb
 Faculty of Electrical Engineering and Computing
-------------------------------------------------------------------------------}

module MorphGrammar.Hofm.Language.Croatian (
  module MorphGrammar.Hofm.Language.Croatian.Transf,
  module MorphGrammar.Hofm.Language.Croatian.Inflection,
  module MorphGrammar.Hofm.Language.Croatian.Derivation) where

import MorphGrammar.Hofm.Language.Croatian.Transf
import MorphGrammar.Hofm.Language.Croatian.Inflection
import MorphGrammar.Hofm.Language.Croatian.Derivation

grammarId = "Croatian HOFM grammar rules 1.3, (c) 2010 TakeLab"

