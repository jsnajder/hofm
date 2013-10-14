{-------------------------------------------------------------------------------
 MorphGrammar.Hofm.Language.German
 Higher-order functions morphology for German language (DE)
 v1.0

 Britta Zeller <zeller@cl.uni-heidelberg.de>
 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

module MorphGrammar.Hofm.Language.German (
  grammarId,
  module MorphGrammar.Hofm,
  module MorphGrammar.Hofm.Language.German.Transf,
  module MorphGrammar.Hofm.Language.German.Inflection,
  module MorphGrammar.Hofm.Language.German.Derivation) where

import MorphGrammar.Hofm
import MorphGrammar.Hofm.Language.German.Transf
import MorphGrammar.Hofm.Language.German.Inflection
import MorphGrammar.Hofm.Language.German.Derivation

grammarId = "German HOFM grammar rules v1.1"


