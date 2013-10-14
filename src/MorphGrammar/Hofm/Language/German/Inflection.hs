{-------------------------------------------------------------------------------
 MorphGrammar.Hofm.Language.German.Inflection
 HOFM German word formation rules

 (c) 2012 
 Britta Zeller <zeller@cl.uni-heidelberg.de>
 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

module MorphGrammar.Hofm.Language.German.Inflection where

import MorphGrammar.Hofm.Transf
import MorphGrammar.Hofm.IPattern
import MorphGrammar.Hofm.Language.German.Transf

--------------------------------------------------------------------------------
-- Inflectional patterns
--------------------------------------------------------------------------------

iPatterns = nouns ++ verbs ++ adjectives

-------------------------------------------------------------------------------
-- Inflectional classes
-------------------------------------------------------------------------------

nouns = mNouns ++ nNouns ++ fNouns

mNouns = [mNoun]
fNouns = [fNoun]
nNouns = [nNoun]

verbs = [verb]

adjectives = [adjective]

-------------------------------------------------------------------------------
-- Inflectional patterns
-------------------------------------------------------------------------------

dummyPattern tag = iPattern tag tag always [(nul,[""])]

[mNoun,fNoun,nNoun] = map dummyPattern ["Nm","Nf","Nn"]
adjective = dummyPattern "A"

--verb = dummyPattern "V"
verb = iPattern "V" "V" always [(sfx "en",["VINF"])]

