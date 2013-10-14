{-------------------------------------------------------------------------------
 MorphGrammar.Hofm
 Higher-order functions morphology grammar

 Jan Snajder <jan.snajder@fer.hr>

 (c) 2009 TakeLab
 Dept. of Electronics, Microelectronics, Computer and Intelligent Systems
 Faculty of Electrical Engineering and Computing
 University of Zagreb
-------------------------------------------------------------------------------}

module MorphGrammar.Hofm (
  IPatternDefault,
  DPatternDefault,
  TransfDefault,
  CondDefault,
  module MorphGrammar,
  module MorphGrammar.Hofm.Transf,
  module MorphGrammar.Hofm.Transf.TTransf,
  module MorphGrammar.Hofm.Transf.FTransf,
  module MorphGrammar.Hofm.IPattern,
  module MorphGrammar.Hofm.DPattern) where

import MorphGrammar
import MorphGrammar.Hofm.Transf
import MorphGrammar.Hofm.Transf.TTransf
import MorphGrammar.Hofm.Transf.FTransf
import MorphGrammar.Hofm.IPattern
import MorphGrammar.Hofm.DPattern

type TransfDefault   = TTransf
type CondDefault     = FCond
type IPatternDefault = IPattern TransfDefault CondDefault String
type DPatternDefault = DPattern TransfDefault IPatternDefault

