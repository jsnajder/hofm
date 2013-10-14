{-------------------------------------------------------------------------------
 MorphGrammar.Hofm.Language.German.Transf
 HOFM German word formation rules

 (c) 2012 
 Britta Zeller <zeller@cl.uni-heidelberg.de>
 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

module MorphGrammar.Hofm.Language.German.Transf where

import MorphGrammar.Hofm.Transf
import Data.List
import Data.Char

--------------------------------------------------------------------------------
-- Graphemes
--------------------------------------------------------------------------------

trueCons = [
  "p","t","k","b","d","g","c","č","ć","dž","đ",
  "f","s","š","h","z","ž"]
sonants = [
  "r","v","m","n","nj","l","lj"]
cons = 
  trueCons ++ sonants ++ ["j"]
pals = [
  "lj","nj","ć","đ","č","dž","š","ž","j"]
nonpals = [
  "v","r","l","m","n","p","b","f","t","d","s",
  "z","c","k","g","h"]
velars = [
  "k","g","h"]

-------------------------------------------------------------------------------
-- Language-specific transformations
-------------------------------------------------------------------------------

-- UMLAUT

uml = aifx [("a","ä"), ("o","ö"), ("u","ü")] 

