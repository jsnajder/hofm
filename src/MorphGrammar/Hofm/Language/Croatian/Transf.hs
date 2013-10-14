{-------------------------------------------------------------------------------
 MorphGrammar.Hofm.Grammar.Croatian.Transf
 HOFM Croatian Inflectional Grammar

 Jan Snajder <jan.snajder@fer.hr>

 (c) 2008 TakeLab
 University of Zagreb
 Faculty of Electrical Engineering and Computing
-------------------------------------------------------------------------------}

module MorphGrammar.Hofm.Language.Croatian.Transf where

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

caps = map (:[]) $ ['A'..'Z'] ++ "ĐŽŠČĆ"

-- consonant groups
cgr = [c1++c2 | c1 <- cons, c2 <- cons, c1++c2 `notElem` ["nj","lj","dž"]]

--------------------------------------------------------------------------------
-- Language-specific transformations
------------------------------------------------------------------------------

-- MORFOLOŠKI UVJETOVANE ALTERNACIJE

-- sibilarizacija
sbl = asfx [("k","c"), ("h","s"), ("g","z")] 

-- palatalizacija
plt = asfx [("k","č"),("g","ž"),("h","š"),("c", "č"),("z","ž")]

-- jotacija
jot = asfx [
  ("k","č"),("g","ž"),("h","š"),("c","č"),("z","ž"),("s","š"),
  ("t","ć"),("d","đ"),("l","lj"),("n","nj"),("p","plj"),
  ("b","blj"),("m","mlj"),("v","vlj"),("f","flj")]

-- zamjena suglasničkih skupova
acg = asfx [
  ("ht","šć"),("sk","šč"),("sk","šć"),("sl","šlj"),("sn","šnj"),
  ("st","šć"),("st","št"),("zd","žđ"),("zn","žnj")]

-- proširenje osnove samoglasnikom a/e
ex v cs = asfx [(c,v++c) | c <- cs]
exa = ex "a" ["nj","lj","dž"] .||. ex "a" cons 
exe = ex "e" ["nj","lj","dž"] .||. ex "e" cons 

-- FONOLOŠKI UVJETOVANE ALTERNACIJE

-- ozvučivanje
ozv = [
  ("p","b",["dž"]),
  ("t","d",["b"]),
  ("s","z",["b","d","g"]),
  ("s","ž",["b"]),
  ("č","dž",["b"]),
  ("k","g",["dž"]),
  ("s","ž",["dž"]),
  ("z","ž",["dž"])]

-- obezvučivanje
obzv = [
  ("b","p",["c","č","ć","f","h","k","s","š","t"]),
  ("d","t",["f","h","k","p"]),
  ("g","k",["c","č"]),
  ("đ","ć",["k"]),
  ("z","s",["c","f","h","k","p","t"]),
  ("ž","š",["c","k"])]

-- jednačenje suglasnika po mjestu tvorbe
jmt = [  
  ("s","š",cs),
  ("z","š",cs),
  ("z","ž",cs),
  ("h","š",cs),
  ("n","m",["p","b"])]
  where cs = ["č","ć","dž","đ","lj","nj"]

-- ispadanje suglasnika
isg = [
  ("t","",cs),("d","",cs)] 
  ++ concat [[(a++"t",a,[b]),(a++"d",a,[b])] | 
     a <- ["s","z","š","ž"], b <- cons \\ ["r","v"]]
  where cs = ["c","č","ština"]

pca = [(x++c,y++c) | (x,y,cs) <- alts, c <- cs]
  where alts = obzv ++ isg
--  where alts = obzv ++ ozv ++ jmt ++ isg

pca1 = asfx pca
--pca1 = nul
pca2 = aifx pca

--sfx s = Hofm.Transf.sfx s & try pca1

--sfx2 s = Hofm.Transf.sfx s & opt pca1

jat1 = rifx "ije" "je"
jat2 = rifx "ije" "e"
jat3 = rifx "ije" "i"

