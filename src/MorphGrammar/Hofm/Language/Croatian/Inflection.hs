{-------------------------------------------------------------------------------
 MorphGrammar.Hofm.Language.Croatian.Inflection
 HOFM Croatian Inflectional Grammar

 Jan Snajder <jan.snajder@fer.hr>

 (c) 2008 TakeLab
 University of Zagreb
 Faculty of Electrical Engineering and Computing
-------------------------------------------------------------------------------}

module MorphGrammar.Hofm.Language.Croatian.Inflection where

import MorphGrammar.Hofm.Transf
import MorphGrammar.Hofm.IPattern
import MorphGrammar.Hofm.Language.Croatian.Transf

--------------------------------------------------------------------------------
-- Inflectional patterns
--------------------------------------------------------------------------------

{-- 
 Use '&' to compose morphological transformations.
 Write MSDs after a morphological transformation preceeded by a '#'.
 You can build transformation alternatives using '++' or 
 simply list each alternative as a separate entry but repeat the MSD.
 Write optional transformations as 'nul++t' (do this if you
 want the rule to be applicable even if transformation t fails).

 The morphosyntactic descriptors used here are in line with the
 MULTEXT-East Version 3 specifications standard. For details consult:

 Tomaz Erjavec: MULTEXT-East Version 3: Multilingual Morphosyntactic
 Specifications, Lexicons and Corpora. In: Proc. of the Fourth
 Intl. Conf. on Language Resources and Evaluation, LREC'04, ELRA,
 Paris, 2004.
--}

iPatterns = nouns ++ verbs ++ adjectives

-------------------------------------------------------------------------------
-- Inflectional classes
-------------------------------------------------------------------------------

nouns = mNouns ++ nNouns ++ fNouns

verbs = tiVerbs ++ ciVerbs

adjectives = pAdjectives ++ qAdjectives

qAdjectives = qndAdjectives ++ qdAdjectives

mNouns = [
  n01,n02,n03,n04,n05,n06,n07,n08,n09,n10,
  n11,n12,n13,n14,n15,n16,n17,n18,n19,
  n30,n37,n38,n41,n43,n44,n45,n46,n48]

nNouns = [
  n20,n21,n22,n23,n24,n25,n26,n27,n40]

fNouns = [
  n28,n47,n29,n31,n32,n33,n34,n35,n36,n39,n49]

tiVerbs = [
  v01,v02,v03,v04,v05,v06,v07,
  v11,v12,v13,v14,v15,v16,v17,v18,v19,v20,
  v21,v22,v23,v24,v25,v26,v27,v28,v32]

ciVerbs = [
  v08,v09,v10,v29,v30,v31]

pAdjectives = [
  a08,a09,a10]

qndAdjectives = [
  a01,a03,a04,a06,a07,
  a11,a14,a15]

qdAdjectives = [
  a12,a13]

-------------------------------------------------------------------------------
-- Inflectional rules
-------------------------------------------------------------------------------

-- IMENIČKI UZORCI

-- vrsta a, ništični nastavak:

-- uzorak 1 (252)
n01 = iPattern "N01" "N-m"   -- izvor, političar
  (ends (nonpals ++ ["j","š","č","ž"]))
  [nul        # ["sn","sa"],
   sfx "a"    # ["sg","pg"], 
   sfx "u"    # ["sd","sl"], 
   sfx "e"    # ["sv","pa"], 
   sfx "om"	  # ["si"], 
   sfx "i"    # ["pn","pv"], 
   sfx "ima"  # ["pd","pl","pi"]]

-- uzorak 2 (253)
n02 = iPattern "N02" "N-m"   -- nokat
  (ends cgr `land` nends ["l"])
  [exa            # ["sn","sa"], 
   sfx "a" & t    # ["sg"], 
   sfx "u" & t    # ["sd","sl"],
   sfx "e" & t    # ["sv","pa"],
   sfx "om" & t   # ["si"],
   sfx "i" & t    # ["pn","pv"],
   sfx "ima" & t  # ["pd","pl","pi"],
   sfx "a" & exa  # ["pg"]]  -- TODO: +nokti +noktiju
  where t = try pca1

-- uzorak 3 (257)
n03 = iPattern "N03" "N-m"   -- otocanin, bugarin
  always
  [sfx "in"    # ["sn"], 
   sfx "ina"   # ["sg","sa"], 
   sfx "inu"   # ["sd","sl"], 
   sfx "ine"   # ["sv"], 
   sfx "inom"  # ["si"], 
   sfx "i"     # ["pn","pv"], 
   sfx "a"     # ["pg"], 
   sfx "ima"   # ["pd","pl","pi"], 
   sfx "e"     # ["pa"]]

-- uzorak 4 (260)
n04 = iPattern "N04" "N-m"   -- vojnik, bubreg, trbuh, kirurg
  (ends velars `land` (neg(ends cgr) `lor` ends ["g"]))
  [nul              # ["sn"],
   sfx "a"          # ["sg","sa","pg"], 
   sfx "u"          # ["sd","sl"],
   sfx "e" & plt    # ["sv"],
   sfx "om"         # ["si"],
   sfx "i" & sbl    # ["pn","pv"],
   sfx "ima" & sbl  # ["pd","pl","pi"],
   sfx "e"          # ["pa"]]

-- uzorak 5 (261)
n05 = iPattern "N05" "N-m"   -- čvorak 
  (ends velars `lor` ends cgr)
  [exa                  # ["sn"],
   sfx "a" & t          # ["sg","sa"],
   sfx "u" & t          # ["sd","sl"],
   sfx "e" & plt & t    # ["sv"],
   sfx "om" & t         # ["si"],
   sfx "i" & sbl & t    # ["pn"],
   sfx "a" & exa        # ["pg"],
   sfx "ima" & sbl & t  # ["pd","pl","pi"],
   sfx "e" & t          # ["pa"]]
  where t = try pca1

-- uzorak 5B  ... TODO
n06 = iPattern "N06" "N-m"   -- napredak, odlazak, kralježak, zadatak
  (ends nonpals)
  [exa                        # ["sn","sa"], 
   sfx "a" & t1               # ["sg"], 
   sfx "u" & t1               # ["sd","sl","sv"],
   sfx "om" & t1              # ["si"],
   sfx "e" & t2 & plt & t1    # ["sv"],
   sfx "i" & t2 & sbl & t1    # ["pn","pv"],
   sfx "a" & exa              # ["pg"],
   sfx "e" & t1               # ["pa"],
   sfx "ima" & t2 & sbl & t1  # ["pd","pl","pi"]]
  where t1 = try pca1
        t2 = opt $ asfx [("tc","c"),("tč","č"),("dc","c"),("dč","č")]

-- uzorak 6 (262)
n07 = iPattern "N07" "N-m"   -- panj, (sprint, kurs), marš, grč, prišt
--      (nends cgr `lor` ends ["nt","ks","rs","rš","rč"])
--      sprint i kurs nisu tu!  ne *sprintevi nego sprintovi
  (ends pals `lor` ends ["št","žđ"])
  [nul            # ["sn","sa"],
   sfx "a"        # ["sg"],
   sfx "u"        # ["sd","sv","sl"], 
   sfx "em"       # ["si"],
   i & sfx "i"    # ["pn","pv"],
   i & sfx "a"    # ["pg"],
   i & sfx "ima"  # ["pd","pl","pi"],
   i & sfx "e"    # ["pa"]]
   where i = sfx "ev"

-- uzorak 6B (262)
n08 = iPattern "N08" "N-m"   -- ražanj, češalj
  ((ends pals `lor` ends ["št","žđ"]) `land` ends cgr)
  [exa              # ["sn","sv"],
   sfx "a" & t      # ["sg"],
   sfx "u" & t      # ["sd","sv","sl"],
   sfx "em" & t     # ["si"],
   sfx "i" & t      # ["pn","pv"],
   sfx "evi" & t    # ["pn","pv"],
   sfx "a" & exa    # ["pg"],
   sfx "ima" & t    # ["pd","pl","pi"],
   sfx "evima" & t  # ["pd","pl","pi"],
   sfx "e" & t      # ["pa"],
   sfx "eve" & t    # ["pa"]]
  where t = try pca1

-- uzorak 7 (264)
n09 = iPattern "N09" "N-m"   -- stric
  (ends ["c"] `land` neg(ends cgr))
  [nul                  # ["sn"],
   sfx "a"              # ["sg","sa"],
   sfx "u"              # ["sd","sl"],
   sfx "e" & plt        # ["sv"],
   sfx "em"             # ["si"],
   sfx "i" & i & plt    # ["pn","pv"],
   sfx "a" & i & plt    # ["pg"],
   sfx "ima" & i & plt  # ["pd","pl","pi"],
   sfx "e" & i & plt    # ["pa"]]
   where i = sfx "ev"

-- uzorak 7B (264)
n10 = iPattern "N10" "N-m"   -- klinac, obrazac
  (ends ["c"])
  [exa                # ["sn","sa"],
   sfx "a" & t        # ["sg"],
   sfx "u" & t        # ["sd","sl"],
   sfx "e" & plt & t  # ["sv"],
   sfx "em" & t       # ["si"],
   sfx "a" & exa      # ["pg"],
   sfx "ima" & t      # ["pd","pl","pi"],
   sfx "i" & t        # ["pn","pv"],
   sfx "e" & t        # ["pa"]]
  where t = try pca1

-- uzorak 8 (268)
n11 = iPattern "N11" "N-m"   -- posjetilac (A/197)
  (ends ["c"])
  [rsfx "oc" "lac"   # ["sn"],
   sfx "a"           # ["sg","sa"],
   sfx "u"           # ["sd","sl"],
   sfx "e" & plt     # ["sv"],
   sfx "em"          # ["si"],
   sfx "i"           # ["pn","pv"],
   rsfx "oc" "laca"  # ["pg"],
   sfx "ima"         # ["pd","pl","pi"],
   sfx "e"           # ["pa"]]

-- uzorak 9 (269)
n12 = iPattern "N12" "N-m"   -- pepeo (A/198)
  (ends ["el"])
  [rsfx "l" "o"  # ["sn","sa"],
   sfx "a"       # ["sg","pg"],
   sfx "u"       # ["sd","sl"],
   sfx "e"       # ["sv","pa"],
   sfx "om"      # ["si"],
   sfx "i"       # ["pn","pv"],
   sfx "ima"     # ["pd","pl","pi"]]

-- uzorak 9B (269)
n13 = iPattern "N13" "N-m"   -- ugao (A/198b)
  (ends ["l"] `land` ends cgr)
  [rsfx "l" "ao"  # ["sn","sa"],
   sfx "a"        # ["sg"],
   sfx "u"        # ["sd","sl"],
   sfx "e"        # ["sv"],
   sfx "om"       # ["si"],
   sfx "ovi"      # ["pn","pv"],
   sfx "ove"      # ["pa"],
   sfx "ovima"    # ["pd","pl","pi"],
   sfx "ova"      # ["pg"]]

-- uzorak 10 (271)
n14 = iPattern "N14" "Ncm"   -- čovjek 
  (ends ["čovjek"])
  [nul                      # ["sn"],
   sfx "a"                  # ["sg","sa"],
   sfx "u"                  # ["sd","sl"],
   sfx "e"                  # ["sv"],
   sfx "om"                 # ["si"],
   rsfx "čovjek" "ljudi"    # ["pn","pg","pv"],
   rsfx "čovjek" "ljude"    # ["pa"],
   rsfx "čovjek" "ljudima"  # ["pd","pl","pi"]]

-- uzorak 11 (272)
n15 = iPattern "N15" "N-m"   -- intervju 
  (ends ["u", "e", "o"])
  [nul        # ["sn","sa"],
   sfx "a"    # ["sg","pg"],
   sfx "u"    # ["sd","sv","sl"],
   sfx "om"   # ["si"],
   sfx "i"    # ["pn","pv"],
   sfx "ima"  # ["pd","pl","pi"],
   sfx "e"    # ["pa"]]

-- uzorak 12 (273)
n16 = iPattern "N16" "N-m"   -- žiri, Emmy (A/201b)
  (ends ["i","y"])
  [nul         # ["sn","sa"],
   sfx "ja"    # ["sg","pg"],
   sfx "ju"    # ["sd","sv","sl"],
   sfx "jem"   # ["si"],
   sfx "ji"    # ["pn","pv"],
   sfx "jima"  # ["pd","pl","pi"],
   sfx "je"    # ["pa"]]

-- nastavak -o ili -e:

-- uzorak 13 (vlastita imena)
n17 = iPattern "N17" "N-m"   -- Danilo
  --TMPOFF (ends nonpals `land` starts caps)
  (ends nonpals)
  [sfx "o"    # ["sn","sv"],
   sfx "a"    # ["sg","sa","pg"],
   sfx "u"    # ["sd","sl"],
   sfx "om"   # ["si"],
   sfx "i"    # ["pn","pv"],
   sfx "ima"  # ["pd","pl","pi"],
   sfx "e"    # ["pa"]]

-- uzorak 13B (274)
n18 = iPattern "N18" "N-m"   -- Hrvoje
  --TMPOFF (ends pals `land` starts caps)
  (ends pals)
  [sfx "e"    # ["sn","sv"],
   sfx "a"    # ["sg","sa","pg"],
   sfx "u"    # ["sd","sl"],
   sfx "em"   # ["si"],
   sfx "i"    # ["pd","pl","pi"],
   sfx "ima"  # ["pa"]]

-- uzorak 14 (276)
n19 = iPattern "N19" "N-m"   -- rašćupanko (A/203b)
  (ends ["k"])
  [sfx "o"          # ["sn","sv"],
   sfx "a"          # ["sg","sa","pg"],
   sfx "u"          # ["sd","sl"],
   sfx "om"         # ["si"],
   sfx "e"          # ["pa"],
   sfx "i" & sbl    # ["pn","pv"],
   sfx "ima" & sbl  # ["pd","pl","pi"]]

n30 = iPattern "N30" "N-m"   -- zeko (A/248)
  always
  [sfx "o"    # ["sn","sv"],
   sfx "e"    # ["sg","pn","pa","pv"],
   sfx "i"    # ["sd","sl"],
   sfx "u"    # ["sa"],
   sfx "om"   # ["si"],
   sfx "a"    # ["pg"],
   sfx "ama"  # ["pd","pl","pi"]]

n37 = iPattern "N37" "N-m"   -- prijatelj
  (ends cons)
  [nul        # ["sn","sv"],
   sfx "a"    # ["sg","sa","pg"],
   sfx "u"    # ["sd"],
   sfx "em"   # ["si"],
   sfx "i"    # ["pn","pv"],
   sfx "ima"  # ["pd","pl","pi"],
   sfx "e"    # ["pa"]]

n38 = iPattern "N38" "N-m"   -- artikl
  (ends cgr `land` neg(ends ["n"] `lor` ends ["nc"]))
  [nul            # ["sn","sa","sv"],
   sfx "a"        # ["sg"],
   sfx "u"        # ["sd"],
   sfx "e"        # ["pa"],
   sfx "om"       # ["si"],
   sfx "i"        # ["pn","pv"],
   sfx "ima"      # ["pd","pl","pi"],
   sfx "a" & exa  # ["pg"]]

n41 = iPattern "N41" "N-m"   -- bod, akt, grb, cvijet, show, Byrne
  --TMPOFF (ends (cons ++ ["y","x","w"]) `lor` starts caps)
  (ends (cons ++ ["y","x","w"]))
  [nul              # ["sn","sa"], 
   sfx "a"          # ["sg"], 
   sfx "u"          # ["sd","sl"],
   sfx "e"          # ["sv"],
   sfx "om"         # ["si"],
   sfx "ovi" & t    # ["pn","pv"],
   sfx "ova" & t    # ["pg"],
   sfx "ove" & t    # ["pa"],
   sfx "ovima" & t  # ["pd","pl","pi"]]
  where t = try jat1

n43 = iPattern "N43" "N-m"   -- dio
  always
  [rsfx "l" "o" & rifx "ije" "i" # ["sn","sa"], 
   sfx "a"      # ["sg"], 
   sfx "u"      # ["sd","sl"],
   sfx "om"     # ["si"],
   sfx "ovi"    # ["pn","pv"],
   sfx "ova"    # ["pg"],
   sfx "ove"    # ["pa"],
   sfx "ovima"  # ["pd","pl","pi"]]

n44 = iPattern "N44" "N-m"   -- kadar, zajam, pojam
  (ends cgr)
  [exa              # ["sn","sa"], 
   sfx "a" & t      # ["sg"], 
   sfx "u" & t      # ["sd","sl"],
   sfx "e" & t      # ["sv","pa"],
   sfx "om" & t     # ["si"],
   sfx "ovi" & t    # ["pn","pv"],
   sfx "ovima" & t  # ["pd","pl","pi"],
   sfx "ova" & t    # ["pg"],
   sfx "a" & exa    # ["pg"]]
  where t = try pca1

n45 = iPattern "N45" "N-m"   -- kolega, mladoženja
  (ends cons)
  [sfx "a"    # ["sn","pg"], 
   sfx "e"    # ["sg","pn","pv","pa"], 
   sfx "i"    # ["sd","sl"],
   sfx "u"    # ["sa"],
   sfx "o"    # ["sv"],
   sfx "om"   # ["si"],
   sfx "ama"  # ["pd","pl","pi"]]

n46 = iPattern "N46" "N-m"   -- mozak
  (ends ["zg"])
  [rsfx "g" "ak"  # ["sn","sa"], 
   sfx "a"        # ["sg"], 
   sfx "u"        # ["sd","sl"],
   sfx "om"       # ["si"],
   sfx "ovi"      # ["pn","pv"],
   sfx "ova"      # ["pg"],
   sfx "ove"      # ["pa"],
   sfx "ovima"    # ["pd","pl","pi"]]

n48 = iPattern "N48" "N-m"   -- žabac, otac, sudac
  (ends ["c"])
  [exa               # ["sn"],
   sfx "a" & t       # ["sg","sa"],
   sfx "u" & t       # ["sd","sl"],
   sfx "e" & plt & t # ["sv"],
   sfx "em" & t      # ["si"],
   sfx "a" & exa     # ["pg"],
   sfx "ima" & t     # ["pd","pl","pi"],
   sfx "i" & t       # ["pn","pv"],
   sfx "e" & t       # ["pa"]]
  where t = try pca1

-- srednji rod:

n20 = iPattern "N20" "N-n"   -- koljeno (A/218)
  (ends nonpals `land` neg(ends ["c","št"]))
  [sfx "o"    # ["sn","sa","sv"],
   sfx "a"    # ["sg","pn","pg","pa","pv"],
   sfx "u"    # ["sd","sl"],
   sfx "om"   # ["si"],
   sfx "ima"  # ["pd","pl","pi"]]

n21 = iPattern "N21" "N-n"   -- jedro (A/219)
  (ends cgr `land` neg(ends ["st","zd"]))
  [sfx "o"        # ["sn","sa","sv"],
   sfx "a"        # ["sg","pn","pa","pv"],
   sfx "u"        # ["sd","sl"],
   sfx "om"       # ["si"],
   sfx "a" & exa  # ["pg"],
   sfx "ima"      # ["pd","pl","pi"]]

n22 = iPattern "N22" "N-n"   -- polje (A/225)
  (ends ["j","lj","nj","ć","đ","c","št","šć","žđ","mor","tl"])
  [sfx "e"    # ["sn","sa","sv"],
   sfx "a"    # ["sg","pn","pg","pa","pv"],
   sfx "u"    # ["sd","sl"],
   sfx "em"   # ["si"],
   sfx "ima"  # ["pd","pl","pi"]]

n23 = iPattern "N23" "N-n"  -- sunce (A/226)
  (ends cgr `land` neg(ends ["št","šć","žđ"] `lor` ends ["j"]))
  [sfx "e"        # ["sn","sa","sv"],
   sfx "a"        # ["sg","pn","pa","pv"],
   sfx "u"        # ["sd","sl"],
   sfx "em"       # ["si"],
   sfx "a" & exa  # ["pg"],
   sfx "ima"      # ["pd","pl","pi"]]

n24 = iPattern "N24" "N-n"  -- uže (A/228)
  (ends ["pet","bet","met","tet","det","let","ret","net","set",
         "zet","šet","žet","čet","jajet"])
  [dsfx "t"   # ["sn","sa","sv"],
   sfx "a"    # ["sg","pn","pg","pa","pv"],
   sfx "u"    # ["sd","sl"],
   sfx "om"   # ["si"],
   sfx "ima"  # ["pd","pl","pi"]]

n25 = iPattern "N25" "N-n"  -- zvonce (A/230)
  (ends ["c","anc","ašc","ešc"])
  [sfx "e"        # ["sn","sa","sv"],
   sfx "a"        # ["sg","pn","pa","pv"],
   sfx "u"        # ["sd","sl"],
   sfx "em"       # ["si"],
   sfx "eta"      # ["sg"],
   sfx "etu"      # ["sd","sl"],
   sfx "etom"     # ["si"],
   sfx "ad"       # ["pn"],
   sfx "a" & exa  # ["pg"],
   sfx "ima"      # ["pd","pl","pi"]]

n26 = iPattern "N26" "N-n"  -- rame (A/231)
  (ends ["breme","ime","pleme","rame","sjeme","tjeme","vime","eme"])
  [nul         # ["sn","sa","sv"],
   sfx "na"    # ["sg","pn","pg","pa","pv"],
   sfx "nu"    # ["sd","sl"],
   sfx "nom"   # ["si"],
   sfx "nima"  # ["pd","pl","pi"]]

n27 = iPattern "N27" "N-n"  -- podne (A/232)
  (ends ["podnev"])
  [dsfx "v"   # ["sn","sa","sv"],
   sfx "a"    # ["sg","pn","pg","pa","pv"],
   sfx "u"    # ["sd","sl"],
   sfx "om"   # ["si"],
   sfx "ima"  # ["pd","pl","pi"]]

n40 = iPattern "N40" "N-n"  -- vrijeme  TODO: rifx!!! 
  (ends nonpals)
  [t0         # ["sn","sa","sv"],
   sfx "a"    # ["sg","pn","pg","pa","pv"],
   sfx "u"    # ["sd","sl"],
   sfx "om"   # ["si"],
   sfx "ima"  # ["pd","pl","pi"]]
  where t0 = inv (jat2 & sfx "n")

-- ženski rod:

n28 = iPattern "N28" "N-f"  -- žaba (A/241)
  (ends cons)
  [sfx "a"    # ["sn","pg"],
   sfx "e"    # ["sg","pn","pa","pv"],
   sfx "i"    # ["sd","sl"],
   sfx "u"    # ["sa"],
   sfx "o"    # ["sv"],
   sfx "om"   # ["si"],
   sfx "ama"  # ["pd","pl","pi"]]

n47 = iPattern "N47" "N-f"  -- crkva
  (ends cons)
  [sfx "a"        # ["sn","pg"],
   sfx "e"        # ["sg","pn","pa","pv"],
   sfx "i"        # ["sd","sl"],
   sfx "u"        # ["sa"],
   sfx "o"        # ["sv"],
   sfx "om"       # ["si"],
   sfx "ama"      # ["pd","pl","pi"],
   (sfx "a" & exa) .|. sfx "i" .|. sfx "a" # ["pg"]]

n29 = iPattern "N29" "N-f"  -- slika (A/242)
  (ends velars)
  [sfx "a"        # ["sn","pg"],
   sfx "e"        # ["sg","pn","pa","pv"],
   sfx "i" & sbl  # ["sd","sl"],
   sfx "i"        # ["sd","sl"],
   sfx "u"        # ["sa"],
   sfx "o"        # ["sv"],
   sfx "om"       # ["si"],
   sfx "ama"      # ["pd","pl","pi"]]

n31 = iPattern "N31" "N-f"  -- mati (A/248)
  (ends ["mater"])
  [rsfx "er" "i"  # ["sn","sv"],
   sfx "e"        # ["sg","pn","pa","pv"],
   sfx "i"        # ["sd","sl"],
   nul            # ["sa"],
   sfx "om"       # ["si"],
   sfx "a"        # ["pg"],
   sfx "ama"      # ["pd","pl","pi"]]

n32 = iPattern "N32" "N-f"  -- riječ (A/251)
  (ends ["č","ž","š","s","z","r","n"])
  [nul        # ["sn","sa"],
   sfx "i"    # ["sg","sd","sv","sl","si","pn","pg","pa","pv"],
   sfx "ima"  # ["pd","pl","pi"],
   sfx "ju"   # ["si"]]

n33 = iPattern "N33" "N-f"  -- kap, krv, svijest (A/252)
  (ends ["p","b","v","m","t","d", "n"])
  [nul            # ["sn","sa"],
   sfx "i"        # ["sg","sd","sv","sl","si","pn","pg","pa","pv"],
   sfx "ima"      # ["pd","pl","pi"],
   sfx "u" & t  # ["si"]]
  where t = acg .||. jot
--   sfx "u" & try pca1 & jot  # ["si"]]
-- ovdje ne treba acg (npr. mast -> mašću) jer se to ionako dogodi:
-- mast -> (jot) -> masć -> (jmt) -> mašć

n34 = iPattern "N34" "N-f"  -- misao (A/252b)
  (ends ["l"])
  [rsfx "l" "ao"  # ["sn","sa"],
   sfx "i"        # ["sg","sd","sv","sl","si","pn","pg","pa","pv"],
   sfx "ima"      # ["pd","pl","pi"],
   sfx "u" & jot  # ["si"]]

n35 = iPattern "N35" "N-f"  -- noć, obitelj (A/253)
  (ends ["ć","đ","lj"])
  [nul        # ["sn","sa"],
   sfx "i"    # ["sg","sd","sv","sl","si","pn","pg","pa","pv"],
   sfx "ima"  # ["pd","pl","pi"],
   sfx "u"    # ["si"]]

n36 = iPattern "N36" "N-f"  -- kći (A/253)
  (ends ["kćer"])
  [rsfx "er" "i" # ["sn"],
   sfx "i"       # ["sg","sd","sv","sl","si","pn","pg","pa","pv"],
   nul           # ["sa"],
   sfx "ju"      # ["si"],
   sfx "ima"     # ["pd","pl","pi"]]

n39 = iPattern "N39" "N-f"  -- banka
  (ends velars `land` ends cgr)
  [sfx "a"        # ["sn","pg"],
   sfx "e"        # ["sg","pn","pa","pv"],
   sfx "i" & sbl  # ["sd","sl"],
   sfx "i"        # ["sd","sl"],
   sfx "u"        # ["sa"],
   sfx "o"        # ["sv"],
   sfx "om"       # ["si"],
   sfx "ama"      # ["pd","pl","pi"],
   sfx "a" & exa  # ["pg"]]

n49 = iPattern "N49" "N-f"  -- novine, hlače (pl. tantum)
  (ends cons)
  [sfx "e"    # ["pn","pa","pv"],
   sfx "a"    # ["pg"],
   sfx "ama"  # ["pd","pl","pi"]]

-- GLAGOLSKI UZORCI

-- pravilni glagoli:

v01 = iPattern "V01" "Vm"  -- plesti (v1r1a)
  (ends ["t"])
  [rsfx "t" "sti"  # ["n"],
   rsfx "t" "st"   # ["n"],
   sfx "em"        # ["ip1s"],
   sfx "eš"        # ["ip2s"],
   sfx "e"         # ["ip3s"],
   sfx "emo"       # ["ip1p"],
   sfx "ete"       # ["ip2p"],
   sfx "u"         # ["ip3p"],
   sfx "i"         # ["m-2s"],
   sfx "imo"       # ["m-1p"],
   sfx "ite"       # ["m-2p"],
   sfx "ući"       # ["gp"],
   sfx "avši"      # ["gs"],
   rsfx "t" "o"    # ["pp-s"],
   rsfx "t" "la"   # ["pp-s"],
   rsfx "t" "lo"   # ["pp-s"],
   rsfx "t" "li"   # ["pp-p"],
   rsfx "t" "le"   # ["pp-p"]]

v02 = iPattern "V02" "Vm"  -- krasti (v1r1b)
  (ends ["d"])
  [rsfx "d" "sti"  # ["n"],
   rsfx "d" "st"   # ["n"],
   sfx "em"        # ["ip1s"],
   sfx "eš"        # ["ip2s"],
   sfx "e"         # ["ip3s"],
   sfx "emo"       # ["ip1p"],
   sfx "ete"       # ["ip2p"],
   sfx "u"         # ["ip3p"],
   sfx "i"         # ["m-2s"],
   sfx "imo"       # ["m-1p"],
   sfx "ite"       # ["m-2p"],
   sfx "ući"       # ["gp"],
   sfx "avši"      # ["gs"],
   rsfx "d" "o"    # ["pp-s"],
   rsfx "d" "la"   # ["pp-s"], 
   rsfx "d" "lo"   # ["pp-s"],
   rsfx "d" "li"   # ["pp-p"],
   rsfx "d" "le"   # ["pp-p"]]

v03 = iPattern "V03" "Vm"  -- tresti (v1r2a)
  (ends ["s"] `land` nends["nes"])
  [sfx "ti"        # ["n"],
   sfx "t"         # ["n"],
   sfx "em"        # ["ip1s"],
   sfx "eš"        # ["ip2s"],
   sfx "e"         # ["ip3s"],
   sfx "emo"       # ["ip1p"],
   sfx "ete"       # ["ip2p"],
   sfx "u"         # ["ip3p"],
   sfx "i"         # ["m-2s"],
   sfx "imo"       # ["m-1p"],
   sfx "ite"       # ["m-2p"],
   sfx "ući"       # ["gp"],
   sfx "avši"      # ["gs"],
   sfx "ao"        # ["pp-s"],
   sfx "la"        # ["pp-s"],
   sfx "lo"        # ["pp-s"],
   sfx "li"        # ["pp-p"],
   sfx "le"        # ["pp-p"]]

v04 = iPattern "V04" "Vm"  -- donesti/donijeti (v1r2b)
  (ends ["nes"])
  [sfx "ti"        # ["n"],
   rsfx "es" "ijeti" # ["n"],
   sfx "t"         # ["n"],
   rsfx "es" "ijet" # ["n"],
   sfx "em"        # ["ip1s"],
   sfx "eš"        # ["ip2s"],
   sfx "e"         # ["ip3s"],
   sfx "emo"       # ["ip1p"],
   sfx "ete"       # ["ip2p"],
   sfx "u"         # ["ip3p"],
   sfx "i"         # ["m-2s"],
   sfx "imo"       # ["m-1p"],
   sfx "ite"       # ["m-2p"],
   sfx "ući"       # ["gp"],
   sfx "avši"      # ["gs"],
   sfx "ao"        # ["pp-s"],
   sfx "la"        # ["pp-s"],
   rsfx "es" "io"  # ["pp-s"],
   rsfx "es" "ijela" # ["pp-s"],
   rsfx "es" "ijelo" # ["pp-s"],
   rsfx "es" "ijele" # ["pp-p"],
   rsfx "es" "ijeli" # ["pp-p"]]

v05 = iPattern "V05" "Vm"  -- gristi (v1r2c)
  (ends ["z"])
  [rsfx "z" "sti"  # ["n"],
   rsfx "z" "st"   # ["n"],
   sfx "em"        # ["ip1s"],
   sfx "eš"        # ["ip2s"],
   sfx "e"         # ["ip3s"],
   sfx "emo"       # ["ip1p"],
   sfx "ete"       # ["ip2p"],
   sfx "u"         # ["ip3p"],
   sfx "i"         # ["m-2s"],
   sfx "imo"       # ["m-1p"],
   sfx "ite"       # ["m-2p"],
   sfx "ući"       # ["gp"],
   sfx "avši"      # ["gs"],
   sfx "ao"        # ["pp-s"],
   sfx "la"        # ["pp-s"],
   sfx "lo"        # ["pp-s"],
   sfx "li"        # ["pp-p"],
   sfx "le"        # ["pp-p"]]

v06 = iPattern "V06" "Vm"  -- crpsti (v1r3a)
  (ends ["p"])
  [sfx "sti"       # ["n"],
   sfx "st"        # ["n"],
   sfx "em"        # ["ip1s"],
   sfx "eš"        # ["ip2s"],
   sfx "e"         # ["ip3s"],
   sfx "emo"       # ["ip1p"],
   sfx "ete"       # ["ip2p"],
   sfx "u"         # ["ip3p"],
   sfx "i"         # ["m-2s"],
   sfx "imo"       # ["m-1p"],
   sfx "ite"       # ["m-2p"],
   sfx "ući"       # ["gp"],
   sfx "avši"      # ["gs"],
   sfx "ao"        # ["pp-s"],
   sfx "la"        # ["pp-s"],
   sfx "lo"        # ["pp-s"],
   sfx "li"        # ["pp-p"],
   sfx "le"        # ["pp-p"]]

v07 = iPattern "V07" "Vm"  -- grepsti (v1r3b)
  (ends ["b"])
  [rsfx "b" "psti" # ["n"],
   rsfx "b" "pst"  # ["n"],
   sfx "em"        # ["ip1s"],
   sfx "eš"        # ["ip2s"],
   sfx "e"         # ["ip3s"],
   sfx "emo"       # ["ip1p"],
   sfx "ete"       # ["ip2p"],
   sfx "u"         # ["ip3p"],
   sfx "i"         # ["m-2s"],
   sfx "imo"       # ["m-1p"],
   sfx "ite"       # ["m-2p"],
   sfx "ući"       # ["gp"],
   sfx "avši"      # ["gs"],
   sfx "ao"        # ["pp-s"],
   sfx "la"        # ["pp-s"],
   sfx "lo"        # ["pp-s"],
   sfx "li"        # ["pp-p"],
   sfx "le"        # ["pp-p"]]

v08 = iPattern "V08" "Vm"  -- vući (v1r4a)
  (ends ["k"])
  [rsfx "k" "ći"    # ["n"],
   rsfx "k" "ć"     # ["n"],
   sfx "em" & plt   # ["ip1s"],
   sfx "eš" & plt   # ["ip2s"],
   sfx "e" & plt    # ["ip3s"],
   sfx "emo" & plt  # ["ip1p"],
   sfx "ete" & plt  # ["ip2p"],
   sfx "u"          # ["ip3p"],
   sfx "i" & sbl    # ["m-2s"],
   sfx "imo" & sbl  # ["m-1p"],
   sfx "ite" & sbl  # ["m-2p"],
   sfx "ući"        # ["gp"],
   sfx "avši"       # ["gs"],
   sfx "ao"         # ["pp-s"],
   sfx "la"         # ["pp-s"],
   sfx "lo"         # ["pp-s"],
   sfx "li"         # ["pp-p"],
   sfx "le"         # ["pp-p"]]

v09 = iPattern "V09" "Vm"  -- strići->strižem (v1r4b)
  (ends ["g"])
  [rsfx "g" "ći"     # ["n"],
   rsfx "g" "ć"      # ["n"],
   sfx "em" & plt  # ["ip1s"],
   sfx "eš" & plt  # ["ip2s"],
   sfx "e" & plt   # ["ip3s"],
   sfx "emo" & plt # ["ip1p"],
   sfx "ete" & plt # ["ip2p"],
   sfx "u"           # ["ip3p"],
   sfx "i" & sbl   # ["m-2s"],
   sfx "imo" & sbl # ["m-1p"],
   sfx "ite" & sbl # ["m-2p"],
   sfx "ući"         # ["gp"],
   sfx "avši"        # ["gs"],
   sfx "ao"          # ["pp-s"],
   sfx "la"          # ["pp-s"],
   sfx "lo"          # ["pp-s"],
   sfx "li"          # ["pp-p"],
   sfx "le"          # ["pp-p"]]

v10 = iPattern "V10" "Vm"  -- vrći->vršem (v1r4c)
  (ends ["h"])
  [rsfx "h" "ći"     # ["n"],
   rsfx "h" "ć"      # ["n"],
   sfx "em" & plt  # ["ip1s"],
   sfx "eš" & plt  # ["ip2s"],
   sfx "e" & plt   # ["ip3s"],
   sfx "emo" & plt # ["ip1p"],
   sfx "ete" & plt # ["ip2p"],
   sfx "u"           # ["ip3p"],
   sfx "i" & sbl   # ["m-2s"],
   sfx "imo" & sbl # ["m-1p"],
   sfx "ite" & sbl # ["m-2p"],
   sfx "ući"         # ["gp"],
   sfx "avši"        # ["gs"],
   sfx "ao"          # ["pp-s"],
   sfx "la"          # ["pp-s"],
   sfx "lo"          # ["pp-s"],
   sfx "li"          # ["pp-p"],
   sfx "le"          # ["pp-p"]]

v11 = iPattern "V11" "Vm"  -- biti, obuti (v1r7a)
  (ends ["i", "u"])
  [sfx "ti"        # ["n"],
   sfx "t"         # ["n"],
   sfx "jem"       # ["ip1s"],
   sfx "ješ"       # ["ip2s"],
   sfx "je"        # ["ip3s"],
   sfx "jemo"      # ["ip1p"],
   sfx "jete"      # ["ip2p"],
   sfx "ju"        # ["ip3p"],
   sfx "j"         # ["m-2s"],
   sfx "jmo"       # ["m-1p"],
   sfx "jte"       # ["m-2p"],
   sfx "jući"      # ["gp"],
   sfx "vši"       # ["gs"],
   sfx "o"         # ["pp-s"],
   sfx "la"        # ["pp-s"],
   sfx "lo"        # ["pp-s"],
   sfx "li"        # ["pp-p"],
   sfx "le"        # ["pp-p"]]

v12 = iPattern "V12" "Vm"  -- smjeti, umjeti (v1r7b)
  (ends ["je"])
  [sfx "ti"        # ["n"],
   sfx "t"         # ["n"],
   sfx "em" & t    # ["ip1s"],
   sfx "eš" & t    # ["ip2s"],
   sfx "e" & t     # ["ip3s"],
   sfx "emo" & t   # ["ip1p"],
   sfx "ete" & t   # ["ip2p"],
   sfx "u" & t     # ["ip3p"],
   t               # ["m-2s"],
   sfx "mo" & t    # ["m-1p"],
   sfx "te" & t    # ["m-2p"],
   sfx "ući" & t   # ["gp"],
   rsfx "je" "ivši" # ["gs"],
   sfx "o"         # ["pp-s"],
   sfx "lo"        # ["pp-s"],
   sfx "la"        # ["pp-s"],
   sfx "li"        # ["pp-p"],
   sfx "le"        # ["pp-p"]]
  where t = rsfx "je" "ij"

v13 = iPattern "V13" "Vm"  -- venuti, tonuti (v2)
  (ends ["n"])
  [sfx "uti"       # ["n"],
   sfx "ut"        # ["n"],
   sfx "em"        # ["ip1s"],
   sfx "eš"        # ["ip2s"],
   sfx "e"         # ["ip3s"],
   sfx "emo"       # ["ip1p"],
   sfx "ete"       # ["ip2p"],
   sfx "u"         # ["ip3p"],
   sfx "i"         # ["m-2s"],
   sfx "imo"       # ["m-1p"],
   sfx "ite"       # ["m-2s"],
   sfx "ući"       # ["gp"],
   sfx "uvši"      # ["gs"],
   sfx "uo"        # ["pp-s"],
   sfx "ula"       # ["pp-s"],
   sfx "ulo"       # ["pp-s"],
   sfx "uli"       # ["pp-p"],
   sfx "ule"       # ["pp-p"]]

v14 = iPattern "V14" "Vm"  -- željeti, vidjeti, vrtjeti (v3r1) 
  (ends cons)
  [sfx "jeti"      # ["n"], 
   sfx "jet"       # ["n"],
   sfx "im"        # ["ip1s"],
   sfx "iš"        # ["ip2s"],
   sfx "i"         # ["ip3s","m-2s"],
   sfx "imo"       # ["ip1p","m-1p"],
   sfx "ite"       # ["ip2p","m-2p"],
   sfx "e"         # ["ip3p"],
   sfx "eći"       # ["gp"],
   sfx "jevši"     # ["gs"],
   sfx "io"        # ["pp-s"],
   sfx "jela"      # ["pp-s"],
   sfx "jelo"      # ["pp-s"],
   sfx "jeli"      # ["pp-p"],
   sfx "jele"      # ["pp-p"]]

v15 = iPattern "V15" "Vm"  -- čučati, držati, blejati (v3r2)
  (ends ["č","ž","j", "št", "žd"])
  [sfx "ati"       # ["n"],
   sfx "at"        # ["n"],
   sfx "im"        # ["ip1s"],
   sfx "iš"        # ["ip2s"],
   sfx "i"         # ["ip3s","m-2s"],
   sfx "imo"       # ["ip1p","m-1p"],
   sfx "ite"       # ["ip2p","m-2p"],
   sfx "e"         # ["ip3p"],
   sfx "eći"       # ["gp"],
   sfx "avši"      # ["gs"],
   sfx "ao"        # ["pp-s"],
   sfx "ala"       # ["pp-s"],
   sfx "alo"       # ["pp-s"],
   sfx "ali"       # ["pp-p"],
   sfx "ale"       # ["pp-p"]]

v16 = iPattern "V16" "Vm"  -- voziti (v4)
  always
  [sfx "iti"       # ["n"],
   sfx "it"        # ["n"],
   sfx "im"        # ["ip1s"],
   sfx "iš"        # ["ip2s"],
   sfx "i"         # ["ip3s","m-2s"],
   sfx "imo"       # ["ip1p","m-1p"],
   sfx "ite"       # ["ip2p","m-2p"],
   sfx "e"         # ["ip3p"],
   sfx "eći"       # ["gp"],
   sfx "ivši"      # ["gs"],
   sfx "io"        # ["pp-s"],
   sfx "ila"       # ["pp-s"],
   sfx "ilo"       # ["pp-s"],
   sfx "ili"       # ["pp-p"],
   sfx "ile"       # ["pp-p"]]

v17 = iPattern "V17" "Vm"  -- pitati (v5r1)
  always
  [sfx "ati"       # ["n"],
   sfx "at"        # ["n"],
   sfx "am"        # ["ip1s"],
   sfx "aš"        # ["ip2s"],
   sfx "a"         # ["ip3s"],
   sfx "amo"       # ["ip1p"],
   sfx "ate"       # ["ip2p"],
   sfx "aju"       # ["ip3p"],
   sfx "aj"        # ["m-2s"],
   sfx "ajmo"      # ["m-1p"],
   sfx "ajte"      # ["m-2p"],
   sfx "ajući"     # ["gp"],
   sfx "avši"      # ["gs"],
   sfx "ao"        # ["pp-s"],
   sfx "ala"       # ["pp-s"],
   sfx "alo"       # ["pp-s"],
   sfx "ali"       # ["pp-p"],
   sfx "ale"       # ["pp-p"]]

-- "vikati" bi ovdje isto trebalo ići???
v18 = iPattern "V18" "Vm"  -- glodati, lagati, micati, brisati, sipati, 
                        -- dozivati (v5r2a)
  always
  [sfx "ati"         # ["n"],
   sfx "at"          # ["n"],
   sfx "em" & jot  # ["ip1s"],
   sfx "eš" & jot  # ["ip2s"],
   sfx "e" & jot   # ["ip3s"],
   sfx "emo" & jot # ["ip1p"],
   sfx "ete" & jot # ["ip2p"],
   sfx "u" & jot   # ["ip3p"],
   sfx "i" & jot   # ["m-2s"],
   sfx "imo" & jot # ["m-1p"],
   sfx "ite" & jot # ["m-2p"],
   sfx "ući" & jot # ["gp"],
   sfx "avši"        # ["gs"],
   sfx "ao"          # ["pp-s"],
   sfx "ala"         # ["pp-s"],
   sfx "alo"         # ["pp-s"],
   sfx "ali"         # ["pp-p"],
   sfx "ale"         # ["pp-p"]]

v32 = iPattern "V32" "Vm"  -- zanijekati, skakati (v18 variant)
  always
  [sfx "ati"         # ["n"],
   sfx "at"          # ["n"],
   sfx "em" & jot  # ["ip1s"],
   sfx "eš" & jot  # ["ip2s"],
   sfx "e" & jot   # ["ip3s"],
   sfx "emo" & jot # ["ip1p"],
   sfx "ete" & jot # ["ip2p"],
   sfx "u" & jot   # ["ip3p"],
   sfx "i" & jot   # ["m-2s"],
   sfx "imo" & jot # ["m-1p"],
   sfx "ite" & jot # ["m-2p"],
   sfx "ući" & jot # ["gp"],
   sfx "avši"        # ["gs"],
   sfx "ao"          # ["pp-s"],
   sfx "ala"         # ["pp-s"],
   sfx "alo"         # ["pp-s"],
   sfx "ali"         # ["pp-p"],
   sfx "ale"         # ["pp-p"]]

v19 = iPattern "V19" "Vm"  -- dihati, vezati (v5r2b)
  always
  [sfx "ati"         # ["n"],
   sfx "at"          # ["n"],
   sfx "em" & jot  # ["ip1s"],
   sfx "eš" & jot  # ["ip2s"],
   sfx "e" & jot   # ["ip3s"],
   sfx "emo" & jot # ["ip1p"],
   sfx "ete" & jot # ["ip2p"],
   sfx "u" & jot   # ["ip3p"],
   sfx "i" & jot   # ["m-2s"],
   sfx "imo" & jot # ["m-1p"],
   sfx "ite" & jot # ["m-2p"],
   sfx "ući" & jot # ["gp"],
   sfx "avši"        # ["gs"],
   sfx "ao"          # ["pp-s"],
   sfx "ala"         # ["pp-s"],
   sfx "alo"         # ["pp-s"],
   sfx "ali"         # ["pp-p"],
   sfx "ale"         # ["pp-p"]]

v20 = iPattern "V20" "Vm"  -- derati, hrvati, očešati (v5r3a)
  (ends ["r","v","š"])
  [sfx "ati"       # ["n"],
   sfx "at"        # ["n"],
   sfx "em"        # ["ip1s"],
   sfx "eš"        # ["ip2s"],
   sfx "e"         # ["ip3s"],
   sfx "emo"       # ["ip1p"],
   sfx "ete"       # ["ip2p"],
   sfx "u"         # ["ip3p"],
   sfx "i"         # ["m-2s"],
   sfx "ite"       # ["m-1p"],
   sfx "imo"       # ["m-1p"],
   sfx "ući"       # ["gp"],
   sfx "avši"      # ["gs"],
   sfx "ao"        # ["pp-s"],
   sfx "ala"       # ["pp-s"],
   sfx "alo"       # ["pp-s"],
   sfx "ali"       # ["pp-p"],
   sfx "ale"       # ["pp-p"]]

v21 = iPattern "V21" "Vm"  -- prati (v5r3b)
  (ends ["r"])
  [rsfx "er" "rati"  # ["n----"],
   rsfx "er" "rat"   # ["n"],
   sfx "em"          # ["ip1s"],
   sfx "eš"          # ["ip2s"],
   sfx "e"           # ["ip3s"],
   sfx "emo"         # ["ip1p"],
   sfx "ete"         # ["ip2p"],
   sfx "u"           # ["ip3p"],
   sfx "i"           # ["m-2s"],
   sfx "ite"         # ["m-1p"],
   sfx "imo"         # ["m-1p"],
   sfx "ući"         # ["gp"],
   rsfx "er" "ravši" # ["gs"],
   rsfx "er" "rao"   # ["pp-s"],
   rsfx "er" "rala"  # ["pp-s"],
   rsfx "er" "ralo"  # ["pp-s"],
   rsfx "er" "rali"  # ["pp-p"],
   rsfx "er" "rale"  # ["pp-p"]]

v22 = iPattern "V22" "Vm"  -- zvati (v5r3c)
  (ends ["v"])
  [rsfx "ov" "vati"  # ["n"],
   rsfx "ov" "vat"   # ["n"],
   sfx "em"          # ["ip1s"],
   sfx "eš"          # ["ip2s"],
   sfx "e"           # ["ip3s"],
   sfx "emo"         # ["ip1p"],
   sfx "ete"         # ["ip2p"],
   sfx "u"           # ["ip3p"],
   sfx "i"           # ["m-2s"],
   sfx "ite"         # ["m-1p"],
   sfx "imo"         # ["m-2p"],
   sfx "ući"         # ["gp"],
   rsfx "ov" "vavši" # ["gs"],
   rsfx "ov" "vao"   # ["pp-s"],
   rsfx "ov" "vala"  # ["pp-s"],
   rsfx "ov" "valo"  # ["pp-s"],
   rsfx "ov" "vali"  # ["pp-p"],
   rsfx "ov" "vale"  # ["pp-p"]]

v23 = iPattern "V23" "Vm"  -- sijati (v5r4a)
  (ends ["j"])
  [sfx "ati"        # ["n"],
   sfx "at"         # ["n"],
   sfx "em"         # ["ip1s"],
   sfx "eš"         # ["ip2s"],
   sfx "e"          # ["ip3s"],
   sfx "emo"        # ["ip1p"],
   sfx "ete"        # ["ip2p"],
   sfx "u"          # ["ip3p"],
   nul              # ["m-2s"],
   sfx "mo"         # ["m-1p"],
   sfx "te"         # ["m-2p"],
   sfx "ući"        # ["gp"],
   sfx "avši"       # ["gs"],
   sfx "ao"         # ["pp-s"],
   sfx "ala"        # ["pp-s"],
   sfx "alo"        # ["pp-s"],
   sfx "ali"        # ["pp-p"],
   sfx "ale"        # ["pp-p"]]

v24 = iPattern "V24" "Vm"  -- kljuvati (v5r4b)
  always
  [sfx "vati"       # ["n"],
   sfx "vat"        # ["n"],
   sfx "jem"        # ["ip1s"], 
   sfx "ješ"        # ["ip2s"],
   sfx "je"         # ["ip3s"],
   sfx "jemo"       # ["ip1p"],
   sfx "jete"       # ["ip2p"],
   sfx "ju"         # ["ip3p"],
   sfx "j"          # ["m-2s"],
   sfx "jmo"        # ["m-1p"],
   sfx "jte"        # ["m-2p"],
   sfx "jući"       # ["gp"],
   sfx "vavši"      # ["gs"],
   sfx "vao"        # ["pp-s"],
   sfx "vala"       # ["pp-s"],
   sfx "valo"       # ["pp-s"],
   sfx "vali"       # ["pp-p"],
   sfx "vale"       # ["pp-p"]]

-- trebalo bi biti "-u-ju" ("u" je tehnički gledano umetak) (v 743)
v25 = iPattern "V25" "Vm"  -- kupovati (v6a)
  always
  [sfx "ovati"      # ["n"],
   sfx "ovat"       # ["n"],
   sfx "ujem"       # ["ip1s"], 
   sfx "uješ"       # ["ip2s"], 
   sfx "uje"        # ["ip3s"], 
   sfx "ujemo"      # ["ip1p"], 
   sfx "ujete"      # ["ip2p"], 
   sfx "uju"        # ["ip3p"], 
   sfx "uj"         # ["m-2s"],
   sfx "ujmo"       # ["m-1p"],
   sfx "ujte"       # ["m-2p"],
   sfx "ujući"      # ["gp"],
   sfx "ovavši"     # ["gs"],
   sfx "ovao"       # ["pp-s"],
   sfx "ovala"      # ["pp-s"],
   sfx "ovalo"      # ["pp-s"],
   sfx "ovali"      # ["pp-p"],
   sfx "ovale"      # ["pp-p"]]

v26 = iPattern "V26" "Vm"  -- kraljevati (v6b)
  always
  [sfx "evati"      # ["n"],
   sfx "evat"       # ["n"],
   sfx "ujem"       # ["ip1s"],
   sfx "uješ"       # ["ip2s"],
   sfx "uje"        # ["ip3s"],
   sfx "ujemo"      # ["ip1p"],
   sfx "ujete"      # ["ip2p"],
   sfx "uju"        # ["ip3p"],
   sfx "uj"         # ["m-2s"],
   sfx "ujmo"       # ["m-1p"],
   sfx "ujte"       # ["m-2p"],
   sfx "ujući"      # ["gp"],
   sfx "evavši"     # ["gs"],
   sfx "evao"       # ["pp-s"],
   sfx "evala"      # ["pp-s"],
   sfx "evalo"      # ["pp-s"],
   sfx "evali"      # ["pp-p"],
   sfx "evale"      # ["pp-p"]]

v27 = iPattern "V27" "Vm"  -- kazivati (v6c)
  always
  [sfx "ivati"      # ["n"],
   sfx "ivat"       # ["n"],
   sfx "ujem"       # ["ip1s"],
   sfx "uješ"       # ["ip2s"],
   sfx "uje"        # ["ip3s"],
   sfx "ujemo"      # ["ip1p"],
   sfx "ujete"      # ["ip2p"],
   sfx "uju"        # ["ip3p"],
   sfx "uj"         # ["m-2s"],
   sfx "ujmo"       # ["m-1p"],
   sfx "ujte"       # ["m-2p"],
   sfx "ujući"      # ["gp"],
   sfx "ivavši"     # ["gs"],
   sfx "ivao"       # ["pp-s"],
   sfx "ivala"      # ["pp-s"],
   sfx "ivalo"      # ["pp-s"],
   sfx "ivali"      # ["pp-s"],
   sfx "ivale"      # ["pp-s"]]

-- nepravilni glagoli:

v28 = iPattern "V28" "Vm"  -- htjeti (621)
  (ends ["hoć"])
  [rsfx "oć" "tjeti"  # ["n"],
   rsfx "oć" "tjet"   # ["n"],
   sfx "u"            # ["ip1s"],
   sfx "eš"           # ["ip2s"],
   sfx "e"            # ["ip3s","ip3p"],
   sfx "emo"          # ["ip1p"],
   sfx "ete"          # ["ip2p"],
   rsfx "oć" "oteći"  # ["gp"],
   rsfx "oć" "tijući" # ["gp"],
   rsfx "oć" "tjevši" # ["gs"],
   rsfx "oć" "tio"    # ["pp-s"],
   rsfx "oć" "tjela"  # ["pp-s"],
   rsfx "oć" "tjelo"  # ["pp-s"],
   rsfx "oć" "tjeli"  # ["pp-s"],
   rsfx "oć" "tjele"  # ["pp-s"]]

v29 = iPattern "V29" "Vm"  -- ići (623)
  always
  [sfx "ći"         # ["n"],
   sfx "ć"          # ["n"],
   sfx "dem"        # ["ip1s"],
   sfx "deš"        # ["ip2s"],
   sfx "de"         # ["ip3s"],
   sfx "demo"       # ["ip1p"],
   sfx "dete"       # ["ip2p"],
   sfx "du"         # ["ip3p"],
   sfx "di"         # ["m-2s"],
   sfx "dimo"       # ["m-1p"],
   sfx "dite"       # ["m-2p"],
   sfx "šavši"      # ["gs"],
   sfx "šao"        # ["pp-s"],
   sfx "šla"        # ["pp-s"],
   sfx "šlo"        # ["pp-s"],
   sfx "šli"        # ["pp-s"],
   sfx "šle"        # ["pp-s"]]

v30 = iPattern "V30" "Vm"  -- doći, naći (624)
  always
  [sfx "ći"         # ["n"],
   sfx "ć"          # ["n"],
   sfx "đem"        # ["ip1s"],
   sfx "đeš"        # ["ip2s"],
   sfx "đe"         # ["ip3s"],
   sfx "đemo"       # ["ip1p"],
   sfx "đete"       # ["ip2p"],
   sfx "đu"         # ["ip3p"],
   sfx "đi"         # ["m-2s"],
   sfx "đimo"       # ["m-1p"],
   sfx "đite"       # ["m-2p"],
   sfx "šavši"      # ["gs"],
   sfx "šao"        # ["pp-s"],
   sfx "šla"        # ["pp-s"],
   sfx "šlo"        # ["pp-s"],
   sfx "šli"        # ["pp-s"],
   sfx "šle"        # ["pp-s"]]

v31 = iPattern "V31" "Vm"  -- moći 
  (ends ["g"])
  [rsfx "g" "ći"    # ["n"],
   sfx "u"          # ["ip1s","ip3p"],
   sfx "eš" & plt   # ["ip2s"],
   sfx "e" & plt    # ["ip3s"],
   sfx "emo" & plt  # ["ip1p"],
   sfx "ete" & plt  # ["ip2p"],
   sfx "avši"       # ["gs"],
   sfx "ao"         # ["pp-s"],
   sfx "la"         # ["pp-s"],
   sfx "lo"         # ["pp-s"],
   sfx "li"         # ["pp-p"],
   sfx "le"         # ["pp-p"]]

-- PRIDJEVSKI UZORCI

-- poduzorci za neodređeni vid

-- poduzorak za osnove koje završavaju na nepčani sugl. (palatal)
ai01 = [
  nul        # ["msn","msa","msv"],
  sfx "a"    # ["msg","nsg","npn","npa","npv","fsn","fsv"],
  sfx "u"    # ["msd","msl","nsd","nsl","fsa"],
  sfx "im"   # ["msi","mpd","mpl","mpi","nsi","npd","npl",
                "npi","fpd","fpl","fpi"],
  sfx "i"    # ["mpn","mpv"],
  sfx "ih"   # ["mpg","npg","fpg"],
  sfx "ima"  # ["mpd","mpl","mpi","npd","npl","npi","fpd",
                "fpl","fpi"],
  sfx "e"    # ["mpa","fsg","fpn","fpa","fpv"],
  sfx "o"    # ["nsn","nsa","nsv"],
  sfx "oj"   # ["fsd","fsl"], 
  sfx "om"   # ["fsi"]]

-- poduzorak za osnove koje završavaju na nenepčani suglasnik
ai02 = [
  nul        # ["msn","msa","msv"],
  sfx "a"    # ["msg","nsg","npn","npa","npv","fsn","fsv"],
  sfx "u"    # ["msd","msl","nsd","nsl","fsa"],
  sfx "im"   # ["msi","mpd","mpl","mpi","nsi","npd","npl",
                "npi","fpd","fpl","fpi"],
  sfx "i"    # ["mpn","mpv"],
  sfx "ih"   # ["mpg","npg","fpg"],
  sfx "ima"  # ["mpd","mpl","mpi","npd","npl","npi","fpd",
                "fpl","fpi"],
  sfx "e"    # ["mpa","nsn","nsa","nsv","fsg","fpn","fpa",
                "fpv"],
  sfx "oj"   # ["fsd","fsl"], 
  sfx "om"   # ["fsi"]]

-- poduzorci za određeni vid

-- poduzorak za osnove koje završavaju na nenepčani suglasnik (nonpals)
ad01 = [
  sfx "i"    # ["msn","msa","msv","mpn","mpv"],
  sfx "og"   # ["msg","nsg"],
  sfx "oga"  # ["msg","nsg"],
  sfx "om"   # ["msd","msl","nsd","nsl","fsi"],
  sfx "ome"  # ["msd","msl","nsd","nsl"], 
  sfx "omu"  # ["msd","msl","nsd","nsl"],
  sfx "im"   # ["msi","mpd","mpl","mpi","nsi","npd","npl",
                "npi","fpd","fpl","fpi"],
  sfx "ih"   # ["mpg","npg","fpg"],
  sfx "ima"  # ["mpd","mpl","mpi","npd","npl","npi","fpd",
                "fpl","fpi"],
  sfx "e"    # ["mpa","fsg","fpn","fpa","fpv"],
  sfx "o"    # ["nsn","nsa","nsv"],
  sfx "a"    # ["npn","npa","npv","fsn","fsv"],
  sfx "oj"   # ["fsl"],
  sfx "u"    # ["fsa"]]

-- poduzorak za osnove koje završavaju na nepčani sugl. (palatal)
ad02 = [
  sfx "i"    # ["msn","msa","msv","mpn","mpv","nsn","nsa",
                "nsv"],
  sfx "eg"   # ["msg","nsg"],
  sfx "ega"  # ["msg","nsg"],
  sfx "em"   # ["msd","msl","nsd","nsl"],
  sfx "om"   # ["fsi"],
  sfx "emu"  # ["msd","msl","nsd","nsl"],
  sfx "im"   # ["msi","mpd","mpl","mpi","nsi","npd","npl",
                "npi","fpd","fpl","fpi"],
  sfx "ih"   # ["mpg","npg","fpg"],
  sfx "ima"  # ["mpd","mpl","mpi","npd","npl","npi","fpd",
                "fpl","fpi"],
  sfx "e"    # ["mpa","nsn","nsa","nsv","fsg","fpn","fpa",
                "fpv"],
  sfx "a"    # ["npn","npa","npv","fsn","fsv"],
  sfx "oj"   # ["fsl"],
  sfx "u"    # ["fsa"]]

-- uzorci

a01 = iPattern "A01" "Af....."  -- skup->skuplji, pust->pušći
  (ends nonpals `land` 
   (neg(ends (cgr ++ ["en","an","on","un","in"])) `lor`
    ends ["st","št","rz","rn","rd"]))
    -- dodano da ne smije završavati na samogl+'n'
  (ai01 <# ["p...n"] ++ 
   ad01 <# ["p...y"] ++ 
   ad02 <& tc <# ["c...-"] ++ 
   ad02 <& tc <& ts <# ["s...-"])
  where tc = jot & try jat1
        ts = pfx "naj"

a03 = iPattern "A03" "Af....."  -- tuđ
  (ends pals `land` 
   (nends cgr `lor` ends ["st", "št"]) `land` nends ["ć"])
  (ai02 <# ["p...n"] ++ 
   ad02 <# ["p...y"] ++ 
   ad02 <& tc <# ["c...-"] ++ 
   ad02 <& tc <& ts <# ["s...-"])
  where tc = sfx "ij"
        ts = pfx "naj"

a04 = iPattern "A04" "Af....."  -- sretan, koristan, drzak, vrijedan
  (ends cgr)
  ((exa # ["msn","msa","msv"] : tail ai01 <& try pca1) <# ["p...n"] ++ 
   ad01 <& try pca1 <# ["p...y"] ++ 
   ad02 <& tc <& try pca1 <# ["c...-"] ++ 
   ad02 <& tc <& try pca1 <& ts <# ["s...-"])
  where tc  = sfx "ij" & opt (jat1 .|. jat2)
        ts  = pfx "naj" 

a06 = iPattern "A06" "Af....."  -- star->stariji, loš->lošiji
  (ends (nonpals ++ ["š"]) `land` 
   (nends cgr `lor` ends ["st", "št", "rt","rn","rm"]))
  (ai01 <# ["p...n"] ++ 
   ad01 <# ["p...y"] ++ 
   ad02 <& tc <# ["c...-"] ++ 
   ad02 <& tc <& ts <# ["s...-"])
  where tc = sfx "ij"
        ts = pfx "naj"

a07 = iPattern "A07" "Af....."  -- lak->lakši, mek->mekši
  (ends ["lak","mek","lijep"])
  (ai01 <# ["p...n"] ++ 
   ad01 <# ["p...y"] ++ 
   ad02 <& tc <# ["c...-"] ++ 
   ad02 <& tc <& ts <# ["s...-"])
  where tc = sfx "š"
        ts = pfx "naj"

a08 = iPattern "A08" "Asp...y"  -- arapski
  (ends ["sk", "šk","čk","ćk"])
  ad01

a09 = iPattern "A09" "Asp...y"  -- kozji
  (ends ["j"] `land` nends ["nj"])
  ad02

a10 = iPattern "A10" "Asp...n"  -- sinov
  (ends ["ov", "ev", "ljev", "in"])
  (ai01 ++ 
   [sfx "om"  # ["msd","msl","nsd","nsl"],
    sfx "og"  # ["msd","nsg","nsl"],
    sfx "oga" # ["msd","nsg","nsl"]])

a11 = iPattern "A11" "Af....."  -- zao, podao
  (ends cgr `land` ends ["l"])
  ((t0 # ["msn","msv"] : tail ai01) <# ["p...n"] ++ 
   ad01 <# ["p...y"] ++ 
   ad02 <& tc <# ["c...-"] ++ 
   ad02 <& tc <& ts <# ["s...-"])
  where t0 = rsfx "l" "ao"
        tc = sfx "ij"
        ts = pfx "naj"

a12 = iPattern "A12" "Afp...y"  -- sinji, idući, drugačiji
  (ends ["ć","n","v","k","nj","t","lj","r","đ","š","j"])
  ad02

a13 = iPattern "A13" "Af....."  -- desni, siroti, mali
  (ends ["ć","n","v","k","nj","t","l"])
  (ad01 <# ["p...y"] ++ 
   ad02 <& tc <# ["c...-"] ++ 
   ad02 <& tc <& ts <# ["s...-"])
  where tc = sfx "ij"
        ts = pfx "naj"

a14 = iPattern "A14" "Af....."  -- zreo, preminuo
  (ends ["el","ul","al","il","rl"])
  ((t0 # ["msn","msv"] : tail ai01) <# ["p...n"] ++ 
   ad01 <# ["p...y"] ++ 
   ad02 <& tc <# ["c...-"] ++ 
   ad02 <& tc <& ts <# ["s...-"])
  where t0 = rsfx "l" "o"
        tc = sfx "ij"
        ts = pfx "naj"
            
a15 = iPattern "A15" "Af....."  -- kratak, plitak, nizak, rijedak, težak, sladak
  (ends ["k"])
  ((exa # ["msn","msa","msv"] : tail ai01 <& try pca1) <# ["p...n"] ++ 
   ad01 <& try pca1 <# ["p...y"] ++
   ad02 <& tc <& dsfx "k" <# ["c...-"] ++ 
   ad02 <& tc <& dsfx "k" <& ts <# ["s...-"])
  where tc = try jot & try jat1  
        ts = pfx "naj"

-- TODO: umrijeti, razumijeti, dozreti
-- TODO: otac -> očevi
--
