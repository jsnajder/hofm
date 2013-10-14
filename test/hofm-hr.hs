{-------------------------------------------------------------------------------
 HofmHr test file

 (c) 2012 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

import MorphGrammar.Hofm
import MorphGrammar.Hofm.Language.Croatian.Inflection

{- PRIMJERI
 
(1) Generiranje flektivnih oblika iz leme

> lWfs (n02::IPatternDefault) "nokat"
["nokat","nokta","noktu","nokte","noktom","nokti","noktima","nokata"]

> lWfsMsd (n04::IPatternDefault) "vojnik"
[("vojnik","N-msn"),("vojnika","N-msg"),("vojnika","N-msa"),("vojnika","N-mpg"),("vojniku","N-msl"),("vojni\269e","N-msv"),("vojnikom","N-msi"),("vojnici","N-mpn"),("vojnici","N-mpv"),("vojnicima","N-mpd"),("vojnicima","N-mpl"),("vojnicima","N-mpi"),("vojnike","N-mpa")]

(2) Generiranje flektivnih oblika iz osnove (engl. stem)

> sWfs (n02::IPatternDefault) "nokt"
["nokat","nokta","noktu","nokte","noktom","nokti","noktima","nokata"]

> sWfsMsd (n04::IPatternDefault) "vojnik"
[("vojnik","N-msn"),("vojnika","N-msg"),("vojnika","N-msa"),("vojnika","N-mpg"),("vojniku","N-msl"),("vojni\269e","N-msv"),("vojnikom","N-msi"),("vojnici","N-mpn"),("vojnici","N-mpv"),("vojnicima","N-mpd"),("vojnicima","N-mpl"),("vojnicima","N-mpi"),("vojnike","N-mpa")]

(3) Generiranje leme iz osnove i obrnuto

> sLemma (n02::IPatternDefault) "nokt" :: [String]
["nokat"]

> lStem (n02::IPatternDefault) "nokat" :: [String]
["nokt"]

(4) Generiranje leme/osnove iz flektivnog oblika

> wLemma (n02::IPatternDefault) "nokata" :: [String]
["nokat"]

> wLemmaMsd (n02::IPatternDefault) "nokata" :: [(String,String)]
[("nokat","N-mpg")]

> wStem (n02::IPatternDefault) "nokata" :: [String]
["nokt"]

> wStemMsd (n02::IPatternDefault) "nokata" :: [(String,String)]
[("nokt","N-mpg")]

(5) Lematizacija (generiranje mogućih lema iz flektivnog oblika)

> lm' (iPatterns::[IPatternDefault]) "noktima" :: [(String,String)]
[("noktim","N01"),("nokt","N01"),("nokat","N02"),("noktimin","N03"),("noktin","N03"),("noktimo","N17"),("nokto","N17"),("noktimo","N30"),("noktim","N37"),("nokt","N37"),("nokt","N38"),("noktim","N41"),("noktima","N45"),("noktimo","N20"),("nokto","N20"),("nokto","N21"),("nokte","N23"),("noktima","N28"),("noktima","N47"),("nokt","N33"),("noktime","N49"),("noktimati","V17"),("noktim","A01"),("nokat","A04"),("noktim","A06"),("nokti","A12"),("nokti","A13")]

> lmMsd' (iPatterns::[IPatternDefault]) "noktima" :: [(String,String,String)]
...

-}

