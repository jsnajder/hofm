{-------------------------------------------------------------------------------
 MorphGrammar.Hofm.Grammar.Croatian.Derivation
 HOFM Croatian Derivation Grammar

 Jan Snajder <jan.snajder@fer.hr>

 (c) 2008 TakeLab
 University of Zagreb
 Faculty of Electrical Engineering and Computing
-------------------------------------------------------------------------------}

module MorphGrammar.Hofm.Language.Croatian.Derivation where

import MorphGrammar.Hofm.Transf hiding (sfx)
import qualified MorphGrammar.Hofm.Transf as Transf (sfx)
import MorphGrammar.Hofm.IPattern
import MorphGrammar.Hofm.DPattern
import MorphGrammar.Hofm.Language.Croatian.Transf
import MorphGrammar.Hofm.Language.Croatian.Inflection

-------------------------------------------------------------------------------
-- Derivational patterns
-------------------------------------------------------------------------------

dPattern = DPatternSL -- default derivation is from a stem to lemma

dPatterns = concat [
  dN01,dN02,dN03,dN04,dN05,dN06,dN07,dN08,dN09,dN10,
  dN11,dN12,dN13,dN14,
  dA01,dA02,dA03,--dA04,
  dV01,dV02,dV03,dV04]

dPatterns1 = concat [dA02,dV01]

dPatterns2 = concat [dPatterns1,dA01,dA03,dV03,dV04,dN04,dN11]

-- sufiksacija + pca nakon toga
sfx s = try pca2 & Transf.sfx s & try pca1
 
-- 1. sufiksalna tvorba imenica
  
-- Div : imenica m. rod kao vrsitelj radnje 
dN01 = [
  dPattern "iv01"  
    -- kositi -> kosac
    (sfx "ac") tiVerbs mNouns,
  dPattern "iv02"
    -- misliti -> mislilac 
    (sfx "ilac") tiVerbs mNouns,
  dPattern "iv03"
    -- orati -> orač, pripovijedati -> pripovjedač
    (sfx "ač" & try jat1) tiVerbs mNouns, 
  dPattern "iv04"
    -- čuvati -> čuvar
    (sfx "ar") tiVerbs mNouns,
  dPattern "iv05"
    -- kišobran -> kišobranar, mlijeko->mljekar, higijena->higijeničar
    (sfx "ar" & opt jat1) nouns mNouns,
  dPattern "iv06"
    -- servis -> serviser
    (sfx "er") mNouns mNouns,
  dPattern "iv07"
    -- hokej -> hokejaš
    (sfx "aš") mNouns mNouns,
  dPattern "iv08"
    -- nišan -> nišandžija, oklop -> oklobdžija
    (sfx "džija") mNouns mNouns,
  dPattern "iv09"
    -- voditi -> vodič
    (sfx "ič") tiVerbs mNouns,
  dPattern "iv10"
    -- bicikl -> biciklist
    (sfx "ist" ) mNouns mNouns,
  dPattern "iv11"
    -- finalan -> finalist  -- TODO! "finalnist"
    (sfx "ist" ) qAdjectives mNouns,
  dPattern "iv12"
    -- potpis -> potpisnik, prijevoz -> prijevoznik
    (sfx "nik") mNouns mNouns,
  dPattern "iv13"
    -- pobjeda -> pobjednik, pravo -> pravnik
    (sfx "nik") (fNouns++nNouns) mNouns,
  dPattern "iv14"
    -- pripadati -> pripadnik, zapovijedati -> zapovjednik
    (sfx "nik" & try jat1) tiVerbs mNouns,
  dPattern "iv15"
    -- voditi -> voditelj, izvijestiti -> izvjestitelj
    (sfx "telj" & try jat1) tiVerbs mNouns,
  dPattern "iv16"
    -- tvornica -> tvorničar, biblioteka -> bibliotekar, 
    -- mehanika->mehaničar, mlijeko -> mljekar
    (sfx "ar" & try jat1 & opt jot) (fNouns++nNouns) mNouns,
  dPattern "iv17"
    -- akademija -> akademik
    (rsfx "ij" "ik") fNouns mNouns,
  dPattern "iv18"
    -- alkemija->alkemičar
    (rsfx "ij" "ičar") fNouns mNouns,
  dPattern "iv19"
    -- alkohol->alkoholičar
    (sfx "ičar") mNouns mNouns,
  dPattern "iv20"
    -- alat->alatničar
    (sfx "ničar") mNouns mNouns,
  dPattern "iv21"
    -- dirigirati -> dirigent
    (rsfx "ir" "ent") tiVerbs mNouns]
--TODO: dokumentalist, dokumentarist
 
-- Div : imenica m. rod kao nositelj osobine
dN02 = [
  dPattern "io01"  
    -- lakom -> lakomac
    (sfx "ac") qAdjectives mNouns,
  dPattern "io02"
    -- sretan -> sretnik, glazben -> glazbenik
    (sfx "ik") qAdjectives mNouns,
  dPattern "io03"
    -- slobodan -> slobodnjak
    (sfx "ak" & jot) qAdjectives mNouns,
  dPattern "io04"
    -- afera -> aferaš
    (sfx "aš") fNouns mNouns,
  dPattern "io05"
    -- droga -> drogeraš
    (sfx "eraš") fNouns mNouns]

-- Dislj: imenica m roda kao sljedbenik
dN03 = [
  dPattern "islj01"
    -- dinamo -> dinamovac, hajduk -> hajdukovac
    (sfx "ovac") mNouns mNouns,
  dPattern "islj02"
    -- franjo -> franjevac, skoj -> skojevac
    (sfx "evac") mNouns mNouns,
  dPattern "islj03"
    -- destruktivan -> destruktivac
    -- TODO: ovo ne bi spadalo u ovu značenjsku skupinu!
    (rsfx "n" "ac") qAdjectives mNouns]

-- Diz: imenice koje znace zensku osobu 
dN04 = [
  dPattern "iz01"
    -- kum -> kuma
    (sfx "a") mNouns fNouns,
  dPattern "iz02"
    -- glupan -> glupača
    (rsfx "n" "ča") mNouns fNouns,
  dPattern "iz03"
    -- rukometaš -> rukometašica
    (sfx "ica") mNouns fNouns,
  dPattern "iz04"
    -- junak -> junakinja, arheolog -> arheologinja
    (sfx "inja") mNouns fNouns,
  dPattern "iz05"
    -- bolničar -> bolničarka, mljekar -> mljekarka
    (sfx "ka") mNouns fNouns, 
  dPattern "iz06"  
    -- državljanin -> državljanka
    (rsfx "in" "ka") mNouns fNouns,
  dPattern "iz07"
   -- stranac -> strankinja, sudac -> sutkinja
   -- sluga -> sluškinja
    (sfx "inja" & rsfx "c" "k") mNouns fNouns,
  dPattern "iz08"
    -- rob -> ropkinja
    (sfx "kinja") mNouns fNouns,
  dPattern "iz09"
    -- mahnit -> mahnitulja
    (sfx "ulja") qAdjectives fNouns,
  dPattern "iz10"
    -- klepet -> klepetuša
    (sfx "uša") mNouns fNouns,
  dPattern "iz11"
    -- disk -> diskašica, košarka -> košarkašica
    (sfx "ašica") (mNouns++fNouns) fNouns]

-- Dimzo: imenice koje znače podjednako mušku i žensku osobu
dN05 = [
  dPattern "imzo01"
    -- ništa -> ništarija, vino -> vinarija -- TODO:vinarija ne spada tu
    (sfx "arija") (fNouns++nNouns) fNouns,
  dPattern "imzo02"
    -- pastir -> pastirče
    (sfx "če") mNouns mNouns,
  dPattern "imzo03"
    -- varati -> varalica
    (sfx "alica") tiVerbs mNouns,
  dPattern "imzo04"
    -- izdati -> izdajica
    (sfx "ajica") tiVerbs mNouns,
  dPattern "imzo05"
    -- piskarati -> piskaralo
    (sfx "alo") tiVerbs nNouns]

-- Dime: etici
dN06 = [
  dPattern "ime01"
    -- varaždin -> varaždinac
    (sfx "ac") mNouns mNouns,
  dPattern "ime02"
    -- afrika -> afrikanac
    (sfx "anac") fNouns mNouns,
  dPattern "ime03"
    -- beč -> bečanin, karlovac -> karlovčanin
    -- atena -> atenjanin
    (sfx "anin" & try jot) (mNouns++fNouns) mNouns,
  dPattern "ime04"
    -- slavonija -> slavonac
    (rsfx "ij" "ac") fNouns mNouns,
  dPattern "ime05"
    -- bjelovar -> bjelovarčanin, zagreb->zagrepčanin
    (sfx "čanin") mNouns mNouns,
  dPattern "ime06"
    -- hrvat -> hrvatica
    (sfx "ica") mNouns fNouns,
  dPattern "ime07"
    -- čeh -> čehinja
    (sfx "inja") mNouns fNouns,
  dPattern "ime08"
    -- rumunj -> rumunjka
    (sfx "ka") mNouns fNouns,
  dPattern "ime09"
    -- slavonac -> slavonka
    (rsfx "ac" "ka") mNouns fNouns,
  dPattern "ime10"
    -- danac -> dankinja
    (rsfx "c" "kinja") mNouns fNouns,
  dPattern "ime11"  
    -- arapin -> arapkinja, srbin -> srpkinja, 
    -- bugarin -> bugarkinja, francuz -> franzuskinja
    (sfx "kinja") mNouns fNouns]
      
-- Dimzb: životinje i bilje
dN07 = [
  dPattern "izb01"
    -- školjka -> školjkaš
    (sfx "aš") fNouns mNouns,
  dPattern "izb02"
    -- mrk -> mrkonja
    (sfx "onja") qAdjectives mNouns,
  dPattern "izb03"
    -- otrovan -> otrovnica, grabljiv -> grabljivica
    (sfx "ica") qAdjectives fNouns,
  dPattern "izb04"
    -- raditi -> radilica
    (sfx "ilica") tiVerbs fNouns,
  dPattern "izb05"
    -- kreketati -> kreketuša
    (sfx "uša") tiVerbs fNouns,
  dPattern "izb06"
    -- cvijet -> cvjetača
    (sfx "ača" & try jat1) mNouns fNouns]

  --TODO: provjeriti SUSTAVNO kad se sve može dogoditi "ije"->"je". Ako nema pravila, dodati svugdje kao "opt"
-- v. http://www.blog.hr/print/id/1621202506/o-refleksu-emjataem.html
-- odnosno pogledaj u gramatici
--
-- TODO:  svjetlo -> svjetiljka ?  svijetliti -> svjetlo

-- Dis: stvari
dN08 = [
  dPattern "is01"
    -- brisati -> brisač, mijenjati -> mjenjač, peglati -> pegljač
    -- nositi -> nosač, upaliti -> upaljač
    (sfx "ač" & opt jot & try jat1) tiVerbs mNouns,
  dPattern "is02"
    -- kuhati -> kuhača
    (sfx "ača") tiVerbs fNouns,
  dPattern "is03"
    -- kapati -> kapaljka
    (sfx "aljka") tiVerbs fNouns,
  dPattern "is04"
    -- cijediti -> cjediljka
    (sfx "iljka" & try jat1) tiVerbs fNouns,
  dPattern "is05"
    -- bušiti -> bušilica, hraniti -> hranilica
    (sfx "ilica") tiVerbs fNouns,
  dPattern "is06"
    -- dizati -> dizalo
    (sfx "alo") tiVerbs fNouns,
  dPattern "is07"
    -- svinja -> svinjetina, tele -> teletina
    (sfx "etina") (fNouns++nNouns) fNouns,
  dPattern "is08"
    -- borov -> borovina
    (sfx "ina") pAdjectives fNouns,
  dPattern "is09"
    -- darovan -> darovnica
    (sfx "ica") qAdjectives fNouns,
  dPattern "is10"
    -- cijena -> cjenik
    (sfx "ik" & try jat1) fNouns mNouns,
  dPattern "is11"
    -- vježba -> vježbenica
    (sfx "enica") fNouns fNouns]

-- Dim: mjesne imenice
dN09 = [
  dPattern "im01"
    -- cigla -> ciglana
    (sfx "na") fNouns fNouns,
  dPattern "im02"
    -- čaj -> čajana
    (sfx "ana") mNouns fNouns,
  dPattern "im03"
    -- piliti -> pilana,
    (sfx "ana") tiVerbs fNouns,
  dPattern "im04"
    -- brijač -> brijačnica, cvjećar -> cvjećarnica
    (sfx "nica") mNouns fNouns,
  dPattern "im05"
    -- čekati -> čekaonica, gostiti -> gostionica, 
    -- ispovijedati -> ispovjedaonica, štedjeti -> štedionica
    -- vježbati -> vježbaonica
    (sfx "aonica" & try jat1) tiVerbs fNouns,
  dPattern "im06" -- <-- TODO
    -- gostiti -> gostionica, štedjeti -> štedionica
    (sfx "ionica" & try jat2) tiVerbs fNouns,
  dPattern "im07"
    -- boj -> bojište
    (sfx "ište") mNouns nNouns,
  dPattern "im08"
    -- polaziti -> polazište, hvatati -> hvatište
    (sfx "ište") tiVerbs nNouns,
  dPattern "im09"
    -- graditi -> gradilište
    (sfx "ilište") tiVerbs nNouns]
-- TODO: svinja -> svinjac, kokoš -> kokošinjac,
-- napajati -> napajalište

-- Dia: apstraktne (mislene) imenice
dN10 = [
  dPattern "ia01"
    -- grub -> grubost, unutrašnji -> unutrašnjost, 
    -- kiseo -> kiselost, suvisao -> suvislost, gibak -> gipkost
    (sfx "ost") qAdjectives fNouns,
  dPattern "ia02"
    -- prijatelj -> prijateljstvo, rob -> ropstvo, 
    -- TODO: divljak -> divljaštvo, izvodilac -> izvodilaštvo
    (sfx "stvo") mNouns nNouns,
  dPattern "ia03"
    -- lud -> ludilo
    (sfx "ilo") qAdjectives nNouns,
  dPattern "ia04"  
    -- trijezan -> treznoća, bijesan -> bjesnoća, gladak -> glatkoća, 
    -- kratak -> kratkoća, težak -> teškoća, gluh -> gluhoća
    (sfx "oća" & try (jat1 .|. jat2)) 
    qAdjectives fNouns,
  dPattern "ia05"
    -- brz -> brzina, bijel -> bjelina
    (sfx "ina" & try jat1) qAdjectives fNouns,
  dPattern "ia06"
    -- strah -> strahota
    (sfx "ota") mNouns fNouns,
  dPattern "ia07"
    -- dobar -> dobrota, vrijedan -> vrednota/vrjednota
    (sfx "ota" & try (jat1 .|. jat2)) qAdjectives fNouns,
  dPattern "ia08"
    -- divan -> divota  (TODO: griješan -> grehota/grjehota)
    (rsfx "an" "ota") qAdjectives fNouns,
  dPattern "ia09"
    -- gol -> golotinja
    (sfx "otinja") qAdjectives fNouns,
  dPattern "ia10"
    -- prljav -> prljavština
    (sfx "ština") qAdjectives fNouns,
  dPattern "ia11"
    -- raketa -> raketiranje
    (sfx "iranje") fNouns nNouns,
  dPattern "ia12"
    -- bogat -> bogatstvo
    (sfx "stvo") qAdjectives nNouns,
  dPattern "ia13"
    -- arhitekt -> arhitektura
    (sfx "ura") mNouns fNouns,
  dPattern "ia14"  
    -- alkohol -> alkoholizam, turist -> turizam, 
    -- barbarin -> barbarizam, boljševik -> boljševizam
    (sfx "izam" & try (dsfx "ist" .|. dsfx "ik")) mNouns mNouns,
  dPattern "ia15"
    -- avangardan -> avangardizam
    (rsfx "n" "izam") qAdjectives mNouns,
  dPattern "ia16"
    -- dokument -> dokumentacija
    (sfx "acija") mNouns fNouns,
  dPattern "ia17"
    -- drama -> dramatika
    (sfx "atika") fNouns fNouns,
  dPattern "ia18"
    -- centralan -> centralizacija, kolektivan -> kolektivizacija
    (rsfx "n" "izacija") qAdjectives fNouns]

-- Dig: glagolske imenice
dN11 = [
  dPattern "ig01"
    -- čuvati -> čuvanje
    (sfx "anje") tiVerbs nNouns,
  dPattern "ig02"
    -- otvoriti -> otvorenje, dojiti -> dojenje, 
    -- unaprijediti -> unapređenje/unaprjeđenje,
    -- čistiti -> čišćenje, doseliti -> doseljenje,
    -- koristiti -> korištenje, bditi -> bdjenje
    (sfx "enje" & try (jat1 .|. jat2) &
     opt (acg .||. jot)) tiVerbs nNouns,
  dPattern "ig03"
    -- otkriti -> otkriće, raspeti -> raspeće,
    -- dospjeti -> dospijeće
    (sfx "će" & try jat1) tiVerbs nNouns,
  dPattern "ig04"  
    -- izvoziti -> izvoz, upisati -> upis, 
    -- izdahnuti->izdah, prelaziti -> prijelaz
    (opt (inv jat3)) tiVerbs mNouns,
  dPattern "ig05"
    -- boriti -> borba
    (sfx "ba") tiVerbs fNouns,
  dPattern "ig06"
    -- gnojiti -> gnojidba, kruniti -> krunidba, 
    -- plijeniti -> pljenidba
    (sfx "dba" & try jat1) tiVerbs fNouns,
  dPattern "ig07"
    -- jecati -> jecaj, natjecati -> natječaj, 
    -- odnositi -> odnošaj
    (sfx "aj" & opt (acg .|. jot)) tiVerbs mNouns,
  dPattern "ig08"
    -- dobiti -> dobitak, ostati -> ostatak
    (sfx "itak" .|. sfx "atak") tiVerbs mNouns,
  dPattern "ig09"
    -- izlaziti -> izlazak
    (sfx "ak") tiVerbs mNouns,
  dPattern "ig10"
    -- tutnjati -> tutnjava
    (sfx "ava") tiVerbs fNouns,
  dPattern "ig11"
    -- grabiti -> grabež
    (sfx "ež") tiVerbs fNouns,
  dPattern "ig12"
    -- graditi -> gradnja
    (sfx "nja") tiVerbs fNouns,
  dPattern "ig13"
    -- juriti -> jurnjava, pucati -> pucnjava
    (sfx "njava") tiVerbs fNouns,
  dPattern "ig14"
    -- govoriti -> govorancija, tjerati -> tjerancija
    (sfx "ancija") tiVerbs fNouns,
  dPattern "ig15"
    -- hvatati -> hvatanija
    (sfx "anija") tiVerbs fNouns,
  dPattern "ig16"
   -- održati -> održavanje, udovoljiti -> udovoljavanje
    (sfx "avanje") tiVerbs fNouns,
  dPattern "ig17"
    -- bolovati -> bolovanje
    (sfx "anje") tiVerbs fNouns,
  dPattern "ig18"
    -- tužiti -> tužilaštvo, izdavati -> izdavalaštvo
    (sfx "ilaštvo" .|. sfx "alaštvo") tiVerbs nNouns,
  dPattern "ig19"
    -- tužiti -> tužiteljstvo
    (sfx "iteljstvo") tiVerbs nNouns,
  dPattern "ig20"
    -- ukinuti -> ukinuće
    (sfx "uće") tiVerbs nNouns,
  dPattern "ig21"
    -- administrirati -> administracija
    (rsfx "ir" "acija") tiVerbs fNouns,
  dPattern "ig22"
    -- vladati -> vladavina
    (sfx "avina") tiVerbs fNouns,
  dPattern "ig23"
    -- opeći -> opeklina
    (sfx "lina") ciVerbs fNouns,
  dPattern "ig24"
    -- padati -> padalina
    (sfx "alina" ) tiVerbs fNouns]
-- TODO: ozepsti -> ozeblina, izrasti -> izraslina

-- Diu: umanjenice i uvećanice
dN12 = [
  dPattern "iu01"  
    -- crv -> crvić, cvijet -> cvjetić,
    -- članak -> člančić, svezak -> sveščić
    (sfx "ić" & try jat1 & try plt) mNouns mNouns,
  dPattern "iu02"  
    -- cvijet -> cvijetak, smijeh -> smiješak
    (sfx "ak" & try plt) mNouns mNouns,
  dPattern "iu03"  
    -- plamen -> plamičak
    (rsfx "en" "ičak") mNouns mNouns,
  dPattern "iu04"  
    -- crkva -> crkvica, svijeća -> svjećica
    (sfx "ica" & try jat1 & try plt) fNouns fNouns,
  dPattern "iu05"  
    -- grana -> grančica, cijev -> cjevčica
    (sfx "čica" & try jat1) fNouns fNouns,
  -- TODO: kost -> koščica
  dPattern "iu06"
    -- jezero -> jezerce, tijelo -> tijelce
    (sfx "ce") nNouns nNouns,
  dPattern "iu07"
    -- vrelo -> vreoce
    (rsfx "l" "oce") nNouns nNouns,
  dPattern "iu08"
    -- društvo -> društvance, bure -> burence
    (sfx "ance" .|. sfx "ence") nNouns nNouns,
  dPattern "iu09"
    -- mjesto -> mjestašce, jaje -> jajašce, dijete -> djetešce
    ((sfx "ašce" .|. sfx "ešce") & try jat1) nNouns nNouns,
  dPattern "iu10"
    -- put -> puteljak
    (sfx "eljak") mNouns mNouns,
  dPattern "iu11"
    -- crv -> crvuljak, brijeg -> brežuljak
    (sfx "uljak" & try jat2 & try plt) mNouns mNouns,
  dPattern "iu12"
    -- pjesma -> pjesmuljak
    (sfx "uljak") fNouns mNouns,
  dPattern "iu13"  
    -- komad -> komadina, svijet -> svjetina, junak -> junačina
    (sfx "ina" & try jat1 & try plt) mNouns fNouns,
  dPattern "iu14"
    -- muškarac -> muškarčina, lanac -> lančina
    (rsfx "c" "čina") mNouns (mNouns++fNouns),
  dPattern "iu15"
    -- oficir -> oficirčina
    (sfx "čina") mNouns mNouns,
  dPattern "iu16"
    -- baba -> babetina, ruka -> ručetina
    (sfx "etina" & try plt) fNouns fNouns,
  dPattern "iu17"
    -- trava -> travurina
    (sfx "urina") fNouns fNouns,
  dPattern "iu18"
    -- kuća -> kućerina
    (sfx "erina") fNouns fNouns,
  dPattern "iu19"
    -- glava -> glavešina
    (sfx "ešina") fNouns fNouns]

-- Dia: zbirne imenice 
-- TODO: dodati paradigmu za zbirne imenice?
-- (ne treba, budući da one imaju samo jedninu a ne i množinu. Ali za
-- pl. tantum imenice bi ti trebala posebna paradigma, ili više njih)
dN13 = [
  dPattern "izbr01"
    -- prase -> prasad
    (sfx "ad") nNouns fNouns,
  dPattern "izbr02"  
   -- biser -> biserje, otok -> otočje, okrug -> okružje, naziv -> nazivlje
    (sfx "je" & try jot) mNouns nNouns,
  dPattern "izbr03"  
   -- naziv -> nazivlje, zvijezda -> zviježđe
    (sfx "e" & try (acg .||. jot)) (mNouns++fNouns) nNouns,
  dPattern "izbr04"
    -- sklop -> sklopovlje
    (sfx "ovlje") mNouns nNouns,
  dPattern "izbr05"
    -- slušatelj -> slušateljstvo
    (sfx "stvo") mNouns nNouns,
  dPattern "izbr06"
    -- svećenik -> svećenstvo
    (rsfx "ik" "stvo") mNouns nNouns,
  dPattern "izbr07"
    -- radnik -> radništvo
    (rsfx "ik" "ištvo") mNouns nNouns]

-- Dipzs: ostale pojedinačne značenjske skupine imenica
dN14 = [
  dPattern "ipzs01"
    -- slagati -> slagaljka
    (sfx "aljka") tiVerbs fNouns,
  dPattern "ipzs02"
    -- pošta -> poštarina
    (sfx "rina") fNouns fNouns,
  dPattern "ipzs03"
    -- najam -> najamnina
    (sfx "nina") mNouns fNouns,
  dPattern "ipzs04"
    -- otprema -> otpremnina
    (sfx "nina") fNouns fNouns,
  dPattern "ipzs05"
    -- kajkavski -> kajkavština
    (rsfx "sk" "ština") qAdjectives fNouns,
  dPattern "ipzs06"
    -- kajkavski -> kajkavica
    (rsfx "sk" "ica") qAdjectives fNouns]

-- 2. SUFIKSALNA TVORBA PRIDJEVA

-- Dpo: opisni pridjevi
dA01 = [
  dPattern "po01"
    -- blato -> blatan, sunce -> sunčan, mlijeko -> mlječan
    (sfx "an" & try jat1 & try plt) nNouns qAdjectives,
  dPattern "po02"
    -- dvorište -> dvorišni
    (rsfx "t" "ni") nNouns qAdjectives,
  dPattern "po03"
   -- izvor -> izvoran, mrak -> mračan, luk -> lučni
   -- grijeh -> grešan/grješan, smijeh -> smiješan, 
    ((sfx "an" .|. sfx "ni") & 
     opt (jat1 .|. jat2) & try plt) mNouns qAdjectives,
  dPattern "po04" 
    -- novac -> novčani, pijesak -> pješčan
    ((sfx "an" .|. sfx "ani") & 
     opt jat1 & try plt) mNouns qAdjectives,
  dPattern "po05" 
    -- noga -> nožni, zvijezda -> zvjezdan, 
    -- bijeda -> bijedan, kartica -> kartični
    ((sfx "an" .|. sfx "ni") & opt jat1 & try plt) 
     fNouns qAdjectives,
  dPattern "po06"
    -- udaranje -> udaran
    (rsfx "anj" "an") nNouns qAdjectives,
  dPattern "po07"
    -- borba -> borben, knjiga -> knjižen, muka -> mučen
    -- spas -> spašen, zdravstvo -> zdravstven
    (sfx "en" & opt jot) (fNouns++nNouns) qAdjectives,
  dPattern "po08"
    -- masa -> masovan, svijet -> svjetovni
    -- cijena -> cjenovni, banka -> bankovni
    ((sfx "ovan" .|. sfx "ovni") & try jat1) mNouns qAdjectives,
  dPattern "po09"
    -- duša -> duševan, knjiga -> književan
    (sfx "evan" & try plt) fNouns qAdjectives,
  dPattern "po10"
    -- rušiti -> ruševan, platiti -> plaćevan
    (sfx "evan" & try jot) tiVerbs qAdjectives,
  dPattern "po11"
    -- ekonomija -> ekonomičan
    (rsfx "j" "čan") fNouns qAdjectives,
  dPattern "po12"
    -- egoist -> egoističan
    (sfx "ičan") mNouns qAdjectives,
  dPattern "po13"
    -- koža -> kožnat, papir -> papirnat
    (sfx "nat") fNouns qAdjectives,
  dPattern "po14"
    -- anketa -> anketiran
    (sfx "iran") fNouns qAdjectives,
  dPattern "po15"
    -- kesten -> kestenjast, trbuh -> trbušast
    -- maslina -> maslinast, stepenica -> stepeničast
    -- vlakno -> vlaknast
    (sfx "ast" & opt jot) nouns qAdjectives,
  dPattern "po16"
    -- kovrča -> kovrčav
    (sfx "av") fNouns qAdjectives,
  dPattern "po17"
    -- krilo -> krilat, glava -> glavat, rep -> repat
    (sfx "at") nouns qAdjectives,
  dPattern "po18"
    -- milost -> milostiv
    (sfx "iv" & opt jot) fNouns qAdjectives,
  dPattern "po19"
    -- briga -> brižljiv, poruga -> porugljiv
    (sfx "ljiv" & try plt) fNouns qAdjectives,
  dPattern "po20"
    -- brdo -> brdovit, kiša -> kišovit
    (sfx "vit" & try (jat1 .|. jat2)) nouns qAdjectives,
  dPattern "po21"
    -- krš -> kršovit, lijek -> ljekovit, brijeg -> bregovit
    (sfx "ovit" & try (jat1 .|. jat2)) mNouns qAdjectives,
  dPattern "po22"
    -- sloj -> slojevit, stupanj -> stupnjevit
    (sfx "evit") mNouns qAdjectives,
  dPattern "po23"
    -- izbrojati -> izbrojiv, ostvariti -> ostvariv
    -- TODO: istežljiv
    (sfx "iv") tiVerbs qAdjectives,
  dPattern "po24"
    -- ostvariti -> ostvarljiv, gledati -> gledljiv
    -- istezati -> istežljiv/istezljiv
    (sfx "ljiv" & opt plt) tiVerbs qAdjectives,
  dPattern "po25"
    -- čitati -> čitak
    (sfx "ak") tiVerbs qAdjectives,
  dPattern "po26"
    -- brljati -> brbljav
    (sfx "av") tiVerbs qAdjectives,
  dPattern "po27"
    -- pun -> puncat
    (sfx "cat") qAdjectives qAdjectives,
  dPattern "po28"
    -- rumen -> rumenkast
    (sfx "kast") qAdjectives qAdjectives,
  dPattern "po29"
    -- plav -> plavičast
    (sfx "ičast") qAdjectives qAdjectives,
  dPattern "po30"
    -- živ -> živahan, mlad -> mlađahan
    (sfx "ahan" & try jot) qAdjectives qAdjectives,
  dPattern "po31"
    -- pun -> punašan
    (sfx "ašan") qAdjectives qAdjectives,
  dPattern "po32"
    -- dug -> duguljast
    (sfx "uljast") qAdjectives qAdjectives,
  dPattern "po33"
    -- slab -> slabunjav
    (sfx "unjav") qAdjectives qAdjectives,
  dPattern "po34"
    -- koncept -> konceptualan
    (sfx "ualan") mNouns qAdjectives,
  dPattern "po35"
    -- kultura -> kulturalan, centar -> centralan
    (sfx "alan") nouns qAdjectives]

-- TODO: zub -> zubac -> zupčast
-- zub -> zupčani
-- baciti -> bačen, izraziti -> izražen

-- Dpp: posvojni pridjevi
dA02 = [
  dPattern "pp01"
    -- djed -> djedov
    (sfx "ov") mNouns pAdjectives,
  dPattern "pp02"  
    -- bratić -> bratićev, jakov -> jakovljev, kupac -> kupčev
    -- mjesec -> mjesečev, pisac -> piščev, sunce -> sunčev
    (sfx "ev" & try jot) (mNouns++nNouns) pAdjectives,
  dPattern "pp03"
    -- jerko -> jerkov
    (sfx "ov") mNouns pAdjectives,
  dPattern "pp04"
    -- sin -> sinovljev
    (sfx "ovljev") mNouns pAdjectives,
  dPattern "pp05"  
    -- tata -> tatin, mama -> mamin, ivica -> ivičin
    (sfx "in" & try plt) (mNouns++fNouns) pAdjectives,
  dPattern "pp06"  
    -- grad -> gradski, klub -> klupski, englez -> engleski, rus -> ruski
    (sfx "ski" & try (asfx [("b","p"),("z",""),("s","")])) 
    nouns pAdjectives,
  dPattern "pp07"
    -- slovenija -> slovenski
    (rsfx "ij" "ski") fNouns pAdjectives,
  dPattern "pp08"
    -- dalmacija -> dalmatinski
    (rsfx "cij" "tinski") fNouns pAdjectives,
  dPattern "pp09"  
    -- gospić -> gospićki, junak -> junački, 
    -- bijelac -> bjelački, brandenburg -> brandenburški
    -- antika -> antički, tvornica -> tvornički
    (sfx "ki" & try jat1 & try plt) nouns pAdjectives,
  dPattern "pp10"
    -- djed -> djedovski
    (sfx "ovski") mNouns pAdjectives,
  dPattern "pp11"
    -- dužd -> duždevski
    (sfx "evski") mNouns pAdjectives,
  dPattern "pp12"
    -- sestra -> sestrinski
    (sfx "inski") fNouns pAdjectives,
  dPattern "pp13"
    -- kapitalist -> kapitalistički
    (sfx "ički") mNouns pAdjectives,
  dPattern "pp14"
    -- zagreb -> zagrebački, valpovo -> valpovački
    (sfx "ački") nouns pAdjectives,
  dPattern "pp15"
    -- koza -> kozji, kukavica -> kukavičji, 
    -- patka -> pačji, guska -> guščji
    -- pas -> pasji, bog -> božji
    (sfx "ji" & opt jot) nouns pAdjectives,
  dPattern "pp16"
    -- krava -> kravlji, mrav -> mravlji,
    -- govedo -> goveđi
    (sfx "i" & opt jot) nouns adjectives,
  dPattern "pp17"
    -- mrav -> mravinji, pčela -> pčelinji
    (sfx "inji") mNouns adjectives,  
  dPattern "pp18"
    -- brijati -> brijaći
    (sfx "aći") tiVerbs adjectives,
  dPattern "pp19"
    -- turizam -> turistički
    (rsfx "izm" "istički") mNouns pAdjectives]

-- Dpgt: pridjevi glagolski trpni  TODO: <-- JESU LI? ne mogu to naći u
-- gramatici
dA03 = [
  dPattern "pgt01"  
    -- GLAGOLSKI PRIDJEVI PASIVNI
    -- tužiti -> tužen, izraziti -> izražen, 
    -- baciti -> bačen, spasiti -> spašen,
    -- tresti -> tresen, tući -> tučen
    (sfx "en" & opt jot) verbs qAdjectives,
  dPattern "pgt02"
    -- adaptirati -> adaptiran, prolaziti -> prolazan
    (sfx "an") tiVerbs qAdjectives,
  dPattern "pgt03"
    -- spomenuti -> spomenut
    (sfx "ut") tiVerbs qAdjectives,
-- ]  spojeno s A04

-- Dpgs : GLAGOLSKI *PRILOZI* SADAŠNJI:
-- (HJP ih ima kao takve, a HML ih ima kao pridjeve)
-- dA04 = [
  dPattern "pgs01"
    -- zastrašivati -> zastrašujući, iscrpljivati -> isrpljujući, 
    -- omamljivati -> omamljujući, onečišćivati -> onečišćujući
    (sfx "ujući") tiVerbs qAdjectives,
  dPattern "pgs02"
    -- dodavati -> dodajući
    (sfx "jući") tiVerbs qAdjectives,
  dPattern "pgs03"
    -- dopunjavati -> dopunjavajući
    (sfx "ajući") tiVerbs qAdjectives,
  dPattern "pgs04"
    -- brisati -> brišući, puzati -> pužući
    (sfx "ući" & try jot) tiVerbs qAdjectives,
  dPattern "pgs05"
    -- dolaziti -> dolazeći, govoriti -> govoreći,
    -- letjeti -> leteći, lebdjeti -> lebdeći
    (sfx "eći") tiVerbs qAdjectives,
  dPattern "pgs06"
    -- bdjeti -> bdijući
    (sfx "ijući") tiVerbs qAdjectives]

-- 3. sufiksalna tvorba glagola

-- Dgv: glagoli iz glagola s promjenom u vidu
dV01 = [
  dPattern "gv01"  
   -- baciti -> bacati, prikupiti -> prikupljati
   -- ispratiti -> ispraćati, izmisliti -> izmišljati
    (sfx "ati" & opt (acg .||. jot)) tiVerbs tiVerbs,
  dPattern "gv02"
    -- darovati -> darivati
    -- izvijestiti -> izvješćivati, poskupiti -> poskupljivati,
    (sfx "ivati" & try jat1 & try jot) tiVerbs tiVerbs,
  dPattern "gv03"  
    -- odobriti -> odobravati, onečistiti -> onečišćavati,
    -- naseliti -> naseljavati, navodniti -> navodnjavati, 
    (sfx "avati" & try jat1 & try jot) tiVerbs tiVerbs,
  dPattern "gv04" -- <---   OVDJE STAO
    -- naviknuti -> navikavati
    (rsfx "n" "avati") tiVerbs tiVerbs,
  dPattern "gv05"
    -- vikati -> viknuti
    (sfx "nuti") tiVerbs tiVerbs,
  dPattern "gv06"
    -- izliti -> izlijevati, sagoriti -> sagorijevati
    -- razumjeti -> razumijevati
    (sfx "ijevati") tiVerbs tiVerbs,
  dPattern "gv07"
    -- blijediti -> blijedjeti, bluditi -> bludjeti, bjesniti -> bjesnjeti
    -- oboliti -> oboljeti
    (sfx "jeti") tiVerbs tiVerbs,
  dPattern "gv08"
    -- izroniti -> izranjati, nasloniti -> naslanjati
    (rsfx "on" "anjati") tiVerbs tiVerbs,
  dPattern "gv09"
    -- početi -> počinjati
    -- TODO!!!: ovdje isto popravi paradigmu: *otpočesti->otpočeti,*napesti->napeti
    (sfx "injati") tiVerbs tiVerbs,
  dPattern "gv10"
    -- nagnuti -> naginjati,
    (rsfx "n" "injati") tiVerbs tiVerbs,
  dPattern "gv11"
    -- proizvoditi -> proizvesti
    (rsfx "oditi" "esti") tiVerbs tiVerbs,
  dPattern "gv12"
    -- napomenuti -> napominjati
    (rsfx "en" "injati") tiVerbs tiVerbs]

-- primjer kada je ok da imamo pravila u "trokutu", ako se
-- inzistira na atestiranju riječi u korpusu (jer možda ne postoji ta
-- riječ u jeziku, pa nam treba trokut):
--  vikati->viknuti->*vikavati
--  *navikati->naviknuti->navikavti

-- Dgdp: deminutivni i pejorativni glagoli
dV02 = [
  dPattern "gdp01"
    -- govoriti -> govorkati, šetati -> šetkati
    (sfx "kati") tiVerbs tiVerbs,
  dPattern "gdp02"
    -- pjevati -> pjevuckati
    (sfx "uckati") tiVerbs tiVerbs,
  dPattern "gdp03"
    -- piti -> pijuckati
    (sfx "juckati") tiVerbs tiVerbs,
  dPattern "gdp04"
    -- pjevati -> pjevušiti
    (sfx "ušiti") tiVerbs tiVerbs,
  dPattern "gdp05"
    -- pisati -> piskarati
    (sfx "karati") tiVerbs tiVerbs,
  dPattern "gdp06"
    -- smijati -> smijuljiti
    (sfx "uljiti") tiVerbs tiVerbs]

-- Dgi: glagoli iz imenica
dV03 = [
  dPattern "gi01"
    -- komad -> komadati, večera -> večerati
    (sfx "ati") nouns tiVerbs,
  dPattern "gi02"
    -- bol -> bolovati, tuga -> tugovati
    (sfx "ovati") nouns tiVerbs,
  dPattern "gi03"
    -- lak -> lakirati, adresa -> adresirati
    (sfx "irati") nouns tiVerbs,
  dPattern "gi04"
    -- filozofija -> filozofirati
    (rsfx "ij" "irati") fNouns tiVerbs,
  dPattern "gi05"
    -- grijeh -> griješiti, drug -> družiti, znak -> značiti
    -- zima -> zimiti, granica -> graničiti
    (sfx "iti" & try plt) nouns tiVerbs]

-- Dgp: glagoli iz pridjeva
dV04 = [
  dPattern "gp01"
    -- bijel -> bijeliti
    (sfx "iti") qAdjectives tiVerbs,
  dPattern "gp02"
    -- sitan -> sitniti
    (sfx "iti") qAdjectives tiVerbs]
--TODO: može se napraviti automatska analiza preoblika da bi se
--utvrdilo je li neko pravilo inverz drugoga

-- treba reći da se neke povezanosti mogu uhvatiti samo kad bi se
-- radila prefiksalno-sufiksalna tvorba, npr:
-- ružan -> poružniti
-- (jer "poružan" ne postoji)

-- nadji redundantna pravila (da je jedno inverz drugoga ili podskup
-- nadji koliko % pravila je "konfliktno" (oba pale)
-- drugoga... test na podskup transformacije možeš implementirati u CTransf)


