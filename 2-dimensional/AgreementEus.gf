concrete AgreementEus of Agreement = open Prelude in {

lincat
  V  = Verb ;
  V2 = Verb2 ;
  VP = Verb ** {sc : Case} ; -- Need to know if subject/agent is in Abs or Erg
  NP = NounPhrase ;

lin
  UseV v = v ** {
    sc = Abs ; -- Comes from intrasitive verb, has absolutive subject
    } ;

  ComplV2 v2 np = {
    s = \\agAgr => np.s ! Abs ++ v2.s ! np.a ! agAgr ;
    sc = Erg ; -- Comes from transitive verb, has ergative agent
    } ;

  PredVP np vp = {
    s = np.s ! vp.sc ++ vp.s ! np.a ;
    } ;

  -- These verbs use the auxiliaries defined in opers
  sleep_V = mkV "lo egin" ;
  drink_V2 = mkV2 "edan" ;

  have_V2 = { -- Whole inflection spelled out
    s = table {
        Sg1 => table { Sg1|Pl1 => nonExist ;
                       Sg2 => "naukazu" ;
                       Sg3 => "nauka" ;
                       Pl2 => "naukazue" ;
                       Pl3 => "naukate" } ;
        Sg2 => table { Sg1 => "zauzkat" ;
                       Sg2|Pl2 => nonExist ;
                       Sg3 => "zauzka" ;
                       Pl1 => "zauzkagu" ;
                       Pl3 => "zauzkate" } ;
        Sg3 => table { Sg1 => "daukat" ;
                       Sg2 => "daukazu" ;
                       Sg3 => "dauka" ;
                       Pl1 => "daukagu" ;
                       Pl2 => "daukazue" ;
                       Pl3 => "daukate" } ;
        Pl1 => table { Pl1|Sg1 => nonExist ;
                       Sg2 => "gauzkazu" ;
                       Sg3 => "gauzka" ;
                       Pl2 => "gauzkazue" ;
                       Pl3 => "gauzkate" } ;
        Pl2 => table { Sg1 => "zauzkatet" ;
                       Sg2|Pl2 => nonExist ;
                       Sg3 => "zauzkate" ;
                       Pl1 => "zauzkategu" ;
                       Pl3 => "zauzkatete" } ;
        Pl3 => table { Sg1 => "dauzkat" ;
                       Sg2 => "dauzkazu" ;
                       Sg3 => "dauzka" ;
                       Pl1 => "dauzkagu" ;
                       Pl2 => "dauzkazue" ;
                       Pl3 => "dauzkate" }
        }
  } ;

  i_NP = mkNP "nik" "ni" Sg1 ;
  you_NP = mkNP "zuk" "zu" Sg2 ;
  water_NP = mkNP "urak" "ura" Sg3 ;

  -- rest of the pronouns prodropped
  he_she_NP = pron Sg3 ;
  we_NP = pron Pl1 ;
  youPl_NP = pron Pl2 ;
  they_NP = pron Pl3 ;

param
  Agr = Sg1 | Sg2 | Sg3 | Pl1 | Pl2 | Pl3 ;
  Case = Abs | Erg ; -- Only the cases needed for this small grammar

oper
  NounPhrase : Type = {s : Case => Str ; a : Agr} ;
  Verb : Type  = {s :        Agr => Str} ;
  Verb2 : Type = {s : Agr => Agr => Str} ;

  mkNP : (erg,abs : Str) -> Agr -> NounPhrase = \erg,abs,ag -> {
    s = table {Erg => erg ; Abs => abs} ;
    a = ag ;
    } ;

  pron : Agr -> NounPhrase = mkNP [] [] ;

  mkV : Str -> Verb = \prc -> {
    s = \\subj => prc ++ izan.s ! subj ;
    } ;

  mkV2 : Str -> Verb2 = \prc -> {
    s = \\obj,agent => prc ++ ukan.s ! obj ! agent ;
    } ;

  izan : Verb = {s =
    table { Sg1 => "naiz" ;
            Sg2 => "zara" ;
            Sg3 => "da" ;
            Pl1 => "gara" ;
            Pl2 => "zarete" ;
            Pl3 => "dira" }
    } ;

  -- NB. this overgenerates, don't use as a resource in learning Basque.
  -- Check rather https://upload.wikimedia.org/wikipedia/commons/3/36/Nor_Nori_Nork_taula_osoa.png
  ukan : Verb2 = {s =
    \\obj,ag =>
       let prefix : Str = object ! obj ;
           suffix : Str = agent ! ag ! obj ;
        in prefix + suffix
  } where {
      object : Agr => Str = table {
        Sg1 => "nau"   ; Pl1 => "gaitu" ;
        Sg2 => "zaitu" ; Pl2 => "zaituzte" ;
        Sg3 => "du"    ; Pl3 => "ditu" } ;

      agent : Agr => Agr => Str = \\a,o => case <a,o> of {
        -- Impossible forms raise exception
        <Sg1, Sg1|Pl1> => nonExist ;
        <Sg2, Sg2|Pl2> => nonExist ;
        <Pl1, Sg1|Pl1> => nonExist ;
        <Pl2, Sg2|Pl2> => nonExist ;

        -- Existing forms
        <Sg1, _> => "t" ;
        <Sg2, _> => "zu" ;
        <Sg3, _> => [] ;
        <Pl1, _> => "gu" ;
        <Pl2, _> => "zue" ;
        -- If agent is Pl3, the morpheme is "zte" before following
        <Pl3, Pl1|Sg2|Pl3> => "zte" ;
        -- Otherwise the Pl3 agent morpheme is "te"
        <Pl3, _>  => "te"
        } ;
  } ;
}
