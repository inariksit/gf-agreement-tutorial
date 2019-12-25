{- This is a restricted version of the mini resource, made to illustrate verbal agreement.
For simplicity, we omit tense and a whole bunch of other things.

This is for Like-English-But-Object-Marking-and-Ergative, LEBOM-ERG for short.
LEBOM-ERG behaves otherwise like English, with two differences:
 * For intransitive verbs, mark the subject.
 * For transitive verbs, mark the object in the same way as
  subject is marked for intransitive verbs.
Example:
    me sleep-∅	      ~ the cat sleep-s
    I drink-∅ me	    ~ the cat drink-∅ me
    I drink-s water	~ the cat drink-s water -}

concrete AgreementLEBOM_ERG of Agreement = {

  param
    Agr = Sg3 | Other ;
    Case = Abs | Erg ;

  lincat
    NP = {s : Case => Str ; a : Agr} ;

    V,
    V2 = {s : Agr => Str} ;

    -- VP includes now subject case!
    VP = {s : Agr => Str ; sc : Case} ;

    Cl = {s : Str} ;

  lin
-----------------------
-- Lexical functions --
-----------------------

    i_NP = {
      s = table {
            Erg => "I" ;
            Abs => "me" } ;
      a = Other
      } ;

    you_NP = mkNP "you" Other ;
    john_NP = mkNP "John" Sg3 ;
    water_NP = mkNP "water" Sg3 ;
    sleep_V = mkV "sleep" ;
    drink_V2 = mkV2 "drink" ;

-------------------------
-- Syntactic functions --
-------------------------

    -- : V -> VP ;
    UseV v = v ** {
      sc = Abs
      } ;

    -- : V2 -> NP -> VP ;
    ComplV2 v2 np = {
      s = \\_ => v2.s ! np.a ++ np.s ! Abs ;
      sc = Erg
      } ;

    -- : NP -> VP -> Cl ;
    PredVP np vp = {s = np.s ! vp.sc ++ vp.s ! np.a} ;

oper

  mkNP : Str -> Agr -> {s : Case => Str ; a : Agr} = \str,agr -> {
    s = \\_ => str ;
    a = agr
    } ;

  mkV, mkV2 : Str -> {s : Agr => Str} = \str -> {
    s = table {
      Sg3   => str + "s" ;
      Other => str }
    } ;
}
