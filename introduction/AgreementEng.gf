{- This is a restricted version of the mini resource, made to illustrate verbal agreement.
For simplicity, we omit tense and a whole bunch of other things.

This is for the ordinary English, which marks subject in the verb:
   I sleep-∅	      ~ the cat sleep-s
   I drink-∅ water	~ the cat drink-s water -}
concrete AgreementEng of Agreement = {

  param
    Agr = Sg3 | Other ;
    Case = Nom | Acc ;

  lincat
    V,
    V2,
    VP = {s : Agr => Str} ;
    NP = {s : Case => Str ; a : Agr} ;
    Cl = {s : Str} ;

  lin
-----------------------
-- Lexical functions --
-----------------------

    i_NP = {
      s = table {
            Nom => "I" ;
            Acc => "me" } ;
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
    UseV v = v ;

    -- : V2 -> NP -> VP ;
    ComplV2 v2 np = {s = \\agr => v2.s ! agr ++ np.s ! Acc} ;

    -- : NP -> VP -> Cl ;
    PredVP np vp = {s = np.s ! Nom ++ vp.s ! np.a} ;

---------------------------------------
-- Helper opers to reduce repetition --
---------------------------------------
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
