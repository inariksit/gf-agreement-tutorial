-- This is a restricted version of the mini resource, made to illustrate verbal agreement.
-- For simplicity, we omit tense and a whole bunch of other things.
--
-- This is for the ordinary English, which marks subject in the verb:
--   I sleep-∅	      ~ the cat sleep-s
--   I drink-∅ water	~ the cat drink-s water
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

    you_NP = {
      s = table {
            Nom => "you" ;
            Acc => "you" } ;
      a = Other
      } ;

    -- Tables that have the same right-hand side can we written in a shorter way.
    -- Actually two shorter ways: see how it's written in john_NP and water_NP:

    john_NP = {
      s = table {
            _ => "John"} ;
      a = Sg3
      } ;

    water_NP = {
      s = \\_ => "water" ;
      a = Sg3
      } ;

    sleep_V = {
      s = table {
        Sg3   => "sleeps" ;
        Other => "sleep" }
      } ;

    drink_V2 = {
      s = table {
        Sg3   => "drinks" ;
        Other => "drink" }
      } ;

-------------------------
-- Syntactic functions --
-------------------------
    -- : V -> VP ;
    UseV v = v ;

    -- : V2 -> NP -> VP ;
    ComplV2 v2 np = {s = \\agr => v2.s ! agr ++ np.s ! Acc} ;

    -- : NP -> VP -> Cl ;
    PredVP np vp = {s = np.s ! Nom ++ vp.s ! np.a} ;

}
