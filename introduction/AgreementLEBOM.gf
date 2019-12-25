-- This is a restricted version of the mini resource, made to illustrate verbal agreement.
-- For simplicity, we omit tense and a whole bunch of other things.
--
-- This is for Like-English-But-Object-Marking, LEBOM for short.
-- LEBOM behaves otherwise like English, with two differences:
--  * For intransitive verbs, always zero marking.
--  * For transitive verbs, always mark the object.
-- Example:
--   I sleep-∅	      ~ the cat sleep-∅
--   I drink-∅ me	    ~ the cat drink-∅ me
--   I drink-s water	~ the cat drink-s water

concrete AgreementLEBOM of Agreement = AgreementEng [
 -- Restricted inheritance: we don't need to redefine things that are same in ordinary English.
  Agr, Case, -- param
  Cl, NP,    -- lincat
  i_NP, you_NP, john_NP, water_NP -- lin
  ] ** {

  lincat
    V,
    VP = {s : Str} ;
    V2 = {s : Agr => Str} ;

  lin
-----------------------
-- Lexical functions --
-----------------------

    sleep_V = {s = "sleep"} ;

    -- This looks the same as in orginary English, but the *meaning* of the
    -- inflection table is the opposite: "drinks" agrees with a Sg3 object.
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
    ComplV2 v2 np = {s = v2.s ! np.a ++ np.s ! Acc} ;

    -- : NP -> VP -> Cl ;
    PredVP np vp = {s = np.s ! Nom ++ vp.s} ;

}
