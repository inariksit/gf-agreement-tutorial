{- This is a restricted version of the mini resource, made to illustrate verbal agreement.
For simplicity, we omit tense and a whole bunch of other things.

This is for Like-English-But-Object-Marking, LEBOM for short.
LEBOM behaves otherwise like English, with two differences:
 * For intransitive verbs, don't mark anything.
 * For transitive verbs, mark the object.
Example:
   I sleep-∅	      ~ the cat sleep-∅
   I drink-∅ me	    ~ the cat drink-∅ me
   I drink-s water	~ the cat drink-s water -}

concrete AgreementLEBOM of Agreement = AgreementEng [
  -- We reuse all that is same in ordinary English.
  Agr, Case,   -- param
    Sg3, Other,             -- Not strictly necessary to import,
    Nom, Acc,               -- just to remind what the values are.

  Cl, NP, V2,   -- lincat

  i_NP, you_NP, -- lin
   john_NP, water_NP,
   drink_V2,

  mkNP, mkV2    -- oper
  ] ** {

  -- Here only cats and funs that are different from ordinary English.
  lincat
    V,
    VP = {s : Str} ;

    -- V2 is the same as in ordinary English:
    --    V2 = {s : Agr => Str} ;
    -- But the *meaning* of the inflection table is the opposite:
    -- "drinks" agrees with a Sg3 object, "drink" agrees with other objs.

  lin
    sleep_V = {s = "sleep"} ;

    -- : V -> VP ;
    UseV v = v ;

    -- : V2 -> NP -> VP ;
    ComplV2 v2 np = {s = v2.s ! np.a ++ np.s ! Acc} ;

    -- : NP -> VP -> Cl ;
    PredVP np vp = {s = np.s ! Nom ++ vp.s} ;

}
