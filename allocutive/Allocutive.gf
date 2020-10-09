abstract Allocutive = {

flags startcat=Cl ;
  cat
    NP ;
    V ;
    V2 ;
    V3 ;
    VP ;
    Cl ;

  fun
    -- Pronouns
    i_NP,
    youSgFamFem_NP,
    youSgFamMasc_NP,
    youSgPol_NP,
    he_she_it_NP,
    we_NP,
    youPl_NP,
    they_NP,

    -- Noun phrases
    beer_NP,
    cats_NP : NP ;

    come_V : V ;
    see_V2 : V2 ;
    give_V3 : V3 ;

    UseV : V -> VP ;
    ComplV2 : V2 -> NP -> VP ;
    ComplV3 : V3 -> NP -> NP -> VP ;
    PredVP : NP -> VP -> Cl ;

    -- Special constructions for allocutive agreement
    PredVPFem, PredVPMasc : NP -> VP -> Cl ;
}
