
abstract Agreement = {
-- This is a restricted version of the mini resource, made to illustrate verbal agreement.
flags startcat=Cl ;
  cat
    NP ;
    V ;
    V2 ;
    VP ;
    Cl ;

  fun

    i_NP,
    you_NP,
    he_she_NP,
    we_NP,
    youPl_NP,
    they_NP,
    john_NP,
    water_NP : NP ;

    sleep_V : V ;

    eat_V2,
    have_V2,
    write_V2,
    drink_V2 : V2 ;

    UseV : V -> VP ;
    ComplV2 : V2 -> NP -> VP ;
    PredVP : NP -> VP -> Cl ;
}
