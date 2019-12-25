-- This is your homework assignment.
-- Write this grammar as instructed in
-- TODO-URL/agreement-1.html#subject-or-object-marking

concrete AgreementVariable of Agreement = {
  lincat
    V,
    V2,
    VP,
    NP,
    Cl = {s : Str} ;
  lin
    i_NP = mkNP "I" ;
    you_NP = mkNP "you" ;
    john_NP = mkNP "John" ;
    water_NP = mkNP "water" ;

    sleep_V  = mkV "sleep" ;
    drink_V2 = mkV2 "drink" ;

    UseV v = v ;
    ComplV2 v2 np = {s = v2.s ++ np.s} ;
    PredVP np vp = {s = np.s ++ vp.s ++ "**TODO:make this work properly**"} ;

  oper
    mkV,
    mkV2,
    mkNP : Str -> {s : Str} = \str -> {s = str} ;
}
