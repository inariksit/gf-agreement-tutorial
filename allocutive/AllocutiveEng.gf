concrete AllocutiveEng of Allocutive = open (S=SyntaxEng), ParadigmsEng, (L=LexiconEng) in {

  lincat
    NP = S.NP ;
    V = S.V ;
    V2 = S.V2 ;
    V3 = S.V3 ;
    VP = S.VP ;
    Cl = S.Utt ;

  lin
    -- Pronouns
    i_NP = S.i_NP ;
    youSgFamFem_NP,
    youSgFamMasc_NP,
    youSgPol_NP,
    youPl_NP = S.you_NP ;
    he_she_it_NP = S.it_NP ;
    we_NP = S.we_NP ;
    they_NP = S.they_NP ;

    -- Noun phrases
    beer_NP = S.mkNP L.beer_N ;
    cats_NP = S.mkNP S.aPl_Det L.cat_N ;

    come_V = L.come_V ;
    see_V2 = L.see_V2 ;
    give_V3 = L.give_V3 ;

    -- : V -> VP ;
    UseV v = S.mkVP v ;
    -- : V2 -> NP -> VP ;
    ComplV2 v2 np = S.mkVP v2 np ;
    -- : V3 -> NP -> NP -> VP ;
    ComplV3 = S.mkVP ;

    -- : NP -> VP -> Cl ;
    PredVP np vp = S.mkUtt (S.mkS (S.mkCl np vp)) ;
}
