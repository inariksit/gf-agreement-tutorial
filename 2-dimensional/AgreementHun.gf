concrete AgreementHun of Agreement = open Prelude in {

lincat
  V,
  VP = Verb ;
  V2 = Verb2 ;
  NP = NounPhrase ;

lin
  sleep_V = mkV "alszok" "alszol" "alszik"
                "alszunk" "alszotok" "alszanak" ;

  drink_V2 =
    let indefV : Verb = mkV "iszok" "iszol" "iszik"
                            "iszunk" "isztok" "isznak" ;
        defV : Verb = mkV "iszom" "iszod" "issza"
                          "isszuk" "isszátok" "isszák" ;
     in nom_accV2 indefV defV ;

  have_V2 =
    let copula : Verb = mkV "vagyok" "vagy" "van" "vagyunk" "vagytok" "vannak" ;
        megvan : Verb = copula ** {s = \\vf => "meg" + copula.s ! vf} ;
        subjcase : Case = Dat ;
        objcase : Case = Nom ;
     in mkV2 subjcase objcase copula megvan ;

  UseV v = v ;

  ComplV2 v2 obj = v2 ** {
    s = \\agr =>
      case <v2.sc, v2.c2> of {
        -- If subj case is dat and obj case Nom, all agreement comes from obj.
        -- Also change word order to SOV.
        <Dat, Nom> => obj.s ! v2.c2 ++ v2.s ! obj.d ! obj.a ;

        -- Otherwise subject agreement comes from the subject, not known yet.
        _ => v2.s ! obj.d ! agr ++ obj.s ! v2.c2}
    } ;

  PredVP subj vp = {s = subj.s ! vp.sc ++ vp.s ! subj.a} ;

  i_NP = mkNP "én" "engem" "nekem" Sg1 Def ;
  you_NP = mkNP "te" "téged" "neked" Sg2 Def ;
  water_NP = mkNP "víz" "vizet" "víznek" Sg3 Indef ;


param
  Agr = Sg1 | Sg2 | Sg3 | Pl1 | Pl2 | Pl3 ;
  Definiteness = Def | Indef ;
  Case = Nom | Acc | Dat ; -- Most cases omitted here

oper
  Verb : Type =  {s :                 Agr => Str ; sc : Case} ;
  Verb2 : Type = {s : Definiteness => Agr => Str ; sc : Case ; c2 : Case} ;
  NounPhrase : Type = {s : Case => Str ; a : Agr ; d : Definiteness} ;

  mkV : (x1,_,_,_,_,x6 : Str) -> Verb = \sg1,sg2,sg3,pl1,pl2,pl3 -> {
      s = table { Sg1 => sg1 ;
                  Sg2 => sg2 ;
                  Sg3 => sg3 ;
                  Pl1 => pl1 ;
                  Pl2 => pl2 ;
                  Pl3 => pl3 } ;
      sc = Nom
    } ;

  mkV2 : (sc, c2 : Case) -> Verb -> Verb -> Verb2 = \sc,c2,idf,dfn -> {
    s = table {Indef => idf.s ;
               Def => dfn.s } ;
    sc = sc ; -- Subject case
    c2 = c2 ; -- Object case
    } ;

  nom_accV2 : Verb -> Verb -> Verb2 = mkV2 Nom Acc ;

  mkNP : (nom,acc,dat : Str) -> Agr -> Definiteness -> NounPhrase =
    \nom,acc,dat,agr,d -> {
    s = table { Nom => nom ;
                Acc => acc ;
                Dat => dat } ;
    a = agr ;
    d = d ;
  } ;

}
