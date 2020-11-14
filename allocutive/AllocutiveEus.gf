concrete AllocutiveEus of Allocutive = open Prelude in {

  lincat
    V, V2, V3, Cl = SS ;
    VP = VerbPhrase ;
    NP = NounPhrase ;

  lin
    UseV v = v ** {
      a = Intrans
      } ;

    ComplV2 v2 np = {
      s = np.s ! Abs ++ v2.s ; -- store object in
      a = Trans np.a ; -- The object's agreement is now recorded in the VP
      } ;

    ComplV3 v3 do io = {
      s = do.s ! Abs ++ io.s ! Dat ++ v3.s ;
      a = Ditrans (agr2num do.a) io.a
      } ;

    CompNP np = {
      s = np.s ! Abs ;
      a = Intrans
      } ;

    PredVP = predVP ;

    PredVPFem = predVPGender Fem ;

    PredVPMasc = predVPGender Masc ;

    -- Verbs
    come_V = mkV "etorri" ;
    see_V2 = mkV "ikusi" ;
    give_V3 = mkV "eman" ;

    -- I'm letting all pronouns be prodropped, and just affect the agreement of verb.
    i_NP = pron Ni ;
    youSgFamFem_NP = pron (Hi Fem) ;
    youSgFamMasc_NP = pron (Hi Masc) ;
    youSgPol_NP = pron Zu ;
    he_she_it_NP = pron Hau ;
    we_NP = pron Gu ;
    youPl_NP = pron Zuek ;
    they_NP = pron Hauek ;

    -- Other NPs
    beer_NP = mkNP "garagardoak" "garagardoa" "garagardoari" Hau ;
    cats_NP = mkNP "katuek" "katuak" "katuei" Hauek ;

-------------------------------------------------------------------------------------

  -- Clauses
  oper
    getSubj : NounPhrase -> VerbPhrase -> Str = \np,vp ->
      case vp.a of {
        Intrans => np.s ! Abs ;
        _       => np.s ! Erg
      } ;

    predVP : NounPhrase -> VerbPhrase -> SS = \np,vp -> {
      s = subj ++ vp.s ++ aux
      } where {
        getAux : NounPhrase -> VerbPhrase -> Str = \np,vp ->
          case vp.a of {
            Intrans => izan ! np.a ;
            Trans objAgr => ukan ! objAgr ! np.a ;
            Ditrans doAgr ioAgr => edun ! doAgr ! np.a ! ioAgr
          } ; -- NB. arg order is different from ukan
        subj : Str = getSubj np vp ;
        aux : Str = getAux np vp ;
      } ;

    predVPGender : Gender -> NounPhrase -> VerbPhrase -> SS = \g,np,vp -> {
      s = subj ++ vp.s ++ aux
      } where {
        getAuxAllocutive : Gender -> NounPhrase -> VerbPhrase -> Str = \g,np,vp ->
          case vp.a of {
            Intrans => allocutive_izan ! g ! np.a ;
            Trans objAgr => allocutive_ukan ! g ! objAgr ! np.a ;
            Ditrans doAgr ioAgr => allocutive_edun ! g ! doAgr ! np.a ! ioAgr
          } ;
        subj : Str = getSubj np vp ;
        aux : Str = getAuxAllocutive g np vp ;
      } ;

  -- Noun phrases
  param
    Agr = Ni | Hi Gender | Zu | Hau | Gu | Zuek | Hauek ;
    Gender = Fem | Masc ;
    Case = Abs | Erg | Dat ; -- Only the cases needed for this small grammar
    Number = Sg | Pl ;
  oper
    NounPhrase : Type = {s : Case => Str ; a : Agr} ;
    mkNP : (erg,abs,dat : Str) -> Agr -> NounPhrase = \erg,abs,dat,ag -> {
      s = table {Erg => erg ; Abs => abs ; Dat => dat} ;
      a = ag ;
      } ;

    pron : Agr -> NounPhrase = mkNP [] [] [] ;

    agr2num : Agr -> Number = \ag ->
      case ag of {Gu|Zuek|Hauek => Pl ; _ => Sg} ;

  -- Verbs and VPs

    -- Type aliases used in inflection tables
    DObj : Type = Number ;
    IObj, Obj, Subj : Type = Agr ;

  param
    ObjAgr = Intrans
           | Trans Obj
           | Ditrans DObj IObj ;
  oper
    VerbPhrase : Type = {
      s : Str ; -- the participle, which carries only a small part of inflection.
                -- In this simplified grammar, we assume always perfective aspect.
      a : ObjAgr
      } ;

    mkV : Str -> SS = \prc -> {
      s = prc ;
      } ;

    -- The actual inflecting verbs
    -- Not part of the lincat of V, V2, V3 nor VP.
    Verb  : Type =                 Subj => Str ;
    Verb2 : Type =          Obj => Subj => Str ;
    Verb3 : Type = DObj => Subj => IObj => Str ;

    -- Intransitive auxiliary
    izan : Verb = table {
      Ni    => "naiz" ;
      Hi _  => "haiz" ;
      Zu    => "zara" ;
      Hau   => "da" ;
      Gu    => "gara" ;
      Zuek  => "zarete" ;
      Hauek => "dira" } ;

    -- Allocutive forms of intransitive auxiliary
    -- There are only special forms for P1 and P3.
    allocutive_izan : Gender => Verb = \\gend,abs =>
      case abs of {
        Hi _ | Zu | Zuek => izan ! abs ;
        _                => ukan ! abs ! Hi gend -- Spurious P2 ergative agreement
      } ;

    -- Transitive auxiliary.
    -- The first argument is the object, second is subject.
    ukan : Verb2 = table {
      Ni => table {
        Ni|Gu   => nonExist ; -- I/we            <verb> me (doesn't exist, need to use reflexive construction)
        Hi Fem  => "naun" ;   -- you.Sg.Fam.Fem  <verb> me
        Hi Masc => "nauk" ;   -- you.Sg.Fam.Masc <verb> me
        Zu      => "nauzu" ;  -- you.Sg.Pol      <verb> me
        Hau     => "nau" ;    -- he/she/it       <verb> me
        Zuek    => "nauzue" ; -- you.Pl          <verb> me
        Hauek   => "naute"    -- they            <verb> me
        } ;
      Hi _ => table {
        Ni    => "haut" ;  -- I         <verb> you.Sg.Fam.*
        Hau   => "hau" ;   -- he/she/it <verb> you.Sg.Fam.*
        Gu    => "haugu" ; -- we        <verb> you.Sg.Fam.*
        Hauek => "haute" ; -- they      <verb> you.Sg.Fam.*
        _     => nonExist  -- you.*     <verb> you.Sg.Fam.* (doesn't exist, need to use reflexive construction)
        } ;
      Zu => table {
        Ni    => "zaitut" ;
        Gu    => "zaitugu" ;
        Hau   => "zaitu" ;
        Hauek => "zaituzte" ;
        _     => nonExist
        } ;
      Gu => table {
        Ni|Gu   => nonExist ;
        Hi Fem  => "gaitun" ;
        Hi Masc => "gaituk" ;
        Zu      => "gaituzu" ;
        Hau     => "gaitu" ;
        Zuek    => "gaituzue" ;
        Hauek   => "gaituzte"
        } ;
      Hau => table {
        Ni      => "dut" ;
        Hi Fem  => "dun" ;
        Hi Masc => "duk" ;
        Zu      => "duzu" ;
        Hau     => "du" ;
        Gu      => "dugu" ;
        Zuek    => "duzue" ;
        Hauek   => "dute"
        } ;
      Zuek => table {
        Ni    => "zaituztet" ;
        Hau   => "zaituzte" ;
        Gu    => "zaituztegu" ;
        Hauek => "zaituztete" ;
        _     => nonExist
        } ;
      Hauek => table {
        Ni      => "ditut" ;
        Hi Fem  => "ditun" ;
        Hi Masc => "dituk" ;
        Zu      => "dituzu" ;
        Hau     => "ditu" ;
        Gu      => "ditugu" ;
        Zuek    => "dituzue" ;
        Hauek   => "dituzte"
        }
      } ;

    -- The allocutive forms of ukan (transitive auxiliary)
    allocutive_ukan : Gender => Verb2 = \\gend,abs,erg =>
      case <gend,abs,erg> of {
       -- Absolutive P1 Sg
       <Fem,  Ni, Hau> => "nain" ;
       <Masc, Ni, Hau> => "naik" ;
       <Fem,  Ni, Hauek> => "naiten" ;
       <Masc, Ni, Hauek> => "naitek" ;

       -- Absolutive P1 Pl
       <Fem,  Gu, Hau> => "gaitin" ;
       <Masc, Gu, Hau> => "gaitik" ;
       <Fem,  Gu, Hauek> => "gaitizten" ;
       <Masc, Gu, Hauek> => "gaitiztek" ;

       -- Absolutive P3 Sg
       <Fem,  Hau, Hauek> => "diten" ;
       <Masc, Hau, Hauek> => "ditek" ;

       -- Absolutive P3 Pl
       <Fem,  Hauek, Ni>    => "ditinat" ;
       <Masc, Hauek, Ni>    => "ditiat" ;
       <Fem,  Hauek, Hau>   => "ditin" ;
       <Masc, Hauek, Hau>   => "ditik" ;
       <Fem,  Hauek, Gu>    => "ditinagu" ;
       <Masc, Hauek, Gu>    => "ditiagu" ;
       <Fem,  Hauek, Hauek> => "ditizten" ;
       <Masc, Hauek, Hauek> => "ditiztek" ;

       -- Any P2 argument: no special forms
       <_, _, Zu|Hi _|Zuek> => ukan ! abs ! erg ;
       <_, Zu|Hi _|Zuek, _>  => ukan ! abs ! erg ;

       -- Rest of the forms: spurious P2 dative agreement from edun
       _ => edun ! (agr2num abs) ! erg ! Hi gend
      } ;

    allocutive_edun : Gender => Verb3 = \\gend,abs,erg,dat =>
      case <gend,abs,erg,dat> of {
        -- Sg P3 absolutive with …
          -- Sg P3 ergative
        <Fem,  Sg, Hau, Ni>    => "zidan" ; -- corresponds to "dit" in edun
        <Masc, Sg, Hau, Ni>    => "zidak" ;
        <Fem,  Sg, Hau, Hau>   => "zion" ;  -- corrresponds to "dio" in edun
        <Masc, Sg, Hau, Hau>   => "ziok" ;
        <Fem,  Sg, Hau, Gu>    => "zigun" ; -- corresponds to "digu" in edun
        <Masc, Sg, Hau, Gu>    => "ziguk" ;
        <Fem,  Sg, Hau, Hauek> => "zien" ;  -- corrresponds to "die" in edun
        <Masc, Sg, Hau, Hauek> => "ziek" ;

          -- Pl P3 ergative
        <Fem,  Sg, Hauek, Ni>    => "zidaten" ; -- corresponds to "didate" in edun
        <Masc, Sg, Hauek, Ni>    => "zidatek" ;
        <Fem,  Sg, Hauek, Hau>   => "zioten" ;  -- corrresponds to "diote" in edun
        <Masc, Sg, Hauek, Hau>   => "ziotek" ;
        <Fem,  Sg, Hauek, Gu>    => "ziguten" ; -- corresponds to "digute" in edun
        <Masc, Sg, Hauek, Gu>    => "zigutek" ;
        <Fem,  Sg, Hauek, Hauek> => "zieten" ;  -- corrresponds to "diete" in edun
        <Masc, Sg, Hauek, Hauek> => "zietek" ;

          -- Sg P1 ergative
        <Fem,  Sg, Ni, Hau>    => "zionat" ; -- corresponds to "diot" in edun
        <Masc, Sg, Ni, Hau>    => "zioat" ;
        <Fem,  Sg, Ni, Hauek>  => "zienat" ; -- corrresponds to "diet" in edun
        <Masc, Sg, Ni, Hauek>  => "zieat" ;

          -- Pl P1 ergative
        <Fem,  Sg, Gu, Hau>    => "zionagu" ; -- corresponds to "diogu" in edun
        <Masc, Sg, Gu, Hau>    => "zioagu" ;
        <Fem,  Sg, Gu, Hauek>  => "zienagu" ; -- corrresponds to "diegu" in edun
        <Masc, Sg, Gu, Hauek>  => "zieagu" ;

        -- Pl P3 absolutive with …
          -- Sg P3 ergative
        <Fem,  Pl, Hau, Ni>    => "zizkidan" ; -- corresponds to "dizkit" in edun
        <Masc, Pl, Hau, Ni>    => "zizkidak" ;
        <Fem,  Pl, Hau, Hau>   => "zizkion" ;  -- corrresponds to "dizkio" in edun
        <Masc, Pl, Hau, Hau>   => "zizkiok" ;
        <Fem,  Pl, Hau, Gu>    => "zizkigun" ; -- corresponds to "dizkigu" in edun
        <Masc, Pl, Hau, Gu>    => "zizkiguk" ;
        <Fem,  Pl, Hau, Hauek> => "zizkien" ;  -- corrresponds to "dizkie" in edun
        <Masc, Pl, Hau, Hauek> => "zizkiek" ;

          -- Pl P3 ergative
        <Fem,  Pl, Hauek, Ni>    => "zizkidaten" ; -- corresponds to "dizkidate" in edun
        <Masc, Pl, Hauek, Ni>    => "zizkidatek" ;
        <Fem,  Pl, Hauek, Hau>   => "zizkioten" ;  -- corrresponds to "dizkiote" in edun
        <Masc, Pl, Hauek, Hau>   => "zizkiotek" ;
        <Fem,  Pl, Hauek, Gu>    => "zizkiguten" ; -- corresponds to "dizkigute" in edun
        <Masc, Pl, Hauek, Gu>    => "zizkigutek" ;
        <Fem,  Pl, Hauek, Hauek> => "zizkieten" ;  -- corrresponds to "dizkiete" in edun
        <Masc, Pl, Hauek, Hauek> => "zizkietek" ;

          -- Sg P1 ergative
        <Fem,  Pl, Ni, Hau>    => "zizkionat" ; -- corresponds to "dizkiot" in edun
        <Masc, Pl, Ni, Hau>    => "zioat" ;
        <Fem,  Pl, Ni, Hauek>  => "zizkienat" ; -- corrresponds to "dizkiet" in edun
        <Masc, Pl, Ni, Hauek>  => "zizkieat" ;

          -- Pl P1 ergative
        <Fem,  Pl, Gu, Hau>    => "zizkionagu" ; -- corresponds to "dizkiogu" in edun
        <Masc, Pl, Gu, Hau>    => "zizkioagu" ;
        <Fem,  Pl, Gu, Hauek>  => "zizkienagu" ; -- corrresponds to "dizkiegu" in edun
        <Masc, Pl, Gu, Hauek>  => "zizkieagu" ;

        -- Other forms that already have P2 agreement: take as is from edun
        _ => edun ! abs ! erg ! dat

      } ;

    -- Ditransitive auxiliary.
    -- The structure is Abs => Erg => Dat
    -- for no particular reason.
    edun : Verb3 = table {
      Sg => table {
        Ni => table { -- Sg1 <verbs> Sg3 to …
          Ni|Gu => nonExist ;
          Hi Fem  => "dinat" ; -- you.Sg.Fam.Fem
          Hi Masc => "diat" ;  -- you.Sg.Fam.Masc
          Zu => "dizut" ;      -- you.Sg.Pol
          Hau => "diot" ;      -- him/her/it
          Zuek => "dizuet" ;   -- you.Pl
          Hauek => "diet"      -- them
          } ;
        Hi Fem => table {
          Ni => "didan" ;
          Hau => "dion" ;
          Gu => "digun" ;
          Hauek => "dien" ;
          _ => nonExist
          } ;
        Hi Masc => table {
          Ni => "didak" ;
          Hau => "diok" ;
          Gu => "diguk" ;
          Hauek => "diek" ;
          _ => nonExist
          } ;
        Zu => table {
          Ni => "didazu" ;
          Hau => "diozu" ;
          Gu => "diguzu" ;
          Hauek => "diezu" ;
          _ => nonExist
          } ;
        Hau => table {
          Ni => "dit" ;
          Hi Fem => "din" ;
          Hi Masc => "dik" ;
          Zu => "dizu" ;
          Hau => "dio" ;
          Gu => "digu" ;
          Zuek => "dizue" ;
          Hauek => "die"
          } ;
        Gu => table {
          Ni|Gu => nonExist ;
          Hi Fem  => "dinagu" ;
          Hi Masc => "diagu" ;
          Zu => "dizugu" ;
          Hau => "diogu" ;
          Zuek => "dizuegu" ;
          Hauek => "diegu"
          } ;
        Zuek => table {
          Ni => "didazue" ;
          Hau => "diozue" ;
          Gu => "diguzue" ;
          Hauek => "diezue" ;
          _ => nonExist
          } ;
        Hauek => table {
          Ni => "didate" ;
          Hi Fem => "dinate" ;
          Hi Masc => "diate" ;
          Zu => "dizute" ;
          Hau => "diote" ;
          Gu => "digute" ;
          Zuek => "dizuete" ;
          Hauek => "diete"
          }
      } ;
      Pl => table {
        Ni => table { -- Sg1 <verb> Pl3 to …
          Ni|Gu => nonExist ;
          Hi Fem  => "dizkinat" ; -- you.Sg.Fam.Fem   -- allocutive transitive: "ditinat"
          Hi Masc => "dizkiat" ;  -- you.Sg.Fam.Masc  -- allocutive transitive: "ditiat"
          Zu => "dizkizut" ;      -- you.Sg.Pol
          Hau => "dizkiot" ;      -- him/her/it
          Zuek => "dizkizuet" ;   -- you.Pl
          Hauek => "dizkiet"      -- them
          } ;
        Hi Fem => table {
          Ni => "dizkidan" ;
          Hau => "dizkion" ;
          Gu => "dizkigun" ;
          Hauek => "dizkien" ;
          _ => nonExist
          } ;
        Hi Masc => table {
          Ni => "dizkidak" ;
          Hau => "dizkiok" ;
          Gu => "dizkiguk" ;
          Hauek => "dizkiek" ;
          _ => nonExist
          } ;
        Zu => table {
          Ni => "dizkidazu" ;
          Hau => "dizkiozu" ;
          Gu => "dizkiguzu" ;
          Hauek => "dizkiezu" ;
          _ => nonExist
          } ;
        Hau => table {
          Ni => "dizkit" ;
          Hi Fem => "dizkin" ;  -- allocutive: "ditin"
          Hi Masc => "dizkik" ; -- allocutive: "ditik"
          Zu => "dizkizu" ;
          Hau => "dizkio" ;
          Gu => "dizkigu" ;
          Zuek => "dizkizue" ;
          Hauek => "dizkie"
          } ;
        Gu => table {
          Ni|Gu => nonExist ;
          Hi Fem  => "dizkinagu" ; -- allocutive: "ditinagu"
          Hi Masc => "dizkiagu" ;  -- allocutive: "ditiagu"
          Zu => "dizkizugu" ;
          Hau => "dizkiogu" ;
          Zuek => "dizkizuegu" ;
          Hauek => "dizkiegu"
          } ;
        Zuek => table {
          Ni => "dizkidazue" ;
          Hau => "dizkiozue" ;
          Gu => "dizkiguzue" ;
          Hauek => "dizkiezue" ;
          _ => nonExist
          } ;
        Hauek => table {
          Ni => "dizkidate" ;
          Hi Fem => "dizkinate" ;
          Hi Masc => "dizkiate" ;
          Zu => "dizkizute" ;
          Hau => "dizkiote" ;
          Gu => "dizkigute" ;
          Zuek => "dizkizuete" ;
          Hauek => "dizkiete"
          }
        }
      } ;
}
