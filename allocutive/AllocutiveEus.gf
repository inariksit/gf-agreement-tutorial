concrete AllocutiveEus of Allocutive = open Prelude in {

  lincat
    V,
    V2,
    V3,
    VP = VerbPhrase ;
    NP = NounPhrase ;
  lin
    UseV v = v ;

    ComplV2 v2 np = {
      s = np.s ! Abs ++ v2.s ; -- store object in
      v = Trans np.a ; -- The object's agreement is now recorded in the VP
      } ;

    ComplV3 v3 do io = {
      s = do.s ! Abs ++ io.s ! Dat ++ v3.s ;
      v = Ditrans (agr2num do.a) io.a
      } ;

    PredVP np vp =
      let subj : Str = case vp.v of {
            Intrans => np.s ! Abs ;
            _ => np.s ! Erg } ;
          aux : Str = case vp.v of {
            Intrans => izan ! np.a ;
            Trans objAgr => ukan ! objAgr ! np.a ;
            Ditrans doAgr ioAgr => edun ! doAgr ! np.a ! ioAgr -- NB. arg order is different from ukan
            } ;
      in {s = subj ++ vp.s ++ aux} ;

    -- These verbs use the auxiliaries defined in opers
    come_V = mkV "etorri" ;
    see_V2 = mkV2 "ikusi" ;
    give_V3 = mkV3 "eman" ;

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
  param
    Valency = Intrans
            | Trans Agr
            | Ditrans Number Agr ;
  oper
    VerbPhrase : Type = {
      s : Str ; -- the participle, which carries only a small part of inflection.
                -- In this simplified grammar, we assume always perfective aspect.
      v : Valency
      } ;

    mkV : Str -> VerbPhrase = \prc -> {
      s = prc ;
      v = Intrans ;
      } ;
    mkV2 : Str -> VerbPhrase = \prc -> mkV prc ** {
      v = Trans Hau -- Default argument, overridden in ComplV2
      } ;
    mkV3 : Str -> VerbPhrase = \prc -> mkV prc ** {
      v = Ditrans Sg Hau -- Default argument, overridden in ComplV3
      } ;

    -- The actual inflecting verbs
    -- Not part of the lincat of V, V2, V3 nor VP.
    Verb  : Type =                  Agr => Str ;
    Verb2 : Type =           Agr => Agr => Str ;
    Verb3 : Type = Number => Agr => Agr => Str ;

    -- Intransitive auxiliary
    izan : Verb = table {
      Ni    => "naiz" ;
      Hi _  => "haiz" ;
      Zu    => "zara" ;
      Hau   => "da" ;
      Gu    => "gara" ;
      Zuek  => "zarete" ;
      Hauek => "dira" } ;

    -- Transitive auxiliary.
    -- The first argument is the object, second is subject.
    ukan : Verb2 = table {
      Ni => table {
        Ni|Gu => nonExist ;
        Hi Fem  => "naun" ;
        Hi Masc => "nauk" ;
        Zu => "nauzu" ;
        Hau => "nau" ;
        Zuek => "nauzue" ;
        Hauek => "naute"
        } ;
      Hi _ => table {
        Ni => "haut" ;
        Hau => "hau" ;
        Gu => "haugu" ;
        Hauek => "haute" ;
        _ => nonExist
        } ;
      Zu => table {
        Ni => "zaitut" ;
        Gu => "zaitugu" ;
        Hau => "zaitu" ;
        Hauek => "zaituzte" ;
        _ => nonExist
        } ;
      Gu => table {
        Ni|Gu => nonExist ;
        Hi Fem => "gaitun" ;
        Hi Masc => "gaituk" ;
        Zu => "gaituzu" ;
        Hau => "gaitu" ;
        Zuek => "gaituzue" ;
        Hauek => "gaituzte"
        } ;
      Hau => table {
        Ni => "dut" ;
        Hi Fem => "dun" ;
        Hi Masc => "duk" ;
        Zu => "duzu" ;
        Hau => "du" ;
        Gu => "dugu" ;
        Zuek => "duzue" ;
        Hauek => "dute"
        } ;
      Zuek => table {
        Ni => "zaituztet" ;
        Hau => "zaituzte" ;
        Gu => "zaituztegu" ;
        Hauek => "zaituztete" ;
        _ => nonExist
        } ;
      Hauek => table { -- they <verb> …
        Ni      => "ditut" ;   -- me
        Hi Fem  => "ditun" ;   -- you.Sg.Fam.Fem
        Hi Masc => "dituk" ;   -- you.Sg.Fam.Masc
        Zu      => "dituzu" ;  -- you.Sg.Pol
        Hau     => "ditu" ;    -- him/her/it
        Gu      => "ditugu" ;  -- us
        Zuek    => "dituzue" ; -- you.Pl
        Hauek   => "dituzte"   -- them
        }
      } ;

    -- The allocutive forms of ukan are otherwise like edun, except for plural.
    -- TODO: confirm this. I got this list from staring a dump from a morphological analyser
    -- so I might be wrong.
    allocutive_ukan : Verb3 = \\abs,erg,dat =>
      case <abs,erg,dat> of {
        <Pl, Ni, Hi Fem> => "ditinat" ;
        <Pl, Ni, Hi Masc> => "ditiat" ;
        <Pl, Hi Fem, Hau> => "ditun" ;
        <Pl, Hi Masc, Hau> => "dituk" ;
        <Pl, Hau, Hi Fem> => "ditin" ;
        <Pl, Hau, Hi Masc> => "ditik" ;
        <Pl, Gu, Hi Fem> => "ditinagu" ;
        <Pl, Gu, Hi Masc> => "ditiagu" ;
        <Pl, Hauek, Hi Fem> => "ditizten" ;
        <Pl, Hauek, Hi Masc> => "ditiztek" ;
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
          Hi Fem  => "dizkinat" ; -- you.Sg.Fam.Fem   -- "ditinat"
          Hi Masc => "dizkiat" ;  -- you.Sg.Fam.Masc  -- "ditiat"
          Zu => "dizkizut" ;      -- you.Sg.Pol
          Hau => "dizkiot" ;      -- him/her/it
          Zuek => "dizkizuet" ;   -- you.Pl
          Hauek => "dizkiet"      -- them
          } ;
        Hi Fem => table {
          Ni => "dizkidan" ;
          Hau => "dizkion" ; -- "ditun"
          Gu => "dizkigun" ;
          Hauek => "dizkien" ;
          _ => nonExist
          } ;
        Hi Masc => table {
          Ni => "dizkidak" ;
          Hau => "dizkiok" ; -- "dituk"
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
          Ni => "dizkiot" ;
          Hi Fem => "dizkion" ;  -- "ditin"
          Hi Masc => "dizkiok" ; -- "ditik"
          Zu => "dizkiozu" ;
          Hau => "dizkio" ;
          Gu => "dizkiogu" ;
          Zuek => "dizkiozue" ;
          Hauek => "dizkiote"
          } ;
        Gu => table {
          Ni|Gu => nonExist ;
          Hi Fem  => "dizkinagu" ; -- "ditinagu"
          Hi Masc => "dizkiagu" ;  -- "ditiagu"
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
          Hi Fem => "dizkinate" ; -- "ditizten"
          Hi Masc => "dizkiate" ; -- "ditiztek"
          Zu => "dizkizute" ;
          Hau => "dizkiote" ;
          Gu => "dizkigute" ;
          Zuek => "dizkizuete" ;
          Hauek => "dizkiete"
          }
        }
      } ;
}
