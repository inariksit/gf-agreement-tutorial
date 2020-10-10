# Allocutive agreement

This GF grammar demonstrates the allocutive agreement in Basque. As an example, here's the sentence "I see cats" depending on if it's adressed to nobody in particular, to a woman or to a man.
The addressee is encoded in the verb form (_ditut_, _ditinat_ or _ditiat_).

```
Allocutive> l -treebank PredVP i_NP (ComplV2 see_V2 cats_NP)
AllocutiveEng: I see cats
AllocutiveEus: katuak ikusi ditut

Allocutive> l -treebank PredVPFem i_NP (ComplV2 see_V2 cats_NP)
AllocutiveEng: I see cats ( spoken to a woman )
AllocutiveEus: katuak ikusi ditinat

Allocutive> l -treebank PredVPMasc i_NP (ComplV2 see_V2 cats_NP)
AllocutiveEng: I see cats ( spoken to a man )
AllocutiveEus: katuak ikusi ditiat
```

Here's a citeable source: [Verbal allocutivity in a crosslinguistic perspective](https://core.ac.uk/download/pdf/47323396.pdf) (Antonov, 2015).

> Allocutivity is a term coined to describe a phenomenon in Basque whereby, in certain pragmatic (and syntactic) circumstances, an addressee who is not an argument of the verb is systematically encoded in all declarative main clause conjugated verb forms. 
