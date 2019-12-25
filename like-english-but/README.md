# Post 1/3: Like-English-but-…

This directory contains the Like-English-but-… examples from my blog post.

I have left Like-English-but-differential-object-marking as an exercise for the reader. 
If you want to do it yourself, keep reading.

### Assignment: Finish `AgreementLEBDAM.gf`

Who says you need to do your argument marking based on its *role*? Instead, you can mark based on something inherent in the argument.

For instance, if a transitive verb has one animate and one inanimate argument, mark the verb based on the animate argument, regardless or their roles. In the following examples, the verb *write* agrees with its animate argument whenever it has one.

|  Animate > Inanimate      |Sg1 subject            | Sg3 subject                     |
|:---------------:|:----------------------|:--------------------------------|
|**Sg1 object**   |<font color="999898">I write-<strong>∅</strong> me</font>       | the blog write-<strong><font color="purple">∅</font></strong> <font color="purple">me</font>         |
|**Sg3 object**   |<font color="green">I</font>  write-<strong><font color="green">∅</font></strong> the blog | <font color="999898">the blog write<strong>s</strong> the blog</font>   |


If there are two animate arguments, they can be ordered by person hierarchy, such as 1st > 2nd > 3rd. Here's an example, where *love* agrees with the argument that is highest in the person hierarchy:

| P1 > P2 > P3    | P2 subject         | P3 subject          |
|:---------------:|:-------------------|:--------------------|
|**P2 object**    |you love-<strong><font color="green">∅</font></strong> you  | he love-<strong><font color="green">∅</font></strong> you   |
|**P3 object**    |you love-<strong><font color="green">∅</font></strong> him  | he love-<strong><font color="purple">s</font></strong> him   |


Some instructions:

* Intransitive verbs mark their only argument.
* You will need to modify the `NP` category (and obviously all verbal categories). However, you can still ignore case in NP: *you love he* and *the blog write I* are acceptable output, if you only want to concentrate on person marking in the verb.
* You don't need to use animacy or person hierarchy—you can make your system as wild as you want, as long as you can encode it in GF.

If you want feedback, make your solution available to me! You can fork this repository and make a pull request, or send your code to [inari.listenmaa@gmail.com](mailto:inari.listenmaa@gmail.com).
