ITERTOOLS
=========
A composable, extendable iteration DSL for Lisp
-----------------------------------------------

Introduction
============
This library defines a simple, extendable iteration DSL for Lisp in a
manner similar to SETF expanders.

The interface for iteration expanders is still slightly in flux at the
moment, so beware before writing your own (or just contact the author).

Quick Examples
--------------

Lets loop over a list, also having the indexes of the items:

.. code:: cl
   
    (loop-iterator (values (count 0)
                           (list '(a b c d)))
                   (x y)
		   collect (list x y))
    =>
    ((0 A) (1 B) (2 C) (3 D))

Or if you prefer the do notation, let's sum up a pair of lists:

.. code:: cl

    (let ((sum 0))
      (do-iterator ((x y) (list '(1 2 3) '(4 5 6)) sum)
        (incf sum (+ x y))))
    => 21

	    
Kitchen sink; loop over all the lines in a file, forming a group every
time the initial character changes; skip until the first item in each
group with line-lengths less than 3, and take the first 10 of what
remains; let's include an index (just because):

.. code:: cl

  (loop-iterator
    (values (count 0)
            (group-by* (file (open "~/misc/words.txt"))
	               (lambda (x) (char-upcase (char x 0)))
		       (take (dropwhile (lambda (x) (< (length x) 3)) (:group)) 10)))
    (x y)
    collect (list x y))
  =>
  ((0
    ("aal" "aalii" "aam" "Aani" "aardvark" "aardwolf" "Aaron" "Aaronic"
     "Aaronical" "Aaronite"))
   (1
    ("baa" "baahling" "Baal" "baal" "Baalath" "Baalish" "Baalism" "Baalist"
     "Baalite" "Baalitical"))
   (2
    ("caam" "caama" "caaming" "caapeba" "caatinga" "cab" "caba" "cabaan" "caback"
     "cabaho"))
   (3
    ("daalder" "dab" "dabb" "dabba" "dabber" "dabble" "dabbler" "dabbling"
     "dabblingly" "dabblingness"))
   (4
    ("each" "eachwhere" "eager" "eagerly" "eagerness" "eagle" "eaglelike"
     "eagless" "eaglestone" "eaglet"))
   (5
    ("Faba" "Fabaceae" "fabaceous" "fabella" "fabes" "Fabian" "Fabianism"
     "Fabianist" "fabiform" "fable"))
   (6
    ("gab" "gabardine" "gabbard" "gabber" "gabble" "gabblement" "gabbler"
     "gabbro" "gabbroic" "gabbroid"))
   (7
    ("haab" "haaf" "Habab" "habanera" "Habbe" "habble" "habdalah" "Habe" "habeas"
     "habena"))
   (8
    ("Iacchic" "Iacchos" "Iacchus" "Iachimo" "iamatology" "iamb" "Iambe"
     "iambelegus" "iambi" "iambic"))
   (9
    ("Jaalin" "jab" "Jabarite" "jabbed" "jabber" "jabberer" "jabbering"
     "jabberingly" "jabberment" "Jabberwock"))
   (10
    ("Kababish" "Kabaka" "kabaragoya" "Kabard" "Kabardian" "kabaya" "Kabbeljaws"
     "kabel" "kaberu" "kabiet"))
   (11
    ("laager" "laang" "lab" "Laban" "labara" "labarum" "labba" "labber"
     "labdacism" "labdacismus"))
   (12
    ("maam" "maamselle" "Mab" "Maba" "Mabel" "Mabellona" "mabi" "Mabinogion"
     "mabolo" "Mac"))
   (13
    ("naa" "naam" "Naaman" "Naassenes" "nab" "nabak" "Nabal" "Nabalism"
     "Nabalite" "Nabalitic"))
   (14
    ("oadal" "oaf" "oafdom" "oafish" "oafishly" "oafishness" "oak" "oakberry"
     "Oakboy" "oaken"))
   (15
    ("paal" "paar" "paauw" "Paba" "pabble" "pablo" "pabouch" "pabular" "pabulary"
     "pabulation"))
   (16
    ("qasida" "qere" "qeri" "qintar" "Qoheleth" "qoph" "qua" "quab" "quabird"
     "quachil"))
   (17
    ("raad" "raash" "Rab" "rab" "raband" "rabanna" "rabat" "rabatine" "rabatte"
     "rabattement"))
   (18
    ("saa" "Saan" "Saarbrucken" "sab" "Saba" "sabadilla" "sabadine" "sabadinine"
     "Sabaean" "Sabaeanism"))
   (19
    ("taa" "Taal" "Taalbond" "taar" "Tab" "tab" "tabacin" "tabacosis" "tabacum"
     "tabanid"))
   (20
    ("uang" "Uaraycu" "Uarekena" "Uaupe" "uayeb" "Ubbenite" "Ubbonite" "uberant"
     "uberous" "uberously"))
   (21
    ("vaagmer" "vaalite" "Vaalpens" "vacabond" "vacancy" "vacant" "vacanthearted"
     "vacantheartedness" "vacantly" "vacantness"))
   (22
    ("Waac" "waag" "waapa" "waar" "Waasi" "wab" "wabber" "wabble" "wabbly"
     "wabby"))
   (23
    ("xanthaline" "xanthamic" "xanthamide" "xanthane" "xanthate" "xanthation"
     "xanthein" "xanthelasma" "xanthelasmic" "xanthelasmoidea"))
   (24
    ("yaba" "yabber" "yabbi" "yabble" "yabby" "yabu" "yacal" "yacca" "yachan"
     "yacht"))
   (25
    ("Zabaean" "zabaglione" "Zabaism" "Zaberma" "zabeta" "Zabian" "Zabism"
     "zabra" "zabti" "zabtie")))

Reference
=========

Macros
------

LOOP-ITERATOR
^^^^^^^^^^^^^

Sytnax
######

loop-iterator (iterator-form var-list loop-forms*)

=> result

Arguments and values
####################

iterator-form
  An unquoted form of iterators (see Iterators Available below)

var-list
  A list of names that will be bound sucessively to the result(s) of
  the iterator-form for each iteration.

loop-forms
  Any forms that would be acceptable in the body of CL:LOOP.  There is
  a WHILE clause that preceeds these forms, so clauses such as for, as
  or repeat are disallowed here.

Description
###########

Loops over iterator described by iterator-form, binding variables in
var-list and executing loop clauses contained in loop-forms

Iterators available
-------------------

LIST
^^^^

Syntax
######

list (&rest lists)

Arguments
#########
lists
  Any forms that evaluate to lists

Description
###########

Yields one value from each list

Terminates when any list terminates

REPEAT
^^^^^^

Syntax
######

repeat (&environment env iter &optional (count 2))

Arguments
#########

iter
  An iterator form

count
  An integer

Description
###########

iterates through *iter* and yields each value *count* times

Example:

.. code:: cl

  (loop-iterator (repeat (list '(1 2)) 3) (x) collect x)
  =>
  (1 1 1 2 2 2)
  

compress
^^^^^^^^

Syntax
######

compress (&environment env i1 i2)

Arguments
#########

Description
###########

chain
^^^

Syntax
######

chain (&environment env &rest iters)

Arguments
#########

Description
###########

count
^^^

Syntax
######

count (&optional (start 0) (step 1))

Arguments
#########

Description
###########

cycle
^^^

Syntax
######

cycle (&rest lists)

Arguments
#########

Description
###########

dropwhile
^^^^^^^^^

Syntax
######

dropwhile (&environment env predicate iter)

Arguments
#########

Description
###########

file
^^^^

Syntax
######

file (file-form &optional

Arguments
#########

Description
###########

group-by*
^^^^^^^^^

Syntax
######

group-by* (iter keyfn &optional

Arguments
#########

Description
###########

group-by
^^^^^^^^

Syntax
######

group-by (iter keyfn &optional

Arguments
#########

Description
###########

take
^^^^

Syntax
######

take (iterator &optional (count 1) &environment env)

Arguments
#########

Description
###########

values
^^^^^^

Syntax
######

values (&environment env &rest iters)

Arguments
#########

Description
###########


How it Works
============

Each iterator is defined in a manner similar to setf expanders, using DEFINE-ITERATOR-EXPANDER.  Here's a quick example of the "list" expander, which takes N lists and expands to iterating over N values:

.. code:: cl

  (define-iterator-expander list (&rest lists)
    (let ((tmps (loop repeat (length lists) collect (gensym)))
	  (store (loop repeat (length lists) collect (gensym))))
    (values
     tmps
     lists
     store
     `(if (every #'identity (list ,@tmps))
	  (values ,@(loop for item in tmps collect `(pop ,item)))
	  (values 'eoi))
     `(if (every #'identity (list ,@tmps))
	  (values ,@(loop for item in tmps collect `(pop ,item)))
	  (values 'eoi)))))

Note that iterator expaders are in a separate namespace from
functions.  Currently macros are expanded preferrentially to iterator
expanders, though whether it will stay this way is still up for debate.

An iterator expander returns up to 6 values with the semantics being:

#. A list of temporary variables
#. A list of values to which the temporary variables will initially be assigned
#. A list of store variables; these will store the current value(s) yielded by the iterator
#. An initial form; this should evaluate to the values to be assigned to the store variables for the first iteration
#. A next form; this should evaluate to the values to update the store variables to for the next iteration
#. A cleanup form; this will be evaulated when exiting the loop; execution is assured by use of  UNWIND-PROTECT

Iterator expanders use macro-lambda-list destructuring, though the
environment object they get is slightly different from the normal one;
it is a dotted pair of a lisp environment object, and the iterator
environment.  If you only pass it to GET-ITERATOR-EXPANSION then you
can treat it opaquely, but if you wish to pass it to macroexpand, then
you will need to take the CAR of it first.

End of iteration is signaled by returning the symbol 'ITERTOOLS::EOI
it is intentionally not exported to make accidental returning of it
by code unaware of ITERTOOLS impossible.

You can get the expansion with GET-ITERATOR-EXPANSION

.. code:: cl

  (get-iterator-expansion '(list '(1 2 3) '(4 5 6)))
  =>
  (#:G1589 #:G1590)
  ('(1 2 3) '(4 5 6))
  (#:G1591 #:G1592)
  (IF (EVERY #'IDENTITY (LIST #:G1589 #:G1590))
      (VALUES (POP #:G1589) (POP #:G1590))
      (VALUES 'EOI))
  (IF (EVERY #'IDENTITY (LIST #:G1589 #:G1590))
      (VALUES (POP #:G1589) (POP #:G1590))
      (VALUES 'EOI))
  

Let's look at the expansion of our first example

.. code:: cl

  (get-iterator-expansion '(values (count 0)
				   (list '(a b c d))))
  
  (#:STEPG1638 #:G1640 #:CURRENT1639 #:G1641)
  (1 '(A B C D) NIL NIL)
  (#:G1642 #:G1643)
  (PROGN
   (MULTIPLE-VALUE-SETQ (#:CURRENT1639) 0)
   (MULTIPLE-VALUE-SETQ (#:G1641)
     (IF (EVERY #'IDENTITY (LIST #:G1640))
         (VALUES (POP #:G1640))
         (VALUES 'EOI)))
   (IF (AND (NOT (EQL #:CURRENT1639 'EOI)) (NOT (EQL #:G1641 'EOI)))
       (VALUES #:CURRENT1639 #:G1641)
       'EOI))
  (PROGN
   (MULTIPLE-VALUE-SETQ (#:CURRENT1639) (+ #:CURRENT1639 #:STEPG1638))
   (MULTIPLE-VALUE-SETQ (#:G1641)
     (IF (EVERY #'IDENTITY (LIST #:G1640))
         (VALUES (POP #:G1640))
         (VALUES 'EOI)))
   (IF (AND (NOT (EQL #:CURRENT1639 'EOI)) (NOT (EQL #:G1641 'EOI)))
       (VALUES #:CURRENT1639 #:G1641)
       'EOI))

And you can see this expands to a relatively straightforward prefix/loop/cleanup (though the cleanup is empty in this case)

.. code:: cl

  (pprint (macroexpand-1 '(loop-iterator (values (count 0)
        				        (list '(a b c d)))
        			         (x y)  collect (list x y))))
  
  (LET* ((#:STEPG1644 1)
         (#:G1646 '(A B C D))
         (#:CURRENT1645 NIL)
         (#:G1647 NIL)
         #:G1648
         #:G1649)
    (MULTIPLE-VALUE-SETQ (#:G1648 #:G1649)
      (PROGN
       (MULTIPLE-VALUE-SETQ (#:CURRENT1645) 0)
       (MULTIPLE-VALUE-SETQ (#:G1647)
         (IF (EVERY #'IDENTITY (LIST #:G1646))
             (VALUES (POP #:G1646))
             (VALUES 'EOI)))
       (IF (AND (NOT (EQL #:CURRENT1645 'EOI)) (NOT (EQL #:G1647 'EOI)))
           (VALUES #:CURRENT1645 #:G1647)
           'EOI)))
    (UNWIND-PROTECT
        (LOOP FOR (X Y) = (LIST #:G1648 #:G1649)
              WHILE (NOT (EQL #:G1648 'EOI))
              COLLECT (LIST X Y)
              DO (MULTIPLE-VALUE-SETQ (#:G1648 #:G1649)
                   (PROGN
                    (MULTIPLE-VALUE-SETQ (#:CURRENT1645)
                      (+ #:CURRENT1645 #:STEPG1644))
                    (MULTIPLE-VALUE-SETQ (#:G1647)
                      (IF (EVERY #'IDENTITY (LIST #:G1646))
                          (VALUES (POP #:G1646))
                          (VALUES 'EOI)))
                    (IF (AND (NOT (EQL #:CURRENT1645 'EOI))
                             (NOT (EQL #:G1647 'EOI)))
                        (VALUES #:CURRENT1645 #:G1647)
                        'EOI))))))
  ; No value
