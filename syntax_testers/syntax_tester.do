*! An opening comment
* this do file really does nothing... it just makes sure that
* syntax is OK for do-files

version 10.1

// does this work?

local bleen 

sysuse auto

regress /// has one space
  mpg price

regress	/// has one tab
  mpg price

regress 	/// has space, then tab
  mpg price

regress	 /// has tab, then space
  mpg price

regress 	 	 	/// has space tab space tab space tab or so
  mpg price

twoway scatter mpg weight

display "5 over 4 is " 5/4 // this will for sure be a problem
sum mpg //
  // an empty line with a comment
  //
 
sum turn displace // now for a comment with something in it.

tab foreign , sum(mpg)

twoway ///
  (scatter mpg weight /// oops should be in a little farther
  ) ///
  (lfit mpg weight)

regress mpg ///        a comment
  weight // here is something to be ignored /// this should be gone
  display "this should be OK"

/* a simple comment --- doesn't go to Stata. Bug or feature? */

display "at the end" /* a comment at the end of a line */

display "multiline" /* a
multiline comment
 which should be understood */

/* the following is OK, because /* */ trumps //?!? */

/* here is a nested /*
comment which has some // other things include "a string" */
inside of it */


regress mpg /* a comment inside a line */ weight head

regress mpg /* an old-school eol comment-out with leading *
*/ weight /*
*/ headroom /// a modern eol comment
  turn /* a nested /* inline comment */ hahah */, beta


  * here is a comment a the far margin
if `c(version)'==10 {
   * here is a comment which is indented properly?
   display "You need to upgrade"
   }
display "between if and else "
else {
   display "Running Stata X on Mac OS X, I hope."
   }

/* * this could be strange */
display "here is open-c after a double-slash, which is an ordering problem" // here is an end-of-line /* oops 


display "here is some odd nesting" /* // + " not here" 
+ " should be silent" // */ /* + " what about this"
+ " or this?" */

capture program drop foobar
program define foobar
version 10.0 
   /* now things are indented properly */
	display "hahaha made it to the end!!!"
end

exit

* some errors for testing
too many right-comments */
this should be a continuation ///


* this is known to fail---the open c-quote gets picked
*   as special. It will not be fixed!
display "/* inside a quote ---eeekkkk---"

