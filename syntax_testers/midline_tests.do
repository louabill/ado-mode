Here is some test text with inline commands. It could regress if there
were problems with the prefix: regress still highlights after a :which
is annoying, but to be expected because prefix commands are a mess

capture regress
capt : regress
quietly regress
noisily regress
by: noi regress // legal but not allowed by us
regress
by: regress
by: levelsof
levelsof
  quietly   levelsof
qui qui
fug: regress
foo regress
fug regress
capture noisily {
   oops need
   }
capture noisily regress --- igit
cap n {
   that worked
   }

cap n logistic
capture n logit
capture no logit
cap noisily logit
cap noisil logit
cap noisi logit
cap nois logit
cap noi logit
cap no logit
cap n logit
    capt n logit
    capt no logit


foo: labe def

capture set more off
capture : set more off
set more off
qui set more  // should be partial
noisily set more  // should be partial
noisily set more off // OK
capture query memory
fff ml max

fooey regress // this is regress

** old-style regexp
quietly areg
fooey areg // this is areg
