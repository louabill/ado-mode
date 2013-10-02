// command, harmful
clear
// command, harmless
regress
// constant
c(pi)
// string
"this is a string"
// quoted string is the same
`"this is a quoted string"'
// macro is (badly) called variable name 
`this'
// a matrix is an underlined version
matrix aMatrix
// a function is slightly different from a command
normal()
// a command needing a subcommand has grey behind it and doesn't care about
//   the harmful/harmless split
import
// a subcommand highlights differently
import excel
// obsolete command are garish
display in green
// nothing special for numbers or operators (which include parens)
display 4 + (2*9)^14/2
// there are also mata highlights which are bad
mata:
   transmorphic vector aFunction(real matrix X, string scalar i, pointer p)
   {
      pointer scalar foo
      complex rowvector bleen
      numeric colvector foobar
      real matrix ohno

      &foo
      }
end
// in the Do-file Editor:
// Plain, Keyword, Comment, Function, Macro, String, Compound String, Number, Operator

input x freq
0 2041
1   79
2   22
3   13
4    5
6    1
7    1
8    1
10   1
13   1
end

expand freq
nbreg x, irr
local mu = exp(_b[_cons])
local size = 1/e(alpha)
local prob = `size'/(`size'+`mu')
local scale = (1-`prob')/`prob'

* indirectly via -rgamma- and -rpoisson-:
gen double xg = rgamma(`size',`scale')

replace xg = 1e-6 if xg< 1e-6
gen double xnb = rpoisson(xg)

nbreg xnb, irr
di "size = " 1/e(alpha) ", prob = " ///
   1/e(alpha)/(1/e(alpha)+exp(_b[_cons]))

