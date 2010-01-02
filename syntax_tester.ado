*! version 1.10.0 January 2, 2010 @ 09:21:20
*! doesnt do anything but work for syntax testing
program def syntax_tester, eclass
   "does it understand strings?"
   `"does it understand "nested" strings?"'
   here is a `macro' that could be used 
   if this=="that" { 
      display foo
      }
   /* what about comments? */
   pro def fummel
   end
   program list foo
   program drop
   program di
   program dir
   
   
version 1.0
version 2.0
version 2b0
version 2.1
version 2b1 
version 3.0
version 3.1
version 3.2
version 4
version 4.0
version 5
version 5.0
version 6
version 6.0
version 7
version 7.0
version 8
version 8.1
version 8.2
version 8.3
version 9
version 9.1
version 9.2
version 9.3
version 9.5
version 10
version 10.1
version 11
version 11.1
   
   
   /* this program does nothing - it merely has some things for testing syntax coloring */
   /* working with the syntax table */
   local dingle = this - that

   /* stuff from incremental updates which need to be moved when new manuals come out */
   include
   
   /* functions in the order of the manuals (for checking for obsolete functions... */
   /* first... the general reference manuals */
   about
   adjust // obsolete in Stata 11
   adoupdate
   alpha
   ameans
   an ano anov anova
   /* estat syntax testing under regression postestimation below regression */
   areg
   asclogit
   asmprobit
   asroprobit
   binreg
   biprobit
   bitest bitesti
   bootstrap
   boxcox
   brier
   bsample
   bstat
   centile
   ci cii
   clog clogi clogit
   cloglog
   cnr cnre cnreg // obsolete as of Stata 11
   cnsreg
   /* constraint commands */
   /* first w/o anything */
   cons
   constrain
   cons de 
   const defin 
   constr d
   constr dir
   constra drop
   cons l
   constrai list
   constrain get
   constraint free
   /* bad constraints -- should not highlight, except as mata reserves */
   /* oops... need to fix for the const abbreviations....*/
   const ge
   const fre
   /* end constraint */
   copyright
   cor corr corre correl correla correlat correlate pwcorr
   cumul
   cusum
   db
   /* diagnostic plots */
   symplot
   quantile
   qqplot
   qnorm
   pnorm
   pchi
   qchi
   /* end... */
   di dis disp displ displa display
   do ru run
   doed doedi doedit
   dotplot
   dstdize  istdize
   dydx
   integ
   
   eivreg

   // common estat
   estat ic
   estat su
   estat summarize
   estat vce
   
   /* estimates */
   est save
   est use
   est des
   est describe
   estim esample:

   est sto
   esti store
   estimat res
   estimate restore
   estimates q
   estimates query
   estimates d /* no longer an acceptable abbrev in Stata 10 */
   estimates dir
   estimates drop
   estimates clear

   estima title
   estimates title:
   estimate note
   est notes
   esti note:
   esti notes:
   esti note l
   estimates notes list
   estimat notes drop

   estimates r
   estimates replay

   estimates t /* no longer good in Stata 10 */
   estimates tab
   estimates table
   estimates f // no longer good in Stata 11
   estimates for
   estimates st /* no longer good in Stata 10 */
   estima stat
   estimates stats
   estim ch // estimates change is not in Stata 11
   estima change
   /* end estimates */
   e // dumb abbreviation
   ex
   exi
   exit
   exlogistic
   expoisson
   
   fracpoly fracgen
   // fracpoly postestimation
   fracplot fracpred
   frontier
   fvrevar
   fvset b
   fvset base
   fvset d
   fvset design
   fvset report
   
   gllamm
   glm
   blogit
   bprobit
   glogit
   gprobit
   gmm
   grmeanby

   hausman
   heckman
   heckprob
   h he hel help
   ch che chel chelp
   whelp
   hetprob
   hist histo histog histogr histogra histogram
   hsearch
   
   intreg
   ivprobit
   ivreg /* obsolete in Stata 10 */
   ivregress
   ivtobit

   jackknife

   kap
   kapwgt
   kappa
   kdensity
   ksmirnov
   kwallis

   ladder
   gladder
   qladder
   set l
   set level
   lincom
   linktest
   lnskew0
   bcskew0
   /* lo as abbreviation for lookup is obsolete */
   lo
   loo
   lookup
   log
   log query
   log c
   log close
   log of
   log off
   cmdlog
   cmdlog c
   cmdlog close
   cmdlog of
   cmdlog off
   
   set logtype t
   set logtype smcl
   set lin
   set linesize
   logistic
   logi logit
   loneway
   lowess
   lpoly
   lrtest
   lv

   margins
   set mat
   set matsize
   set maxiter
   mean
   // mfp is simple now; in Stata 11 it became a prefix command
   mfp
   // mfx is obsolete as of Stata 11
   mfx
   mfx c
   mfx compute
   mfx r
   mfx replay
   /* misstable */
   misstable sum
   misstable summarize
   misstable pat
   misstable patterns
   misstable tree
   misstable nest
   misstable nested
   mkspline
   /* ml commands */
   ml mod
   ml model
   ml clear
   ml q
   ml query
   ml check
   ml sea
   ml search
   ml pl
   ml plot
   ml init
   ml rep
   ml report
   ml trace
   ml count
   ml max
   ml maximize
   ml gr
   ml graph
   ml di
   ml display
   ml foot
   ml footnote
   ml score
   /* end ml */
   mlog mlogi mlogit
   /* should be off */
   set mo
   set mo on
   set more off
   set p // should be off
   set pa
   set pagesize
   mprobit
   mvreg
   
   nbreg
   gnbreg
   nestreg
   /* net commands */
   net from
   net cd
   net link
   net search
   net
   net d
   net describe
   net set ado
   net set other
   net q
   net query
   net ins
   net install
   net get
   net sj
   net stb
   ado
   ado dir
   ado d
   ado describe
   ado uninstall
   /* end net */
   net search
   /* netio, which is also in set */
   se httpproxy on
   set httpproxy off
   set httpproxyhost
   se httpproxyport
   set httpproxyauth on
   set httpproxypw
   set timeout1
   set timeout2
   news
   nl
   nlinit
   nlcom
   nlogit
   nlogitgen
   nlogittree
   nlsur
   nptrend
   
   olog ologi ologit 
   on one onew onewa oneway
   oprob oprobi oprobit
   orthog
   orthpoly

   pcorr
   permute
   pkexamine pksumm pkshape pkcross pkequiv pkcollapse
   poisson
   poisgof
   predict
   predictnl
   prob probi probit
   dprobit // obsolete in Stata 11
   proportion
   prtest prtesti
   
   /* qc commands */
   cchart
   pchart
   rchart
   xchart
   shewhart
   /* end qc */
   qreg iqreg sqreg bsqreg _qreg
   q mem
   qu memory
   que out
   quer output
   query inter
   query interface
   query graph
   query graphics
   query eff
   query efficiency
   quer net
   que network
   query up
   query update
   qu trace
   q mata
   q oth
   q other
   query `foo'
   ranksum
   median
   ratio
   reg3
   reg regr regre regres regress
   /* regression diagnostics */
   
   #r #re #rev #revi #revie #review
   /* don't seem to be in manuals anymore */
   _rmcoll _huber
   roctab roccomp rocgold
   rocfit
   // rocfit postest
   rocplot
   rologit
   rreg
   runtest
   
   sampsi
   
   /* these should be in the programming section... */
   ret li
   retu list
   return `foo'
   eret li
   eretur list
   sret li
   sreturn list
   scobit
   sdtest sdtesti robvar
   search
   set searchdefault local /* rest under set */
   findit
   hsearch
   serrbar
   /* set commands */
   set a
   set adosize
   set autotabgraphs
   set checksum on
   set checksum off
   set conren
   set conren clear
   set conren sf
   set conren bf
   set conren it
   set conren res
   set conren result
   set conren txt
   set conren text
   set conren inp
   set conren input
   set conren err
   set conren error
   set conren li
   set conren link
   set conren hi
   set conren hilite
   set conren ulon
   set conren uloff
   set conren reset 
   set copycolor auto
   set copycolor automatic
   set copycolor asis
   set copycolor gs1
   set copycolor gs2
   set copycolor gs3
   set dockable on
   set dockable off
   set dockingg on
   set dockingguides off
   set doublebuffer on
   set doublebuffer off
   set `foo'
   set dp comma
   set dp period
   set emptycells keep
   set emptycells drop
   set eolch mac
   set eolchar unix
   set fastscroll on
   set fastscroll off
   set floatresults on
   set floatresults off
   set floatwindows on
   set floatwindows off
   set g on
   set graphics off
   set httpproxy on
   set httpproxy off
   set httpproxya on
   set httpproxyauth off
   set httpproxyhost
   set httpproxyport
   set httpproxypw
   set httpproxyuser
   set icmap on // obsolete in Stata 10
   set icmap off // obsolete in Stata 10
   set level
   set lineg
   set linegap
   set li
   set linesize
   set locksplit on
   set locksplitters off
   set logt t
   set logtype text
   set logty s
   set logtype smcl
   set macgph quartz // obsolete in Stata 11
   set macgphengine quickdraw // obsolete in Stata 11
   set mat
   set matsize
   set maxdb
   set maxiter
   set maxvar
   set mem
   set memory
   set mo on
   set more off
   set notifyuser
   set ob
   set obs
   set odbcmg iodbc
   set odbcmgr unixodbc
   set ou proc
   set output p
   set outpu i
   set output inform
   set outp e
   set output error
   set pa
   set pagesize
   set persistfv on
   set persistvtopic off
   set piccom on // obsolete in Stata 11
   set piccomments off // obsolete in Stata 11
   set pinnable on
   set pinnable off
   set playsnd on
   set playsnd off
   set printcolor auto
   set printcolor automatic
   set printcolor asis
   set printcolor grayscale /* obsolete in Stata 9 */
   set printcolor gs1
   set printcolor gs2
   set printcolor gs3
   set processors
   set reventr
   set reventries
   set revkeyboard on
   set revkeyboard off
   set revwin nofloat // appears obsolete in Stata 11
   set revwindow float // appears obsolete in Stata 11
   set r on
   set rmsg off
   set scheme
   set scrollbufsize
   set searchd local
   set searchdefault net
   set searchdefault all
   set se
   set seed
   set smalldlg on                      /* obsolete in Stata 10 */
   set smoothf on
   set smoothfonts off
   set smoothsize 12
   set timeout1
   set timeout2
set tr on
set trace off
   set traced
   set tracedepth
   set tracee on
   set traceexpand off
   set traceh
   set tracehilite
   set tracei off
   set traceindent on
   set tracen on
   set tracenumber off
   set traces off
   set tracesep on
   /* inconsistancy fixed in 0.99: needs float or double */
   set ty float
   set typ double
   set type
   set update_interval
   set update_prompt on
   set update_prompt off
   set update_query on
   set update_query off
   set use_atsui_graph off // obsolete in Stata 11
   set use_qd_text on // obsolete in Stata 11
   set varabbrev on
   set varabbrev off
   set varkeyboard on
   set varkeyboard off
   set varlabelpos
   set varwin float // appears obsolete in Stata 11
   set varwindow nofloat // appears obsolete in Stata 11
   set virt on
   set virtual off
   /* undocumented starting in Stata 10, but still legal */
   set xptheme on
   set_defaults
   /* end set commands */
   signrank
   signtest
   simulate
   sktest
   slogit
   smooth
   spearman
   ktau
   spikeplot
   ssc new
   ssc what // obsolete in Stata 11
   ssc whatsnew // obsolete in Stata 11
   ssc hot
   ssc d
   ssc `foo'
   ssc describe
   ssc inst
   ssc install
   ssc uninstall
   ssc type
   ssc copy
   stem
   /* stepwise or sw now has a syntax bad for highlighting */
   stepwise
   sw 
   /* sw commands no longer exist, due to syntax changes */
   sw clogit
   sw cloglog
   sw `foo'
   sw clogit
   sw cloglog
   sw cnreg
   sw glm
   sw logistic
   sw logit
   sw nbreg
   sw ologit
   sw oprobit
   sw poisson
   sw probit
   sw qreg
   sw reg
   sw regr
   sw regre
   sw regres
   sw regress
   sw stcox
   sw streg
   sw tobit
   sw weibull
   sw gompertz                          /* out of date */
   /* end sw commands */
   suest 
   su sum summ summa summar summari summariz summarize
   sunflower
   sureg
   swilk
   sfrancia
   symmetry
   symmi
   
   table
   tabstat
   ta tab tabu tabul tabula tabulat tabulate
   tab1
   tab2
   tabi
   te tes test
   testparm
   testnl
   tetrachoric
   tob tobi tobit
   total
   print
   translate
   translator q
   translator query
   translator set
   translator reset
   translator `foo'
   transmap q
   transmap query
   transmap def
   transmap define
   transmap `foo'
   treatreg
   truncreg
   ttest
   ttesti

   update
   update from
   update q
   update query
   update ado
   update executable
   update utilities
   update swap
   update all
   update `foo'
   set update_query on
   set update_interval
   set update_prompt off
   
   view
   view file
   view browse
   view help
   view search
   view `foo'
   view news
   view net
   view ado
   view update
   view view_d
   view help_d
   view search_d
   view net_d
   view ado_d
   view update_d
   vwls
   
   which

   xi

   zinb
   zip
   ztnb
   ztb

   /* endless postestimation */
   // estat (from [R])
   estat alt
   estat alternatives
   estat archlm
   estat bgo
   estat bgodfrey
   estat clas
   estat classification
   estat cor
   estat correlation
   estat cov
   estat covariance
   estat dur
   estat durbinalt
   estat dwa
   estat dwatson
   estat endog
   estat endogenous
   estat facw
   estat facweights
   estat first
   estat firststage
   estat gof
   estat hettest
   estat imtest
   estat mfx // not obsolete---still used for some commands
   estat over
   estat overid
   estat predict
   estat se
   estat szroeter
   estat summarize
   estat vif
   
   adjust
   dfbeta
   acprplot
   avplot
   avplots
   lvr2plot
   rvfplot
   rvpplot
   
   /* types of commands, which have their own highlighting */
   estat 
   estimates
   /* end subcommand using postestimation */
   
   /* from data management manual */
   ap app appe appen append
   append using
   as ass asse asser assert
   by bys byso bysor bysort
   cd pwd
   cf
   changeeol
   checksum
   se checksum on
   set checksum off
   clear
   clear mata
   clear results
   clear matrix
   clear programs
   clear ado
   clear all
   clear * /* cannot fix easily, because of special meaning of * in regexps */
   clonevar
   codebook
   collapse
   compare
   compress
   contract
   copy
   corr2data
   cou coun count
   cross using
   byte int long float double
   str str1 str80 str99 str100 str158 str235 str244 str245 str266
   datasig
   datasignature
   datasig set
   datasignature conf
   datasig confirm
   datasig rep
   datasignature report
   datasig conf using
   datasig repo using
   datasignature clear
   /* should there be date format highlighting? */
   /* date-time functions are in the functions */

   d de des desc descr descri describ describe
   ds lookfor
   destring
   tostring
   dir ls man
   drawnorm
   drop keep
   /* duplicates */
   duplicates r
   duplicates report
   duplicates e
   duplicates examples
   duplicates l
   duplicates list
   duplicates b
   duplicates browse
   duplicates t
   duplicates tag
   duplicates drop
   /* end dup */
   ed edi edit
   b br bro brow brows browse
   /* endless egen & options */
   egen
   /* any() has been mapped to anyvalue() but is not listed as obsolete as of 19.oct.05 */
   egen = any() /* problem with mata any() */
   egen = anycount()
   egen = anymatch()
   egen = anyvalue()
   egen = concat() 
   egen = count()
   egen foo = cut(fie)
   egen = diff()
   /* eqany is now anymatch */
   egen = eqany()
   egen = ends()
   egen = fill()
   egen = group()
   egen = iqr()
   egen = kurt()
   /* ma still works, though undocumented through what could be a mistake*/
   egen = ma() /* seems to be obsolete in Stata 10 */
   egen = mad()
   egen = max()
   egen = mdev()
   egen = mean()
   egen = median()
   egen = min()
   egen = mode()
   egen = mtr()
   /* neqany mapped to anycount */
   egen = neqany()
   egen = pc()
   egen = pctile()
   egen = rank()
   /* all the rxxx have been renamed */
   egen = rfirst()
   egen = rlast()
   egen = rmax()
   egen = rmean()
   egen = rmin()
   egen = rmiss()
   egen = robs()
   egen = rsd()
   egen = rsum()
   /* end of obsolete names */
   egen = rowfirst()
   egen = rowlast()
   egen = rowmax()
   egen = rowmean()
   egen = rowmedian()
   egen = rowmin()
   egen = rowmiss()
   egen = rownonmiss()
   egen = rowpctile()
   egen = rowsd()
   egen = rowtotal()
   egen = sd()
   egen = seq()
   egen = skew()
   egen = std()
   /* replaced with total */
   egen = sum()
   egen = tag()
   egen = total()
   /* end egen */
   en enc enco encod encode
   dec deco decod decode
   erase rm
   expand
   expandcl
   fdasav fdasave
   fdause
   fdades fdadesc fdadescr fdadescri fdadescrib fdadescribe
   filef filefi filefil filefilt filefilte filefilter 
   fillin
   form forma format
   /* functions */
   /* math functions */
   abs()
   acos()
   acosh()
   asin()
   asinh()
   atan()
   atan2()
   atanh()
   ceil()
   cloglog()
   comb()
   cos()
   cosh()
   digamma()
   exp()
   floor()
   int()
   invcloglog()
   invlogit()
   ln()
   /* should this count as obsolete? */
   lnfact()
   lnfactorial()
   lngamma()
   log()
   log10()
   logit()
   max()
   min()
   mod()
   reldif()
   round()
   sign()
   sin()
   sinh()
   sqrt()
   sum()
   tan()
   tanh()
   trigamma()
   trunc()
   /* probability functions */
   /* beta density */
   ibeta()
   betaden()
   Binomial() /* finally changed to binomialtail in Stata 10 */
   ibetatail()
   invibeta()
   invibetatail()
   nibeta()
   nbetaden()
   invnibeta()
   binomial()
   binomialp()
   binomialtail()
   invbinomial()
   invbinomialtail()
   binorm()
   chi2()
   chi2tail()
   invchi2()
   invchi2tail()
   nchi2()
   invnchi2()
   npnchi2()
   F()
   Fden()
   Ftail()
   invF()
   invFtail()
   nFden()
   nFtail()
   invnFtail()
   gammap()
   gammaden()
   gammaptail()
   invgammap()
   invgammaptail()
   dgammapda()
   dgammapdada()
   dgammapdadx()
   dgammapdx()
   dgammapdxdx()

   hypergeometric()
   hypergeometricp()

   nbinomial()
   nbinomialp()
   nbinomialtail()
   invnbinomial()
   invnbinomialtail()

   /* Normal and related */
   binormal()
   norm()  /* should be obsolete BUT mata function*/
   normal()
   normden()
   normalden()
   invnorm()
   invnormal()
   lnnormal()
   lnnormalden()
   invttail()
   /* Poisson */
   poisson()
   poissonp()
   poissontail()
   invpoisson()
   invpoissontail()
   /* T */
   tden()
   ttail()
   invttail()
   /* random number functions */
   uniform()
	runiform()
   rbeta()
	rbinomial()
	rchi2()
	rgamma()
	rhypergeometric()
	rnbinomial()
	rnormal()
	rpoisson()
	rt()
   /* string functions */ 
   abbrev()
   char()
   index()
   indexnot()
   itrim()
   length()
   lower()
   ltrim()
   match()
   plural()
   proper()
   real()
   regexm()
   regexr()
   regexs()
   reverse()
   rtrim()
   soundex()
   soundex_nara()
   string()
   strlen()
   strlower()
   strltrim()
   strmatch()
   strofreal()
   strpos()
   strproper()
   strreverse()
   strrtrim()
   strtoname()
   strtrim()
   strupper()
   subinstr()
   subinword()
   substr()
   trim()
   upper()
   word()
   wordcount()
   /* programming functions */
   autocode()
   byteorder()
   c()
   _caller()
   chop()
   clip()
   cond()
   e()
   epsdouble()
   epsfloat()
   float()
   fmtwidth()
   group()
   has_eprop()
   inlist()
   inrange()
   irecode()
   matrix()
   maxbyte()
   maxdouble()
   maxfloat()
   maxint()
   maxlong()
   mi()
   minbyte()
   mindouble()
   minfloat()
   minint()
   minlong()
   missing()
   r(this should not really highlight)
   recode()
   replay()
   return()
   s()
   scalar()
   smallestdouble()
   /* date functions */
   Cdhms()
   Chms()
   Clock()
   clock()
   Cmdyhms()
   Cofc()
   cofC()
   Cofd()
   cofd()
   date()
   day()
   dhms()
   dofC()
   dofc()
   dofh()
   dofm()
   dofq()
   dofw()
   dofy()
   dow()
   doy()
   halfyear()
   halfyearly()
   hh()
   hhC()
   hms()
   hofd()
   hours()
   mdy()
   mdyhms()
   minutes()
   mm()
   mmC()
   mofd()
   month()
   monthly()
   msofhours()
   msofminutes()
   msofseconds()
   qofd()
   quarter()
   quarterly()
   seconds()
   ss()
   ssC()
   tC()
   tc()
   td()
   th()
   tm()
   tq()
   tw()
   week()
   weekly()
   wofd()
   year()
   yearly()
   yh()
   ym()
   yofd()
   yq()
   yw()

   /* single-letter date fns  obsolete in Stata 10 */
   d()
   h()
   m()
   q()
   w()
   y()

   /* time series functions */
   tin()
   twithin()
   
   /* matrix to matrix */
   cholesky()
   corr()
   diag()
   get()
   hadamard()
   I()
   inv()
   invsym()
   J()
   matuniform()
   nullmat()
   sweep()
   syminv()
   vec()
   vecdiag()
   /* matrix to scalar */
   colnumb()
   colsof()
   det()
   diag0cnt()
   el()
   issym()
   issymmetric()
   matmissing()
   mreldif()
   rownumb()
   rowsof()
   trace()
   /* end functions */
   g ge gen gene gener genera generat generate
   replace
   set se
   set seed
   set ty
   set type float
   set type double
   set type foo
   gsort
   hexdump
   /* icd9 commands */
   icd9 check
   icd9p check
   icd9 clean
   icd9p clean
   icd9 gen
   icd9p generate
   icd9 l
   /* got to figure out how to fix the lookup here */
   icd9p lookup
   icd9 sea
   icd9p search
   icd9 q
   icd9 query
   impute
   inf using
   infile
   infix
   inp inpu input
   insheet
   ins insp inspe inspec inspect
   ipolate
   isid
   joinby
   /* label */
   la da
   label data
   la var
   label variable
   la de
   label define
   la val
   lab val
   label values
   la di
   label dir
   label l
   label list
   label copy
   lab drop
   labe save
   /* end label */
   la lang
   label language
   labelbook
   numlabel
   uselabel
   l li lis list
   fl fli flis flist
   lookfor
   memory
   set mem 5b
   q mem
   set vir on
   set virtual off
   mer 1:1
   merg m:1
   merge 1:m
   merge m:m
   mkdir
   mvencode
   mvdecode
   /* notes */
   note notes
   note list
   note list
   note drop
   note search
   notes replace
   notes renumber
   set obs 12
   /* odbc */
   odbc li
   odbc list
   odbc q
   odbc query
   odbc des
   odbc describe
   /* need to fix this */
   odbc lo
   odbc load
   odbc in
   odbc insert
   odbc exe
   odbc exec
   odbc sql
   odbc sqlfile
   set odbcmg iodbc
   set odbcmgr unixodbc
   /* end odbc */
   order
   mov move aorder // obsolete in Stata 11
   ou out outf outfi outfil outfile
   outs outsh outshe outshee outsheet
   pctile
   xtile
   _pctile
   range
   recast
   recode
   ren rena renam rename renpfix
   /* reshape ... */
   reshape long
   reshape wide
   reshape error
   reshape i
   reshape j
   reshape xij
   reshape xi
   reshape
   reshape q
   reshape query
   reshape clear
   // end reshape
   rmdir
   sample
   sa save saveold
   separate
   sh she shel shell
   xsh xshe xshel xshell
   // snapshot
   snapshot save
   snapshot label
   snapshot restore
   snapshot list
   snapshot erase
   // end snapshot
   so sor sort
   split
   stack
   statsby
   sysuse auto
   sysuse dir
   ty typ type
   u us use
   varm varma varman varmana varmanag varmanage
   webuse
   webuse query
   webuse set
   xmlsav xmlsave
   xmluse
   xpose
   zipfile
   unzipfile
   /* end data management */

   /* from multivariate model */
   biplot
   ca
   camat
   cabiplot
   caprojection
   estat coordinates
   estat distances
   estat inertia
   estat loadings
   estat profiles
   estat summarize
   estat table
   screeplot

   candisc

   canon
   estat correlations
   estat loadings
   estat rotate
   estat rotatecompare
   screeplot
   
   /* cluster commands */
   /* ordered by the -cluster- intro */
   /* this groups all the linkage subcommands together */
   
   cluster k
   cluster kmeans
   cluster kmed
   cluster kmedians
   cluster s
   cluster singlelinkage
   cluster a
   cluster averagelinkage
   cluster c
   cluster completelinkage
   cluster wav
   cluster waveragelinkage
   cluster med
   cluster `foo'
   cluster medianlinkage
   cluster cent
   cluster centroidlinkage
   cluster ward
   cluster wardslinkage
   cluster stop
   cluster dend
   cluster dendrogram
   /* cluster tree is listed in online help as synonym for cluster dendogram */
   cluster tr
   cluster tree
   cluster gen
   cluster generate
   cluster note
   cluster notes
   cluster dir
   cluster list
   cluster drop
   cluster rename
   cluster renamevar
   cluster query
   cluster set
   cluster del
   cluster delete
   cluster parsedist
   cluster parsedistance
   cluster measures
   /* clustermat commands */
   clustermat s
   clustermat singlelinkage
   clustermat a
   clustermat averagelinkage
   clustermat c
   clustermat completelinkage
   clustermat wav
   clustermat waveragelinkage
   clustermat med
   clustermat `foo'
   clustermat medianlinkage
   clustermat cent
   clustermat centroidlinkage
   clustermat ward
   clustermat wardslinkage
   /* end clustermat commands */
   /* now extras in order of the manual */
   cluster notes drop
   /* discrim commands from the discrim intro */
   discrim knn
   discrim lda
   discrim logistic
   discrim qda
   /* discrim postestimation */
   estat classtable
   estat errorrate
   estat grsummarize
   estat list
   estat summarize
   /* discrim lda postestimation */
   estat anova
   estat canontest
   estat classfunctions
   estat classtable
   estat correlations
   estat covariance
   estat grmeans
   estat grsummarize
   estat list
   estat loadings
   estat manova
   estat structure
   estat summarize
   loadingplot
   scoreplot
   screeplot
   /* discrim qda postestimation */
   estat classtable
   estat correlations
   estat covariance
   estat errorrate
   estat grdistances
   estat grsummarize
   estat list
   estat summarize
   

   fac fact facto factor
   factormat
   /* uh oh, factor estat stuff */
   estat anti
   estat common
   estat factors
   estat kmo
   estat residuals
   estat rotatecompare
   estat smc
   estat structure
   estat summarize
   loadingplot
   rotate
   scoreplot
   screeplot
   /* end factor estat stuff */
   hotelling
   mano manov manova
   manovatest
   mat dis foo
   matrix dissimilarity bar

   mca
   /* mca postestimation */
   mcaplot
   mcaprojection
   estat coordinates
   estat subinertia
   
   mds
   /* mds postestimation */
   estat config
   estat correlations
   estat pairwise
   estat quantiles
   estat stress
   mdsconfig
   mdsshepard
   /* end mds postestimation */
   mdslong
   mdsmat

   // mvtests in order of intro
   mvtest m
   mvtest means
   mvtest cov
   mvtest covariances
   mvtest corr
   mvtest correlations
   mvtest norm
   mvtest normality
   
   /* score is now obsolete */
   sco scor score
   pca
   pcamat
   /* pca postestimation */
   estat anti
   estat kmo
   estat loadings
   estat residuals
   estat rotatecompare
   estat smc
   loadingplot
   rotate
   scoreplot
   screeplot
   /* end pca postestimation */
   procrustes
   /* procrustes postestimation */
   estat compare
   estat mvreg
   estat summarize
   procoverlay

   rot rota rotat rotate
   rotatemat
   scoreplot
   loadingplot
   greigen /* obsolete in Stata 9 */

   /* from the survival analysis manual */ 
   
   ctset
   cttost
   ir
   iri
   cs
   csi
   cc
   cci
   tabodds
   mhodds
   mcc
   mcci
   
   ltable
   snapspan
   st_is 2
   st_show
   st_ct
   stbase
   stci
   stcox
   /* stcox diagnostics */
   stphplot
   stcoxkm
   /* stcox postestimation */
   estat con
   estat concor
   estat concordance
   estat phtest
   stcurve
   /* end stcox postestimation */
   stcrr stcrre stcrreg
   stphtest
   stcurve
   stdescribe
   stfill
   stgen
   stir
   /* stpower commands */
   stpow cox
   stpowe exponential
   stpower logrank
   
   stptime
   strate
   stmh
   stmc
   streg
   sts
   sts g
   sts graph
   sts l
   sts list
   sts `foo'
   sts t
   sts test
   sts gen
   sts generate
   stset
   streset
   st
   stsplit
   stjoin
   stsum
   sttocc
   sttoct
   stvary

   /* from the survey data manual */
   /* difficult to order, because of the manual */
   /* not using the intro for ordering */
   /* estat commands as listed under -estat- */
   estat svyset
   estat eff
   estat effects
   estat lceff
   estat lceffects
   estat size
   estat sd
   estat strata
   estat vce
   /* end estat commands */
   /* survey commands as listed in the order of the survey estimation section */
   brr
   
   svy: mean
   svy: proportion
   svy: ratio
   svy:total

   svy: cnreg
   svy: cnsreg
   svy: glm
   svy: intreg
   svy: nl
   svy: reg
   svy: regress
   svy: tobit
   svy: treatreg
   svy: truncreg

   svy: stcox
   svy: streg

   svy: biprobit
   svy: cloglog
   svy: hetprob
   svy: logistic
   svy: logit
   svy: probit
   svy: scobit

   svy: clogit
   svy: mlogit
   svy: mprobit
   svy: ologit
   svy: oprobit
   svy: slogit
   
   svy: gnbreg
   svy: nbreg
   svy: poisson
   svy: zinb
   svy: zip
   svy: ztnb
   svy: ztb

   svy: ivprobit
   svy: ivregress
   svy: ivtobit
   
   svy: heckman
   svy: heckprob
   /* ivreg outdated as of Stata 10 */
   svy: ivreg
   
   svy jack: logistic
   svy linear: gnbreg 
   svy brr: gnbreg

   svy: tab
   svy: tabul
   svy: tabulate
   svydes svydescribe
   svymarkout
   svyset
   /* still can be used, but not listed anywhere */
   testparm

   /* end of the survey stats book */

   /* time series */
   arch
   arima
   corrgram
   ac
   pac
   cumsp
   dfactor
   dfgls
   dfuller
   dvech
   fcast c
   fcast com
   fcast compute
   fcast g
   fcast graph
   haver des
   haver describe
   haver use
   /* irf commands ... starting Stata 8.2 */
   irf a
   irf add
   irf `foo' 
   irf cg
   irf cgraph
   irf cr
   irf create
   irf ct
   irf ctable
   irf d
   irf describe
   /* irf dir is dead, even under version control? */
   irf di
   irf dir
   irf drop
   irf erase
   irf g
   irf graph oirf
   irf gr irf
   irf gr foo // foo should fail
   irf og
   irf ograph
   irf ren
   irf rename
   irf set
   irf t
   irf table
   irf tab cdm
   irf tab foo // foo should fail
   newey
   pergram
   pperron
   prais
   rolling
   dwstat
   durbina
   bgodfrey
   archlm
   sspace
   /* should these options be required?? */
   tsappend, add(4) last(foo) tsfmt(string)
   tsfill
   tsline
   tw tsline
   twoway tsline
   graph twoway tsline
   tw tsrline
   twoway tsrline
   tsreport
   tsrevar
   tsset
   tssmooth `foo'
   tssmooth d
   tssmooth dexponential
   tssmooth e
   tssmooth exponential
   tssmooth h
   tssmooth hwinters
   tssmooth ma
   tssmooth nl
   tssmooth s
   tssmooth shwinters
   !! start here
   var
   /* var post estimation commands */
   /* these seem to be common to var and svar */
   fcast compute
   fcast graph
   irf
   vargranger
   varlmar
   varnorm
   varsoc
   varstable
   varwle
   /* end var post estimation commands */
   svar
   varbasic
   
   /* varfcast is obsolete as of July 23, 2004 */
   /* varirf is obsolete as of July 23, 2004 */
   vec
   /* vec post estimation commands */
   veclmar
   vecnorm
   vecrank
   vecstable
   wntestb
   wntestq
   xcorr
   /* end time-series */
   
   
   /* stuff from the crossectional timeseries book */
   /* now called the longitudinal/panel data book */

   /* iis, tis obsolete as of Stata 10 */
   iis
   tis
   xtset
   quadchk
   xtabond
   estat abond
   estat sargan
   xtcloglog
   xtdata
   xtdes
   xtdescribe
   xtdpd
   xtdpdsys
   xtfrontier
   xtgee
   estat wcorrelation
   /* estat wcorrelation replaced xtcorr */
   xtcorr
   xtgls
   xthaus
   xthtaylor
   xtintreg
   xtivreg
   xtline
   xtlogit
   xtmelogit
   estat group
   estat recovariance
   xtmepoisson
   xtmixed
   xtnbreg
   xtpcse
   /* obsolete as of Stata 7 ... but still functional */
   xtpois
   xtpoisson
   xtprobit
   xtrc
   xtreg
   xttest0
   xtregar
   xtset
   xtsum
   xttab
   xttrans
   xttobit

   /* end stuff from crossectionaltime-series */

   /* programming manual */

   nobreak
   break
   /* not highlighting byable() stuff, because an option */
   program dingle, rclass byable(recall)
   end
   program foobar, sclass byable(onecall)
   end
   cap
   capture
   char
   char l
   char list
   char ren
   char rename
   *** class added for saving programs with correct names ***
   a.bc.new
   ab.b.copy
   a.compress.ref
   a.objtype
   a.b.isa
   a.b.classname
   a.b.isofclass
   a.b.objkey
   a.c.uname
   a.b.ref_n
   a.b.arrnels
   a.b.arrindexof
   a.b.classmv
   a.b.instancemv
   a.b.dynamicmv
   a.b.superclass

   a.b.Declare
   a.b.Arrdropel
   a.b.Arrdropall
   a.b.Arrpop
   a.c.Arrpush

   /* cannot seem to get these to work */
   .Global.foo.gringo
   .Local.d.e.f
   .Super.q.e.d
   4.2

   class fooey {
      classwide:
      instance:
      instancespecific:
      } 
   
   class exit
   
   classutil drop
   classutil d
   classutil describe
   classutil dir
   classutil cdir
   classutil which
   classutil `foo'

   /* confirm commands */
   conf e
   confi existence
   confir new f
   confirm file
   /* won't highlight because of need for a subcommand */
   confirm `foo'
   conf numeric fo
   conf str for
   conf string form
   conf date forma
   /* confirm ts seems to be obsolete in Stata 10 */
   conf ts format
   conf name
   confi names
   /* confirm int should not work */
   confirm int number
   confir integer n
   confirm number
   conf mat
   confirm matrix
   conf sca
   conf scalar
   conf new v
   conf numeric va
   confirm str var
   confirm string vari
   confirm byte varia
   confirm int variab
   conf long variabl
   conf float variable
   conf double v
   confirm str11 v

   continue

   /* oh no! the cclass stuff */
   cret l
   creturn list

   c( current_date )
   c(current_date)
   c(current_time)
   c(rmsg_time)
   c(stata_version)
   c(version)
   c(born_date)
   c(flavor)
   c(SE)
   c(MP)
   c(processors)
   c(processors_lic)
   c(processors_mach)
   c(processors_max)
   c(mode)
   c(console)
   c(os)
   c(osdtl)
   c(machine_type)
   c(byteorder)

   c(sysdir_stata)
   c(sysdir_updates)
   c(sysdir_base)
   c(sysdir_site)
   c(sysdir_plus)
   c(sysdir_personal)
   c(sysdir_oldplace)
   c(adopath)
   c(pwd)
   c(dirsep)

   c(max_N_theory)
   c(max_N_current)
   c(max_k_theory)
   c(max_k_current)
   c(max_width_theory)
   c(max_width_current)
   c(max_matsize)
   c(min_matsize)
   c(max_macrolen)
   c(macrolen)
   c(max_cmdlen)
   c(cmdlen)
   c(namelen)

   c(mindouble)
   c(maxdouble)
   c(epsdouble)
   c(minfloat)
   c(maxfloat)
   c(epsfloat)
   c(minlong)
   c(maxlong)
   c(minint)
   c(maxint)
   c(minbyte)
   c(maxbyte)
   c(maxstrvarlen)

   c(N)
   c(k)
   c(width)
   c(changed)
   c(filename)
   c(filedate)

   c(memory)
   c(maxvar)
   c(matsize)

   c(more)
   c(rmsg)
   c(dp)
   c(linesize)
   c(pagesize)
   c(logtype)
   c(eolchar)
   c(icmap)

   /* should have something for platform-specific */
   c(dockable)
   c(dockingguides)
   c(floatresults)
   c(floatwindows)
   c(locksplitters)
   c(pinnable)
   c(persistfv)
   c(persistvtopic)
   c(doublebuffer)
   c(fastscroll)  /* fastscroll not platform dep in Stata 10 */
   c(revwindow)
   c(varwindow)
   c(smoothfonts)
   c(use_qd_text)
   c(smoothsize)
   c(use_atsui_graph)
   c(linegap)
   c(scrollbufsize)
   c(varlabelpos)
   c(reventries)
   c(maxdb)
   /* obsolete in Stata 10 */
   c(smalldlg)
   c(xptheme)
   /* not specific */

   c(autotabgraphs)
   c(graphic)
   c(scheme)
   c(printcolor)
   c(copycolor)
   c(macgphengine)
   c(piccomments)

   c(adosize)
   c(virtual)

   c(checksum)
   c(timeout1)
   c(timeout2)
   c(httpproxy)
   c(httpproxyhost)
   c(httpproxyport)
   c(httpproxyauth)
   c(httpproxyuser)
   c(httpproxypw)

   /* mac/win only */
   c(update_query)
   c(update_interval)
   c(update_prompt)
   /* all */
   c(trace)
   c(tracedepth)
   c(tracesep)
   c(traceindent)
   c(traceexpand)
   c(tracenumber)
   c(tracehilite)

   c(matastrict)
   c(matalnum)
   c(mataoptimize)
   c(matafavor)
   c(matacache)
   c(matalibs)
   c(matamofirst)

   c(type)
   c(level)
   c(maxiter)
   c(searchdefault)
   c(seed)
   c(varabbrev)
   c(odbcmgr)
   c(`foo')

   c(pi)
   c(alpha)
   c(ALPHA)
   c(Mons)
   c(Months)
   c(Wdays)
   c(Weekdays)
   c(rc)

   /* end of that mess */
   _datasig
   _datasignature
   #d cr
#delimit ;

   this is another command;
   this is fine
     this is fine, too;
   this is ok;
#delimit cr
   this is ok /// this should look like a comment
     why is this a comment
   here is something // this is a comment
   this is fine
   
#delimit ;
   
   this is funny?;
   this
     
     /* this */
     command
     continuation;
   
   this is a test /// this is a comment
     this is not a comment /// some more comments
     this is fine!;
   
   this is OK;
   
   commands 
     this should behave as a continuation?;
   this is a new line;
   if this==that {;
      this is an if clause
        another continuation;
      };  
   foo;       glue; silly;
   this is a continuation, it should indent properly
     this is ok;
   
   if this | that {;
      indent;
      }; 
   whooie!
     continuation;
   
   
#delimit cr
   
   this "#delim ;" is inside quotations, and hence is invalid

   if {
      test
      }   
   
#delimit cr

   /* all the dialog stuff is in syntax_tester.dlg, because the dlg stuff should really
be a separate mode ... ugh */
   discard
   di dis disp displ displa display
   display as text
   display as txt
   display as res
   display as result
   display as err
   display as error
   display as inp
   display as input
   display in smcl
   display _asis
   display _s(4)
   display _skip(3)
   display _col(4)
   display _column(2)
   display _new(3)
   display _newline(3)
   display _newline 
   display _c
   display _continue
   display _d(4)
   display _dup(3)
   display _r(fuggy)
   display _request(jiminy)
   display _char(4)
   display in blue
   display in red
   display in yellow
   

   /* ereturn... */
   eret loc
   eret loca
   eretu sca
   eretur scalar
   ereturn mat
   eret matrix
   eretu clear
   /* should fail to highlight */
   ereturn `foo'
   eretur li
   ereturn list
   eret post
   eretu repost
   eretur di
   ereturn display

   /* number not highlighted, because it can be an expression */
   err 444
   error 666

   _est h
   _esti hold
   _estim u
   _estima unhold
   _estimat dir
   _estimate clear
   _estimates drop
   _estimates `foo'
   
   e
   exit

   file open
   file r
   file read
   file w
   file write
   file seek
   file set
   file close
   file q
   file query

   findfile


   foreach grue in shadows {
      }
   foreach bleen of loc hooie {
      }
   foreach mike of local frantie {
      }
   foreach small of glo biggie {
      }
   foreach big of global smallie {
      }
   foreach var of var thevars {
      }
   foreach var of varlist thevars {
      }
   foreach makeme of new newvarlist {
      }
   foreach makeme of newlist newvarlist {
      }
   foreach number of num somenumlist {
      }
   foreach number of numlist somenumlist {
      }
   forv fooie = 1/4 {
      }
   forvalues aNum = 2(3)14 {
      }

   gettoken foo : griminy, parse(" ,")
   gettoken bleeble bauble: foo, parse(",")
   gettoken (local) foo: complex
   gettoken (global) hey ho   : ho
   gettoken (local) bleen (global) hooie: gomp
   gettoken bleen (local) bling : how
   hexdump

   if foo fuggy
   if `this' that
   if `those' {
      something
      }
   if foo {
      fuggy
      }
   else `fortuna'
   else frantabulous
   else {
      frantabulous
      }

   include somefile.doh
   
   levelsof

   /* macro stuff */
   gl fooie
   global fooie
   global `l`fooie''
   lo hmm
   loc ``ooie''
   local fooie
   tempvar ding
   tempname dong
   tempname ding dong
   tempfile the
   tempfile this is a test of many files
   /* right */
   loc ++witch
   local --is
   /* wrong --- will highlight with obsolete*/
   loc which--
   local wrong++
   /* right, though */
   display `foo++'
   display `++foo'
   display `--foo'
   display `--`foo''
   display `+++foo'
   ma di
   macro dir
   macro drop 
   ma l
   macro list
   ma s
   macro shift

   glo fooey : properties
   glo dingle : ty
   global dingle : type
   loc dingle : f
   local dingle : format
   gl h : val lab
   global h : value label
   loc h : var lab
   local h: variable label
   gl h : data l
   global h: data label
   local h: sort
   local h: sortedby
   loc h : lab
   local h : label
   gl h : constraint
   global h: constraint
   loc h : char
   local h: char
   gl h : permname
   global h : permname
   local durn: adosubdir "howdy"
   loc h : dir
   local h: sysdir
   gl h : env
   global h : environment
   loc h : e(scalars)
   local h : e(macros)
   gl h: e(matrices)
   global h: e(functions)
   loc h : r(scalars)
   local h : r(macros)
   gl h: r(matrices)
   global h: r(functions)
   loc h: s(macros)
   global h: all globals
   global h: all scalars
   loc h: all matrices
   local h: all numeric scalars
   local h: all string scalars
   local h: all scalars
   local h: di
   local h: display
   gl h : list
   global h : rown
   gl h : rownames
   local h : coln
   local h :colnames
   local h : rowf
   local h : rowfullnames
   local h : colf
   local h : colfullnames
   local h : rowe
   local h : roweq
   local h : cole
   local h : coleq
   glo foo: tsnorm
   local h : copy local
   local b : copy global
   local h : word
   local h : word count
   /* maybe should change the number highlight? */
   loc h : word 43 of
   local h : word `foo' of bar
   /* should fail */
   local h : word me of you 
   local h : piece
   local h : length loc
   local h : length local
   local h : length gl
   local h : length global hii
   local h : subinstr gl
   local h : subinstr global hi
   local h : subinstr loc ho
   local h : subinstr local
   /* these have become undocumented, fixme */
   local h : tempv
   local h : tempvar
   local h : tempf
   local h : tempfile
   /* needs to be fixed (maybe)! */
   local `foo' : tempfile

   /* macro lists */

   loc foo : list uniq bar
   global foo : list dups bar
   glob foo: list sort bar
   loca foo : list retok bar
   local foo:list retokenize bar
   glo foo : list clean bar
   glob foo : list a | b
   globa foo: list c & d
   global foo : list ding - dong
   global foo: list this == that
   global foo: list this === that
   loc foo: list hey in ho
   local foo: list sizeof hey
   local foo: list posof "this is something" in hooie

   /* ahh the macros are over */

   makecns a
   matcproc a b c

   /* hooie should be macro name */
   marksample hooie
   mark
   markout
   markin
   svymarkout fiem

   matlist
   
   /* matrix commands */
   mat ac m
   matr accum matt
   matri glsa matt
   matrix glsaccum matt
   mat opaccum matt
   matrix veca matt
   matrix vecaccum matt
   /* not listed but still accepted */
   matr makeCns foo 
   matri dispCns
   matcproc a b c

   /* dangerous keyword highlighting which is unavoidable */
   mat def foo
   mat defin
   matrix in
   mat input bling
   matrix jjj

   mat dis foo
   matrix dissimilarity bleen
   
   mat eigenval vgy mmk
   mat eigenvalues bleeble blob 
   /* nothing for matrix get */
   mat_put_rr bling
   mkmat
   svmat fooey
   svmat double noodle
   matname foo
   mat rown njk = kjj
   matrix rownames rrr = ccc
   mat coln ccc = rrr
   matrix colnames ccc = rrr
   mat rowe hi = ho
   mat roweq ho = hi
   mat cole ho = hi
   mat coleq ho = hi
   mat sco fooey
   matrix score fooey
   mat svd g h j
   matrix syme jwjwk foo
   matrix d
   matrix dir
   mat l bleen
   matrix list bleen
   matrix rename foo bar
   /* not quite right, but I'm really stumped. */
   matrix drop mat1 mat2 mat3 mat4
   matrix drop _all
   matrix `foo'
   
   /* phew, matrix is finally done */

   mor
   more

   numlist

pause on
pause off
pause "fuggy"

   /* don't know what would be different here */
   program fooey, plugin
      /* hmm.... */
   end
   
   postfile
   post
   postclose
   postutil dir
   postutil clear

   _predict
   preserve
   restore
   
   pr def foo
      pro bar
         "this is a string"
         display "local freddy hunh?"
      end
   end
   

   /* should fix the following (doesn't need to be flush left
   the problem really is that since define is now optional, it is hard
for the syntax to be corrected */
   pr di
   program dir

   program drop fooie
   pr l fooie
   program list fooie 
   
   qui blah
   quietly {
      n bling
      noisily blang
      }
   set ou p
   set output proc
   set output i
   set ou inform
   set ou e
   set output error

   _ret hold
   _retu res
   _retur restore
   _return drop
   _return dir
   _return `foo'

   return `foo'
   ret clear
   retu sca foo
   return sca foo
   ret loc foo
   return local foo
   /* the third item ought to be a matrix */
   ret mat matt hhh
   return matrix matt mmm
   ret add
   return add

   eret clear
   ereturn clear
   eret post m1 m2
   ereturn post
   eret sca
   ereturn scalar
   eret loc foo
   ereturn local foo
   ereturn matrix bleen
   eret repost
   ereturn repost

   sret clear
   sret loc foo
   sreturn local foo
   /* end return commands */
   
   _rmcoll
   _rmdcoll
   set rmsg on
   set rmsg off
   
   _robust

   /* sca is an ambiguous abbreviation sc for scatterplot and sca for scalar ! */
   sca foo
   scalar define foo
   scalar foo
   scalar di
   scalar dir
   sca l
   sca list 
   scalar drop

   /* new serset commands */

   serset cr
   serset create
   serset create_xmedians
   serset create_cspline
   serset set
   serset sort
   serset su
   serset summarize
   serset
   serset use
   serset reset_id
   serset drop
   serset clear
   serset dir
   file sersetwrite 
   file sersetread

   /* oops - Stata has extended macro functions just for serset */

   loc foo: serset id
   loc foo: serset k
   loc foo: serset N
   loc foo: serset varnum
   glo foo: serset type
   glo foo: serset format
   glo foo: serset varnames
   glo foo: serset min
   glo foo: serset max

   signestimationsample
   checkestimationsample
   
   sleep

   /* smcl */

   INCLUDE help
   /* syntax 1 and 2 */
   {sf}
   {sf:foo}
   {it}
   {it:foo}
   {bf}
   {bf:bar}
   /* should fail */
   {sf should fail}
   {sf should:fail}
   
   {input}
   {input:foo}
   {error}
   {error:hahah}
   {result}
   {result:shocking}
   {text}
   {text:for later reading}

   {inp}
   {inp:foo}
   {err}
   {err:hahah}
   {res}
   {res:shock}
   {txt}
   {txt:later}

   {cmd}
   {cmd:Go Home!}
   /* hybrid syntax */
   {cmdab:this:that}

   /* no checking for bad opt syntax */
   {opt fooey}
   {opt foo(bar)}
   {opt foo(bar,yah)}
   {opt foo(bar|yah)}
   {opt foo:bar}
   {opt foo:bar(3)}
   {opt foo:bar(from,to)}
   {opt foo:bar(this|that)}

   /* syntax 1 & 2 */
   {hilite}
   {hilite:of the day}
   {hi}
   {hi:how are you}

   /* syntax 2 & 3 */
   {ul on}
   {ul:is no. 1 in basketball}
   {ul off}
   /* should fail */
   {ul bogus}

   /* syntax 2 & 3 (book says 2 & 4 but illustrates with 2 & 3) */
   {*:comment}
   {* this is a comment}

   {hline}
   {hline 20}
   {hline bogus}
   {.-}
   {hline `this'}

   {dup 23:some}
   {dup bogus:some}

   {c 666}
   {char 333}
   {char bogus}

   {reset}
   
   /* link commands.... */
   {help someword}
   {help someword:clickable phrase}
   {helpb bold}
   {helpb bold:hack}
   {manhelp unix GS}
   {manhelp unix GS:eunuchs}
   {manhelp damn G:its own syntax}
   {manhelpi fooey Q:cakes}
   /* these should fail */
   {manhelp unix}
   {manhelp this should fail:if I had time}
   {manhelpi fooey}
   /* need yet another @#@#$@ syntax for this hack */
   {help stata##anchors}
   {help stata##anchor|viewer}
   {help stata##anchor:subtext}
   {help stata##anchor|viewer:subtext}
   {marker jumphere}
   {marker ...}
   {help_d:fooie}

   {newvar}
   {newvar:13}
   {var}
   {var:fooey}
   {varname}
   {varname:fooey}
   {vars}
   {vars:huey duey looie}
   {varlist}
   {varlist: huey duey looie}
   {depvar}
   {depvar:fooey gooey}
   {depvars}
   {depvars: ha ho}
   {depvarlist}
   {depvarlist: hee high ho}
   {indepvars}
   {indepvars: hoo who}
   {ifin}
   {weight}
   {dtype}
   {search goofay}
   {search goofus galant:clickable}

   {search_d:fooey}
   {dialog hello}
   {dialog hellp:clickable}
   {browse fooey}
   {browse fooey:click}
   {view fooey}
   {view fooey:click}
   {view_d:hahah}
   {news:is bad}
   {net fishing}
   {net fishing:wide}
   {net_d:hello}
   {netfrom_d:howdydoody}
   {ado foo}
   {ado foo:bar}
   {ado_d : bar}
   {update howdy}
   {update howdy:doody}
   {update_d:morning}
   {back:and forth}
   {clearmore:fooey}
   {stata corp}
   {stata corp:click}
   {matacmd arrg}
   {matacmd arrg:ahoy}
   /* for line mode */
   {title:howdy doody}
   {center:middle}
   {centre:muggle}
   {rcenter:teehee}
   {rcentre 33:friday!}
   {center 23:fiddle}
   {center 43:fuddle}
   {center bogus:haha}
   {centre 59:bosh!}
   {right:wing neocon}
   {lalign 69:ihtfp}
   {ralign 666:nationalist}
   {dlgtab 34:fooey}
   {dlgtab 4 2: hello}
   {dlgtab : fooey}
   {...}
   {col bogus}
   {col 32}
   {col `this'}
   {space bogus}
   {space 43}
   {tab}

   /* for paragraph mode */
   {p}
   {p 4}
   {p bogus}
   {p `hoo'}
   {p 3 4}
   {p 3 `foo' 5}
   {p 3 4 5 oh no}
   {p_end}
   {p2colset 1 2 `foo' 4}
   {p2colset 1 2 3 4 5}
   {p2col 1 2 3 4:something goes here}
   {p2col: something goes here}
   {p2col 1 2:this is bad}
   {p2line 1 2}
   {p2line}
   {p2line 1 2 `bad'}
   {p2colreset}
   /* uh oh, all sorts of equivalent directives */
   {pstd}
   {psee}
   {phang}
   {pmore}
   {pin}
   {phang2}
   {pmore2}
   {pin2}
   {phang3}
   {pmore3}
   {pin3}
   {p_end}
   {p2colset 1 2 3 4}
   /* next one is bad */
   {p2col 2 3 4 5: fooey}
   {p2col : first col}
   {p2colreset}
   {synoptset}
   {synoptset 5 tabbed}
   /* next one is bad */
   {synoptset 5 6}
   {synopthdr}
   {synopthdr: damn}
   {syntab: this}
   {synopt: is}
   {p2coldent: no fun}
   {synoptline}

   {bind:all this together}
   {break}
   {asis}
   {s6hlp}
   {ccl}
   {char 7}
   {c S|}
   /* end smcl, finally */

   args mac1 mactheknife
   args foo
   args foo1 foo2 foo3 foo4, bogus

   /* syntax */
   /* no attempt to get this to fontify properly, sadly enough, because there really is no grammar to the syntax statement */
   syntax

   varlist
   varname
   newvarlist
   newvarname

   /* back to things I can handle */
   sysdir
   sysdir l
   sysdir list
   sysdir set
   personal
   personal dir
   adopath
   adopath + dingle
   adopath ++ freeble
   adopath - foo
   set a 30
   set adosize 99
   tabdisp

   timer clear
   timer on 4
   timer off 3
   timer list 55
   /* this is bad */
   timer off

   token tokeni tokeniz tokenize

set tr on
set trace off
   /* perhaps should add numbers as trailing argument. Some other day. */
   set traced 44
   set tracedepth 34
   set tracee on
   set traceexpand off
   set tracesep on
   set traces off
   set tracei on
   set traceindent off
   set tracen on
   set tracenumber off
   set traceh "fooey"
   set tracehilite "hehe"

   unab lfoo : dingle
   tsunab lfoo : dongle

   unabcmd

   /* more complicated version commands :<( */
version 8
version 8: fooie
   viewsource

   while foo {
      this is some stuff
      }
   /* window commands... seem to have been moved out of the manual?!?*/
   /* will not bother with platform dependencies... */
   win fop
   window fopen
   win fs
   window fsave
   win man minimize
   window manage restore
   win man prefs load
   win man prefs save
   win man prefs default
   win man update variable
   win man associate
   window man maintitle "fooey"
   windo manag maintitle reset
   window manage forward command
   window manage forward doeditor
   window manage forward graph
   window manage forward help
   window manage forward results
   window manage forward review
   window manage forward variables
   window manage forward viewer
   
   win man print graph
   win man forward graph
   win man close graph
   win man rename graph

   win man print viewer
   win man forward viewer
   win man close viewer

   window m clear
   win menu append submenu
   win m append item
   window menu append separator
   window menu refresh
   window menu add_recentfiles

   /* obsolete?? */
   window menu popout
   window menu set
   window menu append popout
   window menu append   string

   window push
   window stop stop
   window stopbox note
   window stop rusure
   /* end programming manual */
   /* the miserable graph commands... */

   gr7 using foo
   graph7 this that

   gr bar
   graph bar
   gr hbar
   graph hbar

   graph box
   graph hbox

   graph combine

   graph copy

   gr des
   graph describe

   graph dir

   graph di
   gr display

   graph dot

   graph drop
   graph drop _all

   gr export
   graph export
   
   graph matrix

   /* these /should/ be previous testing lines, since they are documented in other */
   /*  manuals. Still... they are repeated here */

   histogram
   symplot
   quantile
   qnorm
   pnorm
   qchi
   pchi
   qqplot
   gladder
   qladder
   spikeplot
   dotplot
   sunflower
   
   kdensity
   lowess
   lpoly
   
   avplot
   cprplot
   lvr2plot
   rvfplot
   rvpplot
   ac
   pac
   pergram
   cumsp
   xcorr
   wntestb
   varfcast graph
   varirf graph
   varirf ograph
   varirf cgraph
   fcast graph
   varstable
   vecstable
   irf graph
   irf ograph
   irf cgraph

   xtline

   sts graph
   strate
   ltable
   stci
   stphtest
   stphplot
   stcoxkm
   estat phtest
   stcurve
   
   roctab
   rocplot
   roccomp
   lroc
   lsens

   biplot
   cluster dendrogram
   screeplot
   scoreplot
   loadingplot
   procoverlay
   cabiplot
   caprojection
   mcaplot
   mcaprojection
   mdsconfig
   mdsshepard

   cusum
   cchart
   pchart
   rchart
   xchart
   shewhart
   serrbar
   
   tabodds
   pkexamine

   /* end of so-called graph other */
   gr pie
   graph pie

   graph print
   gr q
   graph query

   graph rename
   gr save

   gr set

   gr twoway fee fie fo
   gr twoway (bar foo) (bar fee)
   twoway bar foo || bar fee

   /* forget the stuff under twoway */
   /* redone in the order of the commands themselves to accommodate abbrevs */

   gr tw scatter
   
   graph twoway area y
   twoway area
   gr twoway bar
   graph twoway bar y
   tw con
   two connected
   graph twoway dot y
   graph twoway dropline y
   graph twoway fpfit y
   graph twoway fpfitci y
   graph twoway function y
   graph twoway hist
   tw histogram
   graph twoway kdensity
   graph twoway lfit
   graph twoway lfitci
   gr two line
   twow line
   line foo bar
   graph twoway lowess
   tw lpoly
   two lpolyci
   graph twoway mband
   graph twoway mspline
   twoway pcarrow
   twoway pcbarrow
   tw pcbarrowi
   two pccapsym
   twoway pci
   two pcscatter
   two pcspike
   graph twoway qfit
   graph twoway qfitci
   graph twoway rarea
   graph twoway rbar
   graph twoway rcap
   graph twoway rcapsym
   graph twoway rconnected
   tw rcon
   twow rl
   graph twoway rline
   twowa rsc
   graph twoway rscatter
   graph twoway rspike
   graph tw scatter
   two sc
   scat
   graph twowa scatteri
   graph twoway spike
   twoway tsline
   two tsrline
   graph use

   palette color
   palette line
   palette linepalette
   palette symbol
   palette symbolpalette

   q graph
   query graphics
   set graphics on
   set graphics off
   set printcolor auto
   set printcolor automatic
   set printcolor asis
   set printcolor gs1
   set printcolor gs2
   set printcolor gs3
   set copycolor auto
   set copycolor automatic
   set copycolor asis
   set copycolor gs1
   set copycolor gs2
   set copycolor gs3

   set scheme

   /* now for some Mata content */
   /* first - all the reserved words */
   aggregate
   array
   boolean
   break /* used elsewhere */
   byte /* used elsewhere */
   case
   catch
   class
   colvector
   complex
   const
   continue /* used elsewhere */
   default
   delegate
   delete
   do /* used elsewhere */ 
   double /* used elsewhere */
   else /* used elsewhere */
   eltypedef
   // end /* used elsewhere, commented out here because of indentation */
   enum
   explicit
   export
   external
   float /* used elsewhere */
   for /* used elsewhere */
   friend
   function
   
   global /* used elsewhere */ 
   goto

   if /* used elsewhere */
   inline
   int /* used elsewhere */

   local /* used elsewhere */
   long /* used elsewhere */

   mata /* always has subcommands? */
   matrix /* used elsewhere */

   namespace
   new
   NULL
   numeric

   operator
   orgtypedef

   pointer
   polymorphic
   pragma
   private
   protected
   public

   quad

   real
   return
   rowvector

   scalar /* used elsewhere */
   short
   signed
   /* sizeof seems to be a function, not a future keyword */
   sizeof
   static
   string
   struct
   super
   switch
   template
   this
   throw
   transmorphic
   try
   typedef
   typename
   union
   unsigned
   using /* used elsewhere */
   vector
version /* used elsewhere */
   virtual
   volatile
   void
   while /* used elsewhere */ 
   
   /* mata building blocks */
   for(hey; ho; wego) {
      hmmmm
      }

   /* trying to have some mata stuff */
   pragma unset
   pragma unused
   
   /* from "commands for controlling mata" */
   mata:
      this is mata code
   end

   mata
   this is mata code (which needs no end statement, as it really needs to be a block, I think.) Sheesh!
   what about this
   
   mata clear
   mata d
   mata describe
   mata drop
   mata help

   mata matsave aFile some names
   mata matuse bar
   mata matd breeble
   mata matd box
   mata matdescribe box

   mata memory

   mata mlib create foo
   mata mlib add bar
   mata mlib index
   mata mlib q
   mata mlib query

   mata mosave momoney()
   mata rename
   
   mata query
   mata set matacache
   mata set matalnum on
   mata set matalnum off
   mata set mataoptimize on
   mata set mataoptimize off
   mata set matafavor space
   mata set matafavor speed
   mata set matastrict on
   mata set matastrict off
   mata set matalibs
   mata set matamofirst on
   mata set matamofirst off
   mata stata
   mata which

   /* mata functions */
   /* now in M-5 order because M-4 has some functions missing */
   abs()
   adosubdir()
   all()
   any()
   allof()
   anyof()
   args()
   ascii()
   char()
   assert()
   asserteq()
   blockdiag()

   bufio()
   bufbyteorder()
   bufmissingvalue()
   bufput()
   bufget()
   fbufput()
   fbufget()
   bufbfmtlen()
   bufbfmtisnum()

   byteorder()
   C()
   c()
   callersversion()
   cat()

   chdir()
   _chdir()
   mkdir()
   _mkdir()
   rmdir()
   _rmdir()
   pwd()
   
   cholesky()
   _cholesky()

   cholinv()
   _cholinv()

   cholsolve()
   _cholsolve()

   comb()
   cond()
   conj()
   corr()
   cross()
   crossdev()
   cvpermute()

   /* date functions... */
   /* all the date functions... (which are normal functions, too */
   clock()
   mdyhms()
   dhms()
   hms()
   hh()
   mm()
   ss()
   dofc()

   Cofc()
   Clock()
   Cmdyhms()
   Cdhms()
   Chms()
   hhC()
   mmC()
   ssC()
   dofC()

   date()
   mdy()
   yw()
   ym()
   yq()
   yh()
   cofd()
   Cofd()
   month()
   day()
   year()
   dow()
   week()
   quarter()
   halfyear()
   doy()

   yearly()
   yofd()
   dofy()

   halfyearly()
   hofd()
   dofh()

   quarterly()
   qofd()
   dofq()

   monthly()
   mofd()
   dofm()

   weekly()
   wofd()
   dofw()

   hours()
   minutes()
   seconds()
   msofhours()
   msofminutes()
   msofseconds()

   /* end of mata date functions */
   designmatrix()
   det()
   dettriangular()
   _diag()
   diag()
   diag0cnt()
   diagonal()
   dir()
   direxists()
   direxternal()
   display()
   displayas()
   displayflush()
   dsign()
   e()

   editmissing()
   _editmissing()
   
   edittoint()
   _edittoint()
   edittointtol()
   _edittointtol()

   edittozero()
   _edittozero()
   edittozerotol()
   _edittozerotol()

   editvalue()
   _editvalue()

   eigensystem()
   lefteigensystem()
   eigenvalues()
   symeigensystem()
   symeigenvalues()
   _eigensystem()
   _lefteigensystem()
   _eigenvalues()
   _symeigensystem()
   _symeigenvalues()

   eltype()
   orgtype()

   epsilon()

   _equilrc()
   _equilr()
   _equilc()
   _perhapsequilrc()
   _perhapsequilr()
   _perhapsequilc()
   rowscalefactors()
   colscalefactors()

   error()
   _error()

   errprintf()
   exit()

   exp()
   ln()
   log()
   log10()

   factorial()
   lnfactorial() /* already regular function */
   gamma()
   lngamma() /* already regular function */
   digamma() /* already regular function */
   trigamma() /* already regular function */

   favorspeed()
   ferrortext()
   freturncode()

   fft()
   invfft()
   _fft()
   _invfft()
   convolve()
   deconvolve()
   Corr()
   ftperiodogram()
   ftpad()
   ftwrap()
   ftunwrap()
   ftretime()
   ftfreqs()

   fileexists()
   _fillmissing()

   findexternal()
   crexternal()
   rmexternal()
   nameexternal()

   findfile()
   floatround()
   fmtwidth()

   fopen()
   _fopen()
   fclose()
   _fclose()
   fget()
   _fget()
   fgetnl()
   _fgetnl()
   fread()
   _fread()
   fput()
   _fput()
   fwrite()
   _fwrite()
   fgetmatrix()
   _fgetmatrix()
   fputmatrix()
   _fputmatrix()
   fstatus()
   ftell()
   _ftell()
   fseek()
   _fseek()
   ftruncate()
   _ftruncate()

   fullsvd()
   fullsdiag()
   _fullsvd()
   _svd_la()

   ghk()
   ghkfastsetup()
   ghkfast()

   halton()
   _halton()

   Hilbert()
   invHilbert()

   I()

   inbase()
   frombase()

   indexnot()
   invorder()
   revorder()
   invsym()
   _invsym()
   invtokens()
   isdiagonal()
   isfleeting()

   isreal()
   iscomplex()
   isstring()
   ispointer()
   isrealvalues()
   issymmetric()
   issymmetriconly()
   isview()
   J()
   liststruct()

   logit()
   invlogit()
   cloglog()
   invcloglog()

   lowertriangle()
   uppertriangle()
   _lowertriangle()
   _uppertriangle()

   lud()
   _lud()
   _lud_la()

   luinv()
   _luinv()
   _luinv_la()

   lusolve()
   _lusolve()

   makesymmetric()
   _makesymmetric()

   matexpsym()
   matlogsym()
   _matexpsym()
   _matlogsym()
   matpowersym()
   _matpowersym()

   mean()
   variance()
   quadvariance()
   meanvariance()
   correlation()
   quadcorrelation()

   mindouble()
   maxdouble()
   smallestdouble()

   minindex()
   maxindex()

   /* many of these are egen functions */
   rowmin()
   colmin()
   min()
   rowmax()
   colmax()
   max()
   rowminmax()
   colminmax()
   minmax()
   rowmaxabs()
   colmaxabs()

   missing() /* already regular function */ 
   rowmissing()
   colmissing()
   nonmissing()
   rownonmissing()
   colnonmissing()
   missingof()

   mod()
   more()
   setmore()
   setmoreonexit()

   /* mostly regular stata functions */
   norm()
   normalden()
   normal()
   invnormal()
   lnnormalden()
   lnnormal()
   binormal()
   chi2()
   chi2tail()
   invchi2()
   invchi2tail()
   nchi2()
   invnchi2()
   npnchi2()
   tden()
   ttail()
   invttail()
   Fden()
   F()
   Ftail()
   invF()
   invFtail()
   nFden()
   nFtail()
   invnFtail()
   binomial()
   binomialtail()
   invbinomial()
   invbinomialtail()
   betaden()
   ibeta()
   ibetatail()
   invibeta()
   invibetatail()
   nbetaden()
   nibeta()
   invnibeta()
   gammaden()
   gammap()
   gammaptail()
   invgammap()
   invgammaptail()
   dgammapda()
   dgammapdx()
   dgammapdada()
   dgammapdadx()
   dgammapdxdx()

   /* phooey, optimize to the max */
   optimize_init()
   optimize_init_which()
   optimize_init_evaluator()
   optimize_init_type()
   optimize_init_params()
   optimize_init_nmsimplexdeltas()
   optimize_init_argument()
   optimize_init_narguments()
   optimize_init_technique()
   optimize_init_singularHmethod()
   optimize_init_conv_maxiter()
   optimize_init_conv_ptol()
   optimize_init_vtol()
   optimize_init_nrtol()
   optimize_init_valueid()
   optimize_init_tracelevel()
   optimize_init_constraints()
   optimize_init_verbose()

   optimize()
   optimize_evaluate()
   _optimize()
   _optimize_evaluate()

   optimize_result_params()
   optimize_result_value()
   optimize_result_value0()
   optimize_result_gradient()
   optimize_result_scores()
   optimize_result_Hessian()
   optimize_result_V()
   optimize_result_Vtype()
   optimize_result_V_oim()
   optimize_result_V_oig()
   optimize_result_V_robust()
   optimize_result_iterations()
   optimize_result_converged()
   optimize_result_iterationlog()
   optimize_result_errorcode()
   optimize_result_returncode()

   optimize_query()

   panelsetup()
   panelstats()
   panelsubmatrix()
   panelsubview()

   pathjoin()
   pathsplit()
   pathbasename()
   pathsuffix()
   pathrmsuffix()
   pathisurl()
   pathisabs()
   pathasciisuffix()
   pathstatasuffix()
   pathlist()
   pathsubsysdir()
   pathsearchlist()

   pinv()
   _pinv()

   polyeval()
   polysolve()
   polytrim()
   polyderiv()
   polyinteg()
   polyadd()
   polymult()
   polydiv()
   polyroots()

   printf()
   sprintf()

   qrd()
   qrdp()
   hqrd()
   _hqrd()
   hqrdmultq()
   hqrdmultqlt()
   hqrdq()
   hqrdq1()
   hqrdr()
   hqrdr1()
   qrdp()
   hqrdp()
   _hqrdp()
   _hqrdp_la()

   qrinv()
   _qrinv()
   qrsolve()
   _qrsolve()

   quadcross()
   quadcrossdev()

   range()
   rank()
   Re()
   Im()

   reldif()
   mreldif()
   mreldifsym()
   mreldifre()

   rows()
   cols()
   length()
   rowshape()
   colshape()

   runningsum()
   quadrunningsum()
   _runningsum()
   _quadrunningsum()

   select()
   st_select()

   setbreakintr()
   querybreakintr()
   breakkey()
   breakkeyreset()

   sign()
   quadrant()

   sin()
   cos()
   tan()
   asin()
   acos()
   atan()
   atan2()
   asinr()
   acosr()
   atanr()
   arg() /* mata only */
   sinh() /* mata only */
   cosh() /* mata only */
   tanh()
   asinh() /* mata only */
   acosh() /* mata only */
   atanh()
   pi() /* mata only */

   sizeof()
   solve_tol()
   solvelower()
   sort()

   spline3()
   sqrt()
   st_addobs()
   _st_addobs()
   st_addvar()
   _st_addvar()
   st_data()
   _st_data()
   st_sdata()
   _st_sdata()

   st_dir()
   st_dropvar()
   st_dropvar()
   st_dropobsin()
   st_dropobsif()
   st_keepvar()
   st_keepobsin()
   st_keepobsif()

   st_global()

   st_isfmt()
   st_isnumfmt()
   st_isstrfmt()

   st_isname()
   st_islmname()

   st_local()

   st_macroexpand()
   _st_macroexpand()

   st_matrix()
   st_matrixrowstripe()
   st_matrixcolstripe()
   st_replacematrix()

   st_numscalar()
   st_strscalar()

   st_nvar()
   st_nobs()

   st_rclear()
   st_eclear()
   st_sclear()

   st_store()
   st_sstore()
   _st_store()
   _st_sstore()

   st_subview()
   st_tempname()
   st_tempfilename()
   st_tsrevar()
   st_updata()

   st_varformat()
   st_varlabel()
   st_varvaluelabel()
   st_varindex()
   st_varname()
   st_varrename()
   st_vartype()
   st_isnumvar()
   st_isstrvar()

   st_view()
   st_sview()
   st_viewvars()
   st_viewobs()

   st_vlexists()
   st_vldrop()
   st_vlmap()
   st_vlsearch()
   st_vlload()
   st_vlmodify()

   stata()
   _stata()

   stataversion()
   statasetversion()

   /* strdup() is not really a function --- use * */
   strlen()
   strmatch()
   strofreal()
   strpos()
   strreverse()
   strtoreal()
   _strtoreal()

   stritrim()
   strltrim()
   strrtrim()
   strtrim()

   strupper()
   strlower()
   strproper()

   subinstr()
   subinword()
   _substr()

   rowsum()
   colsum()
   sum()
   quadrowsum()
   quadcolsum()
   quadsum()

   svd()
   svdsv()
   _svd()
   _svdsv()
   _svd_la()

   svsolve()
   _svsolve()

   swap()
   Toeplitz()
   
   tokeninit()
   tokeninitstata()
   tokenset()
   tokengetall()
   tokenget()
   tokenpeek()
   tokenrest()
   tokenoffset()
   tokenwchars()
   tokenpchars()
   tokenqchars()
   tokenallownum()
   tokenallowhex()

   tokens()
   trace()

   _transpose()
   _conj()

   transposeonly()
   _transposeonly()

   trunc()
   floor()
   ceil()
   round()

   uniform()
   uniformseed()

   uniqrows()
   unitcircle()

   unlink()
   _unlink()

   valofexternal()
   Vandermonde()

   vec()
   vech()
   invvech()
   
   /* now for some things which give trouble... */
   
   /* macros showing up inside other constructions */
   local ding `r(foo)'
   local dong "this is `bramble' and this is `r(foo)'"
   display as text "this is `r(foo)'"
   /* checking long wrapping */
#delimit ;
   reshape this whole thing as this is
     the start of a long command. ;
#delimit cr
   /* wrapping when comments wipe out the end of a line */
   reshape this whole thing as this is /* foo
*/ the start of a long command.
   
   here is a command with /* a following comment */
   here is - somethin

   /* now there appears to be no way to have the doubleslash */
   
   here is a comment... // fooey
   
   /* end a line properly, because the newline ends the comment... */
   
   here is something followed by a comment // this is a comment
   and this continues the command /// with an extended comment
     with, finally, a conclusion /// and a second long cmment
     hahaha
   // did this end /* this */
   
   here is a partial // this is ok
   hie
     
   reshape this whole thing /// hahah
     /// here is a following comment
     this is the test with the strange intervening line! ///
     worked just fine
   this is
   this is something new
   this has stuff // this is a comment
* this is a comment, just one that is not recognized
   this should be rarer ///
     * heirmj (but it falsely highlights as a comment)

   /* macro highlighting problem */

   append using `file`filenum'`fooie'' 
   append using ``file'' 
   this is a `2'
   split this line properly   

   /* should nesting be allowed? */
   /*  start multi line comment
   /* inline comment */
      stuff which does not recognize its commentness
      the end of multi line comment
*/

   ddd

   /* new stuff from the 8.2 July 23, 2004 upgrade */
   vec foo
   veclmar foo
   vecnorm
   vecrank bar
   vecstable

   

   /* varfcast made obsolete */
   varfcast clear
   varfcast c
   varfcast compute
   varfcast g
   varfcast graph
   /* replaced by fcast; some subcommands disappeared */
   fcast c
   fcast com
   fcast compu
   fcast compute
   fcast g
   fcast gra
   fcast graph
   

   /* varirf is obsolete as of July 23, 2004 */
   varirf a
   varirf add
   varirf
   varirf cg
   varirf cgraph
   varirf cr
   varirf create
   varirf ct
   varirf ctable
   varirf d
   varirf describe
   varirf di
   varirf dir
   varirf drop
   varirf erase
   varirf g
   varirf graph
   varirf og
   varirf ograph
   varirf ren
   varirf rename
   varirf set
   varirf t
   varirf table


   veclmar
   vecnorm
   vecstable

   /* comments and conditionals */
   if this {
      then this {
         though this
         }
      }
   
   /* the acid test for comments --- a legal stata command! */
   display 8*9 + /// comment text
     14 - 10 * /* comment mid line
     /* oops, a nested comment */
     /* and another
     which lasts
     and lasts
*/
  which continues */ 1 + /*
*/ 14 /* on And on */ /2 ///
     + 5 /*
*/ +1 
* this is a comment which has ///
     strange continuation
   display "hah"
   
   
   /* this is a test
      
   of how the indents would work
   /* this is much better */
   ahhh, this is great.
*/ this will screw up, but it is legal in stata
   is odd
   
   oops
   darn /* what will happen here? */
   /* this is a comment */ and here
   
   
   
   /*
      
   what happens now
   this
   
   would be nice to have this indented farther
   this is confusing
   /* Tricky start of new comment
   ahhh, indentation is ok
*/
      
   
      
   
   back to where it should be
*/ fooey
   outside the comment.

   /* things to  on */
   tempvar `foo

   /* problem children */
   "filo foen"
   
      
end
