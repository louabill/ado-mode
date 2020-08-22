*! version 1.15.0.1 August 22, 2020 @ 14:16:17
*! doesn't do anything; made for syntax testing
*! also used as a base to generate keywords for auto-completion
program def syntax_tester, eclass
   "does it understand strings?"
   `"does it understand "nested" strings?"'
   here is a `macro' that could be used 
   if this=="that" { 
      display foo
      }
   /* what about comments? */
   prog d fummel
   end
   program list foo
   program define bleen
   end
   pr droop foo // bad, but highlights anyways because define is optional
   end
   program bleen
   end
   pro dir
   program dir
   
   * a starting line comment
   14 * foobar // a non-comment

// last subversion within a main version is always bad
version 1.0
version 2            
version 2.0
version 2b0  // bad
version 2.1
version 2b1  // bad
version 3.0
version 3.1
version 3.2  // bad
version 4
version 4.0
version 4.1  // bad
version 5
version 5.0
version 5.1  // bad
version 6
version 6.0
version 6.1
version 7
version 7.0
version 7.1  // bad
version 8
version 8.0
version 8.1
version 8.2
version 8.3  // bad
version 9
version 9.0
version 9.1
version 9.2
version 9.3  // bad
version 10
version 10.0
version 10.1
version 10.2 // bad
version 11
version 11.0
version 11.1
version 11.2
version 11.3 // bad
version 12
version 12.0
version 12.1
version 12.2 // bad
version 13
version 13.0
version 13.1
version 13.2 // bad
version 14
version 14.0
version 14.1
version 14.2
version 14.3 // bad
version 15
version 15.0
version 15.1
version 15.2 // bad
version 16   //
version 16.1
version 16.2 // bad
version 17   // bad
version 20   // bad for awhile
version 44   // bad for multiple generations
   
   /* this program does nothing - it merely has some things for testing syntax coloring */
   /* working with the syntax table */
   local  // incomplete (overridden as mata keyword)
   global // incomplete
   local dingle = this - that

   /* stuff from incremental updates which need to be moved when new manuals come out */
   include
   
   /* commands in the order of the manuals (for checking for obsolete commands... */
   /* some arbitrary usages */
   _b[foobar]
   _coef[bleen]
   _se[freeble]

   /* first... [R] the general reference manuals */
   about
   adjust // obsolete in Stata 11
   adoupdate // obsolete in Stata 16
   ado update
   ameans
   an
   ano
   anov
   anova
   // anova postestimation skipped because it matches regress
   areg
   asclogit              // obsolete in Stata 16                           
   asmixlogit            // new in Stata 15 // obsolete in Stata 16
   asmprobit             // obsolete in Stata 16 
   asroprobit            // obsolete in Stata 16 

   betareg               // new in Stata 14
   binreg
   biprobit
   bitest bitesti
   bootstrap
   boxcox
   brier
   bsample
   bstat

   centile

   churdle               // new in Stata 14; incomplete 
   churdle lin           // new in Stata 14 
   churdle linear        // new in Stata 14 
   churdle exp           // new in Stata 14 
   churdle exponential   // new in Stata 14 

   ci
   cii                // changed in Stata 14.1
   ci mean
   ci means
   ci prop
   ci proportions
   ci var
   ci variances
   cii mean
   cii prop
   cii variances
   
   clog         // abbrev obsolete in Stata 16 
   clogi        // abbrev obsolete in Stata 16
   clogit
   cloglog
   cls
   cnr cnre cnreg        // obsolete as of Stata 11
   cnsreg
   /* constraint commands */
   /* first w/o anything */
   cons
   constra
   constraint
   cons de
   const defin 
   constr d
   constr dir
   cons l
   constrai list
   constra drop
   constrain get
   constraint free
   /* bad constraints -- should not highlight, except as mata reserved words */
   /* oops... need to fix for the const abbreviations....*/
   const ge
   const fre
   /* end constraint */

   contrast
   // the copyright commands all go to the same help file
   //    because -copyright- runs -help copyright- and nothing else
   //   so -copyright foo- works...
   //  No new subcommands added as of Stata 16
   //   leaving what is here for past compatability only
   copyright             // subcommands new in Stata 14
   copyright apache
   copyright autolink    // new in Stata 15
   copyright boost
   copyright flexmark    // new in Stata 15
   copyright hamcrest    // new in Stata 15
   copyright icd10
   copyright icu
   copyright jsoup       // new in Stata 15
   copyright lapack
   copyright libharu
   copyright libpng
   copyright mersennetwister
   copyright miglayout
   copyright scintilla
   copyright slf4j       // new in Stata 15
   copyright ttf2pt1
   copyright zlib
   copyright foobar // error (which could be marked as well but why?)

   cor
   corr
   corre
   correl
   correla
   correlat
   correlate
   pwcorr
   
   cpoisson              // new in Stata 14 
   cumul
   cusum

   db
   set maxdb
   /* diagnostic plots */
   symplot
   quantile
   qqplot
   qnorm
   pnorm
   pchi
   qchi

   di
   dis
   disp
   displ
   displa
   display


   do
   ru
   run

   doed
   doedi
   doedit

   dotplot
   dstdize
   istdize

   dydx
   integ
   
   eivreg

   // epitab commands
   // removed from [ST] and moved to [R] in Stata 14
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
   // end epitab

   esize two
   esize twosam
   esize twosample
   esize un // bad
   esize unp
   esize unpair
   esize unpaired
   esizei

   // common estat
   estat ic
   estat su
   estat summarize
   estat vce
   // listed after 'common estat' in [R] in Stata 14
   estat clas // ivprobit
   estat classification // ivprobit logistic logit probit
   estat gof
   estat ic  // again
   estat su
   estat summar
   estat summarize
   estat vce
   
   // estimates from omnibus estimates entry
   estimates `foo'
   est save
   est use
   est des
   est describe
   estim esample
   estim esample:

   est sto
   esti store
   estimat res
   estimate restore
   estimates q
   estimates query
   // the following cannot be fixed because -estimates- is OK by itself
   estimates d // obsolete abbrev in Stata 10
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

   estimates descr  // again
   estimates r
   estimates replay

   estimates t  // obsolete abbrev starting Stata 10
   estimates tab
   estimates table
   estimates sel        // new in Stata 16
   estimates selected   // new in Stata 16 
   estimates f // obsolete abbrev starting Stata 11
   estimates for
   estimates st // obsolete abbrev starting Stata 10
   estima stat
   estimates stats
   // once again: highlight because -estimates- allows 0 subcommands
   // estimates change is not in Stata 11
   estim ch // highlights -ch- as obsolete because of -chelp-'s demise
   estima change
   /* end estimates */
   e // dumb abbreviation
   ex
   exi
   exit

   exlogistic
   expoisson
   
   fracpoly // obsolete in Stata 13
   fracgen  // obsolete in Stata 13
   fp
   fp gen
   fp gener
   fp generate

   // fracreg new in Stata 14 
   fracreg log
   fracreg logit
   fracreg pr
   fracreg prob
   fracreg probit
   // end fracreg
   
   frontier
   fvrevar

   fvset b
   fvset base
   fvset d
   fvset design
   fvset clear
   fvset report
   fvset cl // bad
   fvset repo // bad
   
   gllamm  // will only highlight if it is installed
   glm

   blogit  // obsolete in Stata 14 
   bprobit // obsolete in Stata 14 
   glogit  // obsolete in Stata 14 
   gprobit // obsolete in Stata 14 

   gmm
   grmeanby
   
   hausman
   heckman
   heckoprobit
   heckpoisson           // new in Stata 15
   heckprob // synonym for heckprobit (ugh) in Stata 14
   heckprobit

   h
   he
   hel
   help

   ch che chel chelp // obsolete in Stata 14
   whelp

   hetoprobit  // new in Stata 16
   hetprobit
   hetprob // synonym for hetprobit in Stata 14
   hetregress            // new in Stata 15
   hist
   histogram
   hsearch // obsolete in Stata 12

   icc
   
   intreg

   ivpoisson // incomplete
   ivpoisson gmm
   ivpoisson cfunc
   ivpoisson cfunction

   ivprobit
   ivreg                 // obsolete in Stata 10
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

   lfit                  // obsolete in Stata 9; not marked because of twoway (lfit ...)
   score                 // obsolete in Stata 9

   lincom
   linktest

   lnskew0
   bcskew0


   lo     // obsolete in Stata 6
   loo    // obsolete in Stata 6
   lookup // obsolete in Stata 6
   log
   log query
   log c
   log close
   log of
   log off
   log using
   cmdlog
   cmdlog c
   cmdlog close
   cmdlog of
   cmdlog off
   cmdlog on
   // undocumented
   cmdlog query
   
   set logtype t
   set logtype text
   set logtype s
   set logtype smcl
   set li
   set linesize
   
   logistic

   logi    // abbrev obsolete in Stata 16
   logit

   loneway
   lowess
   lpoly
   lroc
   lrtest
   lsens
   lv

   margins
   marginsplot

   set mat       // obsolete in Stata 16 
   set matsize   // obsolete in Stata 16 

   // inside 'maximize' help
   set maxiter

   mean
   // mfp is simple now; in Stata 11 it became a prefix command
   mfp
   // mfp postestimation
   fracplot
   fracpred
   // the old mfp non-prefix commands are not needed
   mfp logit
   mfp clogit
   mfp clogit() // absurd but should unhighlight clogit()
   
   // mfx is obsolete as of Stata 11
   // start obsolete block
   mfx
   mfx c
   mfx compute
   mfx r
   mfx replay
   // end obsolete block

   misstable sum
   misstable summarize
   misstable pat
   misstable patterns
   misstable tree
   misstable nest
   misstable nested

   mkspline

   ml mod
   ml model
   ml clear
   ml cle // bad
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
   ml trace // incomplete
   ml trace on
   ml trace off
   ml trace foo // no highlight
   ml count // OK
   ml count clear
   ml count on
   ml count off
   ml max
   ml maximize
   ml gr
   ml graph
   ml di
   ml display
   ml foot
   ml footnote
   ml score
   // start obsolete block (Stata 8 or earlier)
   ml b
   ml begin
   ml dep
   ml depname
   ml f
   ml function
   ml ml
   ml mlout
   // end obsolete block
   * etc
   // these are 
   mleval
   mlsum
   mlvecsum
   mlmatsum
   mlmatbysum  // has been since at least Stata 9
   
   mlexp
   
   mlog    // abbrev obsolete in Stata 16 
   mlogi   // abbrev obsolete in Stata 16
   mlogit

   set mo // should not highlight
   set mo on
   set more off
   set p // bad
   set pa
   set pagesize

   mprobit

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
   ado update    // new in Stata 16 
   ado uninstall

   net search

   /* netio, which is also in set */
   se httpproxy on
   set httpproxy off
   set httpproxy foo // bad
   set httpproxyhost
   se httpproxyport
   set httpproxyauth bleen // bad
   set httpproxyauth on
   set httpproxyauth off
   set httpproxyuser
   set httpproxypw
   set timeout1
   set timeout2

   news

   nl

   nlinit  // obsolete in Stata 9

   nlcom
   
   nlsur

   npregress kernel      // new in Stata 15
   npregress  // incomplete
   npregress series     // new in Stata 16 

   nptrend

   olog   // abbrev obsolete in Stata 16 
   ologi  // abbrev obsolete in Stata 16 
   ologit 

   on
   one
   onew
   onewa
   oneway

   oprob
   oprobi  // abbrev obsolete in Stata 16 
   oprobit // abbrev obsolete in Stata 16 

   orthog
   orthpoly   

   pcorr
   permute

   // in the order of the manual and not in the -pk- Vorkapitel
   pkcollapse
   pkcross
   pkequiv
   pkexamine
   pkshape
   pksumm

   poisson
   poisgof  // obsolete in Stata 9
   estat gof

   postest  // new in Stata 14; strange to have in a do-file
   predict
   predictnl
   
   prob    // abbrev obsolete in Stata 16 
   probi   // abbrev obsolete in Stata 16 
   probit

   dprobit // obsolete in Stata 11

   proportion

   prtest
   prtesti

   // pw commands
   pwcompare
   pwmean
   
   /* qc commands */
   cchart
   pchart
   rchart
   xchart
   shewhart

   // qreg commands
   qreg
   iqreg
   sqreg
   bsqreg
   _qreg // obsolete in Stata 13 

   q
   query
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
   que java       // new in Stata 16 
   qu putdocx     // new partway into Stata 16
   quer python    // new in Stata 16
   query random   // new in Stata 16 
   que unicode  // new in Stata 14 
   q oth
   q other
   query `foo'

   ranksum
   median
   
   ratio

   reg3
   // highlights all regress's? nope just the last one
   regress : regress : regress
   regress a b, vce(robust) 

   reg       // abbrev obsolete in Stata 16 
   regr      // abbrev obsolete in Stata 16 
   regre     // abbrev obsolete in Stata 16 
   regres    // abbrev obsolete in Stata 16 
   regress

   // regress postestimation left out on purpose here

   // !! fix me? 
   #r
   #re
   #rev
   #revi
   #revie
   #review

   /* _rmcoll in [P] _huber obsolete */
   _rmcoll
   _huber

   /* following manual rather than short entry */
   roccomp
   rocgold

   rocfit
   // rocfit post-est
   rocplot

   rocreg
   estat nproc

   rocregplot

   roctab
   
   rologit  // obsolete in Stata 16 

   rreg

   runtest
   
   sampsi // obsolete in Stata 13 
   
   scobit
   
   sdtest
   sdtesti
   robvar

   search
   set searchdefault local /* rest under set */
   set searchdefault net
   set searchdefault all
   set searchdefault foo // bad
   
   findit // obsolete in Stata 13 
   
   serrbar
   /* set commands */
   set a
   set adosize
   set autotabgraphs // incomplete
   set autotabgraphs on // win only
   set autotabgraphs off // win only
   set cformat // added in Stata 11.1
   set charset mac     // obsolete in Stata 14
   set charset latin1  // // obsolete in Stata 14
   
   set checksum // incomplete
   set checksum on
   set checksum off
   
   set clevel // new in Stata 14

   set coeftabresults // incomplete
   set coeftabresults on
   set coeftabresults off

   set conren // unix console only
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
   set conren ulof
   set conren uloff
   set conren reset

   set copycolor 
   set copycolor auto
   set copycolor automatic
   set copycolor asis
   set copycolor gs1
   set copycolor gs2
   set copycolor gs3

   set dockable
   set dockable on
   set dockable off

   // not in Stata 16 manuals
   set dockingg
   set dockingg on
   set dockingguides off

   // added partway into Stata 16
   set docx_hardbreak
   set docx_hardbreak on   
   set docx_paramode
   set docx_paramode off
   set dots
   set dots on


   set doublebuffer
   set doublebuffer on
   set doublebuffer off

   set `foo'  // should not highlight? should?
   set dp 
   set dp com
   set dp comma
   set dp per
   set dp period

   set emptycells
   set emptycells keep
   set emptycells drop

   // set eolchar undocumented as of Stata 15
   // start obsolete block
   set eolch 
   set eolcha 
   set eolch mac
   set eolchar unix
   // end obsolete block 

   set fastscroll // incomplete
   set fastscroll on
   set fastscroll off

   // out of pdf docs in Stata 14, but still has help file
   set floatresults // incomplete
   set floatresults on
   set floatresults off
   
   set floatwindows on
   set floatwindows off

   set fredkey // new in Stata 15

   // new in Stata 16
   set fvbase
   set fvbase on
   set fvbase off
   
   set fvlabel // incomplete
   set fvlabel on
   set fvlabel off

   set fvtrack // incomplete
   set fvtrack term
   set fvtrack factor
   set fvtrack foobar // bad
   
   set fvwrap

   set fvwrapon // incomplete
   set fvwrapon word
   set fvwrapon width

   set g    // incomplete
   set grap // incomplete
   set g on
   set graphics off

   set haverdir

   set httpproxy // incomplete
   set httpproxy on
   set httpproxy off

   set httpproxya // incomplete
   set httpproxya on
   set httpproxyauth // incomplete
   set httpproxyauth off

   set httpproxyhost
   set httpproxyport

   set httpproxypw
   set httpproxyuser

   set icmap on // obsolete in Stata 10
   set icmap off // obsolete in Stata 10

   set include_bitmap // incomplete
   set include_bitmap on
   set include_bitmap off

   // new in Stata 16
   set iterlog
   set iterlog on
   set iterlog off

   // new in Stata 16 
   set java_heapmax
   set java_home

   set l
   set level

   set lineg
   set linegap

   set li
   set linesize

   set locale_functions // new in Stata 14 

   set locale_ui        // new in Stata 14 

   set locksplit        // incomplete
   set locksplit on
   set locksplitters off

   set logt             // incomplete
   set logt t
   set logtype text
   set logty s
   set logtype smcl

   set lstretch // technically legal
   set lstretch on
   set lstretch off

   // set mata-etc comes later

   set macgph quartz // obsolete in Stata 11
   set macgphengine quickdraw // obsolete in Stata 11

   // set matsize obsolete in Stata 16
   set mat
   set matsize

   // new in Stata 16
   set maxbezierpath // incomplete
   set maxbezierpath 14

   set maxdb

   set maxiter

   set max_memory
   set max_preservemem

   set maxvar

   // begin obsolete block (as of Stata 14)
   set mem
   set memory
   // end obsolete block

   set min_memory
   
   set more  // incomplete 
   set mo on
   set more off

   set niceness

   set notifyuser  // incomplete, mac only
   set notifyuser off
   set notifyuser on

   set ob
   set obs

   set odbcdriver // incomplete 
   set odbcdriver unicode
   set odbcdriver ansi

   // set odbcmg abbreviation obsolete as of Stata 15
   // begin obsolete block
   set odbcmg
   set odbcmg iodbc
   // end obsolete block 
   set odbcmgr // incomplete 
   set odbcmgr iodbc
   set odbcmgr unixodbc

   set ou // incomplete 
   set ou proc
   set output p
   set outpu i
   set output inform
   set outp e
   set output error

   set pa
   set pagesize

   // begin obsolete block as of Stata 12, maybe?
   set persistfv on 
   set persistvtopic off
   // end obsolete block

   set pformat // added in Stata 11.1

   set piccom on // obsolete in Stata 11
   set piccomments off // obsolete in Stata 11

   set pinnable  // incomplete 
   set pinnable on
   set pinnable off

   set playsnd   // incomplete 
   set playsnd on
   set playsnd off

   set printcolor  // incomplete 
   set printcolor auto
   set printcolor automatic
   set printcolor asis
   set printcolor grayscale // obsolete in Stata 9
   set printcolor gs1
   set printcolor gs2
   set printcolor gs3

   set processors

   // new in Stata 16 
   set python_exec
   set python_userpath

   set reventr
   set reventries

   set revkeyboard // incomplete
   set revkeyboard on
   set revkeyboard off

   set revwin nofloat // obsolete in Stata 11 (apparently)
   set revwindow float // obsolete in Stata 11 (seemingly)

   set rmsg  // incomplete 
   set r on
   set rmsg off

   set rng // incomplete; new in Stata 14 ; must have type set
   set rng default // new in Stata 14 
   set rng mt64    // new in Stata 14
   set rng mt64s   // new in Stata 15
   set rng kiss32  // new in Stata 14 

   set rngstate    // new in Stata 14

   set rngstream   // new in Stata 15

   set scheme

   set scrollbufsize

   set searchdefault // incomplete 
   set searchd local
   set searchdefault net
   set searchdefault all

   set se
   set seed

   set segmentsize

   set sformat // added in Stata 11.1 

   set showbaselevels // incomplete 

   set showbaselevels on
   set showbaselevels off
   set showbaselevels all

   set smalldlg on                      // obsolete in Stata 10

   set showemptycells // incomplete 
   set showemptycells on
   set showemptycells off

   set showomitted // incomplete 
   set showomitted on
   set showomitted off

   set smoothf // incomplete 
   set smoothf on
   set smoothfonts off

   set smoothsize 12 // looks to be obsolete in Stata 12

   set timeout1
   set timeout2

   set trace // incomplete 
set tr on
set trace off

   set traced
   set tracedepth

   set tracee // incomplete 
   set tracee on
   set traceexpand off

   set traceh
   set tracehilite

   set traceindent // incomplete 
   set tracei off
   set traceindent on

   set tracen // incomplete 
   set tracen on
   set tracenumber off

   set traces // incomplete 
   set traces off
   set tracesep on

   set type // incomplete 
   set ty float
   set typ double

   set update_interval

   set update_prompt // incomplete 
   set update_prompt on
   set update_prompt off

   set update_query // incomplete 
   set update_query on
   set update_query off

   set use_atsui_graph off // obsolete in Stata 11
   set use_qd_text on // obsolete in Stata 11

   set varabbrev // incomplete 
   set varabbrev on
   set varabbrev off

   set varkeyboard // incomplete 
   set varkeyboard on
   set varkeyboard off
   
   set varlabelpos // obsolete at some point
   set varwin float // obsolete in Stata 11, it seems
   set varwindow nofloat // obsolete in Stata 11, I guess
   set virt on // obsolete in Stata 12
   set virtual off // obsolete in Stata 12
   /* undocumented starting in Stata 10, but still legal */
   /*  can still find info via -help xptheme- */
   set xptheme on
   // end of omnibus -set- section

   // documented in -set- also, hence the duplication
   set cformat
   set pformat
   set sformat

   set_defaults // incomplete 
   set_defaults mem
   set_defaults memory
   set_defaults out
   set_defaults output
   set_defaults inter
   set_defaults interface
   set_defaults graph
   set_defaults graphics
   set_defaults eff
   set_defaults efficiency
   set_defaults net
   set_defaults network
   set_defaults up
   set_defaults update
   set_defaults trace
   set_defaults mata
   set_defaults unicode
   set_defaults oth
   set_defaults other
   set_defaults _all
   set defaults         // incomplete (way incomplete)

   // other set commands are already in the above list
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

   ssc  // incomplete 
   ssc new
   ssc what // obsolete in Stata 11
   ssc whatsnew // obsolete in Stata 11
   ssc hot
   ssc d
   ssc `foo'  // bad? good? dunno?
   ssc describe
   ssc ins // bad
   ssc inst
   ssc install
   ssc uninstall
   ssc type
   ssc copy

   stem
   /* stepwise or sw now has a syntax bad for highlighting */
   stepwise
   // stepwise replaced the following (which do not follow the alphabetical order of the manuals)
   /* sw commands no longer exist, due to syntax changes */
   // start obsolete block
   sw // bad but should be neutral (not obsolete)
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
   // end obsolete block
   /* end sw commands */

   // out of order, but under 'stored results' in [R]
   ret           // incomplete 
   ret li
   retu list
   return list
   ereturn       // incomplete 
   eret li
   ereturn list
   sretu         // incomplete 
   sret li
   sret list
   
   suest 

   su
   sum
   summ
   summa
   summar
   summari
   summariz
   summarize

   sunflower

   sureg
   
   swilk
   sfrancia

   symmetry
   symmi

   table

   tabstat

   ta
   tab
   tabu
   tabul
   tabula
   tabulat
   tabulate

   tab1
   tab2
   tabi

   te
   tes
   test
   testparm

   testnl

   tetrachoric

   tnbreg // new in Stata 11.1

   tob
   tobi
   tobit

   total

   tpoisson // new in Stata 11.1

   // this is all under the translate entry
   print
   translate
   translator         // incomplete 
   translator q
   translator query
   translator set
   translator reset
   translator `foo'   // bad? good?
   transmap           // incomplete 
   transmap q
   transmap query
   transmap def
   transmap define
   transmap `foo'     // bad? good?
   
   treatreg // obsolete in Stata 13

   truncreg

   ttest
   ttesti

   update
   update from
   update q
   update query
   update ado // obsolete as of Stata 12
   update executable // obsolete as of Stata 12
   update utilities // obsolete as of Stata 12
   update swap // obsolete as of Stata 12
   update all
   update `foo'
   set update_query // incomplete
   set update_query on
   set update_query off
   set update_interval
   set update_prompt off
   set update_prompt on
   
   view
   view file
   view browse
   view help
   view search
   view `foo'
   view news // obsolete in Stata 16 
   view net
   view ado
   view update
   // the _d variants still work in Stata 13, but are undocumented !! (come back)
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

   zioprobit // new in Stata 15

   zip
   ztnb // obsolete in Stata 12
   ztb  // obsolete in at least Stata 9---was this ever a Stata command?

   ztest  // new in Stata 14
   ztesti // new in Stata 14

   /* endless postestimation */
   // not checked too much since Stata 14
   // common across most estimation commands
   // should have their own docs anyways

   // anova (regress) postestimation
   adjust // obsolete in Stata 11
   dfbeta
   acprplot
   avplot
   avplots
   cprplot
   lvr2plot
   rvfplot
   rvpplot

   contrast   // areg
   estimates  // areg
   forecast   // areg, incomplete 
   fracplot
   fracpred
   hausman    // areg
   lincom     // areg
   linktest   // areg
   lrtest     // areg
   margins    // areg
   marginsplot  // areg
   nlcom      // areg
   predict    // predict
   predictnl  // areg
   pwcompare  // areg
   suest      // betareg
   test       // areg
   testnl     // areg
   fp plot
   fp predict
   lroc
   lsens
   fracplot
   fracpred
   rocplot
   rocregplot
   
   // estat (from [R])
   // (some) obsolete versions included for testing
   // best way to check is look in the index
   estat alt
   estat alternatives  // asclogit ascprobit asroprobit nlogit
   archlm // obsolete in Stata 9 
   estat archlm // regress/ts
   estat bgo
   bgodfrey // obsolete in Stata 9 
   estat bgodfrey // regress/ts
   estat boot
   estat bootstrap // bootstrap
   estat clas // ivprobit
   estat classification // ivprobit logistic logit probit
   estat cor
   estat correlation // ascprobit asroprobit
   estat cov
   estat covariance // ascprobit asroprobit
   estat cv // added in Stata 11.1
   durbina // obsolete in Stata 9 
   estat dur
   estat durbinalt // regress/ts
   dwstat // obsolete in Stata 9 
   estat dwa
   estat dwatson // regress/ts
   estat endog
   estat endogenous // ivregress
   estat esize // anova
   estat facw
   estat facweights // ascprobit asroprobit
   estat first
   estat firststage // ivregress
   estat gof // logistic logit poisson probit
   hettest // obsolete in Stata 9 
   estat hett
   estat hettest // anova regress
   imtest // obsolete in Stata 9 
   estat ic // areg
   estat imt
   estat imtest // anova regress
   estat mfx // asclogit ascprobit asroprobit, all obsolete in Stata 16  
   estat nproc // rocreg
   estat over
   estat overid // gmm ivregress
   ovtest // obsolete in Stata 9 
   estat ovt
   estat ovtest // anova regress
   estat predict // exlogistic
   estat sbknown // ivregress
   estat sbsingle //  ivregress
   estat se  // exlogistic expoisson
   szroeter // obsolete in Stata 9
   estat steady // new in Stata 16 
   estat szr
   estat szroeter // anova regress
   estat sum // areg
   estat summarize // areg
   estat vce // areg
   vif // obsolete in Stata 9 
   estat vif // anova regress


   /* end subcommand using postestimation */
   /* end [R] */

   /* bayes prefix, introduced in Stata 15 */
   /* no good mechanism for prefix commands....*/
   bayes: regress
   
   /* from Bayes manual [BAYES]; all initially introduced in Stata 14 */
   bayesmh

   // Bayesian postestimation not separated out here, because all commands
   //   have their own documentation
   
   bayesgraph
   bayesgraph matrix
   /* no special highlighting yet */
   /*   no special highlighting from here out for _all */
   bayesgraph name _all

   bayesstats // incomplete

   bayesstats ess
   bayesstats ess _all

   bayesstats grubin     // new in Stata 16
   bayesstats gr
   bayesstats grubin _all

   bayesstats ic

   bayesstats ppvalues   // new in Stata 16 
   bayesstats ppval
   
   bayesstats summ
   bayesstats summary
   bayesstats sum _all // bad (sum too short)
   bayesstats summary _loglikelihood // ok

   bayestest // incomplete

   bayestest int
   bayestest inter
   bayestest interval

   bayestest model

   // not putting in funspec or ysimspec, because the funspec would be impossible to get right
   bayespredict    // new in Stata 16
   bayesreps       // new in Stata 16

   set clevel

   // using bayes: as a prefix; not putting it in with each possible command

   /* [CM] Choice models */
   // manual new in Stata 16
   // contains some commands which were in other manuals

   cmchoiceset

   cmclogit

   cmmixlogit

   cmmprobit

   cmrologit

   cmroprobit

   cmsample

   cmset

   cmsummarize

   cmtab

   cmxtmixlogit

   nlogit
   nlogitgen
   nlogittree
   estat alt
   estat alternatives


   /* from [D] data management manual */
   ap
   app
   appe
   appen
   append
   append using
   
   as
   ass
   asse
   asser
   assert

   assertnested    // new in Stata 16 

   // bcal all new in Stata 12
   bcal c
   bcal ch
   bcal check
   bcal dir
   bcal d
   bcal describe
   bcal load
   bcal create
   
   by
   bys
   byso
   bysor
   bysort

   cd
   pwd
   
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
   clear rngstream  // new in Stata 15
   clear frames     // new in Stata 16 
   clear * // bad but works; cannot fix easily, because of special meaning of * in regexps

   clonevar
   
   codebook

   collapse
   
   compare

   compress

   contract

   copy

   corr2data

   cou
   coun
   count

   cross using

   // data types which could be used for highlighting?!
   byte
   int
   long
   float
   double
   // only str should highlight, I think
   str str1 str80 str99 str100 str158 str244 str245 str1000 str2045 str2046
   strL

   datasig
   datasignature
   datasig set
   datasignature conf
   datasign confirm
   datasigna rep
   datasignature report
   datasig conf using
   datasig repo using
   datasignature clear
   /* should there be date format highlighting? */
   /* date-time functions are in the functions */

   d
   de
   des
   desc
   descr
   descri
   describ
   describe

   destring
   tostring

   dir
   ls

   drawnorm

   drop
   keep

   ds

   duplicates   // incomplete
   duplicates r
   duplicates report
   duplicates e
   duplicates examples
   duplicates l
   duplicates list
   duplicates b       // obsolete in Stata 8 ?
   duplicates browse  // obsolete in Stata 8 ?
   duplicates t
   duplicates tag
   duplicates drop

   dyngen {  // new in Stata 16
      update   // syntax conflict with -update- as a plain old command. yay.
      }
   

   ed
   edi
   edit
   b  // too short, but was mistakenly highlighted in ado-mode forever.
   br
   bro
   brow
   brows
   browse
   /* endless egen & options */
   
   egen
   egen = any() // really was renamed in Stata 9 to anyvalue()
                //  highlights because of mata any() function
   egen breeble = anycount()  // bad, but want to highlight egen
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
   // begin obsolete block 
   egen = rfirst()
   egen = rlast()
   egen = rmax()
   egen = rmean()
   egen = rmin()
   egen = rmiss()
   egen = robs()
   egen = rsd()
   egen = rsum()
   // end obsolete block 
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
   egen = sum() // obsolete; replaced with total
   egen = tag()
   egen = total()
   /* end egen */

   en
   enc
   enco
   encod
   encode
   dec
   deco
   decod
   decode

   erase
   rm
   
   expand

   expandcl

   // export commands (all documented under -import-)
   export delimited
   export excel
   export sasxport  // obsolete in Stata 16
   export sasxport5 // new in Stata 16 
   export sasxport8 // new in Stata 16 
   export dbase // new in Stata 15 
   // fda... commmands obsolete as of Stata 12
   // begin obsolete block 
   fdasav
   fdasave
   fdause
   fdades
   fdadescribe
   // end obsolete block

   filef
   filefi
   filefil
   filefilt
   filefilte
   filefilter

   fillin

   form
   forma
   format
   se dp com
   set dp comma
   set dp per
   set dp period

   // frames are new in Stata 16
   // in order of docs, ignoring -frames- section
   frame change
   cwf

   frame copy

   frame create
   mkf

   frame drop

   frame whatever: oh, no, ambiguous syntax again // just will leave as typical highlight

   frame put

   frame
   frames
   frame pwf
   pwf

   frame rename

   frames dir   // ugh, now there is a plural

   frget   // why not frame get??

   frlink // incomplete
   frlink 1:1
   frlink m:1
   frlink dir
   frlink d
   frlink describe
   frlink rebuild

   g
   ge
   gen
   gene
   gener
   genera
   generat
   generate
   replace
   set ty  // incomplete 
   set ty float
   set type double
   set type foo // bad

   gsort
   
   hexdump

   /* icd9 commands */
   icd9 // incomplete
   icd9 check
   icd9p check
   icd9 clean
   icd9p // incomplete
   icd9p clean
   icd9 gen
   icd9p generate

   icd9 l  // works in practice but is not documented as such
   icd9 look
   icd9p lookup
   icd9 sea
   icd9p search
   icd9 q
   icd9 query
   /* icd10 commands, introduced in Stata 14 */
   /*   these match icd9 commands, at least to start with */
   icd10 // incomplete
   icd10 check
   icd10 clean
   icd10 gen
   icd10 generate
   icd10 look
   icd10 lookup
   icd10 sea     // added in Stata 14.2
   icd10 search
   icd10 q
   icd10 query
   /* icd10cm commands, introduced in Stata 15 */
   /*   these match icd10 commands */
   icd10cm // incomplete
   icd10cm check
   icd10cm clean
   icd10cm gen
   icd10cm generate
   icd10cm look
   icd10cm lookup
   icd10cm sea
   icd10cm search
   icd10cm q
   icd10cm query
   /* icd10pcs commands, introduced in Stata 15 */
   /*   these match icd10 commands */
   icd10pcs // incomplete
   icd10pcs check
   icd10pcs clean
   icd10pcs gen
   icd10pcs generate
   icd10pcs look
   icd10pcs lookup
   icd10pcs sea
   icd10pcs search
   icd10pcs q
   icd10pcs query
   /* end icd9, icd9p, icd10 commands */

   // import/export commands
   import dbase // new in Stata 15 
   export dbase // new in Stata 15 

   import delim
   import delimited
   export delim
   export delimited

   import exc
   import excel
   export exc
   export excel

   set fredkey
   import fred
   freddescribe
   fredsearch

   import hav
   import haver
   // igit... there is no exporting of haver analytics files!
   export hav
   export haver
   set haverdir "/whatever"

   import sas  // new in Stata 16 

   import sasxport // new in Stata 12, obsolete in Stata 16
   export sasxport

   import sasxport5  // new in Stata 16
   import sasxport8  // new in Stata 16 
   export sasxport5
   export sasxport8

   import spss  // new in Stata 16 
   
   impute  // obsolete in Stata 11
   
   inf using
   infile

   infix

   inp
   inpu
   input

   insobs // new in Stata 14

   insheet // obsolete in Stata 13

   ins
   insp
   inspe
   inspec
   inspect

   ipolate

   isid

   joinby

   /* label */
   la // incomplete
   la da
   label data
   la var
   label variable
   la display as error // bad subcommands
   /* inside of a comment label define aaahhh */
   la de
   label define
   la val
   lab val
   label values
   la di
   label dir
   la l
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

   l
   li
   lis
   list
   // flist in help but not in manuals
   fl
   fli
   flis
   flist

   lookfor
   
   memory
   set mem 5b // obsolete in Stata 12
   q mem  // still works but stopped being documented at Stata 16 or earlier
   q memory
   set vir on // obsolete in Stata 12
   set virtual off // obsolete in Stata 12
   set maxvar
   set niceness
   set min_memory
   set max_memory
   set segmentsize

   mer   // incomplete 
   merge // incomplete 
   mer 1:1
   merg m:1
   merge 1:m
   merge m:m  // should be obsolete
   merge 1:1 _n

   mkdir
   
   mvencode
   mvdecode

   /* notes */
   note
   notes
   note: hee hee
   note list
   notes l
   note search
   notes replace
   note drop
   notes renumber

   set ob 93
   set obs 12

   /* odbc */
   odbc  // incomplete 
   odbc li
   odbc list
   odbc q
   odbc query
   odbc des
   odbc describe
   odbc lo
   odbc load
   odbc in
   odbc insert
   odbc exe(needed)
   odbc exe (oops a space)
   odbc exec(needed)
   odbc sql(needed)
   odbc sqlfile(needed)
   odbc sql // bad forgot paren
   set odbcdriver // incomplete; new in Stata 14
   set odbcdriver ansi
   set odbcdriver unicode
   set odbcm iodbc // bad - abbreviation disappeared between 14 and 15
   set odbcmgr iodbc
   set odbcmgr unixodbc
   /* end odbc */
   
   order
   // begin obsolete block from -move-, Stata 11
   mov
   move
   aorder
   // end obsolete block
   
   ou
   out
   outf
   outfi
   outfil
   outfile
   // begin obsolete outsheet block (Stata 13)
   outs
   outsh
   outshe
   outshee
   outsheet
   // end obsolete block
   
   pctile
   xtile
   _pctile

   putmata
   getmata

   range

   recast

   recode

   ren
   rena
   renam
   rename
   renpfix  // obsolete in Stata 12
   
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

   rmdir

   sample

   sa
   sav
   save
   saveold

   separate

   sh
   she
   shel
   shell
   xsh
   xshe
   xshel
   xshell

   snapshot save
   snapshot label
   snapshot restore
   snapshot list
   snapshot erase

   so
   sor
   sort

   split

   splitsample  // new in Stata 16

   stack
   
   statsby

   sysuse auto // bad for command, ok for highlighting
   sysuse dir

   ty
   typ
   type

   /* unicode commands; introduced in Stata 14 */
   // going by order in manual, skipping the -unicode- section
   unicode // incomplete

   // unicode collator must have a -list- subcommand
   unicode coll
   unicode collator
   unicode coll list
   unicode collator list

   unicode conv
   unicode convert
   unicode convertfile // changed in Stata 14.1?

   unicode encoding // incomplete
   unicode encoding list
   unicode encod alias
   unicode enc set

   unicode locale // incomplete
   unicode loc list
   unicode locale list
   unicode uipackage // incomplete
   unicode ui list
   unicode uipackage list

   unicode analyze
   unicode encoding set // also above
   unicode tr
   unicode translate
   unicode retr
   unicode retranslate
   unicode restore
   unicode erasebackups

   u
   us
   use

   varm
   varma
   varman
   varmana
   varmanag
   varmanage

   // all the vl commands are new in Stata 16
   // going by manual entries
   vl create
   vl mod
   vl modify
   vl sub
   vl substitute
   vl lab
   vl label

   vl drop
   vl clear

   vl list
   vl dir

   vl rebuild

   vl set
   vl move

   webuse
   webuse query
   webuse set

   // xmlsave obsolete in Stata 15
   xmlsav
   xmlsave
   xmluse
   
   xpose

   zipfile
   unzipfile
   /* end [D] data management */

   /* begin [DSGE] DSGE manual */
   // New in Stata 15, but just added to ado-mode in Stata 16

   dsge

   // dsge postest
   estat policy
   estat stable
   estat transition
   irf

   dsgenl
   
   /* end [DSGE] DSGE manual (that was quick) */

   /* begin [ERM] extended regression models */
   // New manual in Stata 15

   eintreg

   eoprobit

   eprobit

   eregress
   xteregress
   
   estat teffects

   /* end [ERM] extended regression models */

   /* begin [FMM] finite mixture models */
   // New manual in Stata 15


   fmm 3:  // incomplete, but good enough

   // highlighting changed to be less informative in 1.16.0.0
   // done because there are too many prefix options to get the hilighting to be good
   fmm 1: betareg
   fmm 2, vce(robust): betareg // !! should do better; instead will dump speacial
   fmm 2: cloglog
   fmm 3: glm
   fmm 15: intreg
   fmm 3: ivregress
   fmm 213: logit
   fmm 5  : mlogit
   fmm 9: nbreg
   fmm 3: ologit
   fmm 3: oprobit
   fmm 3: poisson
   fmm 3: probit
   fmm 3: regress
   fmm 3: streg
   fmm 3: tobit
   fmm 3: tpoisson
   fmm 3: truncreg
   // pointmass always highlights though it makes sense only for fmm:
   fmm: pointmass
                     

   // postestimation

   estat eform
   estat lcmean
   estat lcprob


   /* end [FMM] finite mixture models */

   /* begin [FN] functions (split out in Stata 14) */
   /* functions (moved to their own manual in Stata 14 */
   /*  order changed to match split order in Stata 14 manual */
   /* date functions */
   bofd()
   Cdhms()
   Chms()
   Clock()
   clock()
   Cmdyhms()
   Cofc()
   cofC()
   Cofd()
   cofd()
   daily() 
   date()
   day()
   dhms()
   dofb()
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

   /* single-letter date fns---obsolete in Stata 10 */
   // begin obsolete block
   d()
   h()
   m()
   q()
   w()
   y()
   // end obsolete block

   /* math functions */
   /* trig split from math in Stata 14, because trig must not be math */
   abs()
   ceil()
   cloglog()
   comb()
   digamma()
   exp()
   expm1()  // new in Stata 16
   floor()
   int()
   invcloglog()
   invlogit()
   ln()
   ln1m()    // new in Stata 16
   ln1p()    // new in Stata 16
   lnfact()  // obsolete in Stata 10 
   lnfactorial()
   lngamma()
   log()
   log10()
   log1m()   // new in Stata 16
   log1p()   // new in Stata 16
   logit()
   max()
   min()
   mod()
   reldif()
   round()
   sign()
   sqrt()
   sum()
   trigamma()
   trunc()
   /* matrix functions (whether matrix or scalar result) */
   // those returning matrices (to match new ordering)
   cholesky()
   corr()
   diag()
   get()
   hadamard()
   I()
   inv()
   invsym()
   issym() // obsolete in Stata 10 
   J()
   matuniform()
   nullmat()
   sweep()
   syminv() // obsolete in Stata 10 
   vec()
   vecdiag()
   // those returning scalars
   coleqnumb() // new in Stata 15 
   colnfreeparms() // new in Stata 15 
   colnumb()
   colsof()
   det()
   diag0cnt()
   el()
   issymmetric()
   matmissing()
   mreldif()
   roweqnumb() // new in Stata 15
   rownfreeparms() // new in Stata 15 
   rownumb()
   rowsof()
   trace()

   /* programming functions */
   autocode()
   byteorder()
   c()
   _caller()
   chop()
   clip()
   cond()
   e()
   e(sample)  // ? should highlight?
   epsdouble()
   epsfloat()
   fileexists() // became Stata function in Stata 13
   fileread()
   filereaderror()
   filewrite()
   float()
   fmtwidth()
   frval()    // new in Stata 16
   _frval()   // new in Stata 16
   group() // obsolete in Stata 10 
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
   r(this should not really highlight) // bad
   recode()
   replay()
   return()
   s()
   scalar()
   smallestdouble()

   /* random number functions */
   uniform() // obsolete in Stata 10 
	runiform()
   runiformint() // new in Stata 14
   rbeta()
	rbinomial()
   rcauchy() // new in Stata 15
	rchi2()
   rexponential() // new in Stata 14 
	rgamma()
	rhypergeometric()
   rigaussian()  // new in Stata 14.2
   rlaplace() // new in Stata 15 
   rlogistic()   // new in Stata 14 
	rnbinomial()
	rnormal()
	rpoisson()
	rt()
   rweibull()    // new in Stata 14
   rweibullph()  // new in Stata 14

   /* selecting time-span functions [hunh?] */
   tin()
   twithin()


   /* so-called statistical functions, most of which are probability functions */
   /* beta density */
   betaden()
   ibeta()
   ibetatail()
   invibeta()
   nibeta()
   invnibeta()
   
   Binomial() /* finally changed to binomialtail in Stata 10 */
   // binomial
   binomialp()
   binomial()
   binomialtail()
   invbinomial()
   invbinomialtail()


   // cauchy new in Stata 15
   cauchyden()
   cauchy()
   cauchytail()
   invcauchy()
   invcauchytail()
   lncauchyden()

   // chi2
   chi2den()
   chi2()
   chi2tail()
   invchi2()
   invchi2tail()
   nchi2den()
   nchi2()
   nchi2tail()
   invnchi2()
   invnchi2tail()
   npnchi2()
   
   // dunnet's multiple range
   dunnettprob()
   invdunnettprob()

   // exponential; new in Stata 14 
   exponentialden()
   exponential()
   exponentialtail()
   invexponential()       // new in Stata 14 
   invexponentialtail()   // new in Stata 14 

   // F & non-central F
   Fden()
   F()
   Ftail()
   invF()
   invFtail()
   nFden()
   nF()
   nFtail()
   invnF()
   invnFtail()
   npnF()

   // gamma
   gammaden()
   gammap()
   gammaptail()
   invgammap()
   invgammaptail()

   // dgamma & lnigamma
   dgammapda()
   dgammapdada()
   dgammapdadx()
   dgammapdx()
   dgammapdxdx()
   lnigammaden()

   // hypergeometric
   hypergeometricp()
   hypergeometric()

   // igaussian; new in Stata 14.1 or 14.2
   igaussianden()
   igaussian()
   igaussiantail()
   invigaussian()        // new in Stata 15 
   invigaussiantail()    // new in Stata 15
   lnigaussianden()

   // laplace new in Stata 15
   laplaceden()
   laplace()
   laplacetail()
   invlaplace()          // new in Stata 15 
   invlaplacetail()      // new in Stata 15
   lnlaplaceden()

   // logistic
   logisticden()       // new in Stata 14
   logistic()          // new in Stata 14
   logistictail()      // new in Stata 14
   invlogistic()         // new in Stata 14 
   invlogistictail()     // new in Stata 14 

   // negative binomial
   nbinomialp()
   nbinomial()
   nbinomialtail()
   invnbinomial()
   invnbinomialtail()

   // Normal, binormal, mvnormal
   normden()  // obsolete in Stata 10 
   normalden()
   norm()  // as mata function, OK, but obsolete in Stata --- still highlights
   normal()
   invnorm()           // obsolete in Stata 10 
   invnormal()
   lnnormalden()
   lnnormal()   
   binorm() // obsolete in Stata 10 
   binormal()
   lnmvnormalden()
   
   // Poisson
   poissonp()
   poisson()
   poissontail()
   invpoisson()
   invpoissontail()

   // Student's t
   tden()
   t()
   ttail()
   invt() // went obsolete in Stata 7, resurrected in Stata 13
   invttail()
   invnt()
   invnttail()
   ntden()
   nt()
   nttail()
   npnt()

   // tukey studentized range
   tukeyprob()
   invtukeyprob()

   // all weibull new in Stata 14
   weibullden()
   weibull()
   weibulltail()
   invweibull()        // new in Stata 14
   invweibulltail()    // new in Stata 14

   // weibull proportional hazards, also new in Stata 14
   weibullphden()
   weibullph()
   weibullphtail()
   invweibullph()      // new in Stata 14 
   invweibullphtail()  // new in Stata 14

   // Wishart distribution, new in Stata 14
   lnwishartden()
   lniwishartden()

   /* string functions */ 
   abbrev()
   char()
   collatorlocale() // new in Stata 14 
   collatorversion() // new in Stata 14 
   index()
   indexnot()
   itrim()  // obsolete in Stata 14
   length() // obsolete in Stata 14 but not mata 
   lower()  // obsolete in Stata 14 
   ltrim()  // obsolete in Stata 14 
   match()
   plural()
   proper() // obsolete in Stata 14 
   real()
   regexm()
   regexr()
   regexs()
   reverse() // obsolete in Stata 14 
   rtrim()   // obsolete in Stata 14 
   soundex()
   soundex_nara()
   strcat() // bad; fake entry in manual
   strdup() // bad; fake entry in manual
   string()
   // new in Stata 14: -str- prefix for many string functions 
   stritrim() 
   strlen()
   strlower() 
   strltrim() 
   strmatch() 
   strofreal() 
   strpos() 
   strproper() 
   strreverse()
   strrpos()
   strrtrim() 
   strtoname()
   strtrim()
   strupper()
   // end of new Stata 14 -str- functions
   subinstr()
   subinword()
   substr()
   tobytes()  // new in Stata 14 
   trim()  // obsolete in Stata 14

   // new in Stata 14: -u- prefix functions for unicode

   uchar()
   udstrlen()
   uisdigit()
   uisletter() // new in Stata 15
   ustrcompare()
   ustrcompareex()
   ustrfix()
   ustrfrom()
   ustrinvalidcnt()
   ustrleft()
   ustrlen()
   ustrlower()
   ustrltrim()
   ustrnormalize()
   ustrpos()
   ustrregexm()
   ustrregexra()
   ustrregexrf()
   ustrregexs()
   ustrreverse()
   ustrright()
   ustrrpos()
   ustrrtrim()
   ustrsortkey()
   ustrsortkeyex()
   ustrtitle()
   ustrto()
   ustrtohex()
   ustrtoname()
   ustrtrim()
   ustrunescape()
   ustrupper()
   ustrword()
   ustrwordcount()
   usubinstr()
   usubstr()
   // end Stata 14 -u- prefix functions
   upper() // obsolete in Stata 14; valid in mata 
   word()
   wordbreaklocale()
   wordcount()

   /* trig split from math in Stata 14, because trig must not be math */
   acos()
   acosh()
   asin()
   asinh()
   atan()
   atan2()
   atanh()
   cos()
   cosh()
   sin()
   sinh()
   tan()
   tanh()

   /* end [FN] functions manual*/


   /* [G] the miserable graph commands... */
   // gr7 and graph7 are now 'previously documented' (Stata 9)
   gr7 using foo
   graph7 this that

   gr bar
   graph bar
   gr hbar
   graph hbar

   graph box
   graph hbox

   // new in Stata 14 
   gra close
   graph close

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

   // graph manipulation skipped as section; pieces already in sections

   graph matrix
   graph matrix foo 

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
   estat acplot
   estat aroots
   estat sbcusum

   // begin obsolete block 
   varfcast graph
   varirf graph
   varirf ograph
   varirf cgraph
   // end obsolete block

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
   stphtest  // obsolete in Stata 9
   stphplot
   stcoxkm
   estat phtest
   stcurve
   estat gofplot

   // new in Stata 16
   meta forestplot
   meta funnelplot
   meta labbeplot
   estat bubbleplot

   roctab
   rocplot
   roccomp
   rocregplot
   lroc
   lsens

   // new in Stata 16
   coefpath
   cvplot

   irtgraph icc
   irtgraph tcc
   irtgraph iif
   irtgraph tif

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

   marginsplot
   bayesgraph
   power, graph // incomplete
   ciwidth, graph // !! incomplete, new in Stata 16 
   tabodds
   teffects overlap
   tebalance box
   npgraph
   grmap
   pkexamine

   /* end of so-called graph other */
   gr pie
   graph pie

   graph play
   gr play

   graph print

   gr q
   graph query

   graph rename

   graph replay

   gr save

   // graph set
   gr set print 
   graph set ps   
   graph set eps
   graph set svg  // new in Stata 15 
   graph set window fontface 
   graph set window fontfacemono
   graph set window fontfacesans 
   graph set window fontfaceserif
   graph set window fontfacesymbol

   // should the twoway's here be changed to command highlighting?
   gr twoway fee fie fo
   graph twoway (scatter bar foo) (lfitci bar fee) // should lfitci be a command?
   twoway bar foo || bar fee

   /* forget the stuff under twoway */
   /* redone in the order of the commands themselves to accommodate abbrevs */  

   gr tw scatter

   graph twoway area y
   twoway area
   
   twoway bar
   gr twoway bar
   graph twoway bar y
   
   tw con
   two connected
   gr two con
   gr two connected
   
   two contour  // new in Stata 12 
   twoway contour  // new in Stata 12 
   gr two contour  // new in Stata 12

   gr twoway contourline // new in Stata 12

   tw dot
   graph twoway dot y

   graph twoway dropline y

   gr twoway fp // should not be light blue (and is not)
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
   line foo bar // twoway optional for line

   graph twoway lowess

   tw lpoly

   two lpolyci

   graph twoway mband

   graph twoway mspline

   twoway pcarrow
   twoway pcbarrow

   two pcarrowi
   tw pcbarrowi // bad; turns out this never existed

   two pccapsym

   twoway pci

   two pcscatter

   two pcspike

   graph twoway qfit

   graph twoway qfitci

   graph twoway rarea

   graph twoway rbar

   graph twoway rcap

   twoway rcapsym

   graph twoway rconnected
   tw rcon
   two rconnected

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

   tsline
   twoway tsline
   tsrline
   two tsrline
   gr tw tsrline

   graph use
   gr use

   palette color
   palette line
   palette linepalette
   palette symbol
   palette symbolpalette
   palette smclsymbolpalette   // new in Stata 14
   palette smcl

   q graph
   query graphics
   set graphics on
   set graphics off
   set g on

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
   /* end [G] manual */

   /* [IRT] manual (new in Stata 14) */
   // skipping the -irt- section and putting commands in the order of the manual
   // estimation commands all at the end for now
   irt  // incomplete
   irt 1pl
   irt 2pl
   irt 3pl

   irt grm

   irt nrm
   
   irt pcm
   irt gpcm

   irt rsm

   irt hybrid

   // irt-specific postestimation commands
   estat greport  // new in Stata 16
   estat grep
   estat rep
   estat repo
   estat report
   
   irtgraph // incomplete

   irtgraph icc

   irtgraph iif

   irtgraph tcc

   irtgraph tif

   diflogistic

   difmh

   /* end [IRT] manual */

   /* start [LASSO] manual */
   // commands and manual new in Stata 16

   coefpath

   cvplot

   dslogit

   dspoisson

   dsregress

   elasticnet

   lasso

   lassocoef

   lassogof

   lassoinfo

   lassoknots

   lassoselect // not complete
   lassoselect id
   lassoselect lambda
   lassoselect alpha

   poivregress

   pologit

   popoisson

   poregress

   sqrtlasso

   xpoivregress

   xpologit

   xpopoisson

   xporegress
   
   /* end [LASSO] manual */

   /* [M] Mata Manual */

   /* [M-2] - all the reserved words */
   // these *should* have proper highlighting, but the mata
   //   highlighting is not very sophisticated, yet
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
   const // used elsewhere
   continue /* used elsewhere */

   default
   delegate
   delete
   do /* used elsewhere */ 
   double /* used elsewhere */

   else /* used elsewhere */
   eltypedef
   mata
   end /* used elsewhere, commented out here because of indentation */
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

   mata
   end  // needed for indentation here
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
   static
   string
   strL // used elsewhere
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
   void
   volatile

   while /* used elsewhere */ 

   /* mata building blocks */
   for(hey; ho; wego) {
      hmmmm
      }

   /* trying to have some mata stuff */
   pragma unset
   pragma unused

   /* from [M-3 Intro] "commands for controlling mata" */
   lmbuild 
      
   mata:
      this is mata code
   end

   mata
      this is mata code (which needs no end statement, as it really needs to be a block, I think.) Sheesh!
      what about this
   end

   mata: mata clear
   mata mata clear
   mata clear

   mata d
   mata describe

   mata: mata drop
   mata drop

   mata help

   mata matsave aFile some names
   mata matuse bar
   mata matd breeble
   mata matd box
   mata matdescribe box

   mata memory

   mata mlib // incomplete
   mata mlib create foo
   mata mlib add bar
   mata mlib index
   mata mlib q
   mata mlib query

   mata mosave momoney()

   mata rename

   mata query
   mata set // incomplete
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

   abbrev()

   abs()

   adosubdir()

   all()
   any()
   allof()
   anyof()

   args()

   // heaps (associative arrays)
   asarray_create()
   asarray()
   asarray_remove()
   asarray_contains()
   asarray_elements()
   asarray_keys()
   asarray_first()
   asarray_next()
   asarray_key()
   asarray_contents()
   asarray_notfound()

   AssociativeArray()
   // seems strange to highlight class methods
   A.reinit()
   A.put()
   A.get()
   A.notfound()
   A.remove()
   A.exists()
   A.firstval()
   A.nextval()
   A.key()
   A.val()
   A.firstloc()
   A.next()
   A.nextloc()
   A.keys()
   A.N()
   A.clear()

   ascii()
   char()

   // uchar out of order in Stata 14 manual
   uchar()

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

   pwd()
   chdir()
   _chdir()
   mkdir()
   _mkdir()
   rmdir()
   _rmdir()

   cholesky()
   _cholesky()

   cholinv()
   _cholinv()

   cholsolve()
   _cholsolve()

   comb()

   cond()

   conj()
   _conj()

   corr()
   _corr()

   cross()

   crossdev()

   cvpermutesetup()
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
   cofC()

   date()
   mdy()
   yw()
   ym()
   yq()
   yh()
   cofd()
   Cofd()
   dofb()
   bofd()

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
   // start of numerical derivatives
   deriv_init()
   deriv_init_evaluator()
   deriv_init_evaluatortype()
   deriv_init_params()
   deriv_init_argument()
   deriv_init_narguments()
   deriv_init_weights()
   deriv_init_h()
   deriv_init_scale()
   deriv_init_bounds()
   deriv_init_search()
   deriv_init_verbose()

   deriv()
   _deriv()

   deriv_result_value()
   deriv_result_values()
   _deriv_result_values()
   deriv_result_gradient()
   _deriv_result_gradient()
   deriv_result_scores()
   _deriv_result_scores()
   deriv_result_Jacobian()
   _deriv_result_Jacobian()
   deriv_result_Hessian()
   _deriv_result_Hessian()
   deriv_result_h()
   deriv_result_scale()
   deriv_result_delta()
   deriv_result_errorcode()
   deriv_result_errortext()
   deriv_result_returncode()
   deriv_query()
   // end of numerical derivatives

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

   Dmatrix()

   /* the _docx*() functions */
   // create and save
   _docx_new()
   _docx_save()
   _docx_append() // new in Stata 15
   _docx_close()
   _docx_closeall()
   // add paragraph/text
   _docx_paragraph_new()
   _docx_paragraph_new_styledtext()
   _docx_paragraph_add_text()
   _docx_text_add_text()
   // add image
   _docx_image_add()
   // add table
   _docx_new_table()
   _docx_add_matrix()
   _docx_add_mata()
   _docx_add_data()
   // edit table
   _docx_table_add_row()
   _docx_table_del_row()
   _docx_table_add_cell()
   _docx_table_del_cell()
   _docx_cell_set_colspan()
   _docx_cell_set_rowspan()
   _docx_table_mod_cell()
   _docx_table_mod_cell_table()
   _docx_table_mod_cell_image()
   // query
   _docx_query()
   _docx_query_document() // obsolete in Stata 14
   _docx_query_table()
   _docx_table_query_row()
   /* end of the _docx*() functions */

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

   eigensystemselectr()
   lefteigensystemselectr()
   eigensystemselecti()
   lefteigensystemselecti()
   eigensystemselectf()
   lefteigensystemselectf()
   symeigensystemselectr()
   symeigensystemselecti()
   // underscore versions, like _eigenselecti_1a are ignored
   //  because direct use is discouraged and documentation is minimal

   eltype()
   orgtype()
   classname() // appeared in Stata 14
   structname()  // appeared in Stata 14

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
   expm1()   // new in Stata 16
   ln1p()    // new in Stata 16
   log1p()   // new in Stata 16
   ln1m()    // new in Stata 16
   log1m()   // new in Stata 16
   

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

   geigensystem()
   leftgeigensystem()
   geigensystemelectr()
   leftgeigensystemelectr()
   geigensystemelecti()
   leftgeigensystemelecti()
   geigensystemelectf()
   leftgeigensystemelectf()
   _geigensystem_la()
   _geigenselectr_la()
   _geigenselecti_la()
   _geigenselectf_la()
   _geigen_la()

   ghessenbergd()
   _ghessenbergd()

   ghk_init()
   ghk_init_method()
   ghk_init_start()
   ghk_init_pivot()
   ghk_init_antithetics()
   ghk_query_npts()
   ghk()

   ghkfastsetup() // this /looks/ obsolete in Stata 11?!
   ghkfast_init()
   ghkfast_init_pivot()
   ghkfast_init_antithetics()
   ghkfast_query_n()
   ghkfast_query_npts()
   ghkfast_query_dim()
   ghkfast_query_method()
   ghkfast_query_rseed()
   ghkfast_query_pointset_i()
   ghkfast()
   ghkfast_i()

   gschurd()
   _gschurd()
   gschurdgroupby()
   _gschurdgroupby()

   halton()
   _halton()

   hash1()

   hessenbergd()
   _hessenbergd()

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

   isascii()   // new in Stata 16 

   isdiagonal()

   isfleeting()

   isreal()
   iscomplex()
   isstring()
   ispointer()

   isrealvalues()

   issamefile()  // new in Stata 16 

   issymmetric()
   issymmetriconly()

   isview()

   J()

   Kmatrix()

   // crud, LAPACK direct calls
   _flopin()
   _flopout()
   LA_DGBMV()
   LA_DGEBAK()
   LA_DGEBAL()
   LA_DGEES()
   LA_DGEEV()
   LA_DGEHRD()
   LA_DGGBAK()
   LA_DGGBAL()
   LA_DGGHRD()
   LA_DHGEQZ()
   LA_DHSEIN()
   LA_DHSEQR()

   LA_DLAMCH()
   LA_DORGHR()
   LA_DSYEVX()

   LA_DTGSEN()
   LA_DTGEVC()
   LA_DTREVC()
   LA_DTRSEN()

   LA_ZGEBAK()
   LA_ZGEBAL()
   LA_ZGEES()
   LA_ZGEEV()
   LA_ZGEHRD()
   LA_ZGGBAK()
   LA_ZGGBAL()
   LA_ZGGHRD()
   LA_ZHGEQZ()
   LA_ZHSEIN()
   LA_ZHSEQR()

   LA_ZTGSEN()
   LA_ZTGEVC()
   LA_ZTREVC()
   LA_ZTRSEN()
   LA_ZUNGHR()
   // end LAPACK

   // LinearProgram()
   // new in Stata 16

   LinearProgram()
   C.setCoefficients()
   C.setMaxOrMin()
   C.setEquality()
   C.setInequality()
   C.setBounds()
   C.setMaxiter()
   C.setTol()
   C.setTrace()
   C.getCoefficients()
   C.getMaxOrMin()
   C.getEquality()
   C.getInequality()
   C.getBounds()
   C.getMaxiter()
   C.getTol()
   C.getTrace()

   C.optimize()

   C.parameters()
   C.value()
   C.iterations()
   C.converged()
   C.errorcode()
   C.errortext()
   C.returncode()
   C.query()
   // end LinearProgramming 

   liststruct()

   Lmatrix()

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
   _lusolve_la()

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
   quadmeanvariance()
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

   colmissing()
   rowmissing()
   missing()
   colnonmissing()
   rownonmissing()
   nonmissing()
   hasmissing()

   missingof()

   mod()

   // crap, moptimize monstrosity
   // moptimize_init functions
   moptimize_init()
   moptimize_init_which()
   moptimize_init_evaluator()
   moptimize_init_evaluatortype()
   moptimize_init_negH()
   moptimize_init_touse()
   moptimize_init_view() // obsolete in Stata 12
   moptimize_init_ndepvars()
   moptimize_init_depvar()
   moptimize_init_eq_n()
   moptimize_init_eq_indepvars()
   moptimize_init_eq_cons()
   moptimize_init_eq_offset()
   moptimize_init_eq_exposure()
   moptimize_init_eq_name()
   moptimize_init_eq_colnames()
   moptimize_init_eq_freeparm() // new in Stata 15
   moptimize_init_eq_coefs()
   moptimize_init_constraints()
   moptimize_init_search()
   moptimize_init_search_random()
   moptimize_init_search_repeat()
   moptimize_init_search_bounds()
   moptimize_init_search_rescale()
   moptimize_init_weight()
   moptimize_init_weighttype()
   moptimize_init_cluster()
   moptimize_init_svy()
   moptimize_init_by()
   moptimize_init_nuserinfo()
   moptimize_init_userinfo()
   moptimize_init_technique()
   moptimize_init_vcetype()
   moptimize_init_nmsimplexdeltas()
   moptimize_init_gnweightmatrix()
   moptimize_init_singularHmethod()
   moptimize_init_conv_maxiter()
   moptimize_init_conv_warning()
   moptimize_init_conv_ptol()
   moptimize_init_conv_vtol()
   moptimize_init_conv_nrtol()
   moptimize_init_conv_ignorenrtol()
   moptimize_init_iterid()
   moptimize_init_valueid()
   moptimize_init_tracelevel()
   moptimize_init_trace_ado()
   moptimize_init_trace_dots()
   moptimize_init_trace_value()
   moptimize_init_trace_tol()
   moptimize_init_trace_step()
   moptimize_init_trace_coefdiffs() // new in Stata 12
   moptimize_init_trace_coefs()
   moptimize_init_trace_gradient()
   moptimize_init_trace_Hessian()
   moptimize_init_evaluations()
   moptimize_init_verbose()
   // Step 3 functions
   moptimize()
   _moptimize()
   moptimize_evaluate()
   _moptimize_evaluate()
   // Step 4 functions
   moptimize_result_post()
   moptimize_result_display()
   moptimize_result_value()
   moptimize_result_value0()
   moptimize_result_eq_coefs() // new in Stata 12
   moptimize_result_coefs()
   moptimize_result_colstripe()
   moptimize_result_scores()
   moptimize_result_gradient()
   moptimize_result_Hessian()
   moptimize_result_V()
   moptimize_result_Vtype()
   moptimize_result_V_oim()
   moptimize_result_V_opg()
   moptimize_result_V_robust()
   moptimize_result_iterations()
   moptimize_result_converged()
   moptimize_result_iterationlog()
   moptimize_result_evaluations()
   moptimize_result_errorcode()
   moptimize_result_errortext()
   moptimize_result_returncode()
   moptimize_ado_cleanup()
   // moptimize utility functions
   moptimize_query()
   moptimize_util_eq_indices()
   moptimize_util_depvar()
   moptimize_util_xb()
   moptimize_util_sum()
   moptimize_util_vecsum()
   moptimize_util_matsum()
   moptimize_util_matbysum()
   moptimize_util_by() // new in Stata 12

   more()
   setmore()
   setmoreonexit()

   // mvnormal() functions new in Stata 15
   mvnormal()
   mvnormalcv()
   mvnormalqp()
   mvnormalcvqp()
   mvnormalderiv()
   mvnormalcvderiv()
   mvnormalderivqp()
   mvnormalcvderivqp()

   _negate()

   /* mostly regular stata functions */
   norm()
   // normal
   normalden()
   normal()
   invnormal()
   lnnormalden()
   lnnormal()
   // binormal
   binormal()
   // multivariate normal
   lnmvnormalden()  // new in Stata 14
   // beta
   betaden()
   ibeta()
   ibetatail()
   invibeta()
   invibetatail()
   // binomial
   binomialp()
   binomial()
   binomialtail()
   invbinomial()
   invbinomialtail()
   // cauchy // new in Stata 15
   cauchyden()
   cauchy()
   cauchytail()
   invcauchy()
   invcauchytail()
   lncauchyden()
   // chi-squared
   chi2den()
   chi2()
   chi2tail()
   invchi2()
   invchi2tail()
   // Dunnett's multiple range (new in Stata 12)
   dunnettprob()
   invdunnettprob()
   // Exponential (new in Stata 14)
   exponentialden()
   exponential()
   exponentialtail()
   invexponential()
   invexponentialtail()
   // F
   Fden()
   F()
   Ftail()
   invF()
   invFtail()
   // Gamma and inverse gamma
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
   lnigammaden() // new in Stata 14
   // Hypergeometric
   hypergeometricp()
   hypergeometric()
   // inverse gaussian // new in Stata 15
   igaussianden()
   igaussian()
   igaussiantail()
   invigaussian()
   invigaussiantail()
   lnigaussianden()
   // laplace // new in Stata 15
   laplaceden()
   laplace()
   laplacetail()
   invlaplace()
   invlaplacetail()
   lnlaplaceden()
   // Logistic (new in Stata 14)
   logisticden()
   logistic()
   logistictail()
   invlogistic()
   invlogistictail()
   // Negative binomial
   nbinomialp()
   nbinomial()
   nbinomialtail()
   invnbinomial()
   invnbinomialtail()
   // Noncentral beta
   nbetaden()
   nibeta()
   invnibeta()
   // Noncentral chi-squared
   nchi2den() // new in Stata 14
   nchi2()
   nchi2tail()  // new in Stata 14
   invnchi2()
   invnchi2tail()  // new in Stata 14 
   npnchi2()
   // Noncentral F
   nFden()
   nF() // new in Stata 13 
   nFtail()
   invnF() // new in Stata 14 
   invnFtail()
   npnF() // new in Stata 13
   // noncentral t (all new in Stata 13)
   ntden()
   nt()
   nttail()
   invnt() // new in Stata 14 
   invnttail()
   npnt()
   // Poisson
   poissonp()
   poisson()
   poissontail()
   invpoisson()
   invpoissontail()
   // Student's t
   tden()
   t() // new in Stata 13
   ttail()
   invt() // new in Stata 13 
   invttail()
   // Tukey Studentized Range (new in Stata 12)
   tukeyprob()
   invtukeyprob()
   // Weibull (new in Stata 14)
   weibullden()
   weibull()
   weibulltail()
   invweibull()
   invweibulltail()
   // Weibull proportional hazards (new in Stata 14)
   weibullphden()
   weibullph()
   weibullphtail()
   invweibullph()
   invweibullphtail()
   // Wishart
   lnwishartden()
   lniwishartden()

   /* phooey, optimize to the max */
   optimize_init()
   optimize_init_which()
   optimize_init_evaluator()
   optimize_init_evaluatortype()
   optimize_init_negH()
   optimize_init_type() // obsolete in Stata 11 [?]
   optimize_init_params()
   optimize_init_nmsimplexdeltas()
   optimize_init_argument()
   optimize_init_narguments()
   optimize_init_cluster()
   optimize_init_colstripe()
   optimize_init_technique()
   optimize_init_gnweightmatrix() // obsolete in Stata 12
   optimize_init_singularHmethod()
   optimize_init_conv_maxiter()
   optimize_init_conv_warning()
   optimize_init_conv_ptol()
   optimize_init_conv_vtol()
   optimize_init_conv_nrtol()
   optimize_init_ingnorenrtol()
   optimize_init_iterid()
   optimize_init_valueid()
   optimize_init_tracelevel()
   optimize_init_trace_dots()
   optimize_init_trace_value()
   optimize_init_trace_tol()
   optimize_init_trace_step()
   optimize_init_trace_paramdiffs() // new in Stata 12
   optimize_init_trace_params()
   optimize_init_trace_gradient()
   optimize_init_trace_Hessian()
   optimize_init_evaluations()
   optimize_init_constraints()
   optimize_init_verbose()

   optimize()
   _optimize()
   optimize_evaluate()
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
   optimize_result_V_opg()
   optimize_result_V_robust()
   optimize_result_iterations()
   optimize_result_converged()
   optimize_result_iterationlog()
   optimize_result_evaluations() // new in Stata 12
   optimize_result_errorcode()
   optimize_result_errortext() // new in Stata 12
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
   pathresolve()     // new in Stata 16 
   pathgetparent()   // new in Stata 16 

   // Pdf* functions (new in Stata 14)
   PdfDocument()
   // uh oh... class methods
   p.save()
   p.close()
   p.setPageSize()
   p.setMargins()
   p.setHAlignment()
   p.setLineSpace()
   p.setBgColor()
   p.setColor()
   p.setFont()
   p.setFontSize()
   p.addImage()
   p.addParagraph()
   p.addTable()
   p.addNewPage()
   p.addLineBreak()

   PdfParagraph()
   p.addString()
   p.addText()
   p.addLineBreak()
   p.clearContent()
   p.setFirstIndent()
   p.setLeftIndent()
   p.setRightIndent()
   p.setTopSpacing()
   p.setBottomSpacing()
   p.setBgColor()
   p.setColor()
   p.setFont()
   p.setFontSize()
   p.setUnderline()
   p.setStrikethru()
   p.setHAlignment()
   p.setVAlignment() // new in Stata 15 ?
   p.setLineSpace()

   PdfText()
   t.setBgColor()
   t.setColor()
   t.setFont()
   t.setFontSize()
   t.setUnderline()
   t.setStrikethru()
   t.setSuperscript()
   t.setSubscript()
   t.addString()
   t.clearContent()

   PdfTable()
   t.init()
   t.setTotalWidth()
   t.setColumnWidths()
   t.setWidthPercent()
   t.setIndentation()
   t.setHAlignment()
   t.setBorderWidth()
   t.setBorderColor()
   t.setTopSpacing()
   t.setBottomSpacing()
   t.setCellContentString()
   t.setCellContentParagraph()
   t.setCellContentImage()
   t.setCellContentTable()
   t.setCellContentHAlignment()
   t.setCellContentVAlignment()
   t.setCellBgColor()
   t.setCellBorderWidths() // obsolete in Stata 15 
   t.setCellBorderWidth() 
   t.setCellBorderColor() // new in Stata 15  
   t.setCellLeftBorderWidth()  // obsolete in Stata 15 
   t.setCellRightBorderWidth()  // obsolete in Stata 15 
   t.setCellTopBorderWidth()  // obsolete in Stata 15 
   t.setCellBottomBorderWidth()  // obsolete in Stata 15 
   t.setCellMargin() // new in Stata 15
   t.setCellMargins()  // obsolete in Stata 15 
   t.setCellLeftMargin()  // obsolete in Stata 15 
   t.setCellRightMargin()  // obsolete in Stata 15 
   t.setCellTopMargin()  // obsolete in Stata 15 
   t.setCellBottomMargin()  // obsolete in Stata 15 
   t.setCellFont()
   t.setCellFontSize()
   t.setCellColor()
   t.setCellSpan()
   t.setCellRowSpan()
   t.setCellColSpan()
   t.setRowSplit() // new in Stata 15
   t.addRow() // new in Stata 15 
   t.delRow() // new in Stata 15 
   t.addColumn() // new in Stata 15 
   t.delColumn() // new in Stata 15 
   t.fillStataMatrix()
   t.fillMataMatrix()
   t.fillData()
   /* end Pdf* */

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
   hqrd()
   _hqrd()
   hqrdmultq()
   hqrdmultq1t()
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

   // Quadrature fun, new in Stata 16
   Quadrature()
   q.setEvaluator()
   q.setLimits()
   q.setTechnique()
   q.setMaxiter()
   q.setAbstol()
   q.setReltol()
   q.setArgument()
   q.setTrace()
   q.getEvaluator()
   q.getLimits()
   q.getTechnique()
   q.getMaxiter()
   q.getAbstol()
   q.getReltol()
   q.getArgument()
   q.getTrace()
   q.integrate()
   q.value()
   q.iterations()
   q.converged()
   q.errorcode()
   q.errortext()
   q.returncode()

   range()
   rangen()

   rank()

   Re()
   Im()

   reldif()
   mreldif()
   mreldifsym()
   mreldifre()

   rows()
   cols()
   length() // not obsolete in mata!

   rowshape()
   colshape()

   // mata random number stuff (much of it similar to Stata)
   runiform()
   runiformint() // new in Stata 14
   rseed()
   rngstate() // new in Stata 14 
   rbeta()
   rbinomial()
   rcauchy()
   rchi2()
   rdiscrete()
   rexponential() // new in Stata 14 
   rgamma()
   rhypergeometric()
   rigaussian() // new in Stata 15 (or 14.2)
   rlaplace() // new in Stata 15
   rlogistic() // new in Stata 14
   rnbinomial()
   rnormal()
   rpoisson()
   rt()
   rweibull() // new in Stata 14
   rweibullph() // new in Stata 14 

   runningsum()
   quadrunningsum()
   _runningsum()
   _quadrunningsum()

   schurd()
   _schurd()

   select()
   st_select()
   selectindex() // new in Stata 13

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
   asinr() // obsolete in Stata 11?
   acosr() // obsolete in Stata 11? 
   atanr() // obsolete in Stata 11?
   arg() /* mata only */
   sinh() 
   cosh()
   tanh()
   asinh()
   acosh()
   atanh()
   pi() /* mata only */

   sizeof()

   solve_tol()

   solvelower()
   solveupper()
   _solvelower()
   _solveupper()

   // solvenl_... new in Stata 13
   solvenl_init()
   solvenl_init_type()
   solvenl_init_startingvals()
   solvenl_init_numeq()
   solvenl_init_technique()
   solvenl_init_conv_iterchng()
   solvenl_init_conv_nearzero()
   solvenl_init_conv_maxiter()
   solvenl_init_evaluator()
   solvenl_init_argument()
   solvenl_init_narguments()
   solvenl_init_damping()
   solvenl_init_iter_log()
   solvenl_init_iter_dot()
   solvenl_init_iter_dot_indent()

   solvenl_solve()
   _solvenl_solve()

   solvenl_result_converged()
   solvenl_result_conv_iter()
   solvenl_result_conv_iterchng()
   solvenl_result_conv_nearzero()
   solvenl_result_values()
   solvenl_result_Jacobian()
   solvenl_result_error_code()
   solvenl_result_return_code()
   solvenl_result_error_text()

   solvenl_dump()
   // end solvenl

   sort()
   _sort()
   jumble()
   _jumble()
   order()
   unorder()
   _collate()

   soundex()
   soundex_nara()

   spline3()
   spline3eval()

   sqrt()

   st_addobs()
   _st_addobs()

   st_addvar()
   _st_addvar()

   _st_data()
   st_data()
   _st_sdata()
   st_sdata()

   st_dir()

   st_dropvar()
   st_dropobsin()
   st_dropobsif()
   st_keepvar()
   st_keepobsin()
   st_keepobsif()

   // st_frames new in Stata 16
   st_framecurrent()
   st_framedir()
   st_framecreate()
   _st_framecreate()
   st_framecurrent()
   _st_framecurrent()
   st_framerename()
   _st_framerename()
   st_framedrop()
   _st_framedrop()
   st_framedropabc()
   st_framereset()
   st_framecopy()
   _st_framecopy()
   st_frameexists()
   
   st_global()
   st_global_hcat() // new in Stata 15

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
   st_matrix_hcat()

   st_numscalar()
   st_numscalar_hcat() // new in Stata 12
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
   _st_tsrevar()

   st_updata()

   st_varformat()
   st_varlabel()
   st_varvaluelabel()

   st_varindex()
   _st_varindex()

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

   ustrlen()   // new in Stata 14
   ustrinvalidcnt()  // new in Stata 14

   udstrlen() // new in Stata 14

   strmatch()

   strofreal()

   strpos()
   strrpos() // new in Stata 14, mata only

   ustrpos() // new in Stata 14
   ustrrpos() // new in Stata 14

   strreverse()

   ustrreverse() // new in Stata 14

   strtoname()

   ustrtoname()  // new in Stata 14

   strtoreal()
   _strtoreal()

   stritrim()
   strltrim()
   strrtrim()
   strtrim()

   ustrltrim()  // new in Stata 14 
   ustrrtrim()  // new in Stata 14 
   ustrtrim()   // new in Stata 14 

   strupper()
   strlower()
   strproper()

   ustrupper()  // new in Stata 14 
   ustrlower()  // new in Stata 14 
   ustrtitle()  // new in Stata 14 

   subinstr()
   subinword()

   usubinstr()  // new in Stata 14 

   sublowertriangle()
   _sublowertriangle()

   _substr()

   _usubstr() // new in Stata 14 mata only

   substr()

   usubstr()  // new in Stata 14
   ustrleft()
   ustrright()

   udsubstr() // new in Stata 14 mata only

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

   transposeonly()
   _transposeonly()

   trunc()
   floor()
   ceil()
   round()

   // begin obsolete block (Stata 10.1)
   uniform()
   uniformseed()
   // end obsolete block 

   uniqrows()

   unitcircle()

   unlink()
   _unlink()

   urlencode() // new in Stata 15
   urldecode() // new in Stata 15

   ustrcompare() // new in Stata 14
   ustrsortkey() // new in Stata 14
   ustrcompareex() // new in Stata 14
   ustrsortkeyex() // new in Stata 14

   ustrfix() // new in Stata 14

   ustrnormalize() // new in Stata 14

   ustrsplit()    // new in Stata 16 

   ustrto()    // new in Stata 14 
   ustrfrom()  // new in Stata 14

   ustrunescape() // new in Stata 14
   ustrtohex()    // new in Stata 14

   ustrword()      // new in Stata 14 
   ustrwordcount() // new in Stata 14

   valofexternal()

   Vandermonde()

   vec()
   vech()
   invvech()

   // xl class system (highlighting could be hard)
   // Step 1: init
   xl()
   // Step 2: create and open workbook
   B.create_book()
   B.load_book()
   B.clear_book()
   B.set_mode()
   B.close_book()
   // Step 3: setting sheet
   B.add_sheet()
   B.set_sheet()
   B.set_sheet_gridlines() // new in Stata 14
   B.set_sheet_merge() // new in Stata 14
   B.clear_sheet()
   B.delete_sheet() // new in Stata 14 
   B.delete_sheet_merge() // new in Stata 14 
   B.get_sheets()
   // Step 4: reading/writing info
   B.get_string()
   B.get_number()
   B.get_cell_type()
   B.put_string()
   B.put_number()
   B.put_formula() // new in Stata 14
   B.put_picture() // new in Stata 14 
   B.set_missing()
   // Cell Formatting (new in Stata 14)
   B.set_number_format()
   B.set_vertical_align()
   B.set_horizontal_align()
   B.set_border()
   B.set_left_border()
   B.set_right_border()
   B.set_top_border()
   B.set_bottom_border()
   B.set_diagonal_border()
   B.set_fill_pattern()
   B.set_column_width()
   B.set_row_height()
   // Text formatting (new in Stata 14)
   B.set_font()
   B.set_font_bold()
   B.set_font_italic()
   B.set_font_strikeout()
   B.set_font_underline()
   B.set_font_script()
   B.set_text_wrap()
   B.set_shrink_to_fit()
   B.set_text_rotate()
   B.set_text_indent()
   B.set_format_lock()
   B.set_format_hidden()
   // Formatting Cell Ranges (new in Stata 15)
   B.add_fmtid()
   B.set_fmtid()
   B.fmtid_set_number_format()
   B.fmtid_set_vertical_align()
   B.fmtid_set_horizontal_align()
   B.fmtid_set_border()
   B.fmtid_set_left_border()
   B.fmtid_set_right_border()
   B.fmtid_set_top_border()
   B.fmtid_set_bottom_border()
   B.fmtid_set_diagonal_border()
   B.fmtid_set_fill_pattern()
   B.fmtid_set_column_width()
   B.fmtid_set_row_height()
   B.fmtid_set_text_wrap()
   B.fmtid_set_shrink_to_fit()
   B.fmtid_set_text_rotate()
   B.fmtid_set_text_indent()
   B.fmtid_set_format_lock()
   B.fmtid_set_format_hidden()
   B.add_fontid()
   B.add_fmtid_set_fontid()
   B.fontid_set_font()
   B.fontid_set_font_bold()
   B.fontid_set_font_italic()
   B.fontid_set_font_strikeout()
   B.fontid_set_font_underline()
   B.fontid_set_font_script()
   // Utility functions
   B.query()
   B.get_colnum()
   B.get_colletter()        // new in Stata 15 
   B.set_keep_cell_format() // new in Stata 14
   B.set_error_mode()
   B.get_last_error()
   foo.get_last_error_message()

   /* end of [M] mata manual */

   /* [ME] manual (new in Stata 13) */
   // now in order of manual
   estat df

   estat gr
   estat grou
   estat group

   estat icc

   estat recov
   estat recovariance

   estat sd

   estat wcor
   estat wcorr
   estat wcorrelation

   mecloglog

   meglm

   meintreg // new in Stata 15 

   melogit

   menbreg

   menl     // new in Stata 15 

   meologit

   meoprobit

   mepoisson

   meprobit

   meqrlogit     // obsolete in Stata 16 
   meqrpoisson   // obsolete in Stata 16 

   mestreg // new in Stata 14
   
   metobit // new in Stata 15 

   stcurve
   estat group

   mixed
   estat df // new in Stata 14 
   estat group
   estat icc
   estat recovariance
   estat sd
   estat wcorrelation

   /* end of [ME] manual */

   // the [META] manual, new in Stata 16 @@

   meta // incomplete

   // meta data is a concept, not a command

   meta es
   meta esize

   meta set

   meta up
   meta update
   meta q
   meta query
   meta clea    // incomplete
   meta clear

   meta forest
   meta forestplot

   meta sum
   meta summarize

   meta labbe
   meta labbeplot

   meta reg
   meta regress

   estat bubble
   estat bubbleplot

   meta funnel
   meta funnelplot

   meta bias

   meta trim
   meta trimfill

   // end of the [META] manual

   // the [MI] multiple imputation manual...all new in Stata 11
   // ... and vastly expanded in Stata 12
   mi // incomplete
   mi add

   mi append using

   mi convert // incomplete
   mi convert w
   mi convert wide
   mi convert ml
   mi convert mlong
   mi convert fl
   mi convert flong
   mi convert flongs
   mi convert flongsep

   mi copy

   mi q
   mi query
   mi d
   mi describe

   mi erase

   mi est
   mi estimate

   // mi estimate postestimation moved to respective commands

   mi expand

   mi export // incomplete
   mi export ice
   mi export nhanes1

   mi extract

   mi import // incomplete

   mi import flong

   mi import flongsep

   mi import ice

   mi import nhanes1

   mi import wide

   mi imp // incomplete
   mi impute // incomplete

   mi imp chain // new in Stata 12 
   mi impute chained // new in Stata 12

   mi imp intreg // new in Stata 12 
   mi impute intreg // new in Stata 12 

   mi imp log   // log not long enough
   mi imp logi
   mi impu logit

   mi imput mlog
   mi impute mlogit

   mi imp mon
   mi impute monotone

   mi imp mvn

   mi imp nbreg // new in Stata 12 

   mi impute olog
   mi impute ologit

   mi impu pmm

   mi imput poisson // new in Stata 12

   mi imput reg
   mi impute regress

   mi imp truncreg // new in Stata 12
   mi impute foobar // incomplete but technically ok if foobar is a user method

   mi merge // incomplete
   mi merge 1:1
   mi merge 1:m
   mi merge m:1
   mi merge m:m
   mi merge m:2  // no good, of course
   @@ start here

   mi misstab // incomplete
   mi misstab sum
   mi misstable summarize
   mi misstable pat
   mi misstable patterns
   mi misstable tre // incomplete
   mi misstab tree
   mi misstabl nest
   mi misstable nested

   mi pas :
   mi passive:

   mi predict // new in Stata 12
   mi predictnl  // new in Stata 12 

   mi ptrace // incomplete
   mi ptrace d
   mi ptrace describe
   mi ptrace use

   mi ren
   mi rename

   mi replace0

   mi reset

   mi reshape long
   mi reshape wide

   mi select init
   mi select

   mi set // incomplete
   mi set w
   mi set wide
   mi set ml
   mi set mlong
   mi set fl
   mi set flong
   mi set flongs
   mi set flongsep
   // mi register is under mi set
   // look carefully...on one line
   mi reg // incomplete, but regress is winning the battle here
   mi regi // incomplete
   mi reg imp
   mi regi imputed
   mi regis pas
   mi regist passive
   mi registe reg
   mi register regular
   mi unreg
   mi unregister
   mi set M
   mi set m
   mi set q  // nothing good
   mi unset

   mi stsplit
   mi stjoin

   mi test
   mi testtr
   mi testtransform

   mi update

   mi vary
   mi varying

   mi xeq: summarize
   mi xeq : tab

   mi fvset
   mi svyset
   mi stset
   mi streset
   mi st
   mi tsset
   mi xtset
   /* end [MI] multiple imputation */ 

   /* from [MV] multivariate statistics */
   alpha

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
   /* now ordered by the manual */
   /* omnibus -cluster- entry ignored */

   clustermat // incomplete
   clustermat s
   clustermat singlelinkage
   clustermat a
   clustermat averagelinkage
   clustermat c
   clustermat completelinkage
   clustermat wav
   clustermat waveragelinkage
   clustermat med
   clustermat `foo' // incomplete --- good? bad?
   clustermat medianlinkage
   clustermat cent
   clustermat centroidlinkage
   clustermat ward
   clustermat wardslinkage
   /* end clustermat commands */

   cluster // incomplete
   cluster dend
   cluster dendrogram
   /* cluster tree is a synonym for cluster dendogram */
   cluster tr
   cluster tree

   cluster gen
   cluster generate 

   cluster k
   cluster kmeans
   cluster kmed
   cluster kmedians

   // cluster linkage
   cluster s
   cluster singlelinkage
   cluster a
   cluster averagelinkage
   cluster c
   cluster completelinkage
   cluster wav
   cluster waveragelinkage
   cluster med
   cluster `foo' // incomplete
   cluster medianlinkage
   cluster cen
   cluster centroidlinkage
   cluster ward
   cluster wardslinkage

   cluster not // incomplete
   cluster note
   cluster notes
   cluster notes drop

   /* cluster programming utilities */
   cluster query
   cluster set
   cluster del
   cluster delete
   cluster parsedist
   cluster parsedistance
   cluster measures

   /* cluster stop */
   cluster stop
   clustermat stop

   /* cluster utility */
   cluster dir
   cluster list
   cluster drop
   cluster use
   cluster rename
   cluster renamevar


   discrim // this is OK because of replaying results
   /* discrim commands from the discrim intro */
   discrim knn
   discrim lda
   discrim logistic
   discrim qda
   /* discrim estat */
   estat classtable
   estat errorrate
   estat grsummarize
   estat list
   estat summarize

   discrim knn
   estat classtable
   estat errorrate
   estat grsummarize
   estat list
   estat summarize

   discrim lda
   /* discrim lda postestimation */
   estat anova
   estat canontest
   estat classfunctions
   estat classtable
   estat correlations
   estat covariance
   estat errorrate
   estat grdistances
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

   discrim logistic
   estat classtable
   estat errorrate
   estat grsummarize
   estat list
   estat summarize

   discrim qda
   estat classtable
   estat correlations
   estat covariance
   estat errorrate
   estat grdistances
   estat grsummarize
   estat list
   estat summarize


   fac
   fact
   facto
   factor

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

   // only manova listed in manual starting at latest at Stata 16
   //  abbreviations still behave OK
   mano
   manov
   manova
   manovatest
   screeplot

   mat dis foo
   matrix dissimilarity bar

   mca
   /* mca postestimation */
   mcaplot
   mcaprojection
   estat coordinates
   estat subinertia
   estat summarize
   screeplot

   mds
   /* mds postestimation */
   estat config
   estat correlations
   estat pairwise
   estat quantiles
   estat stress
   estat summarize
   mdsconfig
   mdsshepard
   screeplot
   /* end mds postestimation */

   mdslong

   mdsmat

   mvreg

   mvtest corr
   mvtest correlations

   mvtest cov
   mvtest covariances

   mvtest m
   mvtest means

   mvtest norm
   mvtest normality

   /* score is obsolete as of Stata 9 */
   // begin obsolete block 
   sco
   scor
   score
   // end obsolete block

   pca
   pcamat
   /* pca postestimation */
   estat anti
   estat kmo
   estat loadings
   estat residuals
   estat rotatecompare
   estat smc
   estat summarize
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

   rot
   rota
   rotat
   rotate

   rotatemat

   scoreplot
   loadingplot

   greigen // obsolete in Stata 9

   scree // listed as synonym for -screeplot- in Stata 13 manual
   screeplot

   /* end of [MV] manual */

   /* [P] programming manual, moved in list in Stata 14 */
   nobreak
   break

   /* not highlighting byable() stuff, because an option */
   program dingle, rclass byable(recall)
   end
   program foobar, sclass byable(onecall)
   end

   cap
   capture

   char `foo'
   char `foo`bar''
   char ``foo'bar'
   char
   char define
   char l
   char list
   char ren
   char rename
   // a meager attempt at highlighting class programming
   // really just highlighting the tails from the built-in functions
   //   (for updating: jumped to section 8.1 in Stata 14)
   // Starting Stata 16, no longer paying attention to large class section
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
   // built-in modifiers
   a.b.Declare
   a.b.Arrdropel
   a.b.Arrdropall
   a.b.Arrpop
   a.c.Arrpush

   // cannot seem to get these to work well
   //   making . highlight differently is really hard, because it shows
   //   up in too many contexts
   // will not ever fix, as these are supposedly withering

   .Global.foo.gringo
   .Local.d.e.f
   .Super.q.e.d
   4.2


   class fooey {
      class:
      classw:
      classwide:
      instance:
      instancespecific:
      } 

   // now outside of overarching 'class' section
   class exit
   class bl33n

   classutil drop
   classutil d
   classutil describe
   classutil dir
   classutil cdir
   classutil which
   classutil `foo' // bad ? good?

   /* confirm commands */
   conf e
   confi existence
   confir new f
   confirm file

   conf numeric fo
   conf str for
   conf string form
   conf date forma

   /* confirm ts was replaced by confirm date in Stata 10,  */
   conf ts format // obsolete in Stata 10

   // frames new in Stata 16
   confirm new frame
   confirm frame

   conf name
   confi names
   confirm int number // bad ---no abbrev allowed for -integer-
   confir integer n
   conf integer number
   conf n
   conf num
   confirm number
   conf mat
   confirm matrix
   conf sca
   conf scalar

   confirm `foo' // incomplete, bad? good?
   confirm numeric var // ok
   confirm numeric var() // should fail
   confirm numeric `var' // should fail --- good? bad?

   conf new v
   conf numeric va
   confirm str var
   confirm string vari
   confirm str16 var  // allowed specific string sizes
   confirm byte varia
   confirm int variab
   conf long variabl
   conf float variable
   conf double v
   confirm str9 v
   confirm str11 var
   confirm str244 var
   confirm str455 var
   confirm str2045 var
   confirm str2046 var // should fail
   confirm strL var

   continue

   /* oh no! the cclass stuff */
   cret l
   creturn list
   /* system values */
   c( current_date )
   c(current_date)
   c(current_time)
   c(rmsg_time)
   c(stata_version)
   c(version)
   c(userversion) // new in Stata 14
   c(dyndoc_version) // new in Stata 15
   c(born_date)
   c(flavor)
   c(bit) // new in Stata 12
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
   c(hostname) // new in Stata 13 
   c(machine_type)
   c(byteorder)
   c(username)

   /* directories and paths */
   c(sysdir_stata)
   c(sysdir_updates) // (technically obsolete in Stata 13)
   c(sysdir_base)
   c(sysdir_site)
   c(sysdir_plus)
   c(sysdir_personal)
   c(sysdir_oldplace)
   c(tmpdir)
   c(adopath)
   c(pwd)
   c(dirsep)

   /* system limits */
   c(max_N_theory)
   c(max_N_current)  // obsolete in Stata 12
   c(max_k_theory)
   c(max_k_current) // obsolete in Stata 12
   c(max_width_theory)
   c(max_width_current) // obsolete in Stata 12
   c(max_matdim)   // new in Stata 16
   c(max_matsize)  // obsolete in Stata 16; still returns a value < c(max_matdim), though
   c(min_matsize)  // obsolete in Stata 16
   c(max_it_cvars) // new in Stata 16 
   c(max_it_fvars) // new in Stata 16 
   c(max_macrolen)
   c(macrolen)
   c(charlen)    // new in Stata 14
   c(max_cmdlen)
   c(cmdlen)
   c(namelen)   // undocumented/obsolete in Stata 14
   c(namelenbyte)
   c(namelenchar)
   c(eqlen)

   /* numeric and string limits */
   c(mindouble)
   c(maxdouble)
   c(epsdouble)
   c(smallestdouble)
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
   c(maxstrlvarlen) // new in Stata 13 
   c(maxvlabellen) // new in Stata 13 

   /* current dataset */
   c(frame)        // new in Stata 16 
   c(N)
   c(k)
   c(width)
   c(changed)
   c(filename)
   c(filedate)

   /* memory */
   c(memory)
   c(maxvar)
   c(matsize)  // obsolete in Stata 16 
   c(niceness) // new in Stata 12
   c(min_memory) // new in Stata 12
   c(max_memory)   // new in Stata 14 
   c(segmentsize) // new in Stata 12
   c(adosize)
   c(max_preservenum)  // new in Stata 16 

   /* output */
   c(more)
   c(rmsg)
   c(dp)
   c(linesize)
   c(pagesize)
   c(logtype)
   c(noisily)
   c(charset) // new in Stata 13 mac only, obsolete in Stata 14 
   c(eolchar) // no longer documented in Stata 15
   c(notifyuser)
   c(playsnd)
   c(icmap) // obsolete in Stata 10
   c(include_bitmap) // mac only new in Stata 12
   c(iterlog)  // new in Stata 16 
   c(level)
   c(clevel)  // new in Stata 14 
   c(showbaselevels) // new in Stata 11.1
   c(showemptycells) // new in Stata 11.1
   c(showomitted) // new in Stata 11.1
   c(fvlabel) // new in Stata 13
   c(fvwrap) // new in Stata 13
   c(fvwrapon) // new in Stata 13
   c(lstretch) // new in Stata 12
   c(cformat) // new in Stata 11.1
   c(sformat) // new in Stata 11.1
   c(pformat) // new in Stata 11.1
   c(coeftabresults)  // new in Stata 13
   c(dots)  // new in Stata 16

   /* interface */
   c(dockable)
   c(dockingguides) // obsolete in Stata 16
   c(floatresults) // obsolete in Stata 8 or so
   c(floatwindows) // obsolete in Stata 8 or so
   c(locksplitters)
   c(persistfv)  // obsolete in Stata 12
   c(persistvtopic) // obsolete in Stata 12
   c(pinnable)
   c(doublebuffer)
   c(reventries)
   c(fastscroll) // not platform dep in Stata 10, Unix-only in 11
   // Unix/Win only in 12
   c(revwindow) // obsolete in Stata 11 
   c(revkeyboard)
   c(varwindow) // obsolete in Stata 11 
   c(varkeyboard)
   c(smoothfonts)
   c(use_qd_text) // obsolete in Stata 11 
   c(smoothsize) // obsolete in Stata 11 
   c(use_atsui_graph) // obsolete in Stata 11 
   c(linegap)
   c(scrollbufsize)
   c(varlabelpos) // obsolete in Stata 11, it seems
   c(maxdb)
   c(smalldlg)  // obsolete in Stata 10 
   c(xptheme)   // obsolete in Stata 10 

   /* graphics settings */
   c(graphics)
   c(autotabgraphs)
   c(scheme)
   c(printcolor)
   c(copycolor)
   c(macgphengine) // obsolete in Stata 9 [?]
   c(piccomments)  // obsolete in Stata 9 [?]
   c(maxbezierpath)  // new in Stata 16; Mac only

   /* efficiency */
   // in Stata16, efficiency is no longer a section
   c(virtual)  // obsolete in Stata 12

   /* network */
   c(checksum)
   c(timeout1)
   c(timeout2)
   c(httpproxy)
   c(httpproxyhost)
   c(httpproxyport)
   c(httpproxyauth)
   c(httpproxyuser)
   c(httpproxypw)

   /* update settings (not in Unix yet) */
   c(update_query)
   c(update_interval)
   c(update_prompt)

   /* trace settings */
   c(trace)
   c(tracedepth)
   c(tracesep)
   c(traceindent)
   c(traceexpand)
   c(tracenumber)
   c(tracehilite)

   /* mata */
   c(matastrict)
   c(matalnum)
   c(mataoptimize)
   c(matafavor)
   c(matacache)
   c(matalibs)
   c(matamofirst)

   /* java settings, new in Stata 16 */
   c(java_heapmax)
   c(java_home)

   /* putdocx settings, new partway into Stata 16 */
   c(docx_hardbreak)
   c(docx_paramode)
   
   /* python */
   // new in Stata 16
   c(python_exec)
   c(python_userpath)

   /* random number generator (RNG) settings */
   c(rng)
   c(rng_current)
   c(rngstate)
   c(rngseed_mt64s)
   c(rngstream)

   /* unicode settings (new in Stata 14) */
   c(locale_ui)
   c(locale_functions)
   c(locale_icudflt)

   /* other settings */
   c(type)
   c(maxiter)
   c(searchdefault)
   c(seed)   // obsolete in Stata 14 (replaced by c(rngstate))
   c(version_rng) // new in Stata 11.2, returns 14 in Stata 14, no docs
   c(varabbrev)
   c(emptycells) // new in Stata 12
   c(fvtrack) // new in Stata 15
   c(fvbase)  // new in Stata 16
   c(haverdir) // new in Stata 13 
   c(odbcmgr)
   c(odbcdriver) // new sometime in Stata 14
   c(fredkey) // new in Stata 15
   c(`foo')

   /* other (uh notsettings?) */
   c(pi)
   c(alpha)
   c(ALPHA)
   c(Mons)
   c(Months)
   c(Wdays)
   c(Weekdays)
   c(rc)
   "`c(pi)'" !!!

   /* end of that mess */
   _datasig
   _datasignature

   #delimit  // should be incomplete; but doesn't show....
#d cr
#delimit ;

   regress foo bar;
   regress
     foo bar;
   logistic bar foo;

#delimit cr
   regress foo bar /// this should look like a comment
     , someopt(eek)
   logistic bar foo // this is a comment
   ereturn list

#delimit ;

   ologit ohmy;
   mlogit

     /* comment mid-command */
     somevar
     continuationvar;

   areg /// this is a comment
     not_a_comment /// some more comments
     more_continuation;

   describe;

   summarize 
     , detail should behave as a continuation?;
   summarize;
   if this==that {;
      summarize
        another continuation;
      }; 
   summ;       des; tabulate bleen; // impossible-to-fix non-highlighting

   if this | that {;
      regress should indent;
      }; 
   logit should_be_new!
     continuation;   

#delim cr

   display this "#delim ;" is inside quotations, and hence is invalid

   if {
      test
      }   

#delimit cr

   /* all the dialog stuff is in syntax_tester.dlg, because the dlg stuff should really
   be a separate mode ... ugh */

   discard

   di
   dis
   disp
   displ
   displa
   display
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
   display _n // displays as a constant because of system constant _n 
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
   display in blue    // obsolete in Stata 7[?]
   display in red     // obsolete in Stata 7[?]
   display in yellow  // obsolete in Stata 7[?]



   /* ereturn... */
   eret loc bleen
   eret local
   eretu sca
   eretur scalar bloor
   ereturn mat freen
   eret matrix
   eretu clear
   ereturn `foo' // should fail good? bad?
   eretur li
   ereturn list
   eret post
   eretu repost
   eretur di
   ereturn display

   /* number not highlighted, because it can be an expression */
   err 444
   error 666

   /* estat programming has nothing worthwhile */
   // _estimates commands
   _est h
   _esti hold
   _estim u
   _estima unhold
   _estimat dir
   _estimate clear
   _estimates drop
   _estimates `foo' // should fail...should it?

   e // why is this a valid abbrev?
   exit

   file // incomplete
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

   foreach bleen // incomplete
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
   ** this should work, but does not because of starting *
   **   including varlists is pretty difficult
   foreach var of varlist *date {
      }
   foreach var of varlist a-b {
      }
   ** should not work, because emojis cannot start variable names
   foreach var of varlist 😀 {
      }
   ** should not work, because variable names cannot start with a number
   foreach var of varlist 4rrw {
      }
   * should work, but regexp classes don't have one for non-symbol unicode
   foreach var of varlist happy😀 {
      }

   forv // incomplete
   forvalues bleen // incomplete
   forv fooie=1/4 {
      }
   forvalues aNum = 2(3)14 {
      }

   frame create
   frame post

   fvexpand

   gettoken foo : griminy, parse(" ,")
   gettoken bleeble bauble : foo, parse(",")
   gettoken foo 1 : bubble
   gettoken foo 0 : 0, parse(" ")
   gettoken (local) foo: complex
   gettoken (global) hey ho   : ho
   gettoken (local) bleen (global) hooie : gomp
   gettoken (local) bleen (local) bling : how
   qui gettoken (local) bleen (local) bling : how
   by foo: gettoken (local) bleen (local) bling : how
   nothing good gettoken (local) bleen (local) bling : how // should fail

   if foo fuggy // should fail because of the missing brace 
   if `this' that // should fail again because of missing brace
   if `those' {
      display "something"
      }
   if foo {
      display "urf"
      }
   else `fortuna' // should fail: missing right brace
   else frantabulous // should fail, but perhaps should not
   else {
      display "frantabulous"
      }
   // should not highlight as command
   bleen else bling // bad

   include somefile.doh

   // from javautilities
   // new in Stata 16
   java  // incomplete
   java query
   java init
   java initialize
   java set // incomplete
   java set home
   java set heapmax

   javacall // new in Stata 13 

   levelsof

   /* macro stuff */
   gl fooie
   global fooie
   global `l`fooie''
   display $fooie
   display $`bleen' // skip the highlight? 
   lo hmm // should fail, because minabbrev is loc
   loc ``ooie''
   local fooie
   tempvar 
   tempvar ding
   tempvar tmp1 foo4
   tempname
   tempname dong
   tempname ding dong
   tempfile
   tempfile the
   tempfile this is a test of many files
   /* right */
   loc ++witch
   local --is
   /* wrong---since forever due to parsing bug for local macros */
   loc which--   // assigns -- to which
   local wrong++ // assigns ++ to wrong
   /* right, though this points to the need for highlighting operators */
   display `foo++'  // shows value of foo, then increments foo
   display `++foo'  // increments foo, then shows it
   display `--foo'
   display `--`foo''
   display `+++foo' // wrong
   macro define bleen // obsolete
   ma di
   macro dir
   mac drop // incomplete
   ma drop bleen
   macro drop 123 // illegal name
   ma l
   macro list
   ma s
   macro shift

   glo fooey : properties
   globa heha : results    // new in Stata 16
   glo dingle : ty
   global dingle : type
   loc dingle : f
   local dingle : format
   gl s : val l fail
   gl h : val lab
   global h : value label
   loc h : var l
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
   local foo: sysdir STATA
   local foo: sysdir   BASE
   local foo: sysdir SITE
   global blah: sysdir PLUS
   local foo: sysdir PERSONAL
   local bad: sysdir OHNO  // should fail on OHNO
   local hmm: sysdir UPDATE  // technically OK, but not documented

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
   loc h: s(functions)  // should fail because s(functions) no good
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
   // a bunch of new Stata 15 stuff
   local h : rownumb
   local h : colnumb
   local h : roweqnumb
   local h : coleqnumb
   local h : rownfreeparms
   local h : colnfreeparms
   local h : rownlfs
   local h : colnlfs
   local h : rowsof 
   local h : colsof 
   local h : rowvarlist 
   local h : colvarlist
   local h : rowlfnames
   local h : collfnames
   // end of new Stata 15 stuff
   glo foo: tsnorm
   local g : copy loc
   local h : copy local
   local h : copy gl
   local b : copy global
   local h : word // should fail
   local h : word count
   /* maybe should change the number highlight? */
   loc h : word 43 of
   local h : word `foo' of bar
   /* should fail */
   local h : word me of you 
   local h : piece
   local h : length loc // obsolete in Stata 14 
   local h : strlen loc       // new in Stata 14 
   local h : strlen glo       // new in Stata 14 
   local h : ustrlen local    // new in Stata 14 
   local h : udstrlen gl      // new in Stata 14 
   local h : udstrlen loc      // new in Stata 14 
   local h : subinstr gl
   local h : subinstr global hi
   local h : subinstr loc ho
   local h : subinstr local
   /* these have become undocumented  */
   // begin obsolete block
   local h : tempv
   local h : tempvar
   local h : tempf
   local h : tempfile
   // end obsolete block

   /* macro lists */
   loc foo : list uniq bar
   global foo : list dups bar
   glob foo: list sort bar
   loca foo : list retok bar
   local foo:list retokenize bar
   glo foo : list clean bar
   glob foo : list a | b // perhaps operator highlighting would be good
   globa foo: list c & d
   global foo : list ding - dong
   global foo: list this == that
   global foo: list this === that
   loc foo: list hey in ho // perhaps 'in' should highlight as operator?
   local foo: list sizeof hey
   local foo: list posof "this is something" in hooie

   /* ahh the macros are over */

   makecns a
   matcproc a b // bad: 3 matrices are needed
   matcproc a b c

   marksample // incomplete
   marksample hooie // hooie is a macro name
   mark
   markout
   markin
   svymarkout fiem // fiem is a variable name

   matlist

   /* matrix commands */
   mat ac m
   matr accum matt
   matri glsa matt
   matrix glsaccum matt
   matri opacc fooey // no good because no abbrev for opaccum
   mat opaccum matt
   matrix veca matt
   matrix vecaccum matt

   /* not listed but still accepted */
   matr makeCns foo // obsolete, or rather should be 
   matri dispCns // obsolete, or rather should be

   /* dangerous keyword highlighting which is unavoidable */
   mat def foo
   mat defin // correct by the way Stata works
   mat foo
   mat `foo'
   mat define bleen
   matrix in
   mat in blam
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

   // xxxjoinbyname new in Stata 16 
   mat rowjoin A
   mat rowjoinbyname B
   matri coljoin C
   matrix coljoinbyname D

   mat rown njk = kjj
   matrix rownames rrr = ccc
   matrix rowname e∑∑ = should work
   matrix rowname ∑∑∑ = should not work
   mat coln ccc = rrr
   matrix colnames ccc = rrr
   mat rowe hi = ho
   mat roweq ho = hi
   mat cole ho = hi
   mat coleq ho = hi
   mat coleq hå = foo

   mat sco fooey = ...
   matrix score fooey = ...

   mat svd g h j

   mat syme jwjwk foo
   matrix symeigen jwjwk foo

   matrix d
   matrix dir
   mat l bleen
   matrix list bleen
   matrix ren foo bar
   matrix rename foo bar
   /* !! not quite right, but I'm really stumped. */
   matrix drop mat1 mat2 mat3 mat4
   matrix drop _all
   matrix `foo' // no real way to highlight, because `foo' could be matrix or subcommand

   /* phew, matrix is finally done */

   mor  // what an idiotic abbreviation
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
   set max_preservemem // new in Stata 16

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
   // prog l --- is legal (!) but confuses indentation
   program list fooie

   // new in Stata 14 (or maybe 14.1)
   // syntax easier to find via -help projmanager-
   projman
   projmana
   projmanag
   projmanage
   projmanager

   // python new in Stata 16 
   python
      this is python code
   end
   
   python:
      this is python code
   end
   python script
   python set exec
   python set userpath
   python des
   python describe
   python drop
   python clear
   python q
   python query
   python sea
   python search
   python which
   
   qui regress
   quietly {
      n describe
      noisily des
      }
   nois : ologit
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
   _return `foo' // should fail, good? bad?

   // return and its relatives
   return `foo' // highlights like mata
   ret li
   retu list
   ret clear
   retu sca foo
   return scalar foo
   ret loc foo
   return local foo
   /* the third item ought to be a matrix */
   ret mat matt hhh
   return matrix matt mmm
   ret add
   return add

   eretu li
   eretur list
   eret clear
   ereturn clear
   eret post m1 m2
   ereturn post
   eret sca
   ereturn scalar
   eret loc foo
   ereturn local foo
   eretu mat short       // urf, a keyword
   ereturn matrix bleen
   eret repost
   ereturn repost

   sretu li
   sreturn list
   sret clear
   sret loc foo
   sreturn local foo
   /* end return commands */

   _rmcoll
   _rmdcoll

   set r on
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
   scalar drop _all
   scalar drop freddy mikey  // oops, mikey should highlight

   /* serset commands */

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

   // perhaps -default- should highlight?
   set locale_functions default // new in Stata 14
   // do not want to allow all possible aliases!
   set locale_functions latin1  // new in Stata 14 

   // not going to put legal locale_ui values in
   set locale_ui default   // new in Stata 14 
   set locale_ui macroman  // new in Stata 14 

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
   {sf should fail} // should fail
   {sf should:fail} // should fail

   {input}
   {input:foo}
   {error}
   {error:hahah}
   {result}
   {result:shocking}
   {text}
   {text:for later reading}

   {inp} // should not fail
   {inp:foo}
   {input:bwa haha haha}
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
   {opt foo(a,)} // really is wrong, but hey...

   /* syntax 1 & 2 */
   {hilite}
   {hilite:of the day}
   {hi}
   {hi:how are you}
   {hil:should not work} // should fail

   /* syntax 2 & 3 */
   {ul on}
   {ul:is no. 1 in basketball}
   {ul off}
   {ul bogus} // should fail

   /* syntax 2 & 3 (book says 2 & 4 but illustrates with 2 & 3) */
   {*:comment}
   {* this is a comment}

   {hline}
   {hline 20}
   {hline bogus} // should fail 
   {.-}
   {hline `this'}

   {dup 23:some}
   {dup `foo':some}
   {dup bogus:some} // should fail 

   {c 666} // should fail
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
   {manhelp unix} // should fail 
   {manhelp this should fail:if I had time} // should fail 
   {manhelpi fooey} // should fail 
   /* need yet another @#@#$@ syntax for this hack */
   {help stata##anchors}
   {help stata##anchor|viewer}          // !! should have | as constant
   {help stata##anchor:subtext}
   {help stata##anchor|viewer:subtext}  // !! should have | as constant
   {marker jumphere}{...}
   {marker ul}
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
   {view_d fail}  // should fail
   {view_d:hahah}
   {manpage fail} // should fail
   {manpage SVY 99}
   {manpage R notquite} // should fail
   {manpage P:666}

   {manpage docs:awfully unixy}
   {mansection SEM bleen}
   {mansection dopey:fail} // should fail
   {mansection P:haha}
   {manlink hahahah} // should fail
   {manlinki R summarize}

   {news:is bad} // obsolete in Stata 16 !! should mark as obsolete
   {net fishing}
   {net fishing:wide}
   {net_d fail} // should fail
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
   {center 35:bleen}
   {centre 42:voldemort}
   {center bogus:haha} // should fail
   {rcenter:teehee}
   {rcentre 33:friday!}

   {right:wing neocon}
   {lalign 69:ihtfp}
   {ralign 666:nationalist}
   // dlgtab is a pain b/c of 1 or 2 possible numbers
   {dlgtab 34:fooey}
   {dlgtab 4 2: hello} // should change numbers to variable-face
   {dlgtab 1 2 3: fails} // should fail
   {dlgtab : fooey}
   {...}
   {col bogus} // should fail 
   {col 32}
   {col `this'}
   {space bogus} // should fail 
   {space 43}
   {tab}

   /* for paragraph mode */
   {p}
   {p 4}
   {p bogus} // should fail 
   {p `hoo'}
   {p 3 4}
   {p 3 `foo' 5}
   {p 3 4 5 oh no} // should fail
   {p 1 2 3 4}
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
   // back to odd syntax directives
   {p_end}
   {p2colset 1 2 3} // should fail
   {p2colset 1 2 `foo' 4}
   {p2colset 1 2 3 4 5} // should fail
   {p2col 1 2 3} // should fail
   {p2col 1 2 3 4:something goes here}
   {p2col: something goes here}
   {p2col 1 2:this is bad} // should fail
   {p2line 1 2}
   {p2line}
   {p2line 4}  // should fail
   {p2line 1 2 `bad'} // should fail
   {p2colreset}

   {synoptset}
   {synoptset 6}
   {synoptset `foo'}
   {synoptset 5 tabbed}
   {synoptset 12 notes}
   {synoptset tabbed}
   {synoptset notes}
   {synoptset tabbed notes} // should fail
   {synoptset 5 6} // should fail 

   {synopthdr}
   {synopthdr:} // should be OK
   {synopthdr:damn}
   {syntab: this}
   {synopt} // should fail
   {synopt:} // should be OK
   {synopt: is}
   {p2coldent: no fun}
   {synoptline}

   {bind:all this together}
   {break}
   // other odd modes
   {asis}
   {s6hlp} // went undocumented some time in the foggy past
   {ccl pi}
   {ccl current_date}
   // looks nice below, but not really full of testing
   {char 7}
   {c 'g}
   {c -(}
      {c )-}
   {c S|}
   {c -}
   {c |}
   {c +}
   {c TT}
   {c BT}
   {c LT}
   {c RT}
   {c TLC}
   {c TRC}
   {c BRC}
   {c BLC}
   // all the 'western european characters'
   //  likely silly when using Unicode
   {c a'} á {c e'} é {c i'} í {c o'} ó {c u'} ú
   {c A'} Á {c E'} É {c I'} Í {c O'} Ó {c U'} Ú
   {c a'g} à {c e'g} è {c i'g} ì {c o'g} ò {c u'g} ù
   {c A'g} À {c E'g} È {c I'g} Ì {c O'g} Ò {c U'g} Ù
   {c a^} â {c e^} ê {c i^} î {c o^} ô {c u^} û
   {c A^} Â {c E^} Ê {c I^} Î {c O^} Ô {c U^} Û
   {c a~} ã {c o~} õ
   {c A~} Ã {c O~} Õ
   {c a:} ä {c e:} ë {c i:} ï {c o:} ö {c u:} ü
   {c A:} Ä {c E:} Ë {c I:} Ï {c O:} Ï {c U:} Ü
   {c ae} æ {c c,} ç {c n~} ñ {c o/} ø {c y'} ´y
   {c AE} Æ {c C,} Ç {c N~} Ñ {c O/} Ø {c Y'} ´Y
   {c y:} ÿ {c ss} ß {c r?} ¿ {c r!} ¡
   {c L-} £ {c Y=} ¥ {c E=} €
   // smcl allowed in graphs shown with the graph-specific stuff
   // smcl found in help files which is not documented
   {vieweralsosee "somename" "help somename"}
   {viewerdialog "somename" "help that"}
   {viewerjumpto "this" "help this"}

   /* end smcl, finally */

   args mac mactheknife
   args foo
   args foo1 foo2 foo3 foo4
   args øøf

   /* syntax */
   /* no attempt to get this to fontify properly, sadly enough, because there really is no grammar to the syntax statement */
   syntax
   args

   /*
   varlist
   varname
   newvarlist
   newvarname
   exp
   weight
   if
   in
   using
   options
   */

   /* back to things I can handle */
   sysdir
   sysdir l
   sysdir list
   sysdir set  // incomplete
   sysdir set BASE
   sysdir set STATA
   sysdir set SITE
   sysdir set UPDATES  // technically out-of-date
   sysdir set PLUS
   sysdir set PERSONAL
   sysdir set OLDPLACE
   sysdir set NOGOOD // should fail

   personal
   personal dir
   adopath
   // no subcommand highlighting...
   adopath + dingle 
   adopath ++ freeble
   adopath - foo
   set a 30
   set adosize 99

   tabdisp

   timer clear
   timer clear 3
   timer on // should fail
   timer on 4
   timer off 14
   timer list
   timer list 55
   timer off // should fail 

   token
   tokeni
   tokeniz
   tokenize

set trace // incomplete
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
   fvunab bleen: doodle

   unabcmd

   novarabbrev
   varabbrev 

   /* more complicated version commands :<( */
version   // is allowed by itself
vers 8
version 12: fooie
versi 15: aloha
versio 23: howdy // should show as blace for a few years
   viewsource

   while foo {
      this is some stuff
      }
   /* window commands... were moved out of the manual before Stata 9 */
   /* put back in the manual in Stata 13 */
   /* platform dependencies not highlighted */
   window // incomplete
   win fo
   window fopen
   win   // incomplete
   win fs
   window fsave

   window manag // incomplete
   win man minimize
   window manage restore
   win manag prefs // incomplete
   win man prefs load
   win man prefs save
   win man prefs default
   win man update variable // obsolete in Stata 13 
   win man associate // windows only
   win man maintitle // incomplete, but last word is indefinite 
   window man maintitle "fooey" // unix and windows
   windo manag maintitle reset // unix and windows
   windo mana docklabel // mac only 
   window man forward // incomplete
   window manage forward command
   window manage forward doeditor
   window manage forward graph
   window manage forward help
   window manage forward history   // new in Stata 16 
   window manage forward results
   window manage forward review
   window manage forward variables
   window manage forward viewer

   wind mana print // incomplete
   win man print graph
   win man forward graph
   win man close graph
   win man rename graph

   win man print viewer
   win man forward viewer
   win man close viewer

   win menu // incomplete
   window m clear
   win menu append // incomplete
   win menu append submenu
   win m append item
   window me append separator
   window menu refresh
   window menu add_recentfiles

   /* obsolete?? */
   // begin obsolete block 
   window menu popout
   window menu set
   window menu append popout
   window menu append   string
   // end obsolete block 

   window push

   windo stop // incomplete
   window stop stop
   window stopbox note
   window stop rusure
   /* end programming manual, finally */

   /* from the [PSS] manual */
   /* skipping omnibus -power- section */
   power // incomplete
   power onemean

   power twomeans

   power pairedm
   power pairedmeans

   power oneprop
   power oneproportion

   power twoprop
   power twoproportions

   power pairedpr
   power pairedproportions

   power onevar
   power onevariance

   power twovar
   power twovariances

   power onecorr
   power onecorrelation

   power twocorr
   power twocorrelations

   power oneway

   power twoway

   power repeated

   /* new in Stata 15; strangely out of alphabetical order */
   power oneslope

   power rsq
   power rsquared

   power pcorr

   power cmh

   power mcc

   power trend

   power cox

   power exp
   power exponen
   power exponential

   power log
   power logrank

   ciwidth
   ciwidth onemean
   ciwidth twomeans
   ciwidth pairedm
   ciwidth pairedmeans
   ciwidth onevar
   ciwidth onevariance

   
   /* end of Stata 15 additions */   
   /* end of the [PSS] manual */

   // start of [RPT] manual (manual is new in Stata 16)
   // in order of manual, not overviews

   docx2pdf

   /* dyndoc tags; new in Stata 15 */
   <<dd_version:1>> // obsolete in Stata 16; change color?
   <<dd_version:2>>

   <<dd_do>>
   <</dd_do>>
   <<dd_do: qui quietly nocom nocommand noout nooutput noprom noprompt >> 
   <</dd_do>>
   <<dd_do: nocommand >> 
   <</dd_do>>


   <<dd_di:>>
   <<dd_dis:>>
   <<dd_disp:>>
   <<dd_displ: %3.2f 2.3>>
   <<dd_display:>>
   <<dd_display: %4.3f 1.23>>
   <<dd_doc:>>
   <<dd_docx_display:>>

   // for dd_graph, there are too many complicated options to bother highlighting
   <<dd_graph>> // should fail
   <<dd_graph: fooey>> // bad: fooey should not highlight
   // cannot figure out why trailing >> does not highlight, as regexp is
   //   just like that for dd_do 
   <<dd_graph: rep >>
   <<dd_gr: rep >>
   <<dd_gr: replace >>
   <<dd_gra: svg >>
   <<dd_grap: png >>
   <<dd_graph: png pdf eps ps html markd markdown >>
   <<dd_graph: replace >>
   <<dd_graph:  sav() saving() alt() pdf height() gr() graphname() alt() >>
   <<dd_graph: h() height() w() width() >>

   <<dd_ignore>>
   <</dd_ignore>>
   <<dd_ign  >>
   <</dd_ign>>

   <<dd_include>> // should fail
   <<dd_include: fooey>>

   <<dd_remove>>
   <</dd_remove>>
   <<dd_remo>>
   <</dd_remov>>

   <<dd_skip_if>> // should fail
   <<dd_skip_if: fooey>>
   <<dd_skip_else>>
   <<dd_skip_end>>

   dyndoc
   
   dyntext

   html2docx

   markdown // new in Stata 15; conflicts with user-written markdown

   // putdocx new in Stata 15
   // changed to order of sections in [RPT]
   putdocx begin
   putdocx describe
   putdocx save
   putdocx clear
   putdocx append

   putdocx pagebreak
   putdocx sectionbreak

   putdocx paragraph
   putdocx text (...)
   putdocx textblock begin
   putdocx textblock append
   putdocx textblock end
   putdocx pagenumber
   putdocx textfile
   putdocx image

   putdocx table

   // putexcel new in Stata 13; syntax changes don't change ado-mode
   // not sure how putexcel is a reporting command, but still....
   putexcel
   putexcel set
   putexcel save
   putexcel describe
   putexcel clear   

   // putpdf new in Stata 15
   putpdf begin
   putpdf describe
   putpdf save
   putpdf clear

   putpdf pagebreak
   putpdf sectionbreak

   putpdf paragraph
   putpdf text (...)
   putpdf image
   
   putpdf table
   putpdf append // should fail; not legal

   // end of [RPT] manual

   /* from the [SEM] manual */
   // this is strange, because there really are just 2 commands

   estat eform

   estat eqg
   estat eqgof

   estat eqt
   estat eqtest

   estat fra
   estat framework

   estat ggof

   estat gin
   estat ginvariant

   estat gof

   estat lcgof // new in Stata 15

   estat lcmean // new in Stata 15

   estat lcprob // new in Stata 15

   estat mi
   estat mindices

   estat res
   estat residuals

   estat score
   estat scoretests

   estat sd 

   estat sta
   estat stable

   estat std :
   estat stdize :

   estat su
   estat summarize

   estat tef
   estat teffects

   gsem
   sem

   // this is not as ornate as it could be
   ssd  // incomplete
   ssd init
   ssd set
   ssd addgr
   ssd addgroup
   ssd unaddgr
   ssd unaddgroup
   ssd stat
   ssd status
   ssd build
   ssd d
   ssd describe
   ssd l
   ssd list
   ssd repair
   /* end of the [SEM] manual */

   /* start of [SP] manual, new in Stata 15 */

   estat moran

   spbalance  // only dangerous with option

   spcompress // only dangerous with option

   spdistance

   spgenerate varname

   spivregress

   estat impact

   // spmatrix omnibus entry skipped

   spmatrix copy from to // !! matrix matrix?
   spmatrix copy oops // incomplete

   // !! need subcommand matrix
   spmatrix create cont  // incomplete
   spmatrix create cont foo
   spmatrix create contiguity bar
   spmatrix create idist bling
   spmatrix create idistance blang

   spmatrix dir
   spmatrix drop mattie
   spmatrix clear

   spmatrix export foo using bling.txt

   spmatrix fromdata matname = ...

   spmatrix import foobar using ...

   spmatrix matafromsp  amatrix avector =
   spmatrix matafromsp notenough // incomplete  

   spmatrix normalize  amat

   spmatrix note  amat: this is a note
   spmatrix note amat // displays note
   spmatrix note amat: // deletes note

   spmatrix save amat using ...

   spmatrix spfrommata amat = ....

   spmatrix summarize amat

   spmatrix use amat using  ...

   spmatrix userdefined amat  =

   spregress

   spset  // dangerous with options

   spshape2dta 

   spxtregress // new in Stata 16 

   /* end of [SP] manual */

   /* from the [ST] survival analysis manual */
   ct
   ctset
   cttost

   ltable

   snapspan
   // skipping ominbus -st- section

   st_is 2 full // hmmm....
   st_is 2 analysis
   st_show
   st_ct

   stbase

   stci

   stcox
   /* stcox assumption/diagnostics */
   stphplot
   stcoxkm
   estat phtest
   
   /* stcox postestimation */
   estat con
   estat concor
   estat concordance
   stcurve
   /* end stcox postestimation */

   stcrr
   stcrre
   stcrreg

   stphtest // obsolete in Stata 9 
   stcurve

   stdescribe

   stfill

   stgen

   stintreg // new in Stata 15
   estat gofplot // new in Stata 15

   stir
   /* stpower commands (obsolete as of Stata 14) */
   // begin obsolete block
   stpow cox
   stpower cox
   stpowe exp
   stpowe exponential
   stpower log
   stpower logrank
   // end obsolete block 

   stptime

   strate
   stmh
   stmc

   streg
   /* skip over sts entry */
   sts gen
   sts generate

   sts
   sts g
   sts graph

   sts l
   sts list
   sts `foo'

   sts t
   sts test

   stset
   streset
   st

   stsplit
   stjoin

   stsum

   sttocc

   sttoct
   stvary
   /* end [ST] manual */

   /* from the [SVY] survey data manual */
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
   estat cv
   estat gof
   estat vce
   /* end estat commands */
   /* all the -svy whatever- come after the allowable estimation commands */
   /* survey commands as listed in the order of the survey estimation section */
   brr

   svy: mean
   svy: proportion
   svy: ratio
   svy: total
   svy: churdle 
   svy: churdle linear // new in Stata 14 
   svy: cnreg // obsolete in Stata 11
   svy: cnsreg
   svy: eintreg // new in Stata 15
   svy: eregress // new in Stata 15 
   svy: etregress
   svy: glm
   svy: hetregress // new in Stata 15 
   svy: intreg
   svy: nl
   svy: reg
   svy: regress
   svy: tobit
   svy: treatreg // obsolete in Stata 14
   svy: truncreg

   svy: sem
   svy: gsem   // new in Stata 14 

   svy: stcox
   svy: stintreg // new in Stata 15 
   svy: streg

   svy: biprobit
   svy: cloglog
   svy: hetprob       // technically abbrevation doesn't exits in Stata 16
   svy: hetprobit
   svy: logistic
   svy: logit
   svy: probit
   svy: scobit

   svy: clogit
   svy: cmmixlogit // new in Stata 15; renamed in Stata 16
   svy: cmxtmixlogit  // new in Stata 16

   svy: eprobit  // new in Stata 15
   svy: eoprobit // new in Stata 15
   svy: hetoprobit
   svy: mlogit
   svy: mprobit
   svy: ologit
   svy: oprobit
   svy: slogit
   svy: zioprobit // new in Stata 15

   svy: betareg // new in Stata 14 
   svy: fracreg // new in Stata 14

   svy: cpoisson   // new in Stata 14 
   svy: etpoisson  // new in Stata 14 
   svy: gnbreg
   svy: nbreg
   svy: poisson
   svy: tnbreg
   svy: tpoisson   // new in Stata 14 
   svy: zinb
   svy: zip
   svy: ztnb // obsolete in Stata 11 ?
   svy: ztb  // obsolete in Stata 11 ?

   svy: ivprobit
   svy: ivregress
   svy: ivtobit

   svy: heckman
   svy: heckoprobit
   svy: heckpoisson // new in Stata 15
   svy: heckprob    // abbreviation is no longer documented in Stata 16
   svy: heckprobit
   svy: ivreg  // obsolete in Stata 10 

   /* mlmm new in Stata 14 */
   svy: mecloglog
   svy: meglm
   svy: meintreg // new in Stata 15
   svy: melogit
   svy: menbreg
   svy: meologit
   svy: meoprobit
   svy: mepoisson
   svy: meprobit
   svy: mestreg
   svy: metobit // new in Stata 15

   // Finite mixture models new in Stata 15
   svy: fmm: betareg 
   svy: fmm: cloglog
	svy: fmm: glm
	svy: fmm: intreg
	svy: fmm: ivregress
	svy: fmm: logit
	svy: fmm: mlogit
	svy: fmm: nbreg
	svy: fmm: ologit
	svy: fmm: oprobit
	svy: fmm: pointmass
	svy: fmm: poisson
	svy: fmm: probit
	svy: fmm: regress
	svy: fmm: streg
	svy: fmm: tobit
	svy: fmm: tpoisson
	svy: fmm: truncreg
	svy: fmm: // sadly incomplete


   /* irt new in Stata 14 */
   svy: irt
   svy: irt 1pl
   svy: irt 2pl
   svy: irt 3pl
   svy: irt grm
   svy: irt nrm
   svy: irt pcm
   svy: irt rsm
   svy: irt hybrid

   svy jack: logistic
   svy jackknife: regress
   svy linear: gnbreg
   svy bootstrap: logistic
   svy brr: gnbreg
   brr: irt 1pl 
   svy sdr: regress // added in Stata 11.1
   sdr: regress // added in Stata 11.1

   svy jack: fmm: ologit // works in an odd way

   svy jack: // incomplete
   svy       // incomplete
   svy bootstrap: // incomplete
   

   svy: tab
   svy: tabul
   svy: tabulate
   
   svydes  // abbreviation undocumented in Stata 16
   svydescribe
   
   svymarkout
   svyset

   /* end of the [SVY] survey stats book */

   /* [TE] (new in Stata 13 */
   eteffects  // new in Stata 14

   estat endogenous

   etpoisson

   etregress

   /* stteffects in order of sections, not intro, leave out postest */
   /* stteffects new in Stata 14 */
   stteffects // incomplete 
   stteffects ipw

   stteffects ipwra

   stteffects ra

   stteffects wra

   /* tebalance new in Stata 14 */
   tebalance // incomplete
   tebalance box
   tebalance density
   tebalance overid
   tebalance summarize
   // here the omnibus and the sections are both in alphabetical order
   teffects // incomplete
   teffects aipw
   teffects ipw
   teffects ipwra
   teffects nnmatch
   teffects overlap
   teffects psmatch
   teffects ra


   /* [TS] time series */
   arch

   arfima
   
   estat acplot
   irf // incomplete 
   psdensity

   arima
   estat aroots

   corrgram
   ac
   pac

   cumsp

   dfactor

   dfgls

   dfuller

   estat acplot
   estat aroots
   estat sbcusum // new in Stata 15
   estat sbknown // new in Stata 14
   estat sbsingle // new in Stata 14 

   dvech  // obsolete in Stata 12 
   fcast c
   fcast com
   fcast compute

   fcast g
   fcast graph

   // forecast new in Stata 13
   // skipping omnibus forecast section
   fore // incomplete 
   forecast // incomplete 

   fore ad
   forecast adjust

   forec clear

   foreca co
   forecas coefvector

   forecast cr
   fore cre
   forecast create

   fore d
   forecast describe

   forecast dr
   forecast drop

   fore est
   forecast estimates

   forec ex
   foreca exo
   forecast exogenous

   fore id
   forec identity

   forecas l
   forecast list

   forecast q
   forecast query

   forecast s
   forecast solve

   // -haver- replaced by -import haver- in Stata 13
   // begin obsolete block 
   haver // incomplete
   haver des
   haver describe
   haver use
   // end obsolete block

   /* irf commands ... starting Stata 8.2 */
   // skipping irf section
   irf        // incomplete 
   irf graph  // incomplete 
   irf `foo'  // incomplete 

   irf a
   irf add

   irf cg
   irf cgraph

   irf cr
   irf create

   irf ct
   irf ctable

   irf d
   irf describe

   /* irf dir is dead, even under version control? */
   // begin obsolete block 
   irf di
   irf dir
   // end obsolete block

   irf drop
   irf erase // obsolete in Stata 9 ?

   irf g // incomplete
   irf g irf
   irf gr oirf
   irf gr foo // should fail; foo not legal
   irf g dm
   irf gr cirf
   irf gr coirf
   irf gra cdm
   irf grap fevd
   irf graph sirf
   irf gr sfevd

   // irf ograph could be more fancy...
   irf og
   irf ograph

   irf ren
   irf rename

   irf set

   irf t       // incomplete 
   irf table   // incomplete 
   irf t irf
   irf table oirf
   irf table dm
   irf tabl cirf
   irf tab coirf
   irf tab cdm
   irf tab fevd
   irf tab sirf
   irf tab sfevd
   irf tab foo // should fail: foo not legal

   // mgarch section skipped
   mgarch  // incomplete 
   mgarch ccc // new in Stata 12 from here...
   mgarch dcc
   mgarch dvech 
   mgarch vcc // to here....

   // mswitch new in Stata 14
   mswitch     // incomplete 
   mswitch dr
   mswitch ar
   mswitch should fail // should fail: not legal

   estat transition   // new in Stata 14 
   estat duration     // new in Stata 14 

   newey

   pergram

   pperron

   prais

   psdensity

   rolling

   // begin obsolete block (Stata 9)
   dwstat
   durbina
   bgodfrey
   archlm
   // end obsolete block

   sspace

   threshold  // new in Stata 15

   /* should these options be required--no not never */
   tsappend, add(4) last(foo) tsfmt(string)
   tsfill

   tsfilter // incomplete 

   tsfilter `foo' // should fail, good? bad?
   //  allowing subcommands
   tsfilter bk // new in Stata 12 to ...
   tsfilter bw
   tsfilter cf
   tsfilter hp // ... here

   tsline
   tw tsline
   twoway tsline
   graph twoway tsline
   tw tsrline
   twoway tsrline

   tsreport
   tsrevar
   tsset

   tssmooth   // incomplete 
   tssmooth `foo'  // should fail good? bad?
   tssmooth breeble // should fail; breeble no good

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

   ucm // new in Stata 12
   estat period
   psdensity

   var
   /* var post estimation commands */
   /* these seem to be common to var and svar */
   fcast compute
   fcast graph
   irf  // incomplete 
   // could also be below varbasic below
   vargranger
   varlmar
   varnorm
   varsoc
   varstable
   varwle
   /* end var post estimation commands */

   svar

   varbasic

   varfcast  // obsolete as of July 23, 2004
   varirf    // obsolete as of July 23, 2004

   vargranger

   varlmar

   varnorm

   varsoc

   varstable

   varwle

   vec
   /* vec post estimation commands */
   veclmar
   vecnorm
   vecstable
   // end vec postestimation commands
   // now they show up again explicitly with an extra
   veclmar
   vecnorm
   vecrank
   vecstable
   
   wntestb

   wntestq

   xcorr
   /* end time-series [TS] */

   /* stuff from the crossectional timeseries book */
   /* now called the [XT] longitudinal/panel data book */

   // begin obsolete block (Stata 10)
   iis
   tis
   // end obsolete block

   quadchk

   xtabond

   estat abond
   estat sargan

   xtclog // obsolete as of Stata 7

   xtcloglog

   // new in Stata 15
   xtcointtest // incomplete 
   xtcointtest kao
   xtcointtest pedroni
   xtcointtest westerlund
   xtcointtest oops // should fail: illegal

   xtdata

   xtdes
   xtdescribe

   xtdpd

   xtdpdsys

   xteintreg   // new in Stata 16

   xteoprobit  // new in Stata 16

   xteprobit   // new in Stata 16

   xteregress  // new in Stata 15 

   xtfrontier

   xtgee
   estat wcorrelation

   xtcorr  // obsolete in Stata 9 
   xtgls

   xthaus // obsolete in Stata 9

   xtheckman   // new in Stata 16
   
   xthtaylor

   xtintreg

   xtivreg

   xtline

   xtlogit

   // begin obsolete block as of Stata 13
   xtmelogit
   xtmepoisson
   xtmixed
   // end obsolete block

   xtnbreg

   xtologit

   xtoprobit

   xtpcse

   xtpois // obsolete as of Stata 7

   xtpoisson

   xtprobit

   xtrc

   xtreg

   xttest0  // postestimation command

   xtregar

   xtset

   xtstreg // new in Stata 14
   stcurve

   xtsum

   xttab
   xttrans

   xttobit

   xtunitroot  // incomplete 
   xtunitroot llc   
   xtunitroot ht
   xtunitroot breitung
   xtunitroot ips
   xtunitroot fisher
   xtunitroot hadri

   /* end stuff from [XT] */



   /* commands related to cscripts */
   assert
   confirm

   cscript_log
   cscript_log begin
   cscript_log end

   lrecomp

   mkassert
   mkassert bogus
   mkassert r
   mkassert rclass
   mkassert e
   mkassert eclass
   mkassert scl
   mkassert sclass
   mkassert m
   mkassert matrix
   mkassert sca
   mkassert scalar
   mkassert c
   mkassert char
   mkassert f           // new in Stata 16 
   mkassert format
   mkassert o
   mkassert obs

   rcof

   // -save- is a misnomer; should be -store-
   storedresults save
   storedresults comp
   storedresults compare
   storedresults drop

   /* now for some things which give trouble... */

   /* macros showing up inside other constructions */
   // begin ignore block
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
   here is - somethin regress

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
   stuff where ado-mode does not recognize its commentness (Needs a fix)
   the end of multi line comment
*/

   /* this is a multiliner
   which keeps going
   */

*## graph export foo.wmv

   ddd
   // this starts with a c++ comment

   /* new stuff from the 8.2 July 23, 2004 upgrade */
   vec foo
   veclmar foo
   vecnorm
   vecrank bar
   vecstable

   /* odds and ends */
   _merge
   _n
   _N
   _pi
   _rc
   // bad things
   foo_n
   _n1
   // ooooold obsolete stuff
   _result(10)
   _result(1)
   jknife
   parse
   whelp
   window menu popout
   local bleen : set graphics

   // more odds and ends

   `e(V)'
   `r(sum)'
   `s(foo)'


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
*/ +1 // ending in another comment
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

   // for testing ado-highlight-block

   foreach foo in this that {
      display "foo is `foo'" // silly display with end comment
      twoway bleen ///
        `foo', legend( /// comment (within continuation) with parens
        order(1 "confusing") ///
        ) // hmm
      {
         display "bleen"
         display "something for testign blocks"
         }
      }
   < these won't balance (not braces) >
   [ these will balance (square brackets) ]
   {
      foreach oops of local mistake {
         (here ///
           is something in parens) ///
           which continues
         (here are mismatched braces]
         display "Oh no, no closing brace"
         } // delete to test
      quietly(this is a test) // should distiguish between functions and commands
      }
   // a command from the outside world
   slog
   slog(this is bad) // should fail

   // for testing commands needing a subcommand
#delimit

   display "{hline}"

   file read handle `macname'


   // annoying local foo = strpos(`gib', "`gab'")

   mat `foo' = (1 2 \ 3 4)

   http://stata.com // this worked fine

   quietly label variable
   capture noisily foreach bleen of loc hooie {
      }
   capture n foreach bleen of loc hooie {
      }
   capture n regress

   import excel

   // unicode experiments

   local føøbar

   local ∑∑∑ "should work"
   local 8∑∑ "should also work"
   global 8fjel "does not work"
   global w∑∑ "should work"
   matrix ∑∑∑ = should not work
   matrix e∑∑ = should work


   The command smcl2do is user-written, so it will not highlight if you do not have
   it installed, but is will highlight otherwise (no easy way to fix efficiently)
   // end ignore block

   // Buggy behavior with nested smcl 
   {sf:this is some text}
   {res:{sf:fooey}}

   // messy; 
   {synopt:{opt min:abbrev}}


   // rejoin these lines to test sloooooowness from many regexps on one line
   {sf:hi}{sf:hi}{sf:hi}{sf:hi}{sf:hi}{sf:hi}{sf:hi}
   {sf:hi}{sf:hi}{sf:hi}{sf:hi}{sf:hi}{sf:hi}{sf:hi}



end
