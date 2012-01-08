{smcl}
{* July 11, 2010 @ 21:43:16}{...}
{hi:help syntax_tester} 
{hline}

{title:Title}

{phang}
{cmd:syntax_tester} doesn't do anything. This help file is solely for testing
the behavior of ado-mode when saving help files.
{p_end}

{title:Syntax}
{* put the syntax in what follows. Don't forget to use [ ] around optional items}
{p 8 17 2}
   {cmd: syntax_tester}
   {varlist}
   {cmd:=}{it}{help exp}{sf}
   {ifin}
   {weight}
   {help using}
   {cmd:,} [
   {opt min:abbrev}
   {opt min:abbrev(arg)}
   ]
{p_end}

{title:Description}

{pstd}
put the purpose here
{p_end}

{title:Options}

{synopt:    {it:this}}that

{synopt:{opt abbrev:iated}}here is some long piece of text after the abbreviated option name

{phang}{opt whatever} does yak yak
{p_end}

{title:Remarks}

{pstd}
One or two lines... pretty rare
{p_end}

{title:Example(s)}

{phang}{cmd:. some sample input here}{break}
explanation here
{p_end}

{title:Notes}

{pstd}
Some careful notes go here...like remarks but after examples.
This is not part of standard Stata help
{p_end}

{title:References}

{pstd}
Refs go here

{title:Also see}

{psee}
Manual: {hi:[x] Put some stuff here, too.}
{p_end}

{psee}
Online: help for {help smcl}
{p_end}

{psee}
Web:   {browse "http://stata.com":Stata's Home}
{p_end}

{title:Author}

{pstd}
Bill Rising, StataCorp{break}
email: brising@stata.com{break}
web: {browse "http://homepage.mac.com/brising":http://homepage.mac.com/brising}
{p_end}
