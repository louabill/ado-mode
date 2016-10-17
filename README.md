# The ado-mode for Emacs</a></h2>

The `ado-mode` is a major [editing] mode for Emacs which allows truly good editing of Stata, namely `do`, `ado`, `sthlp`, `smcl`, and `mata` files. Here is a <a href="ado_highlighting.png">screenshot</a> to see what the highlighting does. The mode also handles indentation, date stamps, and includes templates for writing help files, project do-files and the like.

Emacs versions 25, 24, and 23 are supported.

Here is the <a href="changes.txt">change log</a> which says what is new.

The current version is <strong>1.14.2.0</strong>, which is made for Stata 14.2. It works just fine with earlier versions of Stata, but it will highlight commands as though you were using Stata 14.2.

Here are the things which make life easier when using the `ado-mode`:

	* `ado-mode` can communicate well with Stata itself, sending code directly to Stata for evaluation. This works for both Mac OS X and MS Windows. It does not yet work for any non-Mac-OS-X unix. For an explanation of the settings (along with hints for how to port it), please <a href="send2stata.html">look here</a>.</li>
	* Ability to open any file on the default ado-path, even automagically opening the ado-file for the current command.</li>
	* Context-sensitive highlighting keeps typos to a minimum. Stata's commands and keywords are highlighted if abbreviated correctly.</li>
	* Commands which need subcommands are highlighted differently, so that you know a subcommand is needed.</li>
	* Downloaded and personal user-written commands can be automatically highlighted once Emacs knows where they are kept.</li>
	* Templates for common files, such as do-files which keep their own logs, ado-files, do-files made for testing and debugging ado-files, and (most importantly, perhaps) Stata help files.</li>
	* Automatic indentation so that long `if`, `foreach`, `forvalues`, and `while` blocks get closed. Many types of indentation styles are supported.</li>
	* Along the same lines, you can open Stata help files (in Stata, of course) from Emacs in Mac OS X and MS Windows.</li> 
	* The name of the ado file always matches the name of the program defined, even if you change the program's name.</li>
	* Ado-files get a time stamp which can be formatted in a variety of ways.</li>
	* Stata's `smcl` directives are correctly highlighted, which saves time when writing help files.</li>
	* Parentheses, braces and double-quotes are balanced.</li>

Installation is not as simple as it could be, so there (will be) installation instructions on the wiki. (!! fixme: Anyways, here are the [installation instructions, for now: ("http://louabill.org/Stata/ado-mode_install.html").)

**Please** let me know if there are any troubles with the installations, since there are little quirks with cross-platform Emacs stuff.

`ado-mode` is on Twitter: @ado-mode. The real reason for this is to have an easy RSS feed for finding out about updates: [http://twitter.com/statuses/user_timeline/106578815.rss]("http://twitter.com/statuses/user_timeline/106578815.rss")http://twitter.com/statuses/user_timeline/106578815.rss
