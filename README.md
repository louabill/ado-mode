# The ado-mode for Emacs

The `ado-mode` is a major [editing] mode for Emacs which allows truly good editing of Stata, namely `do`, `ado`, `sthlp`, `smcl`, and `mata` files. Here is a [screenshot](ado_highlighting.png) to see what the highlighting does. The mode also handles indentation, date stamps, and includes templates for writing help files, project do-files and the like.

Emacs versions 25, 24, and 23 are supported.

Here is the [change log](changes.md) which says what is new.

The current version is **1.15.1.0**, which is made for Stata 15.1. It works just fine with earlier versions of Stata, but it will highlight commands as though you were using Stata 15.1.

Here are the things which make life easier when using the `ado-mode`:

  * `ado-mode` can communicate well with Stata itself, sending code directly to Stata for evaluation. This works for both OS X and MS Windows. It does not yet work for any non-OS-X unix. For an explanation of the settings (along with hints for how to port it), please [look here](send2stata.html).
  * Ability to open any file on the default ado-path, even automagically opening the ado-file for the current command.
  * Context-sensitive highlighting keeps typos to a minimum. Stata's commands and keywords are highlighted if abbreviated correctly.
  * Commands which need subcommands are highlighted differently, so that you know a subcommand is needed.
  * Downloaded and personal user-written commands can be automatically highlighted once Emacs knows where they are kept.
  * Templates for common files, such as do-files which keep their own logs, ado-files, do-files made for testing and debugging ado-files, and (most importantly, perhaps) Stata help files.
  * Automatic indentation so that long `if`, `foreach`, `forvalues`, and `while` blocks get closed. Many types of indentation styles are supported.
  * Along the same lines, you can open Stata help files (in Stata, of course) from Emacs in OS X and MS Windows. 
  * The name of the ado file always matches the name of the program defined, even if you change the program's name.
  * Ado-files get a time stamp which can be formatted in a variety of ways.
  * Stata's `smcl` directives are correctly highlighted, which saves time when writing help files.
  * Parentheses, braces and double-quotes are balanced.

Installation is not as simple as it could be, so there (will be) installation instructions on the wiki. For now, the [installation instructions](http://louabill.org/Stata/ado-mode_install.html) are back on the old webpage.)

**Please** let me know if there are any troubles with the installations, since there are little quirks with cross-platform Emacs stuff.

`ado-mode` is on Twitter: @ado-mode. The real reason for this is to have an easy RSS feed for finding out about updates: [https://twitter.com/adomode](https://twitter.com/adomode)
