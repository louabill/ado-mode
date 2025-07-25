# Version History

## 1.19.5.0

### Big changes

* All the Stata 19 additions

* Included commands from <https://www.stata.com/h2o/h2o19/> for H2O
  integration, because the pdf docs seem to be missing a lot

### Smaller changes

* Fixed some incorrect highlighting, mostly for Stata 18 subcommands
  I missed
  
* Updated docs to reflect that ado-mode has required Emacs 25.1 (something
  true since May 9, 2021)

## 1.18.5.1

* Tried to fix up the AppleScript for sending code from Emacs to Stata
under macOS. The security permissions in macOS can sometimes get messed
up if a new Emacs gets installed. Please file bug a bug report if you
have problems. 

* Added a command for cleaning up when sending code on the
Mac cannot seem to work:
    ado-reset-tcc
This seems to work in Catalina and later, but not in Mojave.

* Added commands added to 18.5 through 13nov2024

* Added manual abbrevs forgotten for 2 versions

* Other minor typo and spaced-out fixes

## 1.18.5.0

* Fixed the autoit and send2ztata.sh scripts so that sending code
to Stata from StataNow works in Windows and Unix (sorry for the
long delay in the fix)

* Added new commands and Mata functions which were introduced into
Stata either via StataNow 18.5 or in updates to Stata 18

* Got stuck with version number for ado-mode, since the versions
had been based on Stata's version, but now there are two current
versions ... so I picked the higher numbered version

## 1.18.0.0

### Big Changes

* Added lots of new commands from Stata 18 (finally)

### Other Changes

* Bettered -foreach- highlighting of varlists. Not perfect, but better.

* Fixed -ml model- to highlight methods properly

* Fixed embarrassingly old -bayesgraph- highlighting errors

* Fixed othe minor highlighting failures

## 1.17.0.2

* Added `set logmsg` from 04oct2022 update.

* Added StataBE. Eek, this is late. Anyways, thanks to Marc Klemp for
pointing out this glaring omission.

## 1.17.0.1

* Added `collect composite` commands from 06apr2022 update.

* Fixed bug where sending command to Stata while within the slashes in a
continuation comment would send the wrong command 
(by fixing `ado-end-of-command`).

* Updated the applescript to do a better job notifying the user when macOS 
permissions need changing.

* Changed the default comment from `/* */` to //. Makes for much nicer
behavior of `M-;`. Also added `ado-comment-start` and `ado-comment-end`,
so you can change these back to `/*` and `*/` if you like the old behavior better.

* Reversed roles of `ado-new-ado` and `ado-new-program`. In the past, the 
former was an alias to the latter. Now it is the other way around. Merely
a pedantic change.

## 1.17.0.0

### Big Changes

* Updated for Stata 17 with the usual adding and deprecating Stata commands.

* There are also many large changes to make installation easier.

	* Commands/ados from sysdirs (`PERSONAL`, `PLUS`, `SITE`, and `OLDPLACE`) now get
  fontified automatically, so there is no longer a need to write your own
  `ado-mode-hook` to do this. The `ado-mode` functions for adding and removing
  font-lock keywords were overhauled to do this more efficiently.

	* Added `ado-add-sysdir-font-lock` flag for auto-loading commands from
  sysdirs.This defaults to `t`, but if you do not want to fontify these
  commands, you can set it to `nil`.

	* Made the so-called `new` directory default to `PERSONAL`. If you have it
  customized already, nothing will change. New users no longer need to
  specify it.

* Reorganized the customization groupings, because of simplifying the
  installation instructions. The most-needed settings are now under `ado-main`
                        . 
* Added `ado-font-lock-refresh` to refresh font locking without having to reload
`ado-mode`. Finally.

### Other Feature Changes

* Added overlooked Stata/IC to script for sending to Stata in unix (thanks to
  Mark Clements)

* Added Stata/BE to keep up with Stata renaming Stata/IC to Stata/BE. For
  them that like Stata history: Stata/IC was once the fancy-pants version of
  Stata (as 'Intercooled' is a kind of turbocharger).

### Bug Fixes (not inclusive)

* Fixed the bug which would error out when trying to grab a block when
  outside of a block. 

### Note

* This is the last update which will include the *.elc files in the zipped
  package. This is being done because it is really a bad practice to include
  them in a repository, as they can be generated from the contents of the
  repo. If you use MELPA, Emacs will create the *.elc files when you grab the
  package. If you are installing by hand, you'll have to do the compilations
  on your own. 

## 1.16.1.6

No feature changes. Fixes for MELPA and for a dumb bug introduced in the last
batch of MELPA changes.

## 1.16.1.5

Again, no feature changes. Did make changes for submitting code to Stata
which should not affect behavior.

Bumped version number for MELPA versioning and testing.

## 1.16.1.4

No feature changes. Just taking care of programming conventions for MELPA.

Needed to bump the number for versioning on MELPA.

## 1.16.1.3

#### Feature changes

* Fixed highlighting problems with local macro names caused by sloppy code
  for allowing emojis in 1.16.1.1.

* Made `\` an escape character, again, so that it is possible to escape
  double-quotes. It had been made a regular character long ago for a
  now-forgotten reason.

* Added `ado-clean-buffer` for turning lines with just whitespace into empty
  lines.

* Changed indenting to use extra indentation for continuations inside of
  functions (specifically for extended `cond()`s.

#### Backward compatibility changes(!)

* Changed default keybindings, because some violated rules found in the
  Key Binding Conventions in the Emacs Lisp manual
    `ado-help-at-point` now defaults to `C-c C-k`
	 `ado-help-command` now defaults to `C-c M-k`
  The only reasons for k is that is not yet taken and it kinda looks like an h.
  
* Changed names of three very old functions (the first ones I put into
  `ado-mode`) and one relatively new function
    `electric-ado-closing-brace` -> `ado-electric-closing-brace`
    `electric-ado-brace`         -> `ado-electric-brace`
	`electric-ado-semi`          -> `ado-electric-semi`
	`set-ado-signature-file`     -> `ado-set-ado-signature-file`
  If you ever wrote any lisp code depending on these (I sure hope not!), then
  you'll witness the first backward compatibility break for `ado-mode`.

MELPA-requested changes
  
* MELPA requested that the syntax tester files not be included in the
  official package, so I moved them into another package. If you really want
  the syntax testers, you can find them here:
  [https://github.com/louabill/ado-mode-syntax-checkers

* Many many coding changes requested by MELPA to make the code more modern.
  None of these should affect features.

* Many many changes to docstrings as requested by MELPA.

Git-repo notes

* I put the .elc files into .gitignore, and then removed them from the list
  of tracked files. (Repos really shouldn't have easily-reproduced files in
  them, as it bloats the history.) The .elc files were originally included so
  that people could unzip a file and have ado-mode work quickly without the
  added instruction to byte-compile the lisp directory. MELPA byte-compiles
  files when doing installations, so the .elc files have become redundant.
  NOTE: if you pulled from this repo in the past, from now on, you'll need to
  byte-compile the lisp directory after pulling:
    M-x d \<path_to_ado-mode\>
	 M-x byte-recompile-directory
  will do the trick.

* About moving the syntax checkers: I did this in the most primitive way
  possible, to help with backwards compatibility: I made a new repo, moved
  the files in the syntax_testers there, and then deleted the syntax_testers
  directory from this repo. I did _not_ go back and alter the git commits.
  The syntax testers were there on purpose, not by mistake, but when using a
  package manager like MELPA, they are out of place. If you want to try your
  hand at changing ado-mode, you'll want the syntax testers, because they'll
  give you feedback about constructions that may or may not highlight
  properly. Stata is not a regular/rational language, so highlighting will
  always have some problems. 

## 1.16.1.2

* No feature changes (I hope). Just trying to make Melpa-ready.

## 1.16.1.1

* Fixed bug where sending code to Stata via a do-file (or include file) on
  the Mac would mess up any multibyte unicode characters (i.e. anything other
  than unaccented Latin letters).

* Fixed highlighting of names containing emojis. Too bad variable names in
  Stata cannot start with emojis, though.

## 1.16.1.0

* Added new -set- subcommands

* Multiple fixes to incomplete and otherwise-missed commands

* Minor fixes to -estat- subcommands

## 1.16.0.0

* Finally got all the changes for Stata 16 added.

* Added some missing command abbreviations.

## 1.15.1.6

* Tried fixing mysterious 'Not enough arguments for format string' error
  message which could pop up when there were %-symbols in commands being
  sent to Stata.

## 1.15.1.5

* Made ado-mode better at guessing names of help files.

* Added undocumented commands associated with -cscript- (see -help cscript-).

* Added menu items for opening help and source files.

* Small syntax highlighting fixes

* Put in embarrassing omission of 4 erm commands

* Cleaned up syntax_tester.ado to make it easier to pull all possible
   Stata commands from it.

* Replaced some emacs-obsolete variables with their newer versions,
  meaning that ado-mode now requires Emacs 24.1 or later.

* Updated header info to be more modern in all files.

* Had to change some function names and arguments to make things more
  compliant with emac usage:
  (ado-delimit-is-semi) was changed to (ado-delimit-is-semi-p)
  (ado-find-ado-dirs) changed to require the subdir instead of defaulting
    to an obscure directory on my computer (oops)


## 1.15.1.4

* Unicode names finally work.

* Updated syntax highlighting to try and be smarter about highlighting
  Stata keywords appearing in the middle of text. This needed to be
  fixed because of the nice new Stata commands which allow mixing
  narrative and Stata. The fix cannot be perfect because Stata is not a
  regular language, so it is hard to detect what any piece of a code
  line means. There are some known shortcomings. 

**Continued lines highlight as a series of independent lines.

** Items which occur mid-line (such as returned values or smcl) still highlight, but that is their nature.

** Mata reserved words and keywords highlight, because they don't follow Stata syntax.

** The first word after a colon (:) will be considered a possible command in most cases, simply because checking for legal prefix commands is nigh-on impossible. This being said, commands for which a prefix command _could_ be legal, but is likely silly will not highlight after a colon.

** All added unofficial commands are assumed to allow prefix commands (cannot be changed).

* Fixed highlighting of local macros which start with numbers.

** Commands like -foreach whatever of global/local- now highlight legal names for locals, but allow globals to have names just like locals (with leading 0's or unicode symbols). This won't likely get fixed, as it'll take a bit of work to make something halfways maintainable. Also: globals are supposed to be exceedingly rare.

* Changed highlighting of local macros to include the opening ` and closing '. It doesn't look that great, but it makes nested local macros, such as `foo`bar'' and ``foo'bar' highlight similarly.

* Fixed some other bugs in highlighting and added highlighting for AssociativeArray methods.


## 1.15.1.3

* Added a submenu to the Ado-mode > Options menu for changing values for
  interacting with Stata (version, flavor, instance, etc.) The instance
  is useful only in MS-Windows.

## 1.15.1.2

* Took out some debugging code which caused extra messages in Windows

* Updated copyright dates in files which had been changed some time recently

## 1.15.1.1

* Fixed a bug for sending to Stata in Windows

* Made sure -version 15.1- highlighed properly (duh)

## 1.15.1.0

* Thanks to a good tip from Helge Liebert to point things in the right
  direction, it is now possible in linux to communicate with Stata
  directly from Emacs! This requires xsel and xdotool, which can be
  downloaded via
     `sudo apt-get install xsel xdotool`
  in Ubuntu/Debian and
     `sudo pacman -S xsel xdotool`
  in Arch

* Highlighting user-written commands found on the ado-path now works in
  linux, also

* Added the one new command, -dataex-

* [Programmers only]: Changed the default behavior of the
  ado-command-to-clip function to strip leading and trailing whitespace


## 1.15.0.0

* Updated for Stata 15, finally

* Changed the keybinding for ado-help-command from C-c C-c to C-c M-h.
  This was done out of confusion that newer Emacs (>= 25.2) bound
  another function to C-c C-c. This was false, but I left the change in
  because it is more consistent with other bindings. After all, C-c C-h
  is for ado-help-at-point, C-c M-h is a generalization.

* Fixed -tssmooth- to display as harmful face

* Fixed highlighting of -manlink-, -manlinki-, -manpage-, and
  -mansection- to only accept proper abbreviations for manual names
  (like R or P or SVY).

* Fixed a bug in indentation of abbreviations of #delimit

* Fixed a bug in creating help file filenames.

## 1.14.2.0

* Thanks to [Leo](https://www.emacswiki.org/emacs/halloleo), for a fix
  to allow ado-send-to-stata to allow spaces in file names. Though
  spaces in do-files, ado-files and the like are not legal in Stata,
  this can still be worthwhile.

* Added -icd10 search- (in reality, this had been added earlier,
  incorrectly, in ado-mode 1.14.0.0)

* Updated version to accept 14.2

## 1.14.1.0

* Updated -ci- highlighting for new subcommands of -ci-.

* Fixed up sending to Stata for Windows. It seems that the text has to
  be translated into utf16 for the pasting to work properly. If you have
  trouble with this, try setting emacs to use utf8 as its encoding.

* Added a new template called testado.blp. It makes a dofile which
  -includes- the ado file of the same name within a self-logging do-file
  so that debugging goes faster.   

* Updated the saving of new programs (ado-files) to ask about saving to
  ado-personal-dir if ado-new-dir doesn't exist (and ado-personal-dir
  does). The whole concept of the 'new' directory was a leftover from
  when Stata first introduced the adopath but didn't separate downloads
  from personal programs. (The last remnant of this in official Stata is
  in c(sysdir_oldplace)).

* Made the embarrassing fix to the default location of the windows
  executable. By default, ado-mode assumes that you installed a 64-bit
  version in Windows.

* Changed the templates so that if you tell emacs where Stata lives by
  setting ado-stata-home, the templates will have the version of your
  Stata. No more updating templates for each version of Stata.

* Changed the way that templates work, so that it is easier to have more
  complex templates. If you used the ado-new-generic function to create
  your own commands for templates, those commands might break. Look at
  the readme in the templates directory for more information.

## 1.14.0.0

* Updated for Stata 14 syntax

* Had to change script for sending text to Stata for Windows to use
  method equivalent to copy and paste (This happened because of the
  change of the control type for the Command window to a type which
  doesn't let its text get set.)

* Updated templates for .ado, .do, and .mata files to use -version 14-
  commands

## 1.13.1.1

* Fixed an embarrassing omission in the customization group for ado-mode

* Fixed a small bug with version numbers in non-ado, non-class, non-do
  files

* Fixed a problem introduced by the cmd-S saving in 1.13.1.0 which
  allowed a newly created file to overwrite an existing file without
  warning.

* Small fixes to templates.

## 1.13.1.0

* Updated for Stata 13.1

* Finally fixed up saving files so that standard save-file methods (e.g.
  cmd-S in Aquamacs Emacs, File > Save File in all OS's) update
  timestamps and keep backup copies. 

* Fixed highlighting bug for // comments starting at the beginning of a
  line

* Fixed indenting bug for -mata- groups (things were fine for -mata:-
  groups). 

* Changed help.blp to say 'stored results' instead of 'saved results'.

* Now am getting warning about syntax-propertize replacing
  font-lock-syntactic-keywords for emacs 24.1 and
  later. I'm not going to worry about this, yet---it is used for
  #delimit and #review.

## 1.13.0.0

* Updated for Stata 13
 
* Fixed highlighting bugs for -irf- and its subcommands

## 1.12.1.1: (never released)

* updated templates to 'version 12.1'

* noticed that in Stata 12 for MS Windows, if you open the filter on the
  review window, AutoIt changes the number of the Command window, so
  that attempting to send commands to Stata sends the commands to the
  filter instead. To fix the problem, move the Command window to the top
  of the screen. There is no possible workaround for the problem, as it
  is a bug in AutoIt.

* fixed a bug where the automatic naming of help files got fooled by
  help files for topics with spaces in their names (e.g. regress
  postestimation).

* changed the way that // comments are recognized so that url's don't
  get highlighted as comments

* minor highlighting and behavior bug fixes

## 1.12.1.0

* fixed a bug introduced by the Emacs 24 change to the
  filter-buffer-substring command

* Added ado-statacorp-defaults which sets indentations, timestamps and
  the like to match StataCorp's style for editing.

* Re-fixed the bugs related to semi-colon delimited files. Maybe this
  time the fix will be real.

* Now replace multiple tabs/spaces with a single space when sending a
  command to the clipboard for consumption by the Command window.
  Necessary because the command window does not understand tab
  characters.

* Fixed minor fontification bugs.

## 1.12.0.2: not released to general public

## 1.12.0.1

* Added two new commands for opening any file on the adopath (think:
  editing in place of -viewsource-): ado-open-command, which tries to
  identify the current command and open its ado-file, and
  ado-open-any-file which will open any file. By default, these files
  are opened in read-only mode, to protect against accidental editing.
  This behavior can be customized as usual, and can be set temporarily
  in the Ado-mode > Options menu.

* Added/Changed some keybindings:
  C-c C-o and C-c C-f allow you to open any file on your default adopath.
  C-c M-o and C-c M-f allow you to open the current command.
  C-c C-e is the new binding for inserting a -foreach- loop.

* ado-stata-home is now set by default to be the typical installation
  directory for Stata on the three platforms to:
    Mac OS X: /Applications/Stata/
    Windows 7/Vista/XP: c:/Program Files/Stata12
    Unix: /usr/local/stata12

* Added commands for (re)setting the named parts of the adopath to their
  values at the start of a Stata session: ado-reset-personal-dir,
  ado-reset-plus-dir, etc.

* Fixed a bug where submitting the whole buffer would submit only the
  current command if the buffer was dirty.

* Changed how ado-mode decides on file types, because of changes to how
  .sthlp files are made in Stata 12. This should also help in ado-files
  with long sets of *! comments at the top.

* Semi-fixed bug after #delimit ; so blocks of commands are sent to Stata
  properly, as version 1.12.0.2 notes. 

* Fixed minor fontification (highlighting) bugs.

## 1.12.0.0

* Updated for Stata 12

* Added different highlighting for commands requiring a subcommand.
  Incomplete multi-word commands should now be clearer. This is likely
  to be a little incomplete, so please send reports to me about it. 

* Added a new function (ado-input-to-stata) which asks for a command in
  the minibuffer, and then sends the command to Stata. This is bound to
  C-c C-t by default. 

* Changed indenting so that continuations inside of parens or braces
  take the parens or braces into account. This (should) be an
  improvement. 

* Changed the way to name .sthlp files, so that the name can be found
  both for Stata 12 as well as pre-Stata 12 help files. This was needed
  because of the new quick access tags at the start of Stata 12
  helpfiles. 

* Updated the help.blp template greatly, so that it contained new smcl
  tags and also contained sections useful for user-written commands.
  Thus, it now differs from the template in the Users Guide. 

* Included a better way of pasting to the Command window under MS
  Windows, thanks to Daniel Green passing on some code from Jeffery
  Arnold and Matthew Botsch. Should be much faster now, especially when
  pasting large blocks of code. 

* Changed formatting in this version history to make it more readable.

## 1.11.2.1

* Fixed bug for sending via do-file on the Mac, where the working
  directory would be changed to the users tmp directory

* Added a new way of sending commands to Stata (as suggested by David
  Lucca): "include". This option is the same as sending via a do-file,
  except that the command sent is -include ...-. The advantage of this
  is that local macros defined earlier will still be defined, allowing
  incremental work inside larger do-files. Thanks, David, for a Very
  Good Idea.

* Stopped adding an extra \<eol\> when working via the do-file editor or
  includes, because this should be handled by the script passing the
  clipboard to Stata.

## 1.11.2.0

* Updated to allow -version 11.2-

* Fixed bug in AutoIt script for Windows where adomode would get
  confused if there was a Graph window open (or even any web browser
  pointed at Stata's website). AutoIt now uses a regexp to find either
  "Small Stata" or "Stata/(IC|SE|MP)". This should be good unless you
  are visiting a site like http://www.stata.com/statamp while using
  ado-mode.  (Checking the window class for a Stata window doesn't help,
  so this will be as good as it gets.)

* Put in better error-checking for a bad value in ado-script-dir. 

* Fixed up code for sending to Stata under Windows so that spaces are
  allowed in filenames. I think.

* Tried to fix highlighting of commands when followed by a special
  character like a paren, because 'generate' should highlight, but
  'generate(' should not.

* Fixed minor highlighting bugs.

* Cleaned up font highlighting of added commands.

* Anyone finding bugs, please report. Passing code to Windows is
  complicated because there can be no passing of the clipboard AND
  waiting to see if there was an error in passing code, so debugging is
  really hard for me, being a non-Windows user. 

## 1.11.1.2

* BEHAVIOR CHANGE: if you have a .ado-signature file, see the last 
  item.

* Added support for sending code to Stata when there are multiple
  instances of Stata running (MS Windows only). How the code is sent is
  ruled by 5 options:
  * ado-send-to-all-flag: if this is t, then all instances get the sent
      code.
  *  ado-stata-version, ado-stata-flavor, and ado-stata-instance:

When these filters are specified, emacs will look at all running
instances and count the number of these that are matched (note
that the default values will match any Stata). If
ado-strict-match-flag is nil (the default), *all* Statas which tie
for most matches are considered candidates for the code. (This is
done so that when all are at their default values, they will
always match any running Stata.) If ado-strict-match-flag is t,
then only those which match all 3 filters are candidates.

Some examples:

For each of the following, suppose that there are two instances of
StataMP 11.1, one instance of StataSE 10.1 running, and one
instance of StataMP 10.1 running.

If no filters are set, the ado-strict-match-flag is nil, there will be 4
matching Statas. If the ado-send-to-all-flag is false, you will get an
error, if the ado-send-to-all flag is true, all 4 instances will get the
same command.  If ado-strict-match-flag is t, there will also be 4
matching Statas. 

If ado-stata-flavor is "SE", and the other filters are unset, there
will be 1 matching Stata, regardless of the state of the
ado-strict-match-flag.

If ado-stata-version is 12, and ado-stata-flavor is "MP", there will
be 0 matching Statas if the ado-strict-match-flag is t (because no
Stata is Stata 12), but there will be 3 matching Statas if
ado-strict-matching is nil, because the 3 StataMP instances will
all match 2 out of 3 filters (the "MP" filter and the unset
filter).

* Added new functions for working with blocks of codes:
  ado-balance-braces will balance {[()]} outside of strings and
  comments. Used multiple times, it will keep expanding matching pairs.
  It is bound to C-c C-b.
  While this is nice, ado-grab-block is nicer. It will balance only {},
  which is useful for grabbing blocks of commands. Because this will
  often be used with loops, it will also grab whatever defines the loop.
  Repeated use will grab the nesting blocks.
  For both of the above, a region is selected, so the region can be sent
  to Stata in the usual fashion.
  If you are sure that you want to submit the current narrowest block,
  you can use ado-send-block-to-stata, which is simply an ado-grab-block
  followd by an ado-send-command-to-stata. This is bound to C-c M-b.
  One warning: in this initial version, emacs' default behavior is
  followed, where different braces can match if there are unbalanced
  braces. Thus {these DO balance].

* Added menu item Ado > New > Do-file

* Updated help-file template to match the not-so-new way that Stata
  likes its help files, using the synopt table for options before the
  Description section.

* Changed the authorship routine so it is easier to not have any author
  (good for me working at Stata). There is a new flag
  ado-help-author-flag---if it is nil, the Author section will be
  deleted, otherwise the Author section will be included. The behavior
  of ado-signature-prompt-flag now makes more sense; if nil, the user
  will be prompted to give a signature file only if the signature file
  is missing. This will affect approximately 0 people.

* If you have a .ado-signature file for filling in the authorship
  section of a sthlp file, you might need to change it. It should now
  just have your info, without any paragraph-mode items. 
  So... the following is good:
    Bill Rising, StataCorp{break}
    email: brising@stata.com{break}
    web: {browse "http://homepage.mac.com/brising":http://homepage.mac.com/brising}
  This will affect at least one person (me).

## 1.11.1.1

* Added fontification for ado files in Stata's -sysdir- locations. So,
  for example, if you download the command -renvars-, -renvars- will now
  use have syntax highlighting in emacs. For this to work, you *must*
  run M-x customize-group RET ado-path to tell emacs what directories
  house your customizations. Look at the personal_scrap.el file for more
  help on doing this automatically. It's magical. Really. Since this is
  so magical, its features might change based on feedback. 

* Added new functions ado-next-error and ado-prev-error, which search
  for something indicating an error (in a text file, a non-zero return
  code, in a smcl file an {err} tag). If tracing is on, this will jump
  to the line creating the error rather than the tag or error code
  itself.

* Fixed an embarrassing bug for using the comeback flag in windows. It
  looks like I didn't test it much originally, if at all.

## 1.11.1.0

* Updated for Stata 11.1

* Big internal change: got rid of make-regexps in favor of regexp-opt,
  so that all called functions were part of standard emacs. Though
  make-regexps was nice to use, it was way out of date.

* Split all the endless font-locking code into its own file, so that
  others who would like to use it in their own packages will have an
  easier time. 

* Fixed the silly *Async Shell Command* buffer problem when sending code
  to Stata in MS Windows.

* Various highlighting bug fixes.

## 1.11.0.1

* Fixed a bug where sending code to Stata from MS Windows via a do-file
  failed.

## 1.11.0.0

* Added functionality for sending code to Stata in MS Windows. So...it
  is now possible to send code to Stata from both Mac OS X and MS
  Windows. The default keystroke is M-RET.
  (MS Windows note: when sending to Stata, *Async Shell Command* buffer
  will open. Since you cannot avoid this, just make the pane as small as
  possible. I cannot figure out how to get autoit's executable to run
  synchronously with emacs.)

* Added functionality for getting help for commands directly from emacs.
  C-cC-c gets help for the command, C-cC-h gets help for word at point.
  Because these work via sending commands, they are also Mac/Win only.

* Fixed highlighting for -estat classification- abbreviations

* Fixed problems with spaces in paths when using ado-send-to-stata

* Fixed some typos in the documentation of functions

* Updated for Staga 11 commands, including new commands and marking
  obsolete commands as such

* Changed the templates to default to version 11

## 1.10.1.2

* (Finally) changed the update of the time stamp to not add the old
timestamp to the kill ring.

* Fixed a bug where stripping comments failed when there were tabs as
part of the whitespace before the comment string. This could occur when
lines were split. To shut off tabs when splitting lines, set
indent-tabs-mode to nil.

## 1.10.1.1

* Added functionality for sending regions or commands to Stata, at least
  inside Mac OS X(!). The design *should* make it possible to implement the
  same behavior in MS Windows or Unix, because the code for putting the proper
  information on the clipboard is written to be platform independent. The
  methods for getting the clipboard to Stata itself are not (of course).
  All that is needed is something which will paste the
  clipboard/pasteboard into Stata's command window. Everything else is
  internal to emacs. Look inside ado-to-stata.el to see how things are
  sent via applescript on the Mac.
  If you come up with a script for unix or windows, please send it to
  me, so I can change the functionality of ado-mode to be more
  platform-agnostic (brising at mac dot com).
  If you use MS Windows or Unix, there is some consolation, because
  running M-RET will take the function containing the point (or the
  region if one is selected) and put it in the clipboard in a way the
  Commmand window can use it. All you need to do is paste into the
  Command window. 

* Along with this functionality is the ability to get help from Stata
  for the word at point. (ado-help-at-point, bound to C-c C-h, and
  ado-help-command, bound to C-c C-c)

* A key binding has changed. Meta-Return now sends the
  region/command to Stata, and Shift-Meta-Return splits a line.
  In OSes which don't work directly with Stata, Meta-Return will put 
  a version of the region or the command in which point is sitting on
  the clipboard, so it can be pasted in the command window.

* Fixed up the methods for determining the type of file and the default
  name. NOTE: for .ado or .class files, the name of the program or class
  must come in the first 200 characters of the file. Lesson: if you use a
  helper program in a do-file, be sure to wait a little before defining
  it. The 200-character cutoff is entirely arbitrary.

* Added a newer (and better) definition of `how-many' to the ado-hacks.el
  file for users who haven't yet updated to emacs 22.1.1.

* Added ado-new-ado for a new ado file. This is identical to ado-new-program. 

* Added do.blp and ado-new-do to make do-files which both log and can be nested.

* Added mata.blp and ado-new-mata to make mata definition files.

* Fixed ado-new-generic to be smarter about naming new files (like mata files). 

* Added new preference flag for updating timestamps... they now can be shut
  off. This is accessible from the Ado-mode->Options menu, like all other
  flags for formatting.

* Added new random number generating functions.

* Finally made ado-insert-nice-current-date to interactively insert timestamps.

* Finally put in leading * comment highlighting.

* Updated the template for help files.

## 1.10.0.0

Added several bits of functionality suggested by Uli Kohler:

* changed method for indenting so that do-files are properly indented:
  the indentation is incremented by each defined program (program [define])
  and decremented by each end statement.

* it is now possible to choose a default column for continuation strings in
  similarly to that for comments. This column is respected by
  ado-split-line if ado-line-up-continuations is non-nil.

* created ado-macify-selection-or-word which will put `' around the current
  selection. If nothing is selected, but the point is in a word, it
  will put `' around the current word. Finally if the point surrounded by
  whitespace it will insert `' and leave point inside the macro.

* created ado-stringify-selection, which will put `""' around the current
  selection (if any). If nothing is selected, inserts `""' and leaves the
  point inside the quotations.

* created ado-strmacify-selection-or-word which macifies and then stringifies
  the object of interest.

Other changes:

* added Stata 10 commands, functions, and the like.

* now try to highlight ++ and -- following local macro defs as obsolete, as
  a warning to people like me who forget that the incrementation must
  preceed the macro name.

* changed indenting, so that mata: alone on a line will cause the
  following lines to indent (until an end statement is found).

* changed name of ado-help-file to ado-new-help.

* added new customizable variable ado-help-extension which can be
  toggled between its default of 'sthlp' and 'hlp' using 
  ado-toggle-help-extension.

* added ado-new-cscript for starting cert scripts.

* changed the menus a little to take advantage of contextual menus.

* changed versioning numbers, but kept increasing sequence.

* arbitrary bug fixes to some of the more complicated highlighting.

## 0.92.0

* Added additional functions introduced in online updates. Some are
  from before the 20jan2006 update, sad to say. Fixed bug for indentation
  before special commands (like end or version). This changed the behavior
  of ado-beginning-of-command [M-a] to sit still when in a blank line when
  the delimiter is cr.

* fixed bad fontification 
  * of local when used as local ``foo'' with double quotes.   
  * of tempvar, tempfile, tempname when followed by quoted local macro.

* added code which runs if the user is using Aquamacs emacs. Not sure of
  the wisdom of having this in the main file, perhaps it will be broken out
  in the future. 

# 0.91.2

* Added Mata functions introduced in the 20jan2006 update to Stata,
  and added the ado-hacks.el file, which includes a function defined in
  Stata 22, but not earlier: line-number-at-pos. This handy function is
  good for complicated indentations and for jumping back to the beginning
  of commands... but missing it will break emacs 21.whatever.

# 0.91.1

* Separated change history off.

* changed format of new history entries

* changed all my addresses to my new job forgot this in 0.91.0.

* changed behavior for saving files to be more emacs-like.
  * When saving a file under its old name, there is no confirmation, like usual
   emacs saving
  * if ado-mode, ahem, corrects the file name *and* there is a saved file of that
   name in the default directory already, it *will* prompt, to avoid mistaken
   overwrites. 
  * thanks to Brendan Halpin for pointing this out

* fixed some minor programming badness, such as using insert-file when
  insert-file-contents was the proper command to use.

* fixed and improved indentation, so that continued commands with embedded
  comments are indented properly.

* fixed bugs in indentation when using a semi-colon for the delimiter

* changed method for comment indentation, so that /* and */ on different
  lines will line up, with the text on in-between lines indented to one
  extra level of indentation. (requested by Patrick Ball) 

* Mata changes

  * added new faces for current mata keywords, future mata keywords, and
   mata functions (though the mata function face defaults to the regular
   function face.
  * included all the mata functions, as far as I can tell.
  * did not change anything else with indentation for Mata functions.

# 0.91.0

* Fixed highlighting to work with all new Stata commands
  except the new Mata commands. They will arrive in another update.
  No real changes to the underpinnings.

# 0.82.5

* Fixed numerous problems with indenting and moving to
  the beginning and end of commands when using semi-colons as
  the delimiter. Bound M-e to ado-end-of-command. Fixed the
  behavior of this and ado-beginning-of-command. 

# 0.82.4

* Hacked the highlighting to make embedded strings, a la
  `"this is "embedded", you know"'
  look right. Emacs won't treat them properly internally, but
  at least they will look like strings.

# 0.82.3

* Added the new commands related to time series from the July 23,
  2004 update of Stata. Made the now obsolete -varfcast- and -varinf- show
  up as obsolete.

# 0.82.2

* Fixed the bug where ado-mode got confused by opening .do files
  which had subprogram defined in them.

# 0.82.1
* Added the new commands, macro extended commands and new SMCL
  directives from the July 1, 2004 update of Stata. Minor stuff.

#0.82

* Changed behavior when -do- files are saved to fix the bug that would cause 
  -do- files which defined programs to get saved as -ado- files named after the 
  first locally defined program. Other bug fixes for highlighting of some
  lesser-used constructions.

#0.81

* Changed behavior of open and close quotes (` and ') so that they no longer
  behave like parentheses, meaning that they don't flash and don't affect indentation.
  This should have been done earlier when ' was allowed for matrix transposition.

#0.80

* Serious upgrade: made to work with Stata 8, customization groups added,
  font-locking made more flexible, buffer-local changes to indentation and
  the like added.

# before 0.80

*  Updates for Stata changes  out-of-date emacs code.

