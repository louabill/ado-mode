# The `ado-mode` for Emacs

The `ado-mode` is a major [editing] mode for Emacs which allows truly good editing of Stata, namely `do`, `ado`, `sthlp`, `smcl`, and `mata` files. Here is a [screenshot](docs4github/ado_highlighting.png) to see what the highlighting does. The mode also handles indentation, date stamps, and includes templates for writing help files, project do-files and the like.

Emacs versions down to 25.1 are supported.

Here is the [change log](changes.md) which says what is new.

The current version is **1.19.5.2**, which is made for Stata 19.5 (aka StataNow). It works just fine with earlier versions of Stata, but it will highlight commands as though you were using Stata 19.5.

Here are the things which make life easier when using the `ado-mode`. Features include

* Sending code directly from Emacs to Stata for evaluation. (See below)
  
* Opening any file on the default ado-path, even automagically opening the ado-file for the current command.

* Opening Stata help files (in Stata, of course) from Emacs. 

* Context-sensitive highlighting for keeping typos to a minimum.
  * Stata's commands and keywords are highlighted even if abbreviated.
  * Commands which need subcommands are highlighted differently, so that you know a subcommand is needed.
  * Downloaded and personal user-written commands can be automatically highlighted once Emacs knows where they are kept.
  * `smcl` directive highlighting (a big help for help files)

* Templates for common files, such as do-files which keep their own logs, ado-files, do-files made for testing and debugging ado-files, and (most importantly, perhaps) Stata help files.

* Automatic indentation so that long `if`, `foreach`, `forvalues`, and `while` blocks get closed. Many types of indentation styles are supported.

* Making the name of the ado file match the name of the program defined, even if you change the program's name.

* Automatic refreshing of a header time stamp which can be formatted in a variety of ways.

* Typical Emacs things:
  * Parentheses, braces and double-quotes are balanced.
  * Auto-completion can be enabled via `M-x auto-complete-mode` or `M-x company-mode`, among other minor modes. Both highlight common words and words already used in the buffer being edited. `auto-complete-mode` comes with Emacs while `company-mode` can be downloaded from Melpa via `M-x list-packages`. 

`ado-mode` can be downloaded via Melpa. It's been accepted as a Melpa project, but the whole Melpa autoupdate thing throws me for a loop. So...

Manual installation is also possible, but takes more work. Take a look at the `INSTALL.md` file.

## External Tools Needed for MS Windows and *nix:

* To send code to Stata from Emacs:
  * If you are using a Mac, this will work out of the box.
  * If you are using MS Windows, you'll need to install of [AutoIt](https://www.autoitscript.com/site/autoit/downloads/).
  * If you are using Unix, you will need to get `xsel` and `xdotool`. In Debian/Ubuntu, this can be done via `sudo apt-get install xsel xdotool`. Other *nixes will be similar.

**Please** let me know if there are any troubles with the installations, since there are little quirks with cross-platform Emacs stuff.

`ado-mode` now gets announced on BlueSky: [https://bsky.app/profile/adomode.bsky.social](https://bsky.app/profile/adomode.bsky.social) so that there is an easy RSS feed for finding out about updates. `ado-mode` was on the disaster now called X, but that account will sit idle as long as X is run by a megalomaniac on ketamine.
