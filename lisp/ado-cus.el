;;; ado-cus.el --- customization for all things ado-mode -*- lexical-binding: t; package-lint-main-file: "ado-mode.el"; -*-

;; Copyright (C) 2003--2021 Bill Rising

;; Author:   Bill Rising <brising@alum.mit.edu>
;; Homepage: https://github.com/louabill/ado-mode

;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.
;; If not, see <https://www.gnu.org/licenses/>

;;; Commentary:

;;
;; This file contains all of the customizable things for ado
;; Warning: there will be `checkdoc' failure for flags whose behavior is
;;   complicated.
;; Warning: there will be `checkdoc' failure for the term do-file, which
;;   is not a Lisp symbol name, but is a common Stata term.

;;; Code:

;; Customization groups
;;

(defgroup ado nil
  "Ado mode: major mode for editing ado and do files for Stata."
  :tag "Ado mode"
  :group 'local)

;; (defgroup ado-essentials nil
;;   "Settings to get ado-mode running properly."
;;   :tag "Ado essentials"
;;   :group 'ado)

(defgroup ado-files nil
  "Information about file locations."
  :tag "Ado file locations"
  :group 'ado)

(defgroup ado-help-info nil
  "Information for simplifying your Stata help files."
  :tag "Ado help-file info"
  :group 'ado)

(defgroup ado-stata-interaction nil
  "How to work when passing information to Stata."
  :tag "Stata interaction"
  :group 'ado)

(defgroup ado-amain nil
  "Main set of customizations: Look, style, and behavior."
  :tag "Ado main"
  :group 'ado)

(defgroup ado-zmisc nil
  "Everything else, the dreaded miscellaneous category."
  :tag "Ado miscellaneous"
  :group 'ado)

(defgroup ado-zpath nil
  "Locations of directories appearing in your ado-path.
It is recommended that you leave these as nil, 
so that ado-mode can set them according to what Stata 
will see when launched.
Setting them by hand will speed up the loading of 
the first Stata-related file you open, but means
you must change them by hand if you change your
ado-path in Stata."
  :tag "Ado-path info"
  :group 'ado)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; standard customizations

(defcustom ado-mode-hook nil
  "Hook for Ado mode."
  :type '(hook)
  :options '(turn-on-auto-fill)
  :group 'ado-amain)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ado directories
;;

;; (defcustom ado-new-dir
;;   (file-name-as-directory
;;    (concat (file-name-as-directory (getenv "HOME"))
;; 		   (cond ((eq system-type 'darwin)
;; 				  "Library/Application Support/Stata/ado/personal")
;; 				 ((eq system-type 'windows-nt)
;; 				  "Stata/ado/personal")
;; 				 (t "Stata/ado/personal"))))
;;   "*The directory where new ado files are stored. This should hold ado
;; files which could be of use in multiple projects, but which have not
;; been properly debugged or documented yet. By default, this is unset,
;; but is ought to be set to the PERSONAL directory used in Stata."
;;   :type 'directory
;;   :group 'ado-files
;;   :group 'ado-essentials)

(defcustom ado-new-dir nil
  "The directory where new ado files are stored.
If left unset, this gets automatically set the PERSONAL directory 
used in Stata.
If set by hand should be set to a directory in the Stata ado-path."
  :type '(choice (const nil) directory)
  :group 'ado-files
  :group 'ado-zpath)

(defcustom ado-personal-dir nil
  "The directory which corresponds to PERSONAL.
By default this is unset.
Used for fontifying user-written commands."
  :type '(choice (const nil) directory)
  :group 'ado-zpath)

(defcustom ado-plus-dir nil
  "The directory which corresponds to PLUS.
By default this is unset.
Used for fontifying installed commands."
  :type '(choice (const nil) directory)
  :group 'ado-zpath)

(defcustom ado-site-dir nil
  "The directory which corresponds to SITE.
By default this is unset.
Used for fontifying installed site-wide additional commands."
  :type '(choice (const nil) directory)
  :group 'ado-zpath)

(defcustom ado-oldplace-dir nil
  "The directory which corresponds to OLDPLACE.
By default this is unset.
Used for fontifying additional commands in the dark and mysterious
cabin with the creaking screen door."
  :type '(choice (const nil) directory)
  :group 'ado-zpath)

(defcustom ado-confirm-overwrite-flag t
  "Non-nil means you must confirm overwriting an already-existing file.
When nil, overwrites happily and dangerously.
Defaults to on, to conform with standard user interface guidelines."
  :type 'boolean
  :group 'ado-amain)

(defcustom ado-add-sysdir-font-lock t
  "Non-nil means that commands from the sysdir directories get fontified.
By default this is set to t."
  :type 'boolean
  :group 'ado-fonts)

(defcustom ado-site-template-dir nil
  "The directory where templates are stored.
When nil, uses the templates which came with ‘ado-mode’.
Unless you are customizing templates for individual use, this is the best setting."
  :type '(choice (const nil) directory)
  :group 'ado-files)

(defcustom ado-script-dir nil
  "A directory for holding scripts and executables useful for ‘ado-mode’.
When nil, uses the script directory which came with ‘ado-mode’
Unless you plan on moving those scripts, leave as nil."
  :type '(choice (const nil) directory)
  :group 'ado-files)

(defcustom ado-mode-home nil
  "Location where the ‘ado-mode’ is installed.
Should be named `ado-mode-dir' to match all other directory variables, but
has been left to fester with its old name for backward compatibility. 
Available for customization just to be dangerous, as it should be left alone."
  :type '(choice (const nil) directory)
  :group 'ado-files)

;; commented out until better-considered
;(defcustom ado-local-label-dir nil
;  "A directory of useful value labels for a particular user."
;  :type 'directory
;  :group 'ado-files)

;(defcustom ado-site-label-dir nil
;  "A directory of useful value labels used at a whole site. Not used
;yet, though..."
;  :type 'directory
;  :group 'ado-files)

(defcustom ado-label-dir nil
  "A directory of useful value labels for a particular user.
Change to non-nil once you have a place for your label do-files."
  :type '(choice (const nil) directory)
  :group 'ado-files)

(defcustom ado-open-read-only-flag t
  "Non-nil means files from the adopath get opened as read-only.
When nil, opens files from the adopath as editable.
Defaults to t, to be safe, especially for official Stata commands.
Change to nil to live dangerously."
  :type 'boolean
  :group 'ado-amain)

;; a couple of variables needed for help files.
(defcustom ado-signature-file nil
  "Signature file to use for help files.
When nil, the user will be prompted when writing the first help file.
When non-nil, points to a file containing the signature footer for help files.
Should be set in each user's .emacs file."
  :type '(choice (const nil) (file :must-match t))
  :group 'ado-help-info)

(defcustom ado-signature-prompt-flag t
  "Controls prompting of the user for a signature at the bottom of help files.
When nil, only the user's name will be appended to help files.
When non-nil, prompts the user for a signature at the bottom of the help files.
Defaults to t."
  :type 'boolean
  :group 'ado-help-info)

(defcustom ado-help-author-flag t
  "Non-nil means help files get an author section.
When nil, help files have no authorship (useful for official Stata help files).
Defaults to on."
  :type 'boolean
  :group 'ado-help-info)

(defcustom ado-claim-name ""
  "Name used in the top of old-style (Stata 6 and earlier) help files.
May be reset using \\[set-ado-claim-name].
When nil, it will be set when the first help file is written."
  :type 'string
  :group 'ado-help-info)

(defcustom ado-help-extension "sthlp"
  "File extension used for help files.
Defaults to extension for Stata 10 and above.
May be changed using ‘ado-toggle-help-extension’."
  :type 'string
  :group 'ado-help-info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ado commenting and indentation variables
;;  all these are made buffer-local ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom ado-smart-indent-flag t
  "Non-nil means smart indenting computes the indentation level of each line.
When nil, indentation must be done manually.
Default value is on."
  :type 'boolean
  :group 'ado-amain)

(defcustom ado-return-also-indents-flag t
  "Non-nil means `RET' behaves like `newline-and-indent'.
When nil, lines must be indented manually using ‘TAB’.
Defaults to on."
  :type 'boolean
  :group 'ado-amain)

(defcustom ado-do-indent-flag t
  "Non-nil means do-files get indented automatically.
When nil, do-files require manual indenting.
Defaults to on."
  :type 'boolean
  :group 'ado-amain)

(defcustom ado-use-modern-split-flag t
  "Non-nil means /// continuation comments are used for splitting lines.
When nil, the old-school /* */ method gets used.
Defaults to on."
  :type 'boolean
  :group 'ado-amain)

(defcustom ado-close-under-line-flag t
  "Non-nil means close braces are indented at the level being closed.
When nil, close braces are indented at the level of the line containing the
open brace.
Defaults to being on, even though that is not the in-house Stata style."
  :type 'boolean
  :group 'ado-amain)

(defcustom ado-auto-newline-flag nil
  "Non-nil means a new line is inserted automatically after special characters.
When nil, nothing special happens.
This is mostly useful for open and close braces.
Can be really neat, it can also be a royal pain, depending on how often braces
get inserted mistakenly.
Defaults to off."
  :type 'boolean
  :group 'ado-amain)

(defcustom ado-closing-brace-alone-flag nil
  "Non-nil means closing braces are alone on a line when entered.
When nil, nothing special happens.
Defaults to off."
  :type 'boolean
  :group 'ado-amain)

(defcustom ado-fontify-new-flag t
  "Non-nil means brand new files have syntax highlighting turned on.
When nil, fontification must be done by hand.
Defaults to on.
Is a relic of the days when computers were very slow so fontification
was a big time hit.
Cannot really see any purpose for this to turned off."
  :type 'boolean
  :group 'ado-amain)

(defcustom ado-tab-width 3
  "Sets the size of tabs when opening or creating ado files.
Defaults to 3.
To change ‘tab-width’ in an individual buffer, use \\[ado-change-tab-width]."
  :type 'integer
  :group 'ado-amain)

(defcustom ado-continued-statement-indent-spaces 2
  "Extra indentation for continued lines.
Needed when when \"#delimit ;\"
has been used or a \\\ or \* comment has been used to split lines.
Defaults to 2."
  :type 'integer
  :group 'ado-amain)

(defcustom ado-comment-column 40
  "Sets the column at which comments within commands begin.
Defaults to 40."
  :type 'integer
  :group 'ado-amain)

(defcustom ado-continuation-column 40
  "Sets the column at which continuations within commands begin.
Defaults to 40."
  :type 'integer
  :group 'ado-amain)

(defcustom ado-line-up-continuations nil
  "Controls whether `ado-split-line' uses the `ado-continuation-column'.
When nil, splitting a line simply splits the line.
When non-nil, the `ado-continuation-column' gets used.
Defaults to off."
  :type 'boolean
  :group 'ado-amain)

(defcustom ado-debugging-indent-flag t
  "Flag for whether debugging commands have a special indent column.
When nil, debugging commands are indented like all other commands.
When non-nil debugging commands get indented at
`ado-debugging-indent-column'.
Defaults to on."
  :type 'boolean
  :group 'ado-amain)

(defcustom ado-debugging-indent-column 0
  "Sets indentation column for debugging commands.
If `ado-debugging-indent-flag' is on, this column gets used.
Defaults to 0."
  :type 'integer
  :group 'ado-amain)

(defcustom ado-delimit-indent-flag t
  "Flag for whether delimit commands have a special indent column.
When nil, #delimit gets indented like all other commands.
When non-nil, #delimit commands are indented to the value of
`ado-delimit-column'.
Defaults to on."
  :type 'boolean
  :group 'ado-amain)

(defcustom ado-delimit-indent-column 0
  "Sets the amount by which #delimit commands are indented.
If `ado-delimit-indent-flag' is on, #delimit commands appear at this column.
Defaults to 0."
  :type 'integer
  :group 'ado-amain)

(defcustom ado-comment-indent-flag t
  "Non-nil means line-starting comments use `ado-comment-indent-column'.
When nil, nothing special happens.
Defaults to on."
  :type 'boolean
  :group 'ado-amain)

(defcustom ado-comment-indent-column 0
  "Sets the amount by which line-starting comments are indented.
Defaults to 0."
  :type 'integer
  :group 'ado-amain)

(defcustom ado-update-timestamp-flag t
  "Non-nil means the timestamp at the top of a file gets updated on save.
When nil, the timestamp must be updated manually.
Defaults to t."
  :type 'boolean
  :group 'ado-amain)

(defcustom ado-date-format "%B %-e, %Y @ %H:%M:%S"
  "Controls the date format used when autoupdating timestamps.
Defaults to %B %-e, %Y @ %H:%M:%S, to match versions of
‘ado-mode’ before 0.92.0. See \\[describe-function] `format-time-string'
for help on setting."
  :type 'string
  :group 'ado-amain)

(defcustom ado-lowercase-date-flag nil
  "Non-nil means autoupdated timestamps are all lowercase.
When nil, uppercase months and days are allowed.
Defaults to nil.
The StataCorp style is to turn this on."
  :type 'boolean
  :group 'ado-amain)

(defcustom ado-initials-flag nil
  "Non-nil means your initials are added after autoupdated timestamps.
When nil, no initials get added.
Useful in group environments with no version (aka revision) control.
Defaults to off."
  :type 'boolean
  :group 'ado-amain)

;; could be misplaced...
(defcustom ado-initials nil
  "Set to your initials if you will ever turn the `ado-initials-flag' on."
  :type 'string
  :group 'ado-amain)

;; Ado-Stata interactions (such as flavors and locations of Statas)
(defcustom ado-submit-default "command"
  "Default method for sending commands from Emacs to Stata.
Set to desired method of sending commands to Stata. Defaults
to \"command\", which means that commands are sent to the Command window.
Other choices are \"dofile\", which sends commands via a do-file,
\"include\" which sends the commands via an -include- (for passing
local macros properly), and \"menu\" which sends commands via a menu item.
The four options differ in how they populate the Review window
\(and hence any cmdlog):
  \"command\" puts the command(s) in the Review window
  \"dofile\" puts a 'do ...' command in the Review window
  \"include\" puts an 'include ...' command in the Review window
  \"menu\" puts nothing in the Review window.
One note: \"menu\" does not work for Stata 10 and earlier. This
does not seem to be much of a loss."
  :type '(choice (const "command")
				 (const "dofile")
				 (const "include")
				 (const "menu"))
  :group 'ado-stata-interaction)

(defcustom ado-comeback-flag nil
"Non-nil means control returns to Emacs after sending a command.
When nil, Stata is the active application after sending a command.
Defaults to nil."
  :type 'boolean
  :group 'ado-stata-interaction)

(defcustom ado-stata-home
  (cond ((string= system-type "darwin") "/Applications/Stata/")
		((string= system-type "windows-nt") "C:/Program Files/Stata16/")
		(t "/usr/local/stata"))
  "Set to the location of your Stata executable(s).
Needed if you want Emacs to launch a particular version of Stata
for setting your adopath or if you always launch Stata before sending
code from Emacs.
Defaults to the the typical install location for Stata 16.
If you are a different version of Stata under MS Windows, you will need to 
customize this setting. Mac and Unix should be universal."
  :type '(choice (const nil) directory)
  :group 'ado-stata-interaction)

(defcustom ado-version-command ""
  "The default version of Stata you want at the top of your Stata-related files.
If unset (left as \"\"), it will try to find your version the first time it is
needed.
The version command gets put at the top of all do-, ado-, mata- and class-files."
  :type 'string
  :group 'ado-stata-interaction)

(defcustom ado-temp-dofile ""
  "Set to the name of the do-file run when sending code to Stata via a do-file.
Defaults to feedStata.do when unset."
  :type 'string
  :group 'ado-stata-interaction)

(defcustom ado-stata-instance 0
  "Set to the instance of Stata you would like ‘ado-mode’ to send its code to.
This can and should be left as a zero unless
you have special reason to choose an instance...and then you
should only change it temporarily."
  :type 'integer
  :group 'ado-stata-interaction)

(defcustom ado-stata-version ""
  "Set to the version of Stata you would like ‘ado-mode’ to send its code to.
This can and should be left as a blank unless
you have special reason to choose a version...and then you
should only change it temporarily."
  :type 'string
  :group 'ado-stata-interaction)

(defcustom ado-stata-flavor ""
  "Set to a flavor of Stata you would like ‘ado-mode’ to send its code to.
This can and should be left as a blank unless
you have special reason to choose a flavor...and then you
should only change it temporarily."
  :type 'string
  :group 'ado-stata-interaction)

(defcustom ado-strict-match-flag nil
  "Control whether you want strict matching for sending code to multiple Statas.
When nil, strict matching is off.
Set to t if you would like code only sent to Stata(s) which match all
3 of `ado-stata-instance', `ado-stata-version', and `ado-stata-flavor'.
By default this is set to nil, so that if there is one instance of Stata
running, the values of the three filters are immaterial."
  :type 'boolean
  :group 'ado-stata-interaction)

(defcustom ado-send-to-all-flag nil
  "Control loose matching is used when sending code to multiple Statas.
When nil, send to all Statas.
Set to t if you would like code sent to all running
Statas whenever you send code to run. If set to nil, ado-mode
will try to match your criteria (instance, version, and flavor)
as best possible, and send the code to the best match, if it is
unique. If there are multiple best matches, you will get an error."
  :type 'boolean
  :group 'ado-stata-interaction)

(defcustom ado-before-save-file-hook 'ado-before-save-file
  "The file hook function you would like to use before saving files.
Defaults to \\[ado-before-save-file], which behaves like the
now-deprecated \\[ado-save-program]. The utility of this hook is
to allow OS-standard save shortcuts to work properly."
  :type 'hook)

(provide 'ado-cus)

;;; ado-cus.el ends here
