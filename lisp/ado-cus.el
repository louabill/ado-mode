;;; ado-cus.el --- customization for all things ado-mode

;; Copyright (C) 2003--2020 Bill Rising

;; Author:   Bill Rising <brising@alum.mit.edu>
;; Keywords: languages, tools
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

;;; Code:

;; Customization groups
;;

(defgroup ado nil
  "Ado mode: major mode for editing ado and do files for Stata."
  :tag "Ado mode"
  :group 'local)

(defgroup ado-essentials nil
  "Settings to get ado-mode running properly."
  :tag "Ado essentials"
  :group 'ado)

(defgroup ado-files nil
  "Information about file locations and behaviors."
  :tag "Ado files"
  :group 'ado)

(defgroup ado-help-info nil
  "Information for making good Stata documentation."
  :tag "Ado help file info"
  :group 'ado)

(defgroup ado-path nil
  "Locations of directories appearing in your ado-path.
Set to add syntax highlighting for user-written commands."
  :tag "adopath information"
  :group 'ado)

(defgroup ado-stata-interaction nil
  "How to work when passing information to Stata."
  :tag "Ado-Stata interaction"
  :group 'ado)

(defgroup ado-style nil
  "Look and Style of ado-mode: indentation and such."
  :tag "Ado style"
  :group 'ado)

(defgroup ado-zmisc nil
  "Everything else, the dreaded miscellaneous category."
  :tag "Ado miscellaneous"
  :group 'ado)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; standard customizations

(defcustom ado-mode-hook nil
  "Hook for Ado mode."
  :type '(hook)
  :options '(turn-on-auto-fill)
  :group 'ado)

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
You MUST set this to some location. This should hold ado files which could
be of use in multiple projects, but which have not been properly
debugged or documented yet. By default, this is unset, but is
ought to be set to the PERSONAL directory used in Stata."
  :type '(choice (const nil) directory)
  :group 'ado-files
  :group 'ado-path
  :group 'ado-essentials)

(defcustom ado-personal-dir nil
  "The directory which corresponds to PERSONAL.
By default this is unset.
Used for fontifying user-written commands."
  :type '(choice (const nil) directory)
  :group 'ado-path)

(defcustom ado-plus-dir nil
  "The directory which corresponds to PLUS.
By default this is unset.
Used for fontifying installed commands."
  :type '(choice (const nil) directory)
  :group 'ado-path)

(defcustom ado-site-dir nil
  "The directory which corresponds to SITE.
By default this is unset.
Used for fontifying installed site-wide additional commands."
  :type '(choice (const nil) directory)
  :group 'ado-path)

(defcustom ado-oldplace-dir nil
  "The directory which corresponds to OLDPLACE.
By default this is unset.
Used for fontifying additional commands in the dark and mysterious
cabin with the creaking screen door."
  :type '(choice (const nil) directory)
  :group 'ado-path)

(defcustom ado-confirm-overwrite-flag t
  "Non-nil turns on confirmation when when overwriting an already-existing file.
Defaults to on, as this conforms with user interface guidelines."
  :type 'boolean
  :group 'ado-files
  :group 'ado-essentials)

(defcustom ado-site-template-dir nil
  "The directory where templates are stored.
If left unset, it will point to the templates which come with ‘ado-mode’.
Unless you are customizing templates for individual use, this is the best setting."
  :type '(choice (const nil) directory)
  :group 'ado-files
  :group 'ado-essentials)

(defcustom ado-script-dir nil
  "A directory for holding scripts and executables useful for ‘ado-mode’.
If left unset, it will point to the scripts directory which comes with
‘ado-mode’. Unless you plan on moving those scripts, leave unset."
  :type '(choice (const nil) directory)
  :group 'ado-files
  :group 'ado-essentials)

(defcustom ado-mode-home nil
  "Location where the ‘ado-mode’ is installed.
Available for customization just to be dangerious, as it should be left alone."
  :type '(choice (const nil) directory)
  :group 'ado-files
  :group 'ado-essentials)

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
  "A directory of useful value labels for a particular user."
  :type '(choice (const nil) directory)
  :group 'ado-files)

(defcustom ado-open-read-only-flag t
  "Non-nil means files opened from the adopath open as read-only.
This defaults to t, to be safe, especially for official Stata commands.
Change to nil to live dangerously."
  :type 'boolean
  :group 'ado-files)

;; a couple of variables needed for help files.
(defcustom ado-signature-file nil
  "Signature file to use for help files.
This ought to be set to your .signature file, but Stata once used the . symbol
in a special fashion...and many folks don't use unix
and hence have no .signature file.
If nil, the user will be prompted when writing the first help file.
If the user wants to be left alone, set ado-no-signature to non-nil.
Should be set in each user's .emacs file."
  :type '(file :must-match t)
  :group 'ado-help-info)

(defcustom ado-signature-prompt-flag t
  "Non-nil prompts the user for a signature at the bottom of the help files.
If nil, the user will never be asked, and just the user's
name will be appended to help files. Defaults to on."
  :type 'boolean
  :group 'ado-help-info)

(defcustom ado-help-author-flag t
  "Non-nil includes the author section help files.
If set to nil there will be no authorship (useful for official
Stata help), otherwise an Author section will be included (useful
for the whole rest of the world). Defaults to on."
  :type 'boolean
  :group 'ado-help-info)

(defcustom ado-claim-name nil
  "Name used in the top of old-style (Stata 6 and earlier) help files.
May be reset using \\[set-ado-claim-name].
If nil, it will be set when the first help file is written."
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
  "Non-nil turns on smart indenting.
May be turned off for those who like.
Default value is on."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-return-also-indents-flag t
  "Non-nil makes return (enter) act like newline-and-indent.
Defaults to on, because this is of great utility.
May be turned off conform with emacs' creators desire to
use \\C-j for this action."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-do-indent-flag t
  "Non-nil turns on indenting in do-files.
If off, there is no indentation of do files at all.
Defaults to on."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-use-modern-split-flag t
  "Non-nil uses the /// method for splitting lines.
Set to nil to use the old-school /* */ method.
Defaults to on."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-close-under-line-flag t
  "Non-nil indents closing braces at the level being closed.
Set to nil to have close braces line up over the following line's
indentation level.
Defaults to being on, even though that is not the in-house Stata style."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-auto-newline-flag nil
  "Non-nil forces an automatic new line after special characters.
While the auto newline can be really neat, it can also be a royal pain,
depending on how often braces are inserted mistakenly.
Defaults to off."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-closing-brace-alone-flag nil
  "Non-nil forces closing braces to be alone on a line when entered.
Defaults to off.
Can be nice, can be annoying."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-fontify-new-flag t
  "Non-nil turns on syntax highlighting (aka fontifying) of new files.
Cannot really see any purpose for this to turned off."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-tab-width 3
  "Sets the size of tabs when opening or creating ado files.
Defaults to 3.
To change ‘tab-width’ in an individual buffer, use \\[ado-change-tab-width]."
  :type 'integer
  :group 'ado-style)

(defcustom ado-continued-statement-indent-spaces 2
  "Extra indentation for continued lines.
Needed when when \"#delimit ;\"
has been used or a \\\ or \* comment has been used to split lines.
Defaults to 2."
  :type 'integer
  :group 'ado-style)

(defcustom ado-comment-column 40
  "Sets the column at which comments within commands begin.
Defaults to 40."
  :type 'integer
  :group 'ado-style)

(defcustom ado-continuation-column 40
  "Sets the column at which continuations within commands begin.
Defaults to 40."
  :type 'integer
  :group 'ado-style)

(defcustom ado-line-up-continuations nil
  "Controls whether \\[ado-split-line] respects the variable `ado-continuation-column'.
If t, the `ado-continuation-column' gets used.
If nil, splitting a line simply splits the line.
Defaults to off."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-debugging-indent-flag t
  "Set to non-nil to have special indentation for debugging commands.
If off, then debugging commands (such as pause or set trace) are
indented like any others. Otherwise, this forces the them to be indented
at ado-debugging-indent-column.
Defaults to on."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-debugging-indent-column 0
  "Sets indentatino column for debugging commands.
If `ado-debugging-indent-flag' is on, this column gets used.
Defaults to 0."
  :type 'integer
  :group 'ado-style)

(defcustom ado-delimit-indent-flag t
  "Non-nil uses special indentation for #delimit commands.
If off, then #delimit commands are indented like any others.
Defaults to on."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-delimit-indent-column 0
  "Sets the amount by which #delimit commands are indented.
If `ado-delimit-indent-flag' is on, #delimit commands appear at this column.
Defaults to 0."
  :type 'integer
  :group 'ado-style)

(defcustom ado-comment-indent-flag t
  "Non-nil makes line-starting comments use `ado-comment-indent-column'.
If off, then initial comments are indented like any other commands."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-comment-indent-column 0
  "Sets the amount by which initial comments are indented.
Defaults to 0."
  :type 'integer
  :group 'ado-style)

(defcustom ado-update-timestamp-flag t
  "Non-nil automatically updates time stamps when files get saved.
Set to nil to turn off automatic timestamp updating."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-date-format "%B %-e, %Y @ %H:%M:%S"
  "Sets the format used when putting a timestamp on a file when saving.
Defaults to %B %-e, %Y @ %H:%M:%S, to match versions of
‘ado-mode’ before 0.92.0. See \\[describe-function] `format-time-string'
for help on setting."
  :type 'string
  :group 'ado-style)

(defcustom ado-lowercase-date-flag nil
  "Non-nil forces timestamps to be all lowercase.
Defaults to off."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-initials-flag nil
  "Non-nil inserts your initials to any time stamps.
\(The initials are set in `ado-initials'.)
Defaults to off."
  :type 'boolean
  :group 'ado-style)

;; could be misplaced...
(defcustom ado-initials nil
  "Set this to your initials if you intend on turning the `ado-initials-flag' on."
  :type 'string
  :group 'ado-style)

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
  "Non-nil brings control to Emacs after sending a command to Stata.
Useful when Emacs' and Stata's windows do not overlap."
  :type 'boolean
  :group 'ado-stata-interaction)

(defcustom ado-stata-home
  (cond ((string= system-type "darwin") "/Applications/Stata/")
		((string= system-type "windows-nt") "C:/Program Files (x86)/Stata15/")
		(t "/usr/local/stata"))
  "Set to the location of your Stata executable(s).
Needed if you want Emacs to launch a particular version of Stata
for setting your adopath or if you always launch Stata before sending
code from Emacs.
Defaults to the the typical install location for Stata 15."
  :type '(choice (const nil) directory)
  :group 'ado-stata-interaction)

(defcustom ado-version-command ""
  "The version command you would like at the top of your do-, ado-, mata- and class-files.
If left unset, it will try to find your version the first time it is needed.
Must start with version to be useful."
  :type 'string
  :group 'ado-stata-interaction)

(defcustom ado-temp-dofile ""
  "Set to the name of the do-file you would like to run when sending code to Stata via a do-file.
If left unset, it will default to feedStata.do"
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
  "Non-nil forces strict matching when sending code to Stata.
Set to t if you would like code only sent to Stata(s) which
match all 3 of ado-stata-instance, ado-stata-version, and
ado-stata-flavor. By default this is set to nil, so that if there
is one instance of Stata running, the values of the three filters
are immaterial."
  :type 'boolean
  :group 'ado-stata-interaction)

(defcustom ado-send-to-all-flag nil
  "Non-nil allows very loose matching when sending code to Stata.
Set to t if you would like code sent to all running
Statas whenever you send code to run. If set to nil, ado-mode
will try to match your criteria (instance, version, and flavor)
as best possible, and send the code to the best match, if it is
unique. If there are multiple best matches, you will get an error."
  :type 'boolean
  :group 'ado-stata-interaction)

(defcustom ado-before-save-file-hook 'ado-before-save-file
  "Set to the file hook you would like to use before saving files.
Defaults to \\[ado-before-save-file], which behaves like the
now-deprecated \\[ado-save-program]. The utility of this hook is
to allow OS-standard save shortcuts to work properly."
  :type 'hook
  )

(provide 'ado-cus)

;;; ado-cus.el ends here
