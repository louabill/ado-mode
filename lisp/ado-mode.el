;; ado-mode.el --- ado mode, and its idiosyncratic commands.

;; Copyright (C) 1999,..., 2009 Bill Rising

;; Maintainer: Bill Rising, brising at stata dot com
;; Keywords: ado-mode, highlighting
;; Version: 1.10.1.2 of June 17, 2006
;;
;; the old version system was 0.stata-version times ten.update
;; the new version system is now 1.stataversion.statasubversion.update
;; 
;; This file is NOT part of GNU Emacs.

;; This ado-mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This ado-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this ado-mode; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; This package provides the ado mode *NOT* documented in the
;; Emacs user's manual.
;; Much of this code originated with the C-mode, but has changed
;; quite a bit since.
;; To add your own additions to this, you will also need the
;; make-regexp package, a copy of which is located with this file
;; in Bill's Current Pile of Stuff
;;  ftp://erdos.math.louisville.edu/pub/brising
;;
;;; Code:

;;; required files
(require 'font-lock)
(require 'ado-cus)
(require 'ado-font)
(require 'ado-clip)
(require 'ado-to-stata)

;;; putting in the proper extensions for using the ado-mode
(if (assoc "\\.ado$" auto-mode-alist) nil
  (setq auto-mode-alist 
		(append (list '("\\.ado\\'" . ado-mode)
					  '("\\.lbl\\'" . ado-mode)
					  '("\\.do\\'"  . ado-mode)	;could be a problem
					  '("\\.hlp\\'" . ado-mode) ; will be a problem for those programming in MS WinWhatever
					  '("\\.ihlp\\'" . ado-mode)
					  '("\\.sthlp\\'" . ado-mode) ; Stata's fix for pernicious .hlp files blockage
					  '("\\.dlg\\'" . ado-mode)
					  '("\\.smcl\\'" . ado-mode)
					  '("\\.class\\'" . ado-mode)
					  '("\\.mata\\'" . ado-mode)
					  '("\\.ADO\\'" . ado-mode) ; for MS-DOG files
					  '("\\.LBL\\'" . ado-mode)
					  '("\\.DLG\\'" . ado-mode)
					  '("\\.DO\\'"  . ado-mode)
					  '("\\.HLP\\'" . ado-mode)
					  '("\\.IHLP\\'" . ado-mode)
					  '("\\.SMCL\\'" . ado-mode) 
					  '("\\.STHLP\\'" . ado-mode)
					  '("\\.CLASS\\'" . ado-mode)
					  '("\\.MATA\\'" . ado-mode)
					  )
				auto-mode-alist
			   )))

(defvar ado-font-lock-keywords nil)
(defvar ado-font-lock-syntactic-keywords nil)
(defvar ado-extension nil)

;; abbrev table
(defvar ado-mode-abbrev-table nil
  "Abbrev table used while in ado mode.")
(define-abbrev-table 'ado-mode-abbrev-table ())

;; syntax table
(defvar ado-mode-syntax-table nil
  "Syntax table used while in ado mode.")
(if ado-mode-syntax-table
    ()
  (setq ado-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "." ado-mode-syntax-table) ;nullify escape meaning
  (modify-syntax-entry ?\$ "." ado-mode-syntax-table)
;; commented out, because ' can be used too many places, now.
;  (modify-syntax-entry ?` "(\'" ado-mode-syntax-table)
;  (modify-syntax-entry ?\' ")`" ado-mode-syntax-table)
  (modify-syntax-entry ?/ ". 124b" ado-mode-syntax-table)
  (modify-syntax-entry ?* ". 23n" ado-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" ado-mode-syntax-table)
  (modify-syntax-entry ?+ "_" ado-mode-syntax-table)
  (modify-syntax-entry ?- "_" ado-mode-syntax-table)
  (modify-syntax-entry ?= "_" ado-mode-syntax-table)
  (modify-syntax-entry ?% "." ado-mode-syntax-table)
  (modify-syntax-entry ?< "." ado-mode-syntax-table)
  (modify-syntax-entry ?> "." ado-mode-syntax-table)
  (modify-syntax-entry ?& "." ado-mode-syntax-table)
  (modify-syntax-entry ?| "." ado-mode-syntax-table)
  (modify-syntax-entry ?~ "." ado-mode-syntax-table)
  ;; added this, because underscores really are parts of words
  (modify-syntax-entry ?_ "w" ado-mode-syntax-table)
  ;; an attempt to fix embedded quote highlighting problems
  ;; fails because string starts and ends can be only single characters
;  (modify-syntax-entry ?` "| 1b" ado-mode-syntax-table)
;  (modify-syntax-entry ?' "| 4b" ado-mode-syntax-table)
;  (modify-syntax-entry ?\" "| 23b" ado-mode-syntax-table)
  )

;;; keymap definitions
(defvar ado-mode-map (make-sparse-keymap)
  "Keymap for Ado mode." )
(define-key ado-mode-map "\t"       'ado-indent-line)
(define-key ado-mode-map "\M-\C-m" 'ado-send-command-to-stata)
(define-key ado-mode-map [(meta shift return)] 'ado-split-line)
(define-key ado-mode-map "\C-c\C-h" 'ado-help-at-point)
(define-key ado-mode-map "\C-c\C-c" 'ado-help-command)
(define-key ado-mode-map "\M-a"     'ado-beginning-of-command)
(define-key ado-mode-map "\M-e"     'ado-end-of-command)
(define-key ado-mode-map "\C-c\C-f" 'ado-foreach-loop)
(define-key ado-mode-map "\C-c\C-v" 'ado-forvalues-loop)
(define-key ado-mode-map "\C-c\C-n" 'ado-new-program)
(define-key ado-mode-map "\C-c\C-i" 'ado-insert-new-program)
(define-key ado-mode-map "\C-c\C-l" 'ado-new-label)
(define-key ado-mode-map "\C-c\C-d" 'ado-new-help)
(define-key ado-mode-map "\C-c\C-m" 'ado-macify-selection-or-word)
(define-key ado-mode-map "\C-c\C-s" 'ado-stringify-selection)
(define-key ado-mode-map "\C-c\C-j" 'ado-strmacify-selection-or-word)
;;(define-key ado-mode-map "\C-c;"    'comment-region)
;;(define-key ado-mode-map "\C-c:"    'uncomment-region)
(define-key ado-mode-map "\C-x\C-s" 'ado-save-program)
(define-key ado-mode-map "{"        'electric-ado-brace)
(define-key ado-mode-map "}"        'electric-ado-closing-brace)
(define-key ado-mode-map ";"        'electric-ado-semi)
;; Mac OS X only stuff (for now)
;(if (string= system-type "darwin")
;	(define-key ado-mode-map "\A-\C-

;;; menu bar definitions!
(define-key ado-mode-map [menu-bar] (make-sparse-keymap))
(define-key ado-mode-map [menu-bar ado]
  (cons "Ado-mode" (make-sparse-keymap "Ado-mode")))
(define-key ado-mode-map [menu-bar ado options]
  (cons "Options" (make-sparse-keymap "options")))
(define-key ado-mode-map [menu-bar ado l3]
  '(menu-item "--single-line"))
(define-key ado-mode-map [menu-bar ado indent-buffer]
  '("Indent Buffer" . ado-indent-buffer))
(define-key ado-mode-map [menu-bar ado new]
  (cons "New program" (make-sparse-keymap "new")))
(define-key ado-mode-map [menu-bar ado l2]
  '(menu-item "--single-line"))
;;(define-key ado-mode-map [menu-bar ado uncomment-region]
;;  '("Uncomment Region" . uncomment-region))
;;(define-key ado-mode-map [menu-bar ado comment-region]
;;  '("Comment Out Region" . comment-region))
(define-key ado-mode-map [menu-bar ado ado-forvalues-loop]
  '("Forvalues loop" . ado-forvalues-loop))
(define-key ado-mode-map [menu-bar ado ado-foreach-loop]
  '("Foreach loop" . ado-foreach-loop))
(define-key ado-mode-map [menu-bar ado l1]
  '(menu-item "--single-line"))
;; place for customizations
(define-key ado-mode-map [menu-bar ado strmacify-word]
  '("Stringify and Macify Word or Selection" . ado-strmacify-selection-or-word))
(define-key ado-mode-map [menu-bar ado stringify-selection]
  '("Stringify Selection" . ado-stringify-selection))
(define-key ado-mode-map [menu-bar ado macify-word]
  '("Macify Word or Selection" . ado-macify-selection-or-word))
(define-key ado-mode-map [menu-bar files save-buffer]
  '("Save buffer" . ado-save-program))

;; submenu New
(define-key ado-mode-map [menu-bar ado new ado-new-label]
  '("Label file" . ado-new-label))
(define-key ado-mode-map [menu-bar ado new ado-new-help]
  '("Help file" . ado-new-help))
(define-key ado-mode-map [menu-bar ado new ado-new-cscript]
  '("Cert script" . ado-new-cscript))
(define-key ado-mode-map [menu-bar ado new ado-insert-new-program]
  '("Insert new subprogram" . ado-insert-new-program))
(define-key ado-mode-map [menu-bar ado new ado-new-program]
  '("Generic new program" . ado-new-program))

;; submenu Options
;;; this submenu follows
(define-key ado-mode-map [menu-bar ado options special-indentation]
  (cons "Special Indentation" (make-sparse-keymap "special-indentation")))

(define-key ado-mode-map [menu-bar ado options ado-comeback-toggle]
  '(menu-item "Return to Emacs after Submission"
			  (lambda () (interactive) (ado-toggle-flag 'ado-comeback-flag))
			  :button (:toggle . ado-comeback-flag)))

(define-key ado-mode-map [menu-bar ado options ado-confirm-overwrite-toggle]
  '(menu-item "Confirm File Overwrite"
	      (lambda () (interactive) (ado-toggle-flag 'ado-confirm-overwrite-flag))
	      :button (:toggle . ado-confirm-overwrite-flag)))

(define-key ado-mode-map [menu-bar ado options ado-comment-column-change]
  '(menu-item "Set Comment Column..." 
	      (lambda () (interactive) ado-change-number 'ado-comment-column 'ask)))

(define-key ado-mode-map [menu-bar ado options ado-continued-statement-indent-spaces-change]
  '(menu-item "Set Continuation Indentation..." ado-continued-statement-indent-spaces-change))

(define-key ado-mode-map [menu-bar ado options ado-tab-width-change]
  '(menu-item "Set Tab Width..." ado-tab-width-change))

(define-key ado-mode-map [menu-bar ado options ado-update-timestamp-toggle]
  '(menu-item "Update Timestamps on Save" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-update-timestamp-flag))
	      :button (:toggle . ado-update-timestamp-flag)))

(define-key ado-mode-map [menu-bar ado options ado-fontify-new-toggle]
  '(menu-item "Fontify New Ado Files" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-fontify-new-flag))
	      :button (:toggle . ado-fontify-new-flag)))

(define-key ado-mode-map [menu-bar ado options ado-auto-newline-toggle]
  '(menu-item "Automatic New Line" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-auto-newline-flag))
	      :button (:toggle . ado-auto-newline-flag)))

(define-key ado-mode-map [menu-bar ado options ado-closing-brace-alone-toggle]
  '(menu-item "Closing Brace Alone" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-closing-brace-alone-flag))
	      :button (:toggle . ado-closing-brace-alone-flag)))

(define-key ado-mode-map [menu-bar ado options ado-close-under-line-toggle]
  '(menu-item "Close Under Line"
	      (lambda () (interactive) (ado-toggle-flag 'ado-close-under-line-flag))
	      :button (:toggle . ado-close-under-line-flag)))

(define-key ado-mode-map [menu-bar ado options ado-use-modern-split-toggle]
  '(menu-item "Use Modern Line-split"
	      (lambda () (interactive) (ado-toggle-flag 'ado-use-modern-split-flag))
	      :button (:toggle . ado-use-modern-split-flag)))

(define-key ado-mode-map [menu-bar ado options ado-do-indent-toggle]
  '(menu-item "Indent Do Files"
	      (lambda () (interactive) (ado-toggle-flag 'ado-do-indent-flag))
	      :button (:toggle . ado-do-indent-flag)))

;; needs its own toggling function, because keymaps must be changed.
(define-key ado-mode-map [menu-bar ado options ado-return-also-indents-toggle]
  '(menu-item "Return also Indents" ado-return-toggle
	      :button (:toggle . ado-return-also-indents-flag)))

(define-key ado-mode-map [menu-bar ado options ado-smart-indent-toggle]
  '(menu-item "Smart Indent"
	      (lambda () (interactive) (ado-toggle-flag 'ado-smart-indent-flag))
	      :button (:toggle . ado-smart-indent-flag)
;	      :help "This is some help, ain't it?"
	      ))

;(define-key ado-mode-map [menu-bar ado options div1]
;  '(menu-item "--shadow-etched-in"))
;(define-key ado-mode-map [menu-bar ado options title]
;  '(menu-item "Check or Uncheck"))
;  '("Toggle smart indent" . ado-smart-indent-toggle))

;; subsubmenu Options/Special Indent
(define-key ado-mode-map [menu-bar ado options special-indentation ado-change-comment-indent]
  '(menu-item "Change comment indent column..." 
	      (lambda () (interactive) (ado-change-number 'ado-comment-indent-column 'ask))
	      :enable ado-delimit-indent-flag))

(define-key ado-mode-map [menu-bar ado options special-indentation ado-comment-indent-flag-toggle]
  '(menu-item "Comment column indentation" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-comment-indent-flag))
 	      :button (:toggle . ado-comment-indent-flag)))

(define-key ado-mode-map [menu-bar ado options special-indentation ado-change-delimit-indent]
  '(menu-item "Change #delimit column..." 
	      (lambda () (interactive) (ado-change-number 'ado-delimit-indent-column 'ask))
	      :enable ado-delimit-indent-flag))

(define-key ado-mode-map [menu-bar ado options special-indentation ado-delimit-indent-flag-toggle]
  '(menu-item "#delimit indented differently" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-delimit-indent-flag))
 	      :button (:toggle . ado-delimit-indent-flag)))

(define-key ado-mode-map [menu-bar ado options special-indentation ado-change-debugging-indent]
  '(menu-item "Change debugging column..." 
	      (lambda () (interactive) (ado-change-number 'ado-debugging-indent-column 'ask))
	      :enable ado-debugging-indent-flag))

(define-key ado-mode-map [menu-bar ado options special-indentation ado-debugging-indent-flag-toggle]
  '(menu-item "debugging indented differently" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-debugging-indent-flag))
 	      :button (:toggle . ado-debugging-indent-flag)))

;; initial mode defintion function
(defun ado-mode ()
  "Major mode for editing ado, do, sthlp, hlp, dlg, and smcl 
files foruse in the Stata statistical package. It indents blocks 
of code properly, highlights command names, many keywords, some
more complicated command structures, strings, Stata macro names 
and the like.

If you downloaded the template folder (directory) which came with this
distribution, you can use this mode to create ado files (programs)
 and help files. To change the templates, edit the
files in the template directory which have .blp extensions. (blp stands
for boilerplate)

The mode comes with a menu (the Ado-mode menu) which shows most all of the
variables which are worth changing locally in a buffer. Global customization
can be done via '\\[customize-group] ado-mode' using emacs customization
routines. More suggestions can be found at 
http://homepage.mac.com/brising/Stata/ado-mode_install.html

Here is a short list of the common commands which come with the mode:
Things for dealing with files:
- \\[ado-new-ado] will make a new buffer ready for a new ado file.
- \\[ado-new-do] will make a buffer ready for a well-logged do file.
- \\[ado-save-program] will save the current buffer and give it a good
    timestamp (if the ado-update-timestamp-flag is true, which it is
    by default). Ensures that the file name matches the name of the
    command (ado program) or class being defined.
- \\[ado-new-help] will start a new help file, ready for editing.
- (Mac OS X only, for now) \\[ado-send-region-to-stata-default] will
  send the current selection to Stata for evaluation. If nothing is
  selected, the current command will be sent.
Things for changing style:
Most of these would be most easily done using emacs' ability to customize
its enviroment using \\[customize-group ado-mode]. Other little things
 are
- \\[ado-tab-width-change] will change the tab-width for the current buffer.
- \\[ado-toggle-flag] which asks for the name of a flag to toggle. Even
    easier: use the Options... submenu of the Ado-mode menu..
Moving about indenting
- \\[ado-indent-buffer] will re-indent the whole buffer.
Things for special Stata manipulations
- \\[ado-beginning-of-command] will move the point back to the beginning
    of the current command. If in the whitespace between two commands, it will
    move to the start of the next command.
- \\[ado-split-line] will split a long line in two using either the /* */ or ///
    style comments, depending on the value of ado-use-modern-split-flag (which
    defaults to on, implying the use of ///).
- \\[ado-foreach-loop] will insert a foreach loop, asking in the minibuffer
    for the particulars.
- \\[ado-forvalues-loop] will insert a forvalues loop, asking in the minibuffer for the particulars.
- \\[ado-insert-new-program] puts a new subprogram at the bottom of the current
    buffer for use within the current ado file.
- \\[ado-macify-selection-or-word] will turn the current selection or the word containing the point into a local macro.
- \\[ado-strmacify-selection-or-word] will turn the current selection or the word containing the point into a local macro enclosed in full string qualification.
- \\[ado-stringify-selection] will enclose the current selection with full string qualification.

Here an esoteric command which I've not yet documented well.
- \\[ado-new-label] will make a new label file useful for storing commonly
    used value labels.

Here are commands for sending code to Stata.
- \\[ado-send-to-stata]

Most all of the commands are accessible from the ado-mode menu.

If you also use ESS (Emacs Speaks Statistics), but you would rather
use this ado-mode to code Stata, include the following in your .emacs
file:

 (setq auto-mode-alist 
      (append (list '(\"\\\\.ado\\\\'\" . ado-mode)
		    '(\"\\\\.do\\\\'\"  . ado-mode)
		    )
	      auto-mode-alist
	      ))

This will make ado-mode load when you open an ado or do file."
;; standard variables for any mode
  (interactive)
  (kill-all-local-variables)
  (use-local-map ado-mode-map)
  (define-abbrev-table 'ado-mode-abbrev-table ())
  (setq local-abbrev-table ado-mode-abbrev-table)
  (set-syntax-table ado-mode-syntax-table)
  (make-local-variable 'ado-return-also-indents-flag)
  (ado-set-return ado-return-also-indents-flag)
  ;; indentation and paragraph definitions
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'ado-indent-line)
;  (make-local-variable 'indent-region-function)
;  (setq indent-region-function 'ado-indent-function)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  ;; comment definitions
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (make-local-variable 'comment-column)
  (setq comment-column ado-comment-column)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'ado-comment-indent)
  (make-local-variable 'comment-multi-line)
  (setq comment-multi-line nil)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  ;; make local copies of all the style stuff
  (make-local-variable 'ado-smart-indent-flag)
  (make-local-variable 'ado-comment-column)
  (make-local-variable 'ado-tab-width)
  (setq tab-width ado-tab-width)
  (make-local-variable 'ado-confirm-overwrite-flag)
  (make-local-variable 'ado-use-modern-split-flag)
  (make-local-variable 'ado-delimit-indent-flag)
  (make-local-variable 'ado-delimit-indent-column)
  (make-local-variable 'ado-comment-indent-flag)
  (make-local-variable 'ado-comment-indent-column)
  (make-local-variable 'ado-debugging-indent-flag)
  (make-local-variable 'ado-debugging-indent-column)
  (make-local-variable 'ado-fontify-new-flag)
  (make-local-variable 'ado-close-under-line-flag)
  (make-local-variable 'ado-continued-statement-indent-spaces)
  (make-local-variable 'ado-auto-newline-flag)
  (make-local-variable 'ado-closing-brace-alone-flag)
  (make-local-variable 'ado-comeback-flag)
  (make-local-variable 'ado-submit-default)
  ;; delete auto-save-file when file is saved for real
  (make-local-variable 'delete-auto-save-files)
  (setq delete-auto-save-files t)
  (use-local-map ado-mode-map)
  (setq mode-name "Ado")
  (setq major-mode 'ado-mode)
  ;; make sure function ends with lf
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'font-lock-defaults)
  (ado-set-font-lock-keywords)
  (setq font-lock-defaults '(ado-font-lock-keywords))
  (make-local-variable 'font-lock-syntactic-keywords)
  (ado-set-font-lock-syntactic-keywords)
  ;; make local copy of the extension, and try to guess the extension
  (make-local-variable 'ado-extension)
  (setq ado-extension (ado-find-extension))
  ;; setup directories which could be needed
  (unless ado-mode-home
	(setq ado-mode-home
		  (file-name-as-directory (expand-file-name (concat (file-name-directory (locate-file "ado-mode.el" load-path)) "..")))))
  (unless ado-site-template-dir
	(setq ado-site-template-dir (file-name-as-directory (concat ado-mode-home "templates"))))
  (unless ado-script-dir 
	(setq ado-script-dir (file-name-as-directory (concat ado-mode-home "scripts"))))
  (if ado-smart-indent-flag
      (if (or 
			  (string= ado-extension "hlp")
			  (string= ado-extension "sthlp")
			  (string= ado-extension "dlg")
			  (string= ado-extension "idlg"))
			 (setq ado-smart-indent-flag nil)
		  (if (string= ado-extension "do")
				(setq ado-smart-indent-flag ado-do-indent-flag)
			 )
		  )
    )
  ;; not a good idea --- since it is a bit heavy handed for custom indentations
  ;; (if ado-smart-indent-flag
  ;;    (ado-indent-buffer))
  (run-mode-hooks 'ado-mode-hook))

; ado-set-return == t -> swap ret and C-j
(defun ado-set-return (state)
  (if state
      (progn
		(define-key ado-mode-map "\C-m" 'ado-newline) 
		(define-key ado-mode-map "\C-j" 'newline) 
		)
    (define-key ado-mode-map "\C-j" 'ado-newline) 
    (define-key ado-mode-map "\C-m" 'newline) 
    ))

;;;; all the style-toggles for local resets rather than global

(defun ado-return-toggle ()
  (interactive)
  (setq ado-return-also-indents-flag (not ado-return-also-indents-flag))
  (ado-set-return ado-return-also-indents-flag))

(defun ado-toggle-flag (flag-name)
  (interactive "vWhat flag would you like to toggle? ")
  (set flag-name (not (eval flag-name))))

;;;; all the style value changers for local changes
;; a function which makes all the prompts and messages look the same.
(defun ado-change-number (variable newvalue)
  "For changing options which have numerical values somewhat nicely. Does
not work if fed an expression."
  (interactive "vWhat variable would you like to change? 
i")
  (if (or (null newvalue)
	  (eq newvalue 'ask))
      (progn 
	(setq newvalue (read-from-minibuffer (concat "Change " (symbol-name variable) " to ")  (number-to-string (eval variable) )))
	(if (or
	     (string= newvalue "")
	     (= (setq newvalue (string-to-number newvalue)) (eval variable)))
	    (progn
	      (message (concat "value of " (symbol-name variable) " left unchanged."))
	      nil)
	  (set variable (eval newvalue))
	  (message (format (concat "value of " (symbol-name variable) " set to %d.") (eval variable)))
	  t))
    (set variable (eval newvalue))
    t
    ))

(defun ado-tab-width-change (&optional new-tab-width)
"Changes the tab-width for the current buffer, and then optionally re-indents the file."
  (interactive)
  (if (not (ado-change-number 'tab-width new-tab-width))
      ()
    (if (y-or-n-p "Reindent buffer now? ")
	(progn
	  (save-excursion)
	  (ado-indent-buffer))
      )))

(defun ado-continued-statement-indent-spaces-change (&optional spaces)
"Changes the tab-width for the current buffer, and then optionally re-indents the file."
  (interactive)
  (if (not (ado-change-number 'ado-continued-statement-indent-spaces spaces))
      ()
    (if (y-or-n-p "Reindent buffer now? ")
	(progn
	  (save-excursion)
	  (ado-indent-buffer))
      )))

;;; scunged from the c-mode indentation
(defun ado-comment-indent ()
  (if (looking-at "^/\\*")
      0				;Existing comment at bol stays there.
    (let ((opoint (point)))
      (save-excursion
	(beginning-of-line)
	(cond ((looking-at "[ \t]*}[ \t]*\\($\\|/\\*\\)")
	       ;; A comment following a solitary close-brace
	       ;; should have only two spaces.
	       (search-forward "}")
	       (+ 2 (current-column)))
	      ((or (looking-at "^#[ \t]*endif[ \t]*")
		   (looking-at "^#[ \t]*else[ \t]*"))
	       7)			;2 spaces after #endif
	      ((progn
		 (goto-char opoint)
		 (skip-chars-backward " \t")
		 (and (= comment-column 0) (bolp)))
	       ;; If comment-column is 0, and nothing but space
	       ;; before the comment, align it at 0 rather than 1.
	       0)
	      (t
	       (max (1+ (current-column))	;Else indent at comment column
		    comment-column)))))))	; except leave at least one spaces.

(defun ado-continuation-indent ()
  "Indents continuation characters to a the \\[ado-continuation-column]. 
If there is no continuation on the current line, inserts the proper
continuation characters."
  (interactive)
  (let (cont-string cont-length (found-it t))
	 (if ado-use-modern-split-flag
		  (setq cont-string "///")
		(setq cont-string "/*"))
	 (setq cont-length (length cont-string))
	 (end-of-line)
	 (skip-chars-backward " \t")
	 (if ado-use-modern-split-flag
		  (if (re-search-backward "[ \t]+///" (point-at-bol) t)
				(skip-chars-forward " \t")
			 (setq found-it nil))
		(if (and (re-search-backward "[ \t]+/\\*" (point-at-bol) t)
					(not (re-search-forward "\\*/" (point-at-eol) t)))
			 (skip-chars-forward " \t")
		  (setq found-it nil)
		  (end-of-line)))
	 (unless found-it
		(unless (looking-back "[ \t]+")
		  (insert " "))
		(insert cont-string)
		(forward-char (- cont-length)))
	 (indent-to-column (max (1+ (current-column)) comment-column))
	 (forward-char cont-length)))
	 

;; useful things which are better than keyboard macros
(defun ado-parse-loop ()
  (interactive)
  (error "This is out of date! Use a foreach loop (\\[ado-foreach-loop]), instead"))

(defun ado-foreach-loop (&optional macname listtype)
"Inserts a foreach loop, after asking for the type of loop to insert."
  (interactive)
  (if (not macname)
      (setq macname (read-from-minibuffer "What local macro should hold the tokens? ")))
  (if (not listtype)
      (setq listtype (read-from-minibuffer "What type of list? (leave blank if not special) ")))
  (insert (concat "foreach " macname))
  (if (equal listtype "")
      (insert " in \"\"")
    (insert (concat " of " listtype "  ")))
  (ado-insert-with-lfd " {")
  (newline-and-indent)
  (insert "}")
  (search-backward "{")
  (backward-char 2))

(defun ado-forvalues-loop (&optional macname range)
"Inserts a forvalues loop, after asking for the range to insert."
  (interactive)
  (if (not macname)
      (setq macname (read-from-minibuffer "What local macro should hold the tokens? ")))
  (while (not range)
      (setq range (read-from-minibuffer "What numlist? ")))
  (ado-indent-line)
  (insert (concat "forvalues " macname " = " range ))
  (ado-insert-with-lfd " {")
  (ado-indent-line)
  (save-excursion 
	(newline-and-indent)
	(insert "}"))
  )

(defun ado-new-generic (type exten srcstr &optional stayput name purpose cusblp)
  "Allows overloading the new program to work for ado, class, do, mata and other files"
  (unless name
	(setq name (read-from-minibuffer (concat "What is the name of the " type "? "))))
  (unless purpose
	(setq purpose (read-from-minibuffer "What does it do? ")))
  (switch-to-buffer
   (generate-new-buffer
    (generate-new-buffer-name (concat name "." exten))))
  (ado-mode)
  (if cusblp
	  (ado-insert-boilerplate cusblp nil t)
	(ado-insert-boilerplate (concat exten ".blp")))
  (if (and ado-new-dir (not stayput) (not (string= type "do-file")))
      (if (y-or-n-p "Put in 'new' directory? ")
	  (cd (directory-file-name ado-new-dir))))
  (ado-mode)
  (goto-char (point-min))
  (end-of-line)
  (insert (ado-nice-current-date))
  (search-forward "*!")
  (end-of-line)
  (insert purpose)
  (if srcstr
	  (if (string= "do-file" type)
		  (progn
			(while (search-forward srcstr nil t)
			  (replace-match name))
			(goto-char (point-min))
			;; awful hack
			(re-search-forward "^clear all")
			(forward-char))
		(search-forward srcstr)
		(forward-char)
		(insert name)))
  (re-search-forward "\t" nil t)
  (if ado-fontify-new-flag (turn-on-font-lock))
  (if (or
	   (string= type "ado")
	   (string= type "class")
	   (string= type "hlp"))
	  (ado-save-program)
	(ado-save-program (concat name "." exten)))
  )

(defun ado-new-do (&optional stayput name purpose)
  "Makes a new do-file by inserting the file do.blp from the template
directory. The do-file is made to keep its own named log so that it
can be called by other do-files."
  (interactive)
  (ado-new-generic "do-file" "do" "putNameHere" stayput name purpose))

(defun ado-new-mata (&optional stayput name purpose)
  "Makes a new buffer by inserting the file mata.blp from the template
directory. Inserts the proper name for the new class and the class file
itself. Asks if the file should be saved in the `new' directory. If the
answer is no, the file will be saved in the current working directory.
Bound to \\[ado-new-mata]"
  (interactive)
  (ado-new-generic "mata file" "mata" nil stayput name purpose))

(defun ado-new-class (&optional stayput name purpose)
  "Makes a new buffer by inserting the file class.blp from the template
directory. Inserts the proper name for the new class and the class file
itself. Asks if the file should be saved in the `new' directory. If the
answer is no, the file will be saved in the current working directory.
Bound to \\[ado-new-class]" 
  (interactive)
  (ado-new-generic "class" "class" "class" stayput name purpose)
  )

(defun ado-new-program (&optional stayput name purpose)
"Makes a new buffer by inserting the file ado.blp from the template
directory. Inserts the proper name for the new command and the ado file
itself. Asks if the file should be saved in the `new' directory. If the
answer is no, the file will be saved in the current working directory.
Bound to \\[ado-new-program]" 
  (interactive)
  (ado-new-generic "program" "ado" "program define" stayput name purpose))

(defalias 'ado-new-ado 'ado-new-program)

(defun ado-marker-program ()
  (interactive)
  (let
      ((long-name (read-from-minibuffer "What is the name of the condition? "))
       (short-name (read-from-minibuffer "What is the default name of the marker? "))
       program-name)
    (setq program-name
	  (concat "_mk" (substring short-name 0 (min 5 (length short-name)))))
    (ado-new-program nil program-name
		       (concat "Generates default marker " short-name " for the condition " long-name))
    (ado-insert-boilerplate "markit.blp")
    (re-search-forward "\"\"")
    (forward-char -1)
    (insert long-name)
    (re-search-forward "()")
    (forward-char -1)
    (insert short-name)))

(defun ado-project-program ()
  (interactive)
  (ado-new-program)
  (ado-insert-boilerplate "proj1.blp")
  (goto-char (point-max))
  (forward-line -2)
  (ado-insert-slog-block)
  (ado-insert-boilerplate "proj2.blp")
  (goto-char (point-min))
  (search-forward "stages \"")
  (recenter))

(defun ado-stage-program ()
  (interactive)
  (ado-new-program t)
  (ado-insert-slog-block "replace")
  (ado-insert-boilerplate "stage.blp")
  ;(search-forward "tmp" nil t)
  )

(defun ado-lookup-program ()
  (interactive)
  (ado-new-program)
  (ado-insert-boilerplate "lookup.blp")
  (goto-char (point-min))
  (search-forward "using \""))

(defun ado-checker-setup ()
  (interactive)
  (let ((var-name (read-from-minibuffer "What is the name of the variable? "))
	err-name)
    (ado-new-program var-name nil (concat "Error checker for " var-name))
    var-name))

(defun ado-checker-err-name (var-name)
  (concat "err" (substring var-name 0 (min 5 (length var-name)))))

(defun ado-ckmiss-program (&optional name)
  (interactive)
  (let ((var-name (ado-checker-setup))
	err-name)
    (setq err-name (ado-checker-err-name var-name))
    (ado-insert-boilerplate "ckmiss.blp")
    (while (search-forward "foovar" nil t)
      (replace-match var-name nil t))
    (while (search-forward "fooerr" nil t)
      (replace-match err-name nil t))
    ))

(defun ado-ckdunno-program (&optional name)
  (interactive)
  (let ((var-name (ado-checker-setup)))
    (ado-insert-boilerplate "ckdunno.blp")
    (while (search-forward "foovar" nil t)
    (replace-match var-name nil t))
    ))

(defun ado-checker-program ()
  (interactive)
  (let ((var-name (ado-checker-setup))
	err-name)
    (setq err-name (ado-checker-err-name var-name))
    (ado-insert-boilerplate "check.blp")
    (while (search-forward "foobar" nil t)
      (replace-match var-name nil t))
    (goto-char (point-min))
    (while (search-forward "errfoobar" nil t)
      (replace-match err-name nil t))
    (goto-char (point-min))
    (search-forward "=")
    ))
  
(defun ado-insert-new-program (&optional name purpose)
  "Inserts a subprogram at the bottom of the current buffer. There is
something broken in that the insertion point is left in the wrong spot..."
  (interactive)
  (if (not name)
      (setq name (read-from-minibuffer "What is the name of the program? ")))
  (goto-char (point-max))
  (ado-insert-boilerplate "smallado.blp")
  (search-forward "program define")
  (end-of-line)
  (insert name)
  (search-forward "	"))

(defun ado-new-label (&optional name)
  "Grab the boilerplate for a label and name the buffer"
  (interactive)
  (if (not name)
      (setq name (read-from-minibuffer "What is the name of the label? ")))
  (switch-to-buffer
   (generate-new-buffer
    (generate-new-buffer-name (concat name ".lbl"))))
  (ado-insert-boilerplate "lbl.blp")
  (if (and ado-label-dir (y-or-n-p "Put in local 'lbl' directory? "))
      (cd (directory-file-name ado-label-dir))
    (pwd))				;put in to avoid troubles later!
  (ado-mode)
  (goto-char (point-min))
  (re-search-forward "def ")
  (insert name)
  (forward-char 1))

(defun ado-write-file-as-buffer-name ()
  "Takes care of the problem in emacs where a buffer can have its name 
changed, but will write itself under it's regular filename."
  (interactive)
  (let (this-buffer)
    (setq this-buffer (buffer-name))
    (if (string-match "*" this-buffer)
	(save-buffer)
      (write-file (substring this-buffer 0 (string-match "<" this-buffer)) ado-confirm-overwrite-flag)
      ))
  )

(defun ado-save-program (&optional filename)
  "Saves file, doing its best to make a good file name and timestamp, if 
needed. The command works differently depending on the type of file:

  .ado files: looks for the first 'program define' statement, and
    attaches the extension .ado. This means that there is no worry
    about the program name and file name being different. *Note* that
    the 'program define' statement may be abbreviated according to
    command rule, but must be at the start of a line. Also: it must be
    the first program defined in the .ado file.

  .class files: looks for the first 'class' statement, and attaches
    the extension .class. This ensures that the class name and the
    class filename match.

  .hlp files: looks for first {hi:help ...} or {cmd:help ...}, and takes
    file name from the next chunk.

  all other Stata-related files: These do not need anything within the
    file to match the file name in order to run properly. So... if the
    file starts with *! in the first line, ado-mode will check
    lines starting with *!'s at the top of the program for something
    approaching a filename with the proper extension. If it is found,
    it will be used as the file name.

If none of these tags are found, then the program tries to save
the file as it's buffer name (without the <#> garbage).

Timestamps are updated only if there is a '*! version xxx'
statement in ado files, a {* Last Updated: } or a {* <date>}{...}
in sthlp (or hlp) files."

  (interactive)
  (save-excursion
	(setq ado-extension (ado-find-extension))
	(if ado-update-timestamp-flag
		(ado-update-timestamp))
    (goto-char (point-min))
    ;; if file name specified, this is just a write-file
    (if filename
		  (write-file filename ado-confirm-overwrite-flag)
		;; try to get new name
		(setq filename (ado-make-ado-name))
		(if filename
			 (if (string= (concat default-directory filename) (buffer-file-name))
				  (save-buffer)
				(write-file filename ado-confirm-overwrite-flag)
				)
		  (save-buffer)))))
		
(defun ado-update-timestamp ()
  "Tries to do a nice job updating a timestamp for the file. Since
Stata has no conventions about headers for files, ado-mode will:

Look for a '*! version xxx' statement in ado/do/ files, a {
* version xxx } or a {smcl}<newline>{* <date>}{...} in sthlp (or hlp)
files, or a version x.y.z <date> in other files."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (or (string= ado-extension "ado") 
			(string= ado-extension "class") 
			(string= ado-extension "do")) 
		(if (re-search-forward "^\*![ \t]+version[ \t]+[0-9\.]*[ \t]*" (point-max) t)
			(progn
			  (delete-region (point) (point-at-eol))
			  (insert (ado-nice-current-date))
			  ))
      (if (or
		   (string= ado-extension "hlp")
		   (string= ado-extension "sthlp"))
		  (if (search-forward "\[*] Last Updated: " (point-max) t)
			  (progn
				(delete-region (point) (point-at-eol))
				(insert (ado-nice-current-date))
				(insert "}{...}"))
			(if (re-search-forward "^[ \t]*{[*]+[ \t]+[*]![ \t]+version[ \t]+[0-9\.]*[ \t]*" (point-max) t)
				(progn
				(delete-region (point) (point-at-eol))
				  (insert (ado-nice-current-date))
				  (insert "}{...}"))
			  (if (looking-at "{smcl}[ \t]*")
				(progn
				  (goto-char (match-end 0))
				  (forward-char)
				  (if (looking-at "{[*] *")
					  (progn 
						(goto-char (match-end 0))
						(delete-region (point) (point-at-eol))
						(insert (ado-nice-current-date))
						(insert "}{...}")))))))
	;;; not in ado or help file
		(if (re-search-forward "^\\(\[*]!\\)*[ \t]+[Vv][Ee][Rr][Ss][Ii][Oo][Nn][ \t]+[0-9\.]*[ \t]*" (point-max) t)
			(progn
			  (delete-region (point) (point-at-eol))
			  (insert (ado-nice-current-date))
			  ))))))
	
    ;; looking for a version number, so that the date stamp can be updated
;; leaves the new date and time, even if the file is not saved... dunno what
;; to do to fix this, since I cannot find error trapping for errors which do
;; not use 'signal to identify the errors (write-file does not use a signal)

(defun ado-set-ado-extension ()
  "This function is now considered obsolete. Try using \\[ado-find-extension] 
instead, which returns a value instead of setting a variable."
  (interactive)
  (setq ado-extension (ado-find-extension)))

(defun ado-show-extension ()
  (interactive)
	(message (concat "I think the extension is " (ado-find-extension))
	))

(defun ado-find-extension (&optional message)
  "Decides from the file contents what the extension should be. Since Stata
has started getting more complicated, will fall back to the current file
if confused. Returns its best guess at the extension."
  ;; try to find the name, trust the buffer if lost, but issue warning if
  ;; lost or a contradiction
  (let (first-program first-mata sez-file sez-contents)
	(setq sez-file
		  (if (and buffer-file-name (file-name-extension buffer-file-name))
			  (downcase (file-name-extension buffer-file-name))
			nil))
	(setq sez-contents
		  (save-excursion
			(goto-char (point-min))
			(cond
			 ((looking-at "{smcl}")
			  (if (re-search-forward "^{\\(hi\\|cmd\\):help" nil t) 
				  ado-help-extension
				"smcl"))
;; 			 ((and
;; 			   (looking-at "------")
;; 			   (progn
;; 				 (forward-line)
;; 				 (beginning-of-line)
;; 				 (looking-at "[ \t]*log:")))
;; 			  "log")
			 ((re-search-forward "^class[ \t]+" nil t) "class")
			 ((re-search-forward "^[ \t]*DIALOG" nil t) "dlg")
			 ;; the rest depend on where first occurances are
			 (t
			  (progn
				;; searches through the first 200 characters. this is arbitrary
				(setq first-program (re-search-forward "^pr\\(o\\|\\og\\|\\ogr\\|\\ogra\\|\\ogram\\)[ \t]+\\(de\\(f\\|fi\\|fin\\|fine\\)[ \t]+\\)*" 200 t))
				(setq first-mata (re-search-forward "^[ \t]*mata[ \t]*:[ \t]*$" 200 t))
				(if first-program
					(if first-mata
						(if (< first-program first-mata) "ado" "mata")
					  "ado")
				  (if first-mata "mata" "do"))
				)))))
	;; rule the file extension as correct automatically
	(if sez-file
		(progn
		  (unless (string= sez-file sez-contents)
			(message (concat "ado-mode thinks that the extension should be " sez-contents " even though the current extension is " sez-file "!")))
		  sez-file)
	  sez-contents)
	))
				  
(defun ado-make-ado-name ()
  (interactive)
  "Creates a file name from the contents of the file. Assumes that
ado-extension has been set properly. Returns nil if the name of the file
cannot be determined from the file contents."
  (let (name-start name-end)
    (save-excursion
      (goto-char (point-min))
		(setq name-start
				(cond ((string= ado-extension "class") 
						 (re-search-forward "^class[ \t]+" nil t))
						((string= ado-extension "ado")
						 (re-search-forward "^pr\\(o\\|\\og\\|\\ogr\\|\\ogra\\|\\ogram\\)[ \t]+\\(de\\(f\\|fi\\|fin\\|fine\\)[ \t]+\\)?" nil t))
						((or (string= ado-extension "hlp")
							 (string= ado-extension "sthlp"))
						 (re-search-forward "^{\\(hi\\|cmd\\):help[ \t]+" nil t))
						((string= ado-extension "lbl")
						 (re-search-forward "^[ \t]*^la\\(b\\|be\\|bel\\)[ \t+]+de\\(f\\|fi\\|fin\\|fine\\)[ \t]+" nil t))
						(t nil)))
		(if (not name-start)
			 (buffer-file-name)
		  (re-search-forward "[a-zA-Z_]+[a-zA-Z0-9_]*\\b")
		  (setq name-end (point))
		  (concat (buffer-substring-no-properties name-start name-end) "." ado-extension))
		)))

(defun ado-find-local-name ()
  "Returns the name of the defining program in which the point is sitting. If the
program define statement cannot be found, returns nil. Returns a mess if the
program name is missing (need to fix this). Currently broken --- need to assess need."
  (let (name-start name-end)
    (save-excursion
      (if (re-search-backward "^pr\\(o\\|\\og\\|\\ogr\\|\\ogra\\|\\ogram\\)[ \t]+" 0 t) ;goes to most recent definition
	  (progn
	    (setq name-start (re-search-forward 
			"^pr\\(o\\|\\og\\|\\ogr\\|\\ogra\\|\\ogram\\)[ \t]+\\(de\\(f\\|fi\\|fin\\|fine\\)[ \t]+\\)*" nil t)
		  name-end (re-search-forward "[a-zA-Z_]+[a-zA-Z0-9_]*\\b"))
	    (buffer-substring-no-properties name-start name-end))
	))))

(defun ado-show-local-name ()
  (interactive)
  (message (concat "The local program is " (ado-find-local-name))))

;; (defun ado-insert-boilerplate (file-name &optional raw)
;;   (if (not ado-site-template-dir)
;;       (error "%s" "Use \\[set-variable] to set ado-site-template-dir to the directory holding the ado templates!"))
;;   (insert-file-contents (concat ado-site-template-dir file-name))
;;   (if (not raw)
;;       (ado-indent-region)
;;     ))

(defun ado-insert-file-and-indent (file)
  "Interactively insert (and properly indent) an ado file.
Can also be called from within another program for inserting
and indenting"
  (interactive "fGive the file to insert: ")
  (let ((beg (point)))
    (insert-file-contents file)
    (ado-indent-region beg (point))
    ))

(defun ado-insert-boilerplate (file-name &optional raw full-path)
  (if full-path
	  (setq full-path file-name)
	(if (not ado-site-template-dir)
		(error "%s" "Use \\[set-variable] to set ado-site-template-dir to the directory holding the ado templates!"))
	(setq full-path (concat (file-name-as-directory ado-site-template-dir) file-name)))
  (if raw
      (insert-file-contents full-path)
    (ado-insert-file-and-indent full-path)))


(defun ado-insert-slog-block (&optional replace-flag)
  (interactive)
  (let ((the-name (ado-find-local-name)))
    (ado-insert-boilerplate "slog.blp")
    (re-search-forward "\"\"")
    (forward-char -1)
    (if the-name
	(insert the-name)
      (error "Could not determine the name of the defining program"))
    (if replace-flag
	(progn
	  (re-search-forward "append")
	  (forward-word -1)
	  (kill-word 1)
	  (insert "replace")))
    (re-search-forward "{")
    (newline-and-indent)
    ))

;;; depth and indentation commands
(defun ado-line-starts-with-end-comment ()
  (interactive)
  (save-excursion
  (beginning-of-line)
  (and (search-forward "*/" (point-at-eol) t)
       (not (search-backward "/*" (point-at-bol) t)))))

(defun ado-eraseme ()
  (interactive)
  (if (looking-at "end")
      (message "At end")
    (message "Not at end")))

(defun ado-out-of-nested-comment (&optional top from-level)
  (interactive)
  (let ((ppsexp (parse-partial-sexp 1 (point))) this-level )
    (if (numberp (setq this-level (nth 4 ppsexp)))
	(if (or (not from-level) (and (<= from-level this-level)))
	    (if (search-backward "/*" 1 t)
		(if top (ado-out-of-nested-comment t)
;		  (forward-char -1)
		  (ado-out-of-nested-comment nil this-level)
		  ))
;	  (if (not top) (forward-char 1))
	  )
      (if top
	  (if (search-backward "*/" (point-at-bol) t)
	      (ado-out-of-nested-comment top)
	    )))))

(defun ado-show-depth ()
  (interactive)
  (let ((depth (ado-find-depth)))
    (message (concat "The depth was " (number-to-string (car depth))
		     (if (nth 1 depth) " with continuation" " without continuation")))))

;; changed left-indenting of 'version' to left-indent iff spelled out
;;   to avoid the indenting, use an abbreviation
(defun ado-find-depth ()
  (let (depth start ppsexp in-continuation (oddend (ado-line-starts-with-end-comment)))
    (save-excursion
      ;; look for line starting with a comment
      (setq in-continuation (ado-beginning-of-command))
      (setq ppsexp (parse-partial-sexp 1 (point)))
      (setq depth (car ppsexp))
      (setq start (point))
      ;; oddities which might need unindenting
      (when (or oddend
				(and (not ado-close-under-line-flag) (looking-at "}"))
				(looking-at "ver\\(s\\|si\\|sio\\|sion\\)"))	  
		(setq depth (1- depth)))
	;; *! should be before the first prgram define
	;;		  (looking-at "*!")
	;; (looking-at "pause")) really should be slammed left
;;	(setq depth (1+ depth))))
	  (end-of-line)
	  (setq depth (- depth (how-many "^[ \t]*\\(end$\\|end[ \t]+\\)" 1 (point))))
	  ;;      (setq depth (- depth (how-many "^[ \t]*end$" 1 (point))))
	  (beginning-of-line)
      ;; words which start blocks
	  ;; need to be careful, because of program dir, drop, and list
      (setq depth (+ depth (how-many "^[ \t]*\\(\\(input\\|p\\(r\\|ro\\|rog\\|rogr\\|rogra\\|rogram\\)\\)[ \t]+\\|mata:[ \t]*$\\)" 1 (point))))
		(setq depth (- depth (how-many "^[ \t]*p\\(r\\|ro\\|rog\\|rogr\\|rogra\\|rogram\\)[ \t]+\\(d\\(i\\|ir\\)[ \t]*$\\|\\(drop\\|l\\|li\\|lis\\|list\\)[ \t]*\\)" 1 (point))))
      ;; words which end blocks
      (setq ppsexp (parse-partial-sexp start (point)))
      (if (numberp (nth 4 ppsexp))
	  (list (+ depth (nth 4 ppsexp)) in-continuation)
	(list depth in-continuation)))
      ))

(defun ado-indent-region (&optional start end)
  (interactive)
  (let (endmark)
    (if (and (null start) (null end))
	(progn
	  (setq start (min (point) (mark)))
	  (setq end (max (point) (mark))))
      )
    (save-excursion
      (goto-char start)
      ;; Advance to first nonblank line.
      (beginning-of-line)
      (setq endmark (copy-marker end))
      (while (and (bolp) (not (eobp)) (< (point) endmark))
	(skip-chars-forward " \t\n")
	(ado-indent-line)
	(forward-line 1)
      ))))

(defun ado-indent-buffer ()
  (interactive)
  (save-excursion
    (ado-indent-region (point-min) (point-max))))

(defun ado-indent-line ()
  "A smart indenter for ado files. Many of the parameters can
be customized using '\\[customize-group] ado-mode'."
  (interactive)
  (if ado-smart-indent-flag
      (let (indent depth beg shift-amt endmark
	    (pos (- (point-max) (point)))
	    (watch-for-semi (ado-delimit-is-semi)))
	(beginning-of-line)
	(setq beg (point))
	(cond ((and ado-delimit-indent-flag (looking-at "[ \t]*#d\\(e\\|el\\|eli\\|elim\\|elimi\\|elimit\\)"))	;#delimits belong at delimit indent
	       (setq indent ado-delimit-indent-column))
	      ((and ado-comment-indent-flag
		    (or (looking-at "^\\*") (looking-at "^*")))	;comments at start of line belong at comment indent
	       (setq indent ado-comment-indent-column))
	      ((and ado-debugging-indent-flag
		    (or (looking-at "^[ \t]*pause")	
			(looking-at "^[ \t]*set t\\(r\\|ra\\|rac\\|race\\)[ \t]+")))
	       (setq indent ado-debugging-indent-column)) ; debugging at proper column (usually 0)
	      (t (setq indent (* tab-width (car (setq depth (ado-find-depth)))))   ; regular indentation
		 (if (nth 1 depth)
		     (setq indent (+ indent ado-continued-statement-indent-spaces)))
		 ))						  ; end of conditional statement
	(skip-chars-forward " \t")
	(setq shift-amt (- indent (current-column)))
	(if (zerop shift-amt)
	    (if (> (- (point-max) pos) (point))
		(goto-char (- (point-max) pos)))
	  (delete-region beg (point))
	  (indent-to indent)
	  ;; If initial point was within line's indentation,
	  ;; position after the indentation.  Else stay at same point in text.
	  (if (> (- (point-max) pos) (point))
	      (goto-char (- (point-max) pos))))
	shift-amt)))

(defun ado-delimit-is-semi ()
  "Returns t if semicolons delimit commands, otherwise returns nil."
  (save-excursion
    ;; if #delimit command is on same line, the delimiter is always cr
    (let ((line-start (point-at-bol)))
      (if (re-search-backward "#d\\(e\\|el\\|eli\\|elim\\elimi\\elimit\\)" 1 t)
	  (let ((ppsexp (parse-partial-sexp 1 (point))))
	    (if (or 
		 (nth 3 ppsexp)		; inside a string
		 (nth 4 ppsexp)		; inside a non-nestable comment
		 (nth 7 ppsexp))		; inside a type-b comment
		(ado-delimit-is-semi)
	      (if (>= (point) line-start)
		  nil
		(forward-sexp)
		(skip-chars-forward " \t")
		(looking-at ";"))))))))

(defun ado-show-delimiter ()
  "Returns the value of the delimiter in ado-functions as a message."
  (interactive)
  (message (if (ado-delimit-is-semi)
	       "The delimiter is ;"
	     "The delimiter is cr"
	     )))

(defun ado-beginning-of-command ()
  "Moves the cursor to the start of the command in which the insertion
point is sitting. This will jump back to the start of a command if the
insertion point is within the command, and jump forward to the start
of a command if the delimiter is in whitespace preceding a command.

When the delimiter is cr, blank lines count as empty commands, so the
cursor will stay put.

When the delimiter is ; even being just after a ; will mean that
the cursor is after the command, and will cause a jump forward. This is
technically correct, even if a bit disturbing.

This is known to have trouble when there are bizarre constructions, such
as /* */-style comments in the middle of a line, or something perverse like
   regress mpg // this is a comment /// with a fake continuation
     weight
Fixing this might or might not happen in the future.

Returns t if inside of a continued function, nil otherwise."
  (interactive)
  (let ((in-continuation nil) (start-line (line-number-at-pos)) (skip-chars " \t"))
    (ado-out-of-nested-comment t)
    ;; first look backwards for either delimiter or delimit command
	(if (= start-line 1)
		(beginning-of-line)
	  (unless (search-backward "#delimit" (point-at-bol) t)
		(if (ado-delimit-is-semi)
			(progn
			  (setq skip-chars " \t\n")
			  (search-backward ";" 1 t)
			  (if (and
				   (< (line-number-at-pos) start-line)
				   (save-excursion
					 (skip-chars-forward " \t\n;")
					 (< (line-number-at-pos) start-line)))
				  (setq in-continuation t)))
		  (search-backward "\n" 1 t))
		(unless (bobp)
		  (if (re-search-backward "\\(\\s-\\|^\\)///+.*$" (point-at-bol) t)
			  (progn
				(forward-char -1)
				(ado-beginning-of-command)
				(setq in-continuation t))
			(forward-char 1)))))
	  ;;    (skip-chars-forward " \t\n")
    (skip-chars-forward skip-chars)
    in-continuation
    ))

(defun ado-end-of-command ()
  "Move to the end of the current command. This can be fooled
by /* */-style commands extending across lines."
  (interactive)
  (let (ppsexp)
    (if (ado-delimit-is-semi)
		(if (not (search-forward ";" nil t))
			(error "No end of command") 
		  (setq ppsexp (parse-partial-sexp 1 (point)))
		  (if (or (nth 4 ppsexp) (nth 3 ppsexp) (nth 7 ppsexp))
			  (ado-end-of-command)
			(backward-char)))
      (if (re-search-forward "/\\*" (point-at-eol) t)
		  (progn
			(backward-char 2)
			(skip-syntax-backward "-"))
		;; look for continuation
		(if (or (search-forward "///" (point-at-eol) t)
				(search-backward "///" (point-at-bol) t))
			(progn
			  (end-of-line)
			  (if (not (< (point) (point-max)))
				  (error "No end of command - moved as far as possible")
				(forward-char)
				(ado-end-of-command)))
		  (if (not (search-forward "//" (point-at-eol) t))
			  (end-of-line)
			(backward-char 2)
			(skip-syntax-backward "-")))
		))))

(defun ado-copy-command (&optional asString)
  "Copies the command in which point is sitting to the clipboard.
May be called interactively, but really meant for using within items
working on regions."
  (interactive)
  (let ((start-here
		 (save-excursion
		   (ado-beginning-of-command)
		   (point)))
		(end-here
		 (save-excursion
		   (ado-end-of-command)
		   (point)))
		 (x-select-enable-clipboard t))
	(if asString
		(filter-buffer-substring start-here end-here nil t)
	  (kill-ring-save start-here end-here))
	))

;; stolen from c-mode, and changed slightly, since Stata does not allow
;; braces on separate lines
(defun electric-ado-closing-brace (arg)
  "Insert closing character, possibly on new line, and correct line's indentation."
  (interactive "P")
  (if (and (not arg)
	   (eolp)
;	   ado-auto-newline-flag
	   ado-closing-brace-alone-flag)
      (if (save-excursion
	    (skip-chars-backward " \t")
	    (not (bolp)))
	(newline-and-indent)))
  (electric-ado-brace arg))

(defun electric-ado-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos)
    (if (and (not arg)
	     (eolp)
	     ado-auto-newline-flag)
	(progn
	  (insert last-command-char)
	  (ado-indent-line)
	  (if ado-auto-newline-flag
	      (progn
		(newline)
		;; (newline) may have done auto-fill
		(setq insertpos (- (point) 2))
		(ado-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun ado-newline ()
  "Justifies current line before doing a newline-and-indent"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (ado-indent-line))
  (newline-and-indent))

(defun ado-split-line ()
  "Splits line at point, putting in the proper continuation
characters, depending on the value of \\[ado-use-modern-split-flag]"
  (interactive)
  (if ado-line-up-continuations
		(progn
		  (newline)
		  (backward-char)
		  (ado-continuation-indent)
		  (forward-char)
		  (ado-indent-line)
		  (unless ado-use-modern-split-flag
			 (insert "*/")))
	 (if ado-use-modern-split-flag
		  (progn
			 (insert " ///")
			 (newline-and-indent))
		(insert "/*")
		(newline-and-indent)
		(insert "*/"))  
	 ))

(defun ado-macify-selection-or-word (&optional stringify)
  "Puts local macro markers `' around the word in which the point sits, leaving point after word. If point is not in a word, simply inserts `'."
  (interactive)
  (let (noregion popmark)
	(unless (and transient-mark-mode mark-active)
;;	(unless mark-active
	  (if (condition-case nil
				 (beginning-of-thing 'word)
			  (error nil))
			(progn
			  (setq popmark 't)
			  (mark-word))
		 (setq noregion 't)))
	(if noregion
		 (progn
			(insert "`'")
			(forward-char -1))
	  
	  (save-excursion (goto-char (region-beginning)) 
							(if stringify (insert "`\""))
							(insert "`"))
	  (goto-char (region-end)) 
	  (insert "'")
	  (if stringify (insert "\"'"))
	  (if popmark	(pop-mark))
	  )))

(defun ado-strmacify-selection-or-word ()
  (interactive)
  (ado-macify-selection-or-word t))

(defun ado-stringify-selection ()
  "Puts full string delimiters around the selection. If nothing is selected, inserts full string delimiters at point."
  (interactive)
  (if (and transient-mark-mode mark-active)
		(progn 
		  (save-excursion (goto-char (region-beginning)) (insert "`\""))
		  (goto-char (region-end)) (insert "\"'"))
	 (insert "`\"\"'")
	 (forward-char -2)))

(defun electric-ado-semi (arg)
  "Puts in extra newline if the semi-colon is the end-of-line delimiter"
  (interactive "P")
  (if (ado-delimit-is-semi)
      (electric-ado-brace arg)
    (self-insert-command (prefix-numeric-value arg))))

;; here comes all the stuff for auto-highlighting!
;;
(defun ado-set-font-lock-keywords ()
;  (make-local-variable 'ado-font-lock-keywords)
  (interactive)
  (setq
   ado-font-lock-keywords
   (list
    ;; an attempt to get nested quotes to work
    (eval-when-compile
      (make-regexps
       '(("`\".*?\"'") ado-string-face t)
       ))
	;; trying to get starting-line comments to work
	(eval-when-compile
	  (make-regexps
	   "^[ \t]*"
	   '(("[*].*") ado-comment-face t)
	   ))
		  
    ;; special highlighting
	 ;; starting a mata program; not allowing comments, though
	 (eval-when-compile
		(make-regexps
		 "^[ \t]*"
		 '(("mata") ado-builtin-harmful-face)
		 '((":") ado-constant-face)
		 "[ \t]*$"
		 ))
    ;; program definitions
    (eval-when-compile
      (make-regexps
       '(("^\\*!.*") ado-builtin-harmful-face)
       ))
    ;; does not force program define for keywords; could look ragged if ado-subcommand-face has a background
    (eval-when-compile
     (make-regexps
      "^[ \t]*"
      '((
			"pr" "pro" "prog" "progr" "progra" "program"
			) ado-builtin-harmful-face)
      '((
			"\\([ \t]+\\(\\(d\\(e\\(f\\|fi\\|fin\\|fine\\)\\|rop\\)\\)\\|\\(l\\|li\\|lis\\|list\\)\\)\\)?"
			) ado-subcommand-face t)
      "[ \t]+"
      '(("[_a-zA-Z.]+[_a-zA-Z0-9]*") ado-builtin-harmful-face t)
      ))
    ;; the program dir thingy
    (eval-when-compile
		(make-regexps
      "\\b"
      '((
			"pr" "pro" "prog" "progr" "progra" "program"
			) ado-builtin-harmless-face t)
      "[ \t]+"
      '((
			"di""dir"
			) ado-subcommand-face t)
      ))
    (eval-when-compile
      (make-regexps
       "\\b"
      '((
			"vers" "versi" "versio" "version"
			) ado-builtin-harmless-face)
      "[ \t]+"
      '(("1[.]0" "2[.]0" "2[.]1" "3[.]0" "3[.]1" "4\\([.]0\\)?" "5\\([.]0\\)?" "6\\([.]0\\)?" "7\\([.]0\\)?" 
			"8\\([.]\\(1\\|2\\)\\)?" 
			"9\\([.]\\(1\\|2\\)\\)?" 
			"10\\([.]\\(0\\|1\\)\\)?"
			"11\\([.]0\\)?"
			) ado-subcommand-face)
      "\\b"
      ))
    (eval-when-compile
      (make-regexps
       "^[ \t]*"
       '(("pause") ado-builtin-harmful-face)
       "[ /t]+"
       '(("off" "on") ado-subcommand-face t)
       "\\b"
       ))
     (eval-when-compile
       (make-regexps
        "^[ \t]*"
        '(("end" "pause"
 	  ) ado-builtin-harmful-face)
	"\\b"
        ))
    ;; delimit command
    (eval-when-compile
      (make-regexps
       "^[ \t]*"
       '((
	  "#d" "#de" "#del" "#deli" "#delim" "#delimi" "#delimit" ) ado-builtin-harmless-face)
       "\\s-*"
       '(("\\(cr\\|;\\)\\s-*$") ado-subcommand-face nil)
       ))
    ;;
    ;; obsolete stuff which appears as OK as subcommands
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("lfit"
			 "sco" "scor" "score") ado-obsolete-face)
       "\\b"
       ))
    ;;
    ;; the cluster commands
    (eval-when-compile
      (make-regexps
       '(("\\bcluster") ado-builtin-harmless-face)
       "[ \t]+"
       '((
			 "dend" "dendr" "dendro" "dendrog" "dendrogr" "dendrogra" "dendrogram"
			 "dir"
			 "k" "km" "kme" "kmea" "kmean" "kmeans" 
			 "kmed" "kmedi" "kmedia" "kmedian" "kmedians" 
			 "list"
			 "note" "notes"
			 "parsedist" "parsedista" "parsedistan" "parsedistanc" "parsedistance" 
			 "query"
			 "stop"
			 "tr" "tre" "tree"
			 ) ado-subcommand-face)
       "\\b"
       ))
    ;; 
    ;; data altering cluster commands
    ;; comment region 2
    (eval-when-compile
      (make-regexps
       '(("\\bcluster") ado-builtin-harmful-face)
       "[ \t]+"
       '((
			 "del" "dele" "delet" "delete" 
			 "drop"
			 "gen" "gene" "gener" "genera" "generat" "generate" 
			 "measures"
			 "rename" "renamevar"
			 "set"
			 "use"
			 ) ado-subcommand-face)
       "\\b"
       ))
    ;; putting together common cluster and clustermat commands
    (eval-when-compile
      (make-regexps
       '(("\\bcluster\\(mat\\)?") ado-builtin-harmless-face)
       "[ \t]+"
       '((
			 "a" "anova"
			 "av" "ave" "aver" "avera" "averag" "average" "averagel" "averageli" 
			 "averagelin" "averagelink" "averagelinka" "averagelinkag" "averagelinkage" 
			 "c" "cent" "centr" "centro" "centroi" "centroid" "centroidl" "centroidli" 
			 "centroidlin" "centroidlink" "centroidlinka" "centroidlinkag" "centroidlinkage" 
			 "co" "com" "comp" "compl" "comple" "complet" "complete" "completel" "completeli" 
			 "completelin" "completelink" "completelinka" "completelinkag" "completelinkage" 
			 "manova" "med" "medi" "media" "median" "medianl" "medianli" "medianlin" 
			 "medianlink" "medianlinka" "medianlinkag" "medianlinkage" 
			 "s" "si" "sin" "sing" "singl" "single" "singlel" "singleli" "singlelin" 
			 "singlelink" "singlelinka" "singlelinkag" "singlelinkage"
			 "ward" "wards" "wardsl" "wardsli" "wardslin" "wardslink" "wardslinka" "wardslinkag" "wardslinkage"
			 "wav" "wave" "waver" "wavera" "waverag" "waverage" "waveragel" 
			 "waverageli" "waveragelin" "waveragelink" "waveragelinka" "waveragelinkag" "waveragelinkage" 
			 ) ado-subcommand-face)
       "\\b"
       ))

	 ;; discrim commands
	 (eval-when-compile
		(make-regexps
		 '(("discrim") ado-builtin-harmless-face)
		 "[ \t]+"
		 '(("knn"
			 "lda"
			 "logistic"
			 "qda"
			 ) ado-subcommand-face)
		 "\\b"
		 ))

	 ;; stpower commands
	 (eval-when-compile
		(make-regexps
		 '((
			 "stpow" "stpowe" "stpower" 
			 ) ado-builtin-harmless-face)
		 "[ \t]+"
		 '((
			 "cox"
			 "exp" "expo" "expon" "expone" "exponen" "exponent" "exponenti" "exponentia" "exponential" 
			 "log" "logr" "logra" "logran" "logrank" 
			 ) ado-subcommand-face)
		 "\\b"
		 ))

;;     ;; set command and its variations
    (eval-when-compile
      (make-regexps
       '(("^[ \t]*s\\(e\\|et\\)") ado-builtin-harmless-face)
       "[ \t]+"
       '(("a" "ad" "ado" "ados" "adosi" "adosiz" "adosize" 
			 "autotabgraphs"
			 "conren\\(?:[ \t]+\\(?:clear\\|sf\\|bf\\|it\\|res\\|resu\\|resul\\|result\\|reset\\|txt\\|text\\|inp\\|inpu\\|input\\|li\\|lin\\|link\\|hi\\|hil\\|hili\\|hilit\\|hilite\\|uloff\\|ulon\\)\\)?"
			 "copycolor[ \t]+\\(?:auto\\|autom\\|automa\\|automat\\|automati\\|automatic\\|asis\\|gs[123]\\)"
			 "dp[ \t]+\\(?:com\\|comm\\|comma\\|per\\|peri\\|perio\\|period\\)"
			 "eolc\\(?:h\\|ha\\|har\\)[ \t]+\\(?:mac\\|unix\\)"
			 "httpproxyhost" "httpproxyport" "httpproxypw" "httpproxyuser"
			 "l" "le" "lev" "leve" "level"
			 "li" "lin" "line" 
			 "lineg" "linega" "linegap" 
			 "lines" "linesi" "linesiz" "linesize" 
			 "log\\(?:t\\|ty\\|typ\\|type\\)[ \t]+\\(?:t\\|te\\|tex\\|text\\|s\\|sm\\|smc\\|smcl\\)"
			 "macgp\\(?:h\\|he\\|hen\\|heng\\|hengi\\|hengin\\|hengine\\)[ \t]+\\(?:qu\\(?:artz\\|ickdraw\\)\\)"
			 "mat" "mats" "matsi" "matsiz" "matsize"
			 "maxdb" "maxiter" "maxvar"
			 "mem" "memo" "memor" "memory"
			 "odbcm\\(?:\\g\\|gr\\)[ \t]+\\(?:iodbc\\|unixodbc\\)"
			 "ob" "obs"
			 "pa" "pag" "page" "pages" "pagesi" "pagesiz" "pagesize" 
			 "printcolor[ \t]+\\(?:auto\\|autom\\|automa\\|automat\\|automati\\|automatic\\|asis\\|gs[123]\\)"
			 "processors"
			 "reventr" "reventri" "reventrie" "reventries" 
			 "revwi\\(?:n\\|nd\\|ndo\\|ndow\\)[ \t]+\\(?:no\\)?float"
			 "scheme" "scrollbufsize"
			 "search\\(?:d\\|de\\|def\\|defa\\|defau\\|defaul\\|default\\)[ \t]+\\(?:all\\|local\\|net\\)"
			 "se" "see" "seed"
			 "smoothsize"
			 "timeout1"
			 "timeout2"
			 "traced" "tracede" "tracedep" "tracedept" "tracedepth" 
			 "traceh" "tracehi" "tracehil" "tracehili" "tracehilit" "tracehilite" 
			 "t\\(?:y\\|yp\\|ype\\)[ \t]+\\(?:double\\|float\\)" 
			 "update_interval"
			 "varlab" "varlabe" "varlabel" 
			 "varlabelpos"
			 "varwi\\(?:n\\|nd\\|ndo\\|ndow\\)[ \t]+\\(?:no\\)?float"
			 )
			ado-subcommand-face t)
       "\\b"
       ))
    ;; set command : on/off settable items
    (eval-when-compile
      (make-regexps
       '(("^[ \t]*s\\(e\\|et\\)") ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "checksum" "fastscroll"
	  "dockable"
	  "dockingg" "dockinggu" "dockinggui" "dockingguid" "dockingguide" "dockingguides"
	  "doublebuffer"
	  "floatresults" "floatwindows"
	  "g" "gr" "gra" "grap" "graph" "graphi" "graphic" "graphics"
	  "httpproxy" 
	  "httpproxya" "httpproxyau" "httpproxyaut" "httpproxyauth" 
	  "locksplit" "locksplitt" "locksplitte" "locksplitter" "locksplitters" 
	  "mo" "mor" "more" 
	  "piccom" "piccomm" "piccomme" "piccommen" "piccomment" "piccomments" 
	  "persistfv" "persistvtopic"
	  "pinnable"
	  "r" "rm" "rms" "rmsg" 
	  "smoothf" "smoothfo" "smoothfon" "smoothfont" "smoothfonts" 
	  "tr" "tra" "trac" "trace"
	  "tracee" "traceex" "traceexp" "traceexpa" "traceexpan" "traceexpand" 
	  "tracei" "tracein" "traceind" "traceinde" "traceinden" "traceindent" 
	  "tracen" "tracenu" "tracenum" "tracenumb" "tracenumbe" "tracenumber" 
	  "traces" "tracese" "tracesep" 
	  "update_prompt" "update_query" "use_atsui_graph" "use_qd_text"
	  "varabbrev"
	  "vir" "virt" "virtu" "virtua" "virtual"
	  "xptheme"
	  )
	 ado-subcommand-face t)
       "[ \t]+"
       '(("off" "on") ado-subcommand-face t)
       ))
       ;;
       ;; set command (with endless options!)
     (eval-when-compile
       (make-regexps
        '(("^[ \t]*set") ado-builtin-harmless-face)
        "[ \t]+"
        '((
 	  "ou" "out" "outp" "outpu" "output"
 	  )
 	 ado-subcommand-face)
        "[ \t]+"
        '((
 	  "e" "er" "err" "erro" "error"
 	  "i" "in" "inf" "info" "infor" "inform" 
 	  "p" "pr" "pro" "proc" 
 	  ) ado-subcommand-face)
	"\\b"
        ))
    ;;
    ;; the set subcommands which appear to be obsolete (which don't show in the Stata Reference)
     (eval-when-compile
       (make-regexps
        '(("^[ \t]*set") ado-builtin-harmless-face)
        "[ \t]+"
        '(("ANSI" 
 	  "b" "be" "bee" "beep" "contents" 
 	  "d" "di" "dis" "disp" "displ" "displa" "display"
 	  "help"
 	  "IBM" 
	  "icmap"
 	  "log"
 	  "maxobs" 
	  "printcolor[ \t]+grayscale"
 	  "seed0" "shell" "smalldlg"
 	  "te" "tex" "text" "texts" "textsi" "textsiz" "textsize"
 	  "video"
 	  )
 	 ado-obsolete-face t)
        "\\b"
        ))

	  ;; the timer command
	  (eval-when-compile
		 (make-regexps
		  '(("timer") ado-builtin-harmless-face)
		  "[ \t]+"
		  '(("clear" "list") ado-subcommand-face t)
		  ))

	  (eval-when-compile
		 (make-regexps
		  '(("timer") ado-builtin-harmless-face)
		  "[ \t]+"
		  '(("clear" "list" "off" "on") ado-subcommand-face)
		  "[ \t]+"
		  '(("\\([0-9]+\\|`[a-zA-Z0-9_`']'\\)") ado-subcommand-face)
		  ))
		  

     ;; the args command 
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("args") ado-builtin-harmful-face)
	"[ \t]+"
        '((
 	  "\\([a-zA-Z_][a-zA-Z_0-9]*[ \t]*\\)+"
 	  ) ado-variable-name-face)
        "\\b"
        ))


     ;; char with sub commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("char") ado-builtin-harmless-face)
        "[ \t]+"
        '((
 	  "l" "li" "lis" "list" 
 	  ) ado-subcommand-face)
        "\\b"
        ))
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("char") ado-builtin-harmful-face)
        "[ \t]+"
        '((
	   "ren" "rena" "renam" "rename"
 	  ) ado-subcommand-face)
        "\\b"
        ))


    ;; the constraint commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "cons" "const" "constr" "constra" "constrai" "constrain" "constraint" 
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "d"
	  "de" "def" "defi" "defin" "define" 
	  "di" "dir"
	  "drop"
	  "free" "get"
	  "l" "li" "lis" "list"
	  ) 
	 ado-subcommand-face)
       "\\b"
       ))

    ;; the confirm commands - could be a mess!
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "conf" "confi" "confir" "confirm"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "e" "ex" "exi" "exis" "exist" "existe" "existen" "existenc" "existence"
	  "f" "fi" "fil" "file"
	  "fo" "for" "form" "forma" "format" 
	  "mat" "matr" "matri" "matrix" 
	  "n" "name" "names" "nu" "num" "numb" "numbe" "number" 
	  "sca" "scal" "scala" "scalar" 
	  "v" "va" "var" "vari" "varia" "variab" "variabl" "variable"
	  ) ado-subcommand-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "conf" "confi" "confir" "confirm"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "integer"
	  ) ado-subcommand-face)
       "[ \t]+"
       '((
	  "n" "nu" "num" "numb" "numbe" "number"
	  ) ado-subcommand-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "conf" "confi" "confir" "confirm"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "n" "ne" "new"
	  ) ado-subcommand-face)
       "[ \t]+"
       '((
	  "f" "fi" "fil" "file"
	  "v" "va" "var" "vari" "varia" "variab" "variabl" "variable"
	  ) ado-subcommand-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "conf" "confi" "confir" "confirm"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "byte" "double" "float" "int" "long"
	  "numeric"
	  "str" "stri" "strin" "string"
	  ) ado-subcommand-face)
       "[ \t]+"
       '((
	  "v" "va" "var" "vari" "varia" "variab" "variabl" "variable" 
	  ) ado-subcommand-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
			 "conf" "confi" "confir" "confirm"
			 ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
			 "date"
			 "numeric"
			 "str" "stri" "strin" "string"
			 "ts"
			 ) ado-subcommand-face)
       "[ \t]+"
       '((
			 "fo" "for" "form" "forma" "format" 
			 ) ado-subcommand-face)
       "\\b"
       ))
	 ;; obsolete confirm command
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
			 "conf" "confi" "confir" "confirm"
			 ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
			 "ts"
			 ) ado-obsolete-face t)
       "[ \t]+"
       '((
			 "fo" "for" "form" "forma" "format" 
			 ) ado-subcommand-face)
       "\\b"
       ))

    ;;; the str# won't quite look right, but that's the breaks for using
    ;;; a tool like this...
    ;;; won't allow big long strings which are allowed in wicked huge stata
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "conf" "confi" "confir" "confirm"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "str[1-9]+[0-9]*[ \t]+"
	  ) ado-subcommand-face)
       '((
	  "v" "va" "var" "vari" "varia" "variab" "variabl" "variable" 
	  ) ado-subcommand-face)
       "\\b"
       ))
    ;; the duplicates commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("duplicates") ado-builtin-harmless-face)
       "[ \t]+"
       '((
			 "b" "br" "bro" "brow" "brows" "browse" 
			 "e" "ex" "exa" "exam" "examp" "exampl" "example" "examples" 
			 "l" "li" "lis" "list" 
			 "r" "re" "rep" "repo" "repor" "report" 
			 ) ado-subcommand-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("duplicates") ado-builtin-harmful-face t)
       "[ \t]+"
       '((
			 "drop"
			 "t" "ta" "tag" 
			 ) ado-subcommand-face t)
       "\\b"
       ))
	 ;; estimates commands moved to just after estat commands
    ;; the _estimates commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "_est" "_esti" "_estim" "_estima" "_estimat" "_estimate" "_estimates"
	  ) ado-builtin-harmless-face t)
       "[ \t]+"
       '((
	  "clear"
	  "dir" 
	  "drop"
	  "h" "ho" "hol" "hold"
	  "u" "un" "unh" "unho" "unhol" "unhold"
	  ) 
	 ado-subcommand-face t)
       "\\b"
       ))

    ;; the estat commands --- moved to after the obsolete commands

    ;; the etest commands
    ;;  FIX when at regression post estimation
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("etest") ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "hett" "hette" "hettes" "hettest" 
	  ) ado-subcommand-face)
       "\\b"
       ))
    ;; the file commands

    (eval-when-compile
       (make-regexps
        "\\b"
        '(("file") ado-builtin-harmless-face)
        "[ \t]+"
        '((
			  "close" "open" 
			  "q" "qu" "que" "quer" "query" 
			  "r" "re" "rea" "read" 
			  "seek" 
			  "set"
			  ) ado-subcommand-face)
        "\\b"
        ))

    (eval-when-compile
       (make-regexps
        "\\b"
        '(("file") ado-builtin-harmful-face)
        "[ \t]+"
        '((
	   "sersetread" "sersetwrite"
	   "w" "wr" "wri" "writ" "write" 
 	  ) ado-subcommand-face)
        "\\b"
        ))

    ;; the gph commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "gph"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "arc"
	  "box"
	  "clear" "close"
	  "font"
	  "line"
	  "open"
	  "pen" "point"
	  "text"
	  "vline" "vpoint" "vpoly" "vtext"
	  ) 
	 ado-subcommand-face)
       "\\b"
       ))
    ;;
    ;; the gprefs commands
    ;;   (in multiple pieces)
    ;; comment region 5
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
 	  "gprefs"
 	  ) ado-builtin-harmless-face)
        "[ \t]+"
        '((
 	  "q" "qu" "que" "quer" "query" 
 	  ) 
 	 ado-subcommand-face)
	"[ \t]"
        '((
 	  "window"
 	  ) ado-subcommand-face)
        "\\b"
        ))
     ;;
     ;; the gprefs set window scheme commands
     ;; comment region 6
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
			  "gprefs"
			  ) ado-builtin-harmless-face)
        "[ \t]+"
        '(("set") ado-subcommand-face)
        "[ \t]+"
        '(("window") ado-subcommand-face)
        "[ \t]+"
        '(("scheme") ado-subcommand-face)
        "[ \t]+"
        '((
			  "black" "blackb" "blackbg" 
			  "custom1" "custom2" "custom3"
			  "mono" "monoc" "monoch" "monochr" "monochro" "monochrom" "monochrome" 
			  "white" "whiteb" "whitebg" 
			  ) 
			 ado-subcommand-face)
        "\\b"
        ))
     ;;
     ;; the other gprefs set window 
     ;; comment region 7
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
			  "gprefs"
			  ) ado-builtin-harmless-face)
        "[ \t]+"
        '(("set") ado-subcommand-face)
        "[ \t]+"
        '(("window") ado-subcommand-face)
        "[ \t]+"
        '((
			  "displaythick[ \t]+off" "displaythick[ \t]+on"
			  "update"
			  "usegphsize[ \t]+off" "usegphsize[ \t]+on"
			  "xsize"
			  "ysize"
			  ) 
			 ado-subcommand-face)
        "\\b"
        ))
     ;;
     ;; the gprefs set scheme commands 
     ;; comment region 8
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
			  "gprefs"
			  ) ado-builtin-harmless-face)
        "[ \t]+"
        '(("set") ado-subcommand-face)
        "[ \t]+"
        '((
			  "custom1" "custom2" "custom3"
			  ) ado-subcommand-face)
        "[ \t]+"
        '((
			  "background_color"
			  "pen1_color" "pen2_color" "pen3_color" "pen4_color" "pen5_color" "pen6_color" "pen7_color" "pen8_color" "pen9_color"
			  "pen1_thick" "pen2_thick" "pen3_thick" "pen4_thick" "pen5_thick" "pen6_thick" "pen7_thick" "pen8_thick" "pen9_thick"
			  "symmag_all"
			  ) 
			 ado-subcommand-face)
        "_*\\b"
        ))
     ;;
     ;; shoulda never started this - the gprefs query scheme layout all by its lonesome
     ;; comment region 9
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
			  "gprefs"
			  ) ado-builtin-harmless-face)
        "[ \t]+"
        '(("query") ado-subcommand-face)
        "[ \t]+"
        '((
			  "custom1" "custom2" "custom3"
			  ) ado-subcommand-face)
        "\\b"
        ))
     ;;; worst than smcl ---- it's graph!
     ;;;  -> will need multiple copies of the subcommands for the () and || and plain versions
     ;;;     argh, what a pain in the rear.
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("gr" "gra" "grap" "graph") ado-builtin-harmless-face t)
		  "[ \t]+"
		  '((
			  "bar" "box"
			  "combine" "copy"
			  "des" "desc" "descr" "descri" "describ" "describe" 
			  "di" "dir" "dis" "disp" "displ" "displa" "display"
			  "dot"
			  "export"
			  "hbar" "hbox"
			  "matrix"
			  "pie" "print"
			  "q" "qu" "que" "quer" "query" 
			  "save" "set"
			  "tw" "two" "twow" "twowa" "twoway"
			  ) ado-subcommand-face t)
		  "\\b"
		  ))
	  ;; the initial graph commands which are destructive
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("gr" "gra" "grap" "graph") ado-builtin-harmful-face t)
		  "[ \t]+"
		  '((
			  "drop\\([ \t]+_all\\)?"
			  "rename"
			  "use"
			  ) ado-subcommand-face t)
		  "\\b"
		  ))
	  
	  ;; the graph twoway stuff
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("\\(\\(gr\\|gra\\|grap\\|graph\\)[ \t]+\\)?") ado-builtin-harmless-face)
		  '(("tw" "two" "twow" "twowa" "twoway" ) ado-builtin-harmless-face t)
		  "[ \t]+"
		  '((
			  "area"
			  "bar"
			  "con" "conn" "conne" "connec" "connect" "connecte" "connected" 
			  "dot" "dropline"
			  "fpfit" "fpfitci" "function"
			  "hist" "histo" "histog" "histogr" "histogra" "histogram" 
			  "kdensity"
			  "line"
			  "lfit" "lfitci"
			  "lowess" "lpoly" "lpolyci"
 			  "mband" "mspline"
			  "pcarrow" "pcbarrow" "pcbarrowi" "pccapsym" "pci" "pcscatter" "pcspike"
			  "qfit" "qfitci"
			  "rarea" "rbar" "rcap" "rcapsym" 
			  "rcon" "rconn" "rconne" "rconnec" "rconnect" "rconnecte" "rconnected" 
			  "rl" "rli" "rlin" "rline" 
			  "rsc" "rsca" "rscat" "rscatt" "rscatte" "rscatter" 
			  "rspike"
			  "sc" "sca" "scat" "scatt" "scatte" "scatter" 
			  "scatteri" "spike"
			  "tsline" "tsrline"
			  ) ado-subcommand-face t)
		  "\\b"
		  ))
	  ;; even more aggravating: things for which both graph and twoway are optional
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("\\(\\(gr\\|gra\\|grap\\|graph\\)[ \t]+\\)?") ado-builtin-harmless-face)
		  '(("\\(\\(tw\\|two\\|twow\\|twowa\\|twoway\\)[ \t]+\\)?" ) ado-builtin-harmless-face t)
		  '((
			  "tsline" "tsrline" ) ado-builtin-harmless-face t)
		  "\\b"
		  ))
	  
     ;; icd9, icd9p commands
     (eval-when-compile
       (make-regexps
		  "\\b"
		  '(("\\(icd\\(9\\|9p\\)\\)"
			  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
			  "check"
			  "l" "lo" "loo" "look" "looku" "lookup"
			  "q" "qu" "que" "quer" "query" 
			  "sea" "sear" "searc" "search" 
			  ) ado-subcommand-face)
		  "\\b"
		  ))
     ;; icd9s with generate
     (eval-when-compile
       (make-regexps
		  "\\b"
		  '(("\\(icd\\(9\\|9p\\)\\)"
			  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
			  "clean"
			  "gen" "gene" "gener" "genera" "generat" "generate" 
			  ) ado-builtin-harmful-face)
		  "\\b"
		  ))
     ;;
     ;; some of the matrix commands
     ;; with no matrix arguments - harmless
     ;; with one following argument but no subcommand
     (eval-when-compile
       (make-regexps
		  "\\b"
		  '(("mat" "matname" "mat_put_rr" "matr" "matri" "matrix"
			  ) ado-builtin-harmful-face)
		  "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face)
		  "\\b"
		  ))
     (eval-when-compile
       (make-regexps
		  "\\b"
		  '(("mat" "matr" "matri" "matrix"
			  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
			  "d" "di" "dir" "dispCns"
			  "post"
			  ) 
			 ado-subcommand-face t)
		  "\\b"
		  ))
     ;; with no matrix arguments - harmful
     (eval-when-compile
       (make-regexps
		  "\\b"
		  '(("mat" "matr" "matri" "matrix"
			  ) ado-builtin-harmless-face t)
		  "[ \t]+"
		  '((
			  "sco" "scor" "score" 
			  ) 
			 ado-subcommand-face t)
		  "\\b"
		  ))
     ;; with an indeterminant number of matrix arguments - harmful
     (eval-when-compile
       (make-regexps
		  "\\b"
		  '(("mat" "matr" "matri" "matrix"
			  ) ado-builtin-harmful-face)
		  "[ \t]+"
		  '((
			  "drop[ \t]+_all"
			  "makeCns"
			  ) 
			 ado-subcommand-face t)
		  "\\b"
		  ))
	  ;; doesn't quite work, because it underlines the spaces
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("mat" "matr" "matri" "matrix"
			  ) ado-builtin-harmful-face)
		  "[ \t]+"
		  '(("drop") ado-subcommand-face t)
        '(("\\([ \t]+[a-zA-Z][a-zA-Z0-9_]*\\)+"
			  ) ado-matrix-name-face t)
		  ))
     ;; with one following argument
     (eval-when-compile
       (make-regexps
		  "\\b"
		  '(("mat" "matr" "matri" "matrix"
			  ) ado-builtin-harmful-face)
		  "[ \t]+"
		  '((
			  "ac" "acc" "accu" "accum" 
			  "cole" "coleq" 
			  "coln" "colna" "colnam" "cloname" "colnames"
			  "def" "defi" "defin" "define"
			  "dis" "diss" "dissi" "dissim" "dissimi" "dissimil" "dissimila" "dissimilar" 
			  "dissimilari" "dissimilarit" "dissimilarity" 
			  "glsa" "glsac" "glsacc" "glsaccu" "glsaccum"
			  "in" "inp" "inpu" "input" 
			  ;; 	  "mlou" "mlout"
			  "l" "li" "lis" "list" 
			  "opaccum"
			  "rowe" "roweq" 
			  "rown" "rowna" "rownam" "rowname" "rownames"
			  ;; 	  "sub" "subs" "subst" "substi" "substit" "substitu" "substitut" "substitute" 
			  "veca" "vecac" "vecacc" "vecaccu" "vecaccum"
			  )  ado-subcommand-face t)
		  "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
		  "\\b"
		  ))
     ;; with one following arguments -- but harmless (good grief!)
     (eval-when-compile
       (make-regexps
		  "\\b"
		  '(("mat" "matr" "matri" "matrix"
			  ) ado-builtin-harmful-face)
		  "[ \t]+"
		  '((
			  "l" "li" "lis" "list" 
			  )  ado-subcommand-face)
		  "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
		  "\\b"
		  ))
     ;; with two following arguments
     (eval-when-compile
       (make-regexps
		  "\\b"
		  '(("mat" "matr" "matri" "matrix"
			  ) ado-builtin-harmful-face)
		  "[ \t]+"
		  '((
			  "eigenval" "eigenvalu" "eigenvalue" "eigenvalues" 
			  "ren" "rena" "renam" "rename" 
			  "syme" "symei" "symeig" "symeige" "symeigen"
			  )  ado-subcommand-face t)
		  "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
		  "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
		  "\\b"
		  ))
     ;; with three(!) following arguments
     (eval-when-compile
       (make-regexps
		  "\\b"
		  '(("mat" "matr" "matri" "matrix"
			  ) ado-builtin-harmful-face)
		  "[ \t]+"
		  '(("svd" 
			  )  ado-subcommand-face t)
		  "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
		  "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
		  "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
		  "\\b"
		  ))
     ;; with three(!) following arguments but no friggin matrix command
     (eval-when-compile
       (make-regexps
		  "\\b"
		  '(("matcproc"
			  ) ado-builtin-harmful-face)
		  "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
		  "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
		  "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
		  "\\b"
		  ))
	  ;; now for the svmat command
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("svmat") ado-builtin-harmful-face)
		  "[ \t]+\\(\\(byte\\|double\\|float\\|int\\|long\\)[ \t]\\)?"
		  '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
		  "\\b"
		  ))
     ;; the ml commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("ml"
			  ) ado-builtin-harmless-face)
        "[ \t]+"
        '((
			  "check" "clear" "count"
			  "di" "dis" "disp" "displ" "displa" "display"
			  "foot" "footn" "footno" "footnot" "footnote"
			  "gr" "gra" "grap" "graph"
			  "init"
			  "max" "maxi" "maxim" "maximi" "maximiz" "maximize"
			  "me" "met" "meth" "metho" "method"
			  "mod" "mode" "model"
			  "p" "pl" "plo" "plot" 
			  "q" "qu" "que" "quer" "query"
			  "rep" "repo" "repor" "report"
			  "sea" "sear" "searc" "search"
			  "trace")
			 ado-subcommand-face)
        "\\b"
        ))
     ;; obsolete ml commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("ml") ado-builtin-harmless-face)
        "[ \t]+"
        '(("b" "be" "beg" "begi" "begin" 
			  "de" "dep" "depn" "depna" "depnam" "depname" "depnames" 
			  "f" "fu" "fun" "func" "funct" "functi" "functio" "function"
			  "ml" "mlo" "mlou" "mlout"
			  "pl" "plo" "plot"
			  "po" "pos" "post" 
			  "sa" "sam" "samp" "sampl" "sample")
			 ado-obsolete-face)
        "\\b"
        ))
	  ;; the net commands
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("net") ado-builtin-harmless-face)
		  "[ \t]"
		  '((
			  "cd"
			  "d" "de" "des" "desc" "descr" "descri" "describ" "describe"
			  "from" "get" 
			  "ins" "inst" "insta" "instal" "install" 
			  "link"
			  "q" "qu" "que" "quer" "query"
			  "search" "sj" "stb")
			 ado-subcommand-face)
		  "\\b"
		  ))
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("net") ado-builtin-harmless-face)
		  "[ \t]+"
		  '(("set") ado-builtin-harmless-face)
		  "[ \t]+"
		  '(("ado" "other") ado-subcommand-face)
		  "\\b"
		  ))
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("ado") ado-builtin-harmless-face)
		  "[ \t]+"
		  '(("d" "de" "des" "desc" "descr" "descri" "describ" "describe"
			  "dir"
			  "uninstall")
			 ado-subcommand-face)
		  "\\b"
		  ))
	  ;; odbc commands 
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("odbc") ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "des" "desc" "descr" "descri" "describ" "describe"
		  "li" "lis" "list"
		  "q" "qu" "que" "quer" "query"
		  ) ado-subcommand-face)
		  "\\b"
		  ))
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("odbc") ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "exe" "exec" 
		  "in" "ins" "inse" "inser" "insert" 
		  "lo" "loa" "load" 
		  "sql" "sqlf" "sqlfi" "sqlfil" "sqlfile" 
		  ) ado-builtin-harmful-face)
		  "\\b"
		  ))
	  ;; palette commands     (eval-when-compile
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("palette") ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "color"
		  "line" "linep" "linepa" "linepal" "linepale" "linepalet" "linepalett" "linepalette" 
		  "symbol" "symbolp" "symbolpa" "symbolpal" "symbolpale" "symbolpalet" "symbolpalett" "symbolpalette" 
		  ) ado-subcommand-face)
		  "\\b"
		  ))

	  ;; postutil commands - both of them
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("postutil") ado-builtin-harmless-face)
		  "[ \t]+"
		  '(("dir") ado-subcommand-face t)
		  ))
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("postutil") ado-builtin-harmful-face)
		  "[ \t]+"
		  '(("clear") ado-subcommand-face t)
		  ))
	  ;; query commands 
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "q" "qu" "que" "quer" "query"
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "eff" "effi" "effic" "effici" "efficie" "efficien" "efficienc" "efficiency" 
		  "graph" "graphi" "graphic" "graphics" 
		  "inter" "interf" "interfa" "interfac" "interface"
		  "mata"
		  "mem" "memo" "memor" "memory" 
		  "net" "netw" "netwo" "networ" "network" 
		  "out" "outp" "outpu" "output" 
		  "oth" "othe" "other" 
		  "trace"
		  "up" "upd" "upda" "updat" "update" 
		  ) ado-subcommand-face)
		  "\\b"
		  ))
    
	  ;; the reshape commands
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("reshape") ado-builtin-harmful-face)
		  "[ \t]+"
		  '((
		  "clear"
		  "error"
		  "i" "j"
		  "long"
		  "wide"
		  "xi" "xij")
			 ado-subcommand-face)
		  "\\b"
		  ))
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("reshape") ado-builtin-harmful-face)
		  '(("\\([ \t]\\(q\\|qu\\|que\\|quer\\|query\\)\\)?"
		  ) ado-subcommand-face)
		  "\\b"
		  ))

	  ;; the _return commands (not the return commands)
	  (eval-when-compile
       (make-regexps
        "\\b"
        '(("_ret" "_retu" "_retur" "_return") ado-builtin-harmless-face)
        "[ \t]+"
        '((
		  "dir" "drop"
		  "hold"
		  "res" "rest" "resto" "restor" "restore" 
		  ) ado-subcommand-face t)
        "\\b"
        ))

	  ;; the return commands
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("ret" "retu" "retur" "return") ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
			  "add" "clear"
			  "li" "lis" "list" 
			  "loc" "loca" "local" 
			  "mat" "matr" "matri" "matrix" 
			  "sca" "scal" "scala" "scalar" 
			  ) ado-subcommand-face t)
		  "\\b"
		  ))
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("eret" "eretu" "eretur" "ereturn") ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "clear"
		  "di" "dis" "disp" "displ" "displa" "display" 
		  "li" "lis" "list" 
		  "loc" "loca" "local" 
		  "mat" "matr" "matri" "matrix" 
		  "post" "repost"
		  "sca" "scal" "scala" "scalar" 
		  ) ado-subcommand-face t)
		  "\\b"
		  ))
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("sret" "sretu" "sretur" "sreturn") ado-builtin-harmless-face)
		  "[ \t]+"
		  '(("clear" 
		  "li" "lis" "list" 
		  "loc" "loca" "local" 
		  ) ado-subcommand-face)
		  "\\b"
		  ))
	  ;; scc commands
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("ssc") ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "copy"
		  "d" "de" "des" "desc" "descr" "descri" "describ" "describe" 
		  "inst" "insta" "instal" "install" 
		  "type" "uninstall"
		  "what" "whats" "whatsn" "whatsne" "whatsnew" 
		  ) ado-subcommand-face)
		  "\\b"
		  ))
	  ;; the serset commands
	  (eval-when-compile
       (make-regexps
        "\\b"
        '(("serset") ado-builtin-harmful-face)
        "[ \t]+"
        '((
		  "clear"
		  "cr" "cre" "crea" "creat" "create" "create_cspline" "create_xmedians"
		  "drop"
		  "reset_id"
		  "set" "sort"
		  "use"
		  ) ado-subcommand-face)
        "\\b"
        ))
	  (eval-when-compile
       (make-regexps
        "\\b"
        '(("serset") ado-builtin-harmless-face)
        "[ \t]+"
        '((
		  "dir"
		  "su" "sum" "summ" "summa" "summar" "summari" "summariz" "summarize" 
		  ) ado-subcommand-face)
        "\\b"
        ))

	  ;; the sts commands
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("sts") ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "g"
		  "gr" "gra" "grap" "graph"
		  "l" "li" "lis" "list"
		  "t" "te" "tes" "test"
		  )
			 ado-subcommand-face)
		  "\\b"
		  ))
	  ;; the sts harmful stuff
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("sts") ado-builtin-harmful-face)
		  "[ \t]+"
		  '((
			  "gen" "gene" "gener" "genera" "generat" "generate"
			  )
			 ado-subcommand-face)
		  "\\b"
		  ))

	  ;; the sw commands
	  ;; the sw commands are all now obsolete, because of syntax change
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("sw") ado-builtin-harmless-face)
		  "[ \t]+"
		  '(( 
		  "clogit" "cloglog" "cnreg" "cox" 
		  "ereg" 
		  "gamma" "glm" "gompertz" 
		  "hetprob" 
		  "llogistic" "lnormal" "logistic" "logit" "nbreg" "ologit" "oprobit"
		  "poisson" "probit" "qreg" "reg" "regr" "regre" "regres" "regress"
		  "scobit" "stcox" "streg" "tobit" "weibull"
		  )
			 ado-obsolete-face)
		  "\\b"
		  ))
	  ;; the sysdir commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("sysdir") ado-builtin-harmless-face)
        "[ \t]+"
        '((
		  "l" "li" "lis" "list"
		  "set"
		  ) ado-subcommand-face t)
        "\\b"
        ))
	  ;; the personal commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("personal") ado-builtin-harmless-face)
        "[ \t]+"
        '((
		  "dir"
		  ) ado-subcommand-face t)
        "\\b"
        ))
	  ;; tsunab and unab commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("tsunab" "unab") ado-builtin-harmful-face)
        "[ \t]+"
		  '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-variable-name-face)
		  "[ \t]*:[ \t]*"
        ))

	  ;; the tssmooth commands
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("tssmooth") ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "d" "de" "dex" "dexp" "dexpo" "dexpon" "dexpone" "dexponen" "dexponent" "dexponenti" "dexponentia" "dexponential" 
		  "e" "ex" "exp" "expo" "expon" "expone" "exponen" "exponent" "exponenti" "exponentia" "exponential" 
		  "h" "hw" "hwi" "hwin" "hwint" "hwinte" "hwinter" "hwinters" 
		  "ma" "nl"
		  "s" "sh" "shw" "shwi" "shwin" "shwint" "shwinte" "shwinter" "shwinters" 
		  )
			 ado-subcommand-face)
		  "\\b"
		  ))
	  ;;
	  ;; the translator commands
	  ;; comment region 12
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("translator") ado-builtin-harmless-face)
        "[ \t]+"
        '((
		  "q" "qu" "que" "quer" "query" 
		  "reset"
		  "set"
		  ) ado-subcommand-face)
        "\\b"
        ))
	  ;;
	  ;; the transmap commands
	  ;; comment region 13
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("transmap") ado-builtin-harmless-face)
        "[ \t]+"
        '((
		  "def" "defi" "defin" "define" 
		  "q" "qu" "que" "quer" "query" 
		  ) ado-subcommand-face)
        "\\b"
        ))
     ;; 
     ;; the update commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("update") ado-builtin-harmful-face)
        "[ \t]+"
        '((
		  "ado" "all"
		  "executable"
		  "from"
		  "swap"
		  ) ado-subcommand-face)
        "\\b"
        ))
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("update") ado-builtin-harmless-face)
        "[ \t]+"
        '((
		  "q" "qu" "que" "quer" "query" 
		  ) ado-subcommand-face)
        "\\b"
        ))
     ;; the fcast commands which leave data alone
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("fcast") ado-builtin-harmless-face)
        "[ \t]+"
        '((
		  "g" "gr" "gra" "grap" "graph" 
		  ) ado-subcommand-face)
        "\\b"
        ))
     ;; fcast commands which alter data
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("fcast") ado-builtin-harmful-face)
        "[ \t]+"
        '((
		  "c" "co" "com" "comp" "compu" "comput" "compute" 
		  ) ado-subcommand-face)
        "\\b"
        ))
     ;; the obsolete varfcast commands with sub commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("varfcast") ado-obsolete-face)
        "[ \t]+"
        '((
		  "c"
		  "cl" "cle" "clea" "clear" 
		  "co" "com" "comp" "compu" "comput" "compute" 
		  "g" "gr" "gra" "grap" "graph" 
		  ) ado-subcommand-face)
        "\\b"
        ))
     ;; the irf functions which leave data alone
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("irf") ado-builtin-harmless-face)
        "[ \t]+"
        '((
		  "cg" "cgr" "cgra" "cgrap" "cgraph" 
		  "ct" "cta" "ctab" "ctabl" "ctable" 
		  "d" "de" "des" "desc" "descr" "descri" "describ" "describe" 
		  "di" "dir"
		  "g" "gr" "gra" "grap" "graph" 
		  "og" "ogr" "ogra" "ograp" "ograph" 
		  "t" "ta" "tab" "tabl" "table" 
		  ) ado-subcommand-face)
        "\\b"
        ))
     ;; 
     ;; the irf commands which alter data
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("irf") ado-builtin-harmful-face)
        "[ \t]+"
        '((
		  "a" "ad" "add" 
		  "cr" "cre" "crea" "creat" "create" 
		  "drop"
		  "ren" "rena" "renam" "rename" 
		  "set"
		  ) ado-subcommand-face)
        "\\b"
        ))
     ;; the irf functions which are obsolete
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("irf") ado-builtin-harmless-face)
        "[ \t]+"
        '((
		  "di" "dir"
		  "erase"
		  ) ado-obsolete-face t)
        "\\b"
        ))

     ;; obsolete varirf functions
     ;; the varirf functions which leave data alone
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("varirf") ado-obsolete-face)
        "[ \t]+"
        '((
		  "a" "ad" "add" 
		  "cg" "cgr" "cgra" "cgrap" "cgraph" 
		  "cr" "cre" "crea" "creat" "create" 
		  "ct" "cta" "ctab" "ctabl" "ctable" 
		  "d" "de" "des" "desc" "descr" "descri" "describ" "describe" 
		  "di" "dir"
		  "drop" "erase"
		  "g" "gr" "gra" "grap" "graph" 
		  "og" "ogr" "ogra" "ograp" "ograph" 
		  "ren" "rena" "renam" "rename" 
		  "set"
		  "t" "ta" "tab" "tabl" "table" 
		  ) ado-subcommand-face)
        "\\b"
        ))

     ;;
     ;; the view commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("view") ado-builtin-harmless-face)
        "[ \t]+"
        '((
		  "ado" "ado_d"
		  "browse"
		  "file"
		  "help" "help_d"
		  "net" "net_d" "news"
		  "search" "search_d"
		  "view_d"
		  "update" "update_d"
		  ) ado-subcommand-face)
        "\\b"
        ))
     ;; the webuse commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("webuse") ado-builtin-harmless-face)
        "[ \t]+"
        '((
		  "query" "set"
		  ) ado-subcommand-face)
        "\\b"
        ))
	  ;; the window commands
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "win" "wind" "windo" "window"
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "d"
		  "di" "dia" "dial" "dialo" "dialog"
		  "dir" "drop"
		  "fo" "fop" "fope" "fopen"
		  "fs" "fsa" "fsav" "fsave"
		  "l" "list"
		  "push"
		  ) ado-subcommand-face)
		  "\\b"
		  ))
	  ;; the window controls
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "win" "wind" "windo" "window"
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "c" "co" "con" "cont" "contr" "contro" "control"
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "button" "check" "clear"
		  "edit"
		  "mcombo" "msimple"
		  "radbegin"
		  "radend"
		  "radio"
		  "scombo"
		  "ssimple"
		  "static"
		  ) ado-subcommand-face)
		  "\\b"
		  ))
	  ;; the window manage commands
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "win" "wind" "windo" "window"
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "man" "mana" "manag" "manage"
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "associate"
		  "close[ \t]+\\(graph\\|viewer\\)"
		  "maintitle\\([ \t]+reset\\)?"
		  "minimize" 
		  "prefs[ \t]+load" 
		  "prefs[ \t]+save" 
		  "prefs[ \t]+default"
		  "print[ \t]+graph"
		  "print[ \t]+viewer"
		  "rename[ \t]+graph"
		  "restore"
		  "update[ \t]+variable"
		  ) 
			 ado-subcommand-face)
		  "\\b"
		  ))
	  ;; the window manage forward commands [sheesh]
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "win" "wind" "windo" "window"
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "man" "mana" "manag" "manage"
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '(("forward") ado-subcommand-face)
		  "[ \t]+"
		  '(("command" "doeditor" "graph" "help" "results" "review" "variables" "viewer"
		  ) 
			 ado-subcommand-face)
		  "\\b"
		  ))
	  ;; the window manage close/print commands [sheesh]
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "win" "wind" "windo" "window"
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "man" "mana" "manag" "manage"
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '(("forward") ado-subcommand-face)
		  "[ \t]+"
		  '(("command" "doeditor" "graph" "help" "results" "review" "variables" "viewer"
		  ) 
			 ado-subcommand-face)
		  "\\b"
		  ))
	  ;; the window menu commands
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "win" "wind" "windo" "window"
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "m" "me" "men" "menu"
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "add_recentfiles"
		  "append[ \t]+item"
		  "append[ \t]+separator"
		  "append[ \t]+submenu"
		  "clear"
		  "refresh"
		  ) 
			 ado-subcommand-face)
		  "\\b"
		  ))
	  ;; the window stopbox commands
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "win" "wind" "windo" "window"
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "stop" "stopb" "stopbo" "stopbox"
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "note"
		  "rusure"
		  "stop"
		  ) 
			 ado-subcommand-face)
		  "\\b"
		  ))
	  ;; the xwindow commands
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "xwin" "xwind" "xwindo" "xwindow"
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "de" "def" "defi" "defin" "define" 
		  "di" "dir"
		  "drop"
		  "l" "li" "lis" "list"
		  ) 
			 ado-subcommand-face)
		  "\\b"
		  ))

	  ;; all the endless Stata keywords (not in a good order)
	  ;; first those keywords which must start line
	  ;; note that these will look like text if preceded by a comment
	  ;; (but comments shouldn't be before the command, anyway, 'cuz it makes things hard to read)

	  (eval-when-compile
		 (make-regexps
		  "^[ \t]*"
		  '((
		  "by"
		  "cap" "capt" "captu" "captur" "capture"
		  "char" "err" "erro" "error" "e" "ex" "exi" "exit" 
		  "for"
		  "set"
		  ) ado-builtin-harmless-face)
		  "\\b"
		  ))
	  ;; here are some keywords which appear in the middle of lines
	  ;; note that the really short abbreviations could make a mess of things
	  ;;
	  ;; These are split to allow compiling!
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "_coef_table"
		  "_datasig" "_datasign" "_datasigna" "_datasignat" "_datasignatu" "_datasignatur" "_datasignature" 
		  "_qreg" "_rmcoll" "_rmdcoll" "_robust"
		  "#r" "#re" "#rev" "#revi" "#revie" "#review" 
		  "about" "ac" "acprplot" "adjust" 
		  "ado" "adopath" "adoupdate" "alpha" "ameans" 
		  "an" "ano" "anov" "anova" 
		  "arch" "areg" "arima" 
		  "as" 
		  "asclogit"
		  "asmprobit"
		  "asroprobit"
		  "ass" "asse" "asser" "assert" 
		  "avplot" "avplots"
		  "b" "be" "bee" "beep"
		  "binreg" "biprobit" "biplot" "bitest" "bitesti" "blogit"
		  "bootstrap" "boxcox" "bprobit" "br" "break" "brier" 
		  "bro" "brow" "brows" "browse" 
		  "bsqreg" "bstat"
		  "ca" "cabiplot" "camat" "candisc" "canon" "caprojection" "cat" 
		  "cc" "cci" "cchart" "centile" "cf" 
		  "ch" "che"
		  "checkestimationsample" "checksum" 
		  "chel" "chelp"
		  "ci" "cii" 
		  "clog" "clogi" "clogit" "clogitp" "cloglog"
		  "close" "cluster" "clustermat" "cmdlog" "cmdtool" 
		  "cnr" "cnre" "cnreg" "cnsreg" "codebook" "compare" 
		  "cons" "const" "constr" "constra" "constrai" "constrain" "constraint"
		  "continue"
		  "copy" "copyright" 
		  "cor" "corc" "corr" "corre" "correl" "correla" "correlat" "correlate"
		  "corrgram"
		  "cou" "coun" "count" 
		  "cox"	"cprplot" "_crcswxx" "cs" "csi" 
		  "ct" "ctset" 
		  "cumsp" "cumul" "cusum")
			 ado-builtin-harmless-face)
		  "\\b"
		  ))
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "d"
		  "datasig" "datasign" "datasigna" "datasignat" "datasignatu" "datasignatur" "datasignature"
		  "db"
		  "de" "des" "desc" "descr" "descri" "describ" "describe"
		  "dfbeta" "dfgls" "dfuller" 
		  "di" "dir" "dis" "discrim" "disp" "disp_res" "disp_s" 
		  "displ" "displa" "display"
		  "do" 
		  "doed" "doedi" "doedit" 
		  "dotplot"
		  "dprobit" "ds" "dstdize" 
		  "eivreg" "eq" "ereg" "exlogistic" "expoisson"
		  "fac" "fact" "facto" "factor" "factormat"
		  "findfile" "findit" "fit"
		  "fl" "fli" "flis" "flist"
		  "for" "fpredict" 
		  "fracplot" "fracpoly" "frontier" "fsl"
		  ) ado-builtin-harmless-face)
		  "\\b"
		  ))
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "gamma" "gammahet" "gladder" "gllamm" "glm" "glmpred" "glogit" "gnbreg" "gompertz"
		  "gphdot" "gphpen" "gprobit" "gr7" "graph7" "grmeanby"
		  "h"
		  "hadimvo" "hausman" "heckman" "heckprob" 
		  "he" "hel" "help" 
		  "hetprob" "hexdump" "hilite"
		  "hist" "histo" "histog" "histogr" "histogra" "histogram" 
		  "hlu" "hotel" "hotelling" "hsearch"
		  "include" "ins" "insp" "inspe" "inspec" "inspect" "intreg" 
		  "iqreg" "ir" "iri" 
		  "isid" "istdize" 
		  "ivprobit" "ivregress" "ivtobit"
		  "jackknife"
		  "kap" "kappa" "kapwgt" "kdensity" "ksm" "ksmirnov" "ktau"
		  "kwallis"
		  ) ado-builtin-harmless-face)
		  "\\b"
		  ))
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "l"
		  "labelbook" "ladder"
		  "levelsof"
		  "li" "line"
		  "lincom" "linktest" 
		  "lis" "list"
		  "lo" "loadingplot" "log"
		  "logi" "logistic" "logit" 
		  "loneway" "lookfor" "lowess" "lpredict" "lpoly"
		  "lroc" "lrtest" "ls" "lsens" "ltable" "lv" "lvr2plot"
		  "man" "mano" "manov" "manova" "manovatest" "matlist"
		  "mca" "mcaplot" "mcaprojection" "mcc" "mcci" 
		  "mds" "mdsconfig" "mdslong" "mdsmat" "mdsshepard"
		  "mean" "median" "memory" "mfx" "mhodds"
		  "mlog" "mlogi" "mlogit"
		  "mor" "more"
		  "mprobit" "mvreg" "mx_param"
		  "n" "nbreg" "nestreg" "net" "newey" "news"
		  "nl" "nlcom" "nlogit" "nlogittree" "nlsur" 
		  "no" "noi" "nois" "noisi" "noisil" "noisily"
		  "note" "notes"
		  "numlabel"
		  "nptrend" "numlist"
		  "olog" "ologi" "ologit"
		  "ologitp" 
		  "on" "one" "onew" "onewa" "oneway"
		  "oprob" "oprobi" "oprobit"
		  "oprobitp")
			 ado-builtin-harmless-face)
		  "\\b"
		  ))
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("pac" "pca" "pcamat" "pchart" "pchi" "pcorr" "pergram" "permute" "personal"
		  "pkcross" "pkequiv" "pkexamine" "pksumm"
		  "pl" "plo" "plot"
		  "pnorm" "poisson" "pperron"
		  "prais" "print"
		  "prob" "probi" "probit"
		  "procoverlay" "procrustes" "proportion"
		  "prtest" "prtesti"
		  "pwcorr" "pwd"
		  "q" "qchi" "qnorm" "qqplot" "qreg" "qladder" "quadchk" "quantile" 
		  "qu" "que" "quer" "query"
		  "qui" "quie" "quiet" "quietl" "quietly"
		  "ranksum" "ratio" "rchart" "regdw" "regph" 
		  "reg" "reg3" "regr" "regre" "regres" "regress"
		  "robvar"
		  "roccomp" "rocfit" "rocgold" "rocplot" "roctab"
		  "rologit" "rolling"
		  "rot" "rota" "rotat" "rotate"
		  "rotatemat"
		  "rreg"
		  "ru" "run" "runtest" "rvfplot" "rvpplot"
		  ) ado-builtin-harmless-face)
		  "\\b"
		  ))
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "sampsi" "sconfirm" 
		  "sc" "sca" "scat" "scatt" "scatte" "scatter" 
		  "scobit" "scoreplot" "screeplot"
		  "sdtest" "sdtesti" "search" "serrbar" "serset"
		  "set_defaults"
		  "sfrancia" 
		  "sh" "she" "shewhart" "shel" "shell" 
		  "signestimationsample" "signrank" "signtest"
		  "sktest" "sleep" "slog" "slogit" "spearman" "spikeplot" "sqreg"
		  "ssc"
		  "st" "st_is" "st_show" "st_ct" "stci"
		  "stcox" "stcoxkm" "stcurv" "stcurve" "stdescribe"
		  "stem" "stepwise"
		  "stereg" "stir" "stmc" "stmh" "stphplot" "stptime" 
		  "strate" "streg" "streset"
		  "sts" "stse" "stset" "stsum" "stvary" "stweib"
		  "su" "suest" "sum" "summ" "summa" "summar" "summari" "summariz" "summarize"
		  "sureg" "sunflower" "svar"
		  "svydes" "svydescribe" "svyset"
		  "sw" "swilk" "symmetry" "symmi" "symplot" "syntax" "sysdir" 
		  ) ado-builtin-harmless-face)
		  "\\b"
		  ))
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "ta" "tab" 
		  "tab1" "tab2" 
		  "tabdisp"
		  "tabi" 
		  "table" "tabodds" "tabstat"
		  "tabu" "tabul" "tabula" "tabulat" "tabulate"
		  "te" "tes" "test"
		  "testnl" "testparm" "tetrachoric"
		  "tob" "tobi" "tobit"
		  "token" "tokeni" "tokeniz" "tokenize" 
		  "total" "touch" 
		  "translator" "transmap" "treatreg" "truncreg"
		  "tsreport" "tsset" "tssmooth" "tsunab" "ttest" "ttesti"
		  "ty" "typ" "type"
		  "unab" "unabcmd" "update" "using"
		  "var" "varbasic" "vargranger"  
		  "varlmar" "varnorm" "varsoc" "varstable" "varwle" 
		  "vec" "veclmar" "vecnorm" "vecrank" "vecstable"
		  "verinst" "view" "viewsource" "vwls"
		  "weibull" "which" "who" "wntestb" "wntestq" 
		  "xchart" "xcorr"
		  "xsh" "xshe" "xshel" "xshell" 
		  "xtabond" "xtclog" "xtcloglog" 
		  "xtdes" "xtdesc" "xtdescr" "xtdescri" "xtdescrib" "xtdescribe"
		  "xtdpd" "xtdpdsys"
		  "xtfrontier"
		  "xtgee" "xtgls" "xthtaylor" "xtintreg" "xtivreg"
		  "xtline" "xtlogit" "xtmelogit" "xtmepoisson" "xtmixed" 
		  "xtnbreg" "xtpcse" "xtpois" "xtpoisson" "xtprobit"
		  "xtrc" "xtreg" "xtregar" "xtset" "xtsum" "xttab" "xttest0" "xttobit" "xttrans"
		  "zinb" "zip" "ztnb" "ztb"
		  ) ado-builtin-harmless-face)
		  "\\b"
		  ))

	  ;; things to use with the svy ... : prefix
	  ;; can be fooled by svy brr, nothing but garbage:
	  ;; another problem: don't know how to make the hilighted stuff in the middle
	  ;;  optional
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("svy") ado-builtin-harmless-face)
		  "[ \t]*\\(,\\)?.*?:[ \t]*"
		  '((
			  "biprobit"
			  "clogit" "cloglog" "cnreg" "cnsreg"
			  "glm" "gnbreg" 
			  "heckman" "heckprob" "hetprob" 
			  "intreg" "ivprobit" "ivregress" "ivtobit"
			  "logistic" "logit"
			  "mean" "mprobit" "mlogit"
			  "nl"
			  "nbreg" "ologit" "oprobit"
			  "poisson" "probit" "proportion" 
			  "ratio" 
			  "reg" "regr" "regre" "regres" "regress"
			  "scobit" "slogit" "stcox" "streg"
			  "tab" "tabu" "tabul" "tabula" "tabulat" "tabulate" 
			  "tobit" "total" "treatreg" "truncreg"
			  "zinb" "zip" "ztnb" "ztb"
			  ) ado-builtin-harmless-face)
		  "\\b"
		  ))
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("svy") ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "brr"
		  "jack" "jackk" "jackkn" "jackkni" "jackknif" "jackknife" 
		  "linear" "lineari" "lineariz" "linearize" "linearized" 
		  ) ado-subcommand-face t)
		  ".*?\\(,\\)?.*?:[ \t]*"
		  '((
		  "gnbreg" 
		  "heckman" "heckprob" 
		  "intreg" "ivreg" 
		  "logistic" "logit"
		  "mean" "mlogit" 
		  "nbreg" "ologit" "oprobit"
		  "poisson" "probit" "proportion" 
		  "ratio" 
		  "reg" "regr" "regre" "regres" "regress" 
		  "tab" "tabu" "tabul" "tabula" "tabulat" "tabulate" 
		  "total"
		  ) ado-builtin-harmless-face)
		  "\\b"
		  ))

	  ;; haver subcommands
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("haver") ado-builtin-harmless-face)
		  "[ \t]+"
		  '(("des" "desc" "descr" "descri" "describ" "describe") ado-builtin-harmless-face)))
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("haver") ado-builtin-harmful-face)
		  "[ \t]+"
		  '(("use") ado-builtin-harmful-face)))

	  ;; Conditional statements 
	  ;; if might not work right ('cuz it is also a keyword)
	  (eval-when-compile
		 (make-regexps
		  "\\w+[ \t]+"
		  '(("if"
		  ) ado-builtin-harmless-face t)
		  "\\b"
		  ))

	  (eval-when-compile
		 (make-regexps
		  "^[ \t]*"
		  '(("if" "while"
		  ) ado-builtin-harmless-face t)
		  "[ \t]+.*?[ \t]+.+?"
		  ))
	  ;; else statement (which may just have a {)
	  (eval-when-compile
		 (make-regexps
		  "^[ \t]*"
		  '(("else"
		  ) ado-builtin-harmless-face)
		  "[ \t]+?.+?"
		  ))

	  ;; short version of list --- which can get fooled if used as a var
	  (eval-when-compile
		 (make-regexps
		  '(("^[ \t]*l\\b" 
		  ) ado-builtin-harmless-face)
		  ))

	  ;; all the Stata options
	  ;; commonly used options
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("byte" "int" "long" "str" "str[1-7]?[0-9]?" "str80" "float" "double"
		  "width" "maxobs" "maxvar"
		  ) ado-subcommand-face)
		  "\\b"
		  ))
	  ;; special local variables (used in parsing)
	  ;; now obsolete, no?
	  (eval-when-compile
		 (make-regexps
		  "^[ \t]*\\(local\\)+[ \t]+"
		  '(("varlist" "exp" "weight" "if" "in" "using" "options"
		  ) ado-subcommand-face nil t t)
		  "\\b"
		  ))

	  ;; things used with display
	  ;; since these are often split across lines, and Stata commands are hard
	  ;; to delimit, this will highlight even if out of context

	  ;;
	  ;; plain display
	  ;;
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("di" "dis" "disp" "displ" "displa" "display") ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "_asis"
		  "_c" "_co" "_con" "_cont" "_conti" "_contin" "_continu" "_continue"
		  "_n" "_ne" "_new" "_newl" "_newli" "_newlin" "_newline" 
		  "_quote"
		  ) 
			 ado-subcommand-face)
		  "\\b"
		  ))
	  ;;
	  ;; request (for a local macro)
	  ;;
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("di" "dis" "disp" "displ" "displa" "display") ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "_r" "_re" "_req" "_requ" "_reque" "_reques" "_request"
		  ) 
			 ado-subcommand-face)
		  "([ \t]*"
		  '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-variable-name-face)
		  "[ \t]*)"
		  ))
	  ;;
	  ;; things which take numbers
	  ;; some left as is, because they are used in dictionaries...
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("di" "dis" "disp" "displ" "displa" "display") ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "_char"
		  "_col" "_colu" "_colum" "_column"
		  "_d" "_du" "_dup"
		  "_n" "_ne" "_new" "_newl" "_newli" "_newlin" "_newline"
		  "_s" "_sk" "_ski" "_skip"
		  ) 
			 ado-subcommand-face)
		  "([ \t]*"
		  '(("[1-9]+[0-9]*") ado-constant-face)
		  "[ \t]*)"
		  ))
	  ;;
	  ;; other display subcommands
     (eval-when-compile
       (make-regexps
		  '(("di" "dis" "disp" "displ" "displa" "display") ado-builtin-harmless-face)
		  "[ \t]+"
		  '(("as") ado-subcommand-face)
		  "[ \t]+"
        '((
		  "err" "erro" "error" 
		  "inp" "inpu" "input"
		  "res" "resu" "resul" "result" 
		  "text" "txt"
		  ) ado-subcommand-face)
        "\\b"
        ))
	  ;; trust Stata corp to use different prepositions...
     (eval-when-compile
       (make-regexps
        "\\b"
		  '(("di" "dis" "disp" "displ" "displa" "display" ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '(("in") ado-builtin-harmless-face)
		  "[ \t]+"
        '((
		  "smcl"
		  ) ado-subcommand-face)
        "\\b"
        ))
     ;;
     ;; obsolete coloring
     (eval-when-compile
       (make-regexps
        "\\b"
		  '(("di" "dis" "disp" "displ" "displa" "display") ado-builtin-harmless-face)
		  "[ \t]+"
		  '(("in") ado-obsolete-face)
		  "[ \t]+"
        '((
		  "b" "bl" "blu" "blue" 
		  "g" "gr" "gre" "gree" "green"
		  "r" "re" "red"
		  "w" "wh" "whi" "whit" "white"
		  "y" "ye" "yel" "yell" "yello" "yellow"
		  ) ado-obsolete-face)
        "\\b"
        ))
     ;;
     ;; foreach ... in
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("foreach") ado-builtin-harmless-face)
        "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-variable-name-face)
        '(("[ \t]+in[ \t]+") ado-subcommand-face t)
        ))
	  ;; 
	  ;; foreach ... of
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("foreach") ado-builtin-harmless-face)
        "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-variable-name-face t)
        '(("[ \t]+of[ \t]+") ado-subcommand-face t)
        '((
		  "glo" "glob" "globa" "global"
		  "loc" "loca" "local" 
		  "new" "newl" "newli" "newlis" "newlist" 
		  "num" "numl" "numli" "numlis" "numlist" 
		  "var" "varl" "varli" "varlis" "varlist" 
		  ) ado-subcommand-face t)
        "\\b"
        ))
	  ;; forvalues ... = ??
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("forv" "forva" "forval" "forvalu" "forvalue" "forvalues") ado-builtin-harmless-face)
        "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-variable-name-face t)
        "[ \t]*=[ \t]*"
        ))

     ;; gettoken
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("gettoken") ado-builtin-harmless-face)
		  '(("\\([ \t]+(\\(loc\\|glob\\)al)\\)?") ado-subcommand-face t)
        '(("\\([ \t]+[a-zA-Z]+[a-zA-Z0-9_]*\\)\\{1,2\\}") ado-variable-name-face t)
        "[ \t]*:[ \t]*"
		  '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-variable-name-face t)
        ))
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("gettoken") ado-builtin-harmless-face)
		  '(("\\([ \t]+(\\(loc\\|glob\\)al)\\)?") ado-subcommand-face t)
        '(("[ \t]+[a-zA-Z]+[a-zA-Z0-9_]*") ado-variable-name-face t)
		  '(("\\([ \t]+(\\(loc\\|glob\\)al)\\)?") ado-subcommand-face t)
        '(("[ \t]+[a-zA-Z]+[a-zA-Z0-9_]*") ado-variable-name-face t)
        "[ \t]*:[ \t]*"
		  '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-variable-name-face t)
        ))
     

    
	  ;; labels
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("la" "lab" "labe" "label"
		  ) ado-builtin-harmless-face t)
		  "[ \t]"
		  '((
		  "da" "dat" "data"
		  "de" "def" "defi" "defin" "define"
		  "di" "dir" 
		  "drop" 
		  "lang" "langu" "langua" "languag" "language" 
		  "l" "li" "lis" "list"
		  "save"
		  "val" "valu" "value" "values"
		  "var" "vari" "varia" "variab" "variabl" "variable"
		  ) ado-subcommand-face t)			 ; was nil t t
		  "\\b"
		  ))
	  ;; mfp arguments
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("mfp") ado-builtin-harmless-face t)
		  "[ \t]"
		  '(( 
			  "clogit" "cnreg" "glm" "logistic" "logit" "mlogit"
			  "nbreg" "ologit" "oprobit" "poisson"
			  "probit" "qreg" "regress" "stcox" "streg" "xtgee"
			  ) ado-subcommand-face t) 
		  "\\b"
		  ))
	  ;; mfx
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("mfx") ado-builtin-harmless-face t)
		  "[ \t]+"
		  '(("c" "co" "com" "comp" "compu" "comput" "compute" 
		  "r" "re" "rep" "repl" "repla" "replay" 
		  ) ado-subcommand-face t) 
		  "\\b"
		  ))
    
	  ;; all Stata data-altering stuff
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "_pctile" "_predict"
		  "aorder" 
		  "ap" "app" "appe" "appen" "append" 
		  "bcskew0" "bs" "bsample" "bstrap"
		  "bys" "byso" "bysor" "bysort" 
		  "cd" "chdir" "clear" "clonevar" "collapse" "compress" 
		  "contract" "convert" "corr2data" "cross" "cttost" 
		  "dec" "deco" "decod" "decode" "destring"
		  "discard" "drawnorm" "drop" "dydx"
		  "ed" "edi" "edit" "egen" 
		  "en" "enc" "enco" "encod" "encode"
		  "erase"
		  "expand" "expandcl"
		  "fdades" "fdadesc" "fdadescr" "fdadescri" "fdadescrib" "fdadescribe" 
		  "fdasav" "fdasave" 
		  "fdause"
		  "filef" "filefi" "filefil" "filefilt" "filefilte" "filefilter" 
		  "fillin"
		  "form" "forma" "format"
		  "fracgen" "fracpred"
		  "g" "ge" "gen" "gene" "gener" "genera" "generat" "generate"
		  "gsort"
		  "impute" 
		  "inf" "infi" "infile" "infix" 
		  "inp" "inpu" "input"
		  "insheet" "integ" "ipolate" 
		  "joinby"
		  "keep" 
		  "lnskew0"
		  ) ado-builtin-harmful-face)
		  "\\b"
		  ))
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "makecns"
		  "mark" "markin" "markout"
		  "mata" "matrix"
		  "mer" "merg" "merge"
		  "mkdir" "mkmat" "mkspline"
		  "mleval" "mlmatsum" "mlsum""mlvecsum"
		  "modify" "mov" "move"
		  "mvdecode" "mvencode" 
		  "nlogitgen" "nlpred" "nobreak" 
		  "order" "orthog" "orthpoly"
		  "ou" "out" "outf" "outfi" "outfil" "outfile"
		  "outs" "outsh" "outshe" "outshee" "outsheet"
		  "pctile" 
		  "pkcollapse" "pkshape"
		  "post" "postclose" "postfile" 
		  "predict" "predictnl"
		  "preserve" "range"
		  "recast" "recode" 
		  "ren" "rena" "renam" "rename"
		  "renpfix" "replace" "restore" "rm" "rmdir"
		  "sappend" 
		  "sa" "sav" "save" "saveold"
		  "sample" "sdrop"
		  "separate"
		  "simul" "simulate" "sinfile" "smerge" 
		  "smooth" "snapspan" 
		  "so" "sor" "sort" "sortpreserve"
		  "split"
		  "ssave" "ssort" "stack" "statsby"
		  "stbase" "stfill" "stgen" "stjoin" "stsplit" "sttocc" "sttoct"
		  "suse" "svmat" "svymarkout" "sysuse"
		  "tostring"
		  "translate"
		  "tsappend" "tsfill" "tsrevar"
		  "u" "us" "use" "uselabel"
		  "webuse"
		  "xi" "xi:" 
		  "xmlsav" "xmlsave" "xmluse" 
		  "xtile" "xpose" 
		  "xtdata" "xtpred"
		  ) ado-builtin-harmful-face)
		  "\\b"
		  ))

	  ;; clear subcommands (new in Stata 10)
	  (eval-when-compile
		 (make-regexps
		  "^[ \t]*"
		  '(("clear") ado-builtin-harmful-face)
		  "[ \t]*"
		  '((
			  "ado" "all"
			  "mata"
			  "programs"
			  "results"
			  "[*]") ado-subcommand-face t)
		  "\\b"
		  ))
			  
	  ;; assignment of macros
	  ;;  local macros have different naming conventions (boo)
	  ;;  marksample added, because need harmful/scalar name
	  (eval-when-compile
		 (make-regexps
		  "[ \t]*"
		  '((
		  "gl" "glo" "glob" "globa" "global" 
		  "marksample"
		  "sca" "scal" "scala" "scalar" 
		  ) ado-builtin-harmful-face)
		  "[ \t]+`*"
		  '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
		  ))
	  (eval-when-compile
		 (make-regexps
		  "[ \t]*"
		  '((
		  "loc" "loca" "local" 
		  ) ado-builtin-harmless-face)
		  "[ \t]+\\(--\\|\\+\\+\\|[`]+\\)?"
		  '(("[a-zA-Z_0-9]+"
		  ) ado-variable-name-face t)
		  "\\b"
		  ))
	  ;; warning for local wrong--
	  (eval-when-compile
		 (make-regexps
		  "[ \t]*"
		  '((
		  "loc" "loca" "local" 
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '(("\\(`\\|[a-zA-Z_0-9]\\|'\\)+"
			  ) ado-variable-name-face t)
		  '(("++" "--") ado-obsolete-face t)
		  ))

	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("sca" "scal" "scala" "scalar" ) ado-builtin-harmful-face)
		  '(("\\([ \t]+\\(def\\|defi\\|defin\\|define\\)\\)?" ) ado-subcommand-face)
		  "[ \t]+"
		  '(("[a-zA-Z_][a-zA-Z0-9_]*") ado-variable-name-face t)
		  ))
	  (eval-when-compile
		 (make-regexps
		  "[ \t]*"
		  '((
		  "gl" "glo" "glob" "globa" "global" 
		  "ma" "mac" "macr" "macro"
		  "sca" "scal" "scala" "scalar" 
		  ) ado-builtin-harmful-face)
		  "[ \t]+"
		  '(("drop") ado-subcommand-face t)
		  "[ \t]+"
		  '(("\\([a-zA-Z_]+[a-zA-Z_0-9]*\\b\\)?"
		  ) ado-variable-name-face t)
		  ))
	  (eval-when-compile
		 (make-regexps
		  "[ \t]*"
		  '((
		  "loc" "loca" "local" 
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '(("drop") ado-subcommand-face t)
		  "[ \t]+"
		  '(("\\([a-zA-Z_0-9]+\\b\\)?"
		  ) ado-variable-name-face t)
		  ))
    
	  ;;
	  ;; an attempt to include the extended macro names (which will probably fail)
	  ;; single word extended macro names
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		  ) ado-builtin-harmless-face t)
		  "[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
        '((
		  "adosubdir"
		  "char" "cole" "coleq" 
		  "colf" "colfu" "colful" "colfull" "colfulln" "colfullna" "colfullnam" "colfullname" "colfullnames" 
		  "coln" "colna" "colnam" "colname" "colnames" 
		  "constraint"
		  "copy[ \t]+global" "copy[ \t]+local"
		  "dirsep" 
		  "di" "dir" "dis" "disp" "displ" "displa" "display" 
		  "env" "envi" "envir" "enviro" "environ" "environm" "environme" "environmen" "environment" 
		  "f" "fo" "for" "form" "forma" "format" 
		  "lab" "labe" "label"
		  "list"
		  "permname" "piece" "properties" "pwd"
		  "rowe" "roweq" 
		  "rowf" "rowfu" "rowful" "rowfull" "rowfulln" "rowfullna" "rowfullnam" "rowfullname" "rowfullnames" 
		  "rown" "rowna" "rownam" "rowname" "rownames" 
		  "sort" "sorte" "sorted" "sortedb" "sortedby" 
		  "tempf" "tempfi" "tempfil" "tempfile" "tempv" "tempva" "tempvar" 
		  "tsnorm" 
		  "ty" "typ" "type"
		  "word[ \t]+count"
		  ) ado-subcommand-face t)
		  "\\b"
        ))
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		  ) ado-builtin-harmless-face t)
		  "[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
        '(("word") ado-subcommand-face t)
		  "[ \t]+\\([0-9]+\\|`[^ \t]*'\\)[ \t]+"
		  '(("of") ado-subcommand-face t)
		  "\\b"
        ))
     ;; things with parens in them (sheesh)
     ;; not included above, incase someone uses a font which has a background color
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		  ) ado-builtin-harmless-face t)
		  "[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
        '(("e" "r") ado-subcommand-face t)
		  "([ \t]*"
		  '(("functions" "macros" "matrices" "scalars"
		  ) ado-subcommand-face t)
		  "[ \t]*)"
        ))
     ;; damn s(macros)
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		  ) ado-builtin-harmless-face t)
		  "[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
        '(("s") ado-subcommand-face t)
		  "([ \t]*"
		  '(("macros") ado-subcommand-face t)
		  "[ \t]*)"
        ))
     ;; twin word macros length and subinstr
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		  ) ado-builtin-harmless-face t)
		  "[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
        '(("length" "subinstr") ado-subcommand-face t)
		  "[ \t]+"
		  '((
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" ) ado-subcommand-face t)
		  "\\b"
        ))
     ;; serset macros
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		  ) ado-builtin-harmless-face t)
		  "[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
        '(("serset") ado-subcommand-face t)
		  "[ \t]+"
		  '((
		  "N"
		  "format" "k" "id" "max" "min" "type" "varnames" "varnum"
		  ) ado-subcommand-face t)
		  "\\b"
        ))
     ;;
     ;; sheesh, now there are combined abbreviations!
     ;;
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		  ) ado-builtin-harmless-face t)
		  "[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
        '((
		  "data"
		  "val" "valu" "value" 
		  "var" "vari" "varia" "variab" "variabl" "variable" 
		  ) ado-subcommand-face t)
        "[ \t]+"
        '((
		  "l" "la" "lab" "labe" "label"
		  ) ado-subcommand-face t)
        "\\b"
        ))
     ;; macro list commands start here
     ;; single word commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		  ) ado-builtin-harmless-face t)
		  "[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
		  '(("list") ado-subcommand-face t)
		  "[ \t]+"
        '((
		  "clean"
		  "dups"
		  "retok" "retoke" "retoken" "retokeni" "retokeniz" "retokenize"
		  "sizeof"
		  "sort"
		  "uniq"
		  ) ado-subcommand-face t)
		  "[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
		  "\\b"
        ))
     ;; operator-like word commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		  ) ado-builtin-harmless-face t)
		  "[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
		  '(("list") ado-subcommand-face t)
		  "[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
		  "[ \t]*\\([|&-]\\|==\\|===\\|in\\)[ \t]*"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
		  "\\b"
        ))
     ;; friggin' posof subcommand
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		  ) ado-builtin-harmless-face t)
		  "[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
		  '(("list") ado-subcommand-face t)
		  "[ \t]+"
		  '(("posof" ) ado-subcommand-face t)
		  "[ \t]+\".*?\"[ \t]+"
		  '(("in") ado-subcommand-face t)
		  "[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
		  "\\b"
        ))
     ;; all subcommand added to build 01-jul-2004
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		  ) ado-builtin-harmless-face t)
		  "[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
		  '(("all") ado-subcommand-face t)
		  "[ \t]+"
        '((
		  "globals" "matrices" "scalars"
		  ) ado-subcommand-face t)
		  "\\b"
        ))
     ;; all numeric/string
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		  ) ado-builtin-harmless-face t)
		  "[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
		  '(("all") ado-subcommand-face t)
		  "[ \t]+"
		  '((
		  "numeric" "string"
		  ) ado-subcommand-face t)
		  "[ \t]+"
        '(("scalars") ado-subcommand-face t)
		  "\\b"
        ))
    
	  ;; choosing temp names
	  (eval-when-compile
		 (make-regexps
		  "^[ \t]*"
		  '(("tempname" "tempfile" "tempvar"
		  ) ado-builtin-harmless-face)
		  "[ \t]+`*"
		  '(("\\([a-zA-Z_]+[a-zA-Z_0-9]*?[ \t]*\\)+"
		  ) ado-variable-name-face t)
		  ))
	  ;; other macro commands
	  (eval-when-compile
		 (make-regexps
		  "[ \t]*"
		  '((
		  "ma" "mac" "macr" "macro"
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "di" "dir"
		  "l" "li" "lis" "list"
		  )
			 ado-subcommand-face t)
		  "\\b"
		  ))
	  (eval-when-compile
		 (make-regexps
		  "[ \t]*"
		  '((
		  "ma" "mac" "macr" "macro"
		  ) ado-builtin-harmful-face)
		  "[ \t]+"
		  '((
		  "s" "sh" "shi" "shif" "shift"
		  )
			 ado-subcommand-face t)
		  "\\b"
		  ))

	  ;; other macro commands
	  (eval-when-compile
		 (make-regexps
		  "[ \t]*"
		  '((
		  "sca" "scal" "scala" "scalar" 
		  ) ado-builtin-harmless-face t)
		  "[ \t]+"
		  '((
		  "di" "dir"
		  "l" "li" "lis" "list"
		  )
			 ado-subcommand-face t)
		  "\\b"
		  ))

	  ;; stata functions i.e. things which require () after them 
	  ;; obsolete functions are after this list
	  ;; finally included matrix functions
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("_byn1"  "_byn2" "_caller" 
			 "abbrev" "abs" "acos" "asin" "atan" "atan2" "atanh" "autocode"
			 "betaden" "binomial" "binomialtail" "binormal" "byteorder"
			 "Cdhms" "Chms" "Clock" "Cmdyhms" "Cofc" "Cofd"
			 "c" "ceil" "char" "chi2" "chi2tail" "cholesky" "chop" "cofC" "cofd" "comb" 
			 "clip" "cloglog" "clock" 
			 "colnumb" "colsof" "cond" "corr" "cos" 
			 "daily" "date" "day"
			 "det"
			 "dgammapda" "dgammapdada" "dgammapdadx" "dgammapdx" "dgammapdxdx"
			 "dhms"
			 "diag" "diag0cnt" "digamma" 
			 "dofC" "dofc" "dofd" "dofh" "dofm" "dofq" "dofw" "dofy" "dow" "doy"
			 "e" "el" "epsdouble" "epsfloat" "exp"
			 "F" "Fden" "Ftail" "float" "floor" "fmtwidth" 
			 "gammaden" "gammap" "gammaptail" "get"
			 "hadamard" "halfyear" "halfyearly" "has_eprop" "hh" "hhC" "hofd" "hms" "hours"
			 "I" "ibeta" "ibetatail" "indexnot" "inlist" "inrange" "int"
			 "inv" "invbinomial" "invbinomialtail" "invchi2" "invchi2tail" "invcloglog"
			 "invF" "invFtail" "invgammap" "invgammaptail" "invibeta" "invibetatail" "invlogit" 
			 "invnchi2" "invnFtail" "invnibeta" 
			 "invnormal" "invsym" "invttail"
			 "irecode" "issymetric" "itrim"
			 "J"
			 "length" "ln" "lnfactorial" "lngamma" "log" "log10" "logit" "lower" "ltrim" 
			 "matmissing" "matrix" "matuniform" 
			 "max" "maxbyte" "maxdouble" "maxfloat" "maxint" "maxlong" 
			 "mdy" "mdyhms" "mi"
			 "min" "minbyte" "mindouble" "minfloat" "minint" "minlong" "minutes"
			 "missing" "mm" "mmC" "mod" "mofd" "month" "monthly" "mreldif"
			 "msofhours" "msofminutes" "msofseconds"
			 "nbetaden" "nchi2" "normal" "normalden" "nFden" "nFtail" "nibeta" "npnchi2" "nullmat"
			 "plural" "proper"
			 "qofd" "quarter" "quarterly"
			 "r" "rbeta" "rbinomial" "rchi2" "real" "recode"
			 "regexm" "regexr" "regexs"
			 "reldif" "replay" "return" "reverse" 
			 "rgamma" "rhypergeometric" "rnbinomial" "rnormal"
			 "round" "rownumb" "rowsof" "rpoisson" "rt" "rtrim" "runiform"
			 "s" "scalar" "seconds" "sign" "sin" "sqrt" "ss" "ssC"
			 "string" "strlen" "strmatch" "strofreal" "strpos"
			 "subinstr" "subinword" "substr" "sum" 
			 "sweep" 
			 "tC"
			 "tan" "tanh" "tc" "td" "tden" "th" "tin" "tm" "tq" "trace" "trigamma" "trim" "trunc" "ttail" "tw" "twithin"
			  "upper"
			  "vec" "vecdiag"
			  "week" "weekly" "wofd" "word" "wordcount"
			  "year" "yearly" "yh" "ym" "yofd" "yq" "yw"
			  )
			 ado-function-name-face t)
		  "("
		  ))
	  ;;
	  ;; obsolete functions requiring () after them
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
			  "Binomial"
			  "binorm" 
			  "chiprob"
			  "d"
			  "fprob"
			  "group"
			  "h" 
			  "index"
			  "invchi" "invfprob" "invnchi" "invnorm" "invt" 
			  "issym"
			  "lnfact"
			  "m" "match"
			  "nchi" "norm" "normd" "normden" "normprob" "npnchi"
			  "q" 
			  "syminv" "tprob" 
			  "uniform" "uniform0"
			  "y"
			  "w"
			  )
			 ado-obsolete-face t)
        "("
        ))
	  ;; stata 'functions' i.e. things which require [] after them
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("_b" "_coef" "_se")
			 ado-builtin-harmless-face t)
		  "\\["
		  ))
	  ;; common Stata options which require a () after them
	  (eval-when-compile
		 (make-regexps
		  "[, \t]+"
		  '(("bands" "by" "connect" "density" "gap" "iterate" "ltolerance" "margin"
		  "psize" "saving" "tlabel" "tolerance"
		  "xlabel" "xscale" "ylabel" "yscale")
			 ado-subcommand-face t)
		  "("
		  ))
	  ;; egen 'function' options
	  (eval-when-compile
		 (make-regexps
		  "[ \t]*egen[ \t]+.*=[ \t]*"
		  '((
			  "anycount" "anymatch" "anyvalue"
			  "concat" "count" "cut" 
			  "diff"
			  "ends" 
			  "fill" "group" "iqr"
			  "kurt"
			  "mad" "max" "mdev" "mean" "median" "min" "mode" "mtr" 
			  "pc" "pctile" 
			  "rank" 
			  "rowfirst" "rowlast" "rowmax" "rowmean" "rowmin" "rowmiss" "rownonmiss" "rowsd" "rowtotal" 
			  "sd" "seq" "skew" "std" 
			  "tag" "total"
			  )
			 ado-function-name-face t)
		  "("
		  ))
	  ;; egen 'function' options -- obsolete
	  (eval-when-compile
		 (make-regexps
		  "[ \t]*egen[ \t]+.*=[ \t]*"
		  '((
			  "any"
			  "eqany"
			  "ma"
			  "neqany"
			  "rfirst" "rlast" "rmax" "rmean" "rmin" "rmiss" "robs" "rsd" "rsum" 
			  "sum"
			  )
			 ado-obsolete-face t)
		  "("
		  ))

	  ;; All Custom ado files which are 'reliable' and which are not file killers
	  ;; this might be a useless endeavor --- but I cannot generate tag files
	  ;; all the s-extensions are listed under Stata's name (since they alter
	  ;; data and will be moved tot he utils directory
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("anypath" "autolab" "checkvar" "ck1icd9" "ckicd9"
		  "dtapath" "dupclean" "echo"
		  "getdate" "getlbl" "getnames" "getobs"
		  "icd9x" "issorted" "isfile" "jultoe" "jultof" "jultomdy" "knowndup"
		  "labeldir" "linker" 
		  "markit" "makewide" "missize" "mpcounts" 
		  "nodups" "notefile" "prov2zip"
		  "qcolsum" "qorder" 
		  "random" "readraw" "readzip" "repart"
		  "setup" "stdrate"
		  "timeslot"
		  "_addext" "_brclean" "_brckado" "_brdlog"
		  "_ckbad" "_ckdunno" "_ckdupl" "_ckmiss" "_ckok" "_ckwarn"
		  "_delimit" "_filenm" "_lookup" "_mk_ck"
		  ) ado-site-harmless-face)
		  "\\b"
		  ))


	  ;;
	  ;; now, presenting smcl
	  ;;
	  ;; Syntax 1
	  (eval-when-compile
		 (make-regexps
		  '(("{") ado-constant-face)
		  "[ \t]*"
		  '((
		  "\.\.\."
		  "\.-"
		  "asis"
		  "bf" "break"
		  "cmd"
		  "depvar" "depvars" "depvarlist" "dtype"
		  "err" "error"
		  "hi" "hilite" "hline"
		  "ifin" "imp" "indepvars" "input" "it"
		  "newvar"
		  "p" "p_end" "p2colreset" "p2line" 
		  "phang" "phang2" "phang3" "pin" "pin2" "pin3" "pmore" "pmore2" "pmore3" "psee" "pstd"
		  "res" "reset" "result"
		  "s6hlp"
		  "sf" "synopthdr" "synoptline" "synoptset"
		  "tab" "text" "txt"
		  "var" "varlist" "varname" "vars"
		  "weight"
		  ) ado-builtin-harmless-face t)
		  "[ \t]*?"
        '(("}") ado-constant-face)
        ))

	  ;; Syntax 2
	  (eval-when-compile
		 (make-regexps
		  '(("{") ado-constant-face)
		  "[ \t]*"
		  '((
		  "ado_d"
		  "back" "bf" "bind" 
		  "center" "centre" "clearmore" "cmd"
		  "depvar" "depvars" "depvarlist" "dlgtab"
		  "err" "error"
		  "help_d" "hi" "hilite"
		  "indepvars" "inp" "input" "it"
		  "net_d" "netfrom_d" "news" "newvar"
		  "p2col" "p2coldent"
		  "rcenter" "rcentre" "res" "reset" "result" "right"
		  "search_d"
		  "sf" "synopt" "synopthdr" "syntab"
		  "text" "title" "txt"
		  "ul" "update_d"
		  "var" "varlist" "varname" "vars" "view_d"
		  ) ado-builtin-harmless-face t)
		  "[ \t]*?"
		  '((":") ado-constant-face)
		  '(("[^}]+") ado-subcommand-face t)
		  '(("}") ado-constant-face)
		  ))
	  ;; making comments look like comments
	  (eval-when-compile
		 (make-regexps
		  '(("{") ado-constant-face)
		  "[ \t]*"
		  '(("\\*") ado-builtin-harmless-face t)
		  "[ \t]*?"
		  '((":") ado-constant-face)
		  '(("[^}]+") ado-comment-face t)
		  '(("}") ado-constant-face)
		  ))

	  ;; Syntax 2plus for cmdab
	  (eval-when-compile
		 (make-regexps
		  '(("{") ado-constant-face)
		  "[ \t]*"
		  '(("cmdab") ado-builtin-harmless-face)
		  '((":") ado-constant-face)
		  '(("[a-zA-Z][a-zA_Z_0-9]*") ado-subcommand-face)
		  '((":") ado-constant-face)
		  '(("[^}]*?") ado-subcommand-face)
		  '(("}") ado-constant-face)
		  ))

	  ;; Syntax 3 with free form args
	  (eval-when-compile
		 (make-regexps
		  '(("{") ado-constant-face)
		  "[ \t]*"
		  '((
		  "ado"
		  "browse"
		  "c" "char"
		  "dialog"
		  "help" "helpb"
		  "marker" "matacmd"
		  "net"
		  "opt"
		  "search" "stata"
		  "update"
		  "view"
		  ) ado-builtin-harmless-face t)
		  "[ \t]+"
		  '(("[^:]*?") ado-subcommand-face t)
		  '(("}") ado-constant-face)
		  ))
	  ;; Syntax 3 with on/off choices
	  (eval-when-compile
		 (make-regexps
		  '(("{") ado-constant-face)
		  "[ \t]*"
		  '(("ul") ado-builtin-harmless-face t)
		  "[ \t]*"
		  '(("off" "on") ado-subcommand-face t)
		  '(("}") ado-constant-face)
		  ))
	  ;; Syntax 3 comments
	  (eval-when-compile
		 (make-regexps
		  '(("{") ado-constant-face)
		  "[ \t]*"
		  '(("\\*") ado-builtin-harmless-face t)
		  '((".*?") ado-comment-face)
		  '(("}") ado-constant-face)
		  ))
	  ;; Syntax 3 with single numerical args --- allow simple macros, too
	  (eval-when-compile
		 (make-regexps
		  '(("{") ado-constant-face)
		  "[ \t]*"
		  '((
			  "col" "hline" "space" "synoptset"
			  ) ado-builtin-harmless-face t)
		  "[ \t]+"
		  '(("\\([0-9]+\\|`[a-zA-Z0-9_`']*'\\)") ado-subcommand-face t)
		  "[ \t]*"
		  '(("}") ado-constant-face)
		  ))
	  ;; Syntax 3 with a single numerical arg and a choice (nice syntax)
	  (eval-when-compile
		 (make-regexps
		  '(("{") ado-constant-face)
		  "[ \t]*"
		  '((
			  "synoptset"
			  ) ado-builtin-harmless-face t)
		  '(("\\([ \t]+\\([0-9]+\\|`[a-zA-Z0-9_`']*'\\)\\)?") ado-subcommand-face t)
		  '(("\\([ \t]+\\(notes\\|tabbed\\)\\)?") ado-subcommand-face t)
		  "[ \t]*"
		  '(("}") ado-constant-face)
		  ))
	  ;; Syntax 3 with 0 through 4 numerical args --- allow simple macros, too
	  (eval-when-compile
		 (make-regexps
		  '(("{") ado-constant-face)
		  "[ \t]*"
		  '((
			  "p"
			  ) ado-builtin-harmless-face t)
		  '(("\\([ \t]+\\([0-9]+\\|`[a-zA-Z0-9_`']*'\\)\\)\\{0,4\\}") ado-subcommand-face t)
		  "[ \t]*"
		  '(("}") ado-constant-face)
		  ))
	  ;; Syntax 3 with exactly 4 numerical args --- allow simple macros, too
	  (eval-when-compile
		 (make-regexps
		  '(("{") ado-constant-face)
		  "[ \t]*"
		  '((
			  "p2colset"
			  ) ado-builtin-harmless-face t)
		  '(("\\([ \t]+\\([0-9]+\\|`[a-zA-Z0-9_`']*'\\)\\)\\{4\\}") ado-subcommand-face t)
		  "[ \t]*"
		  '(("}") ado-constant-face)
		  ))
	  ;; Syntax 3 with exactly 2 numerical arguments. Doesn't SMCL have a clean syntax?
	  (eval-when-compile
		 (make-regexps
		  '(("{") ado-constant-face)
		  "[ \t]*"
		  '((
			  "p2line"
			  ) ado-builtin-harmless-face t)
		  '(("\\([ \t]+\\([0-9]+\\|`[a-zA-Z0-9_`']*'\\)\\)\\{2\\}") ado-subcommand-face t)
		  "[ \t]*"
		  '(("}") ado-constant-face)
		  ))
       
	  ;; Syntax 4
	  ;; those whose subcommands are not easy
	  ;;  on second thought: for all!
	  (eval-when-compile
		 (make-regexps
		  '(("{") ado-constant-face)
		  "[ \t]*"
		  '((
		  "ado"
		  "browse"
		  "center" "centre"
		  "dialog" "dlgtab" "dup"
		  "help" "helpb"
		  "lalign"
		  "matacmd"
		  "net"
		  "opt"
		  "ralign" "rcenter" "rcentre"
		  "search" "stata"
		  "update"
		  "view"
		  ) ado-builtin-harmless-face prepend)
		  "[ \t]+"
		  '((".*?") ado-subcommand-face)
		  '((":") ado-constant-face t)
		  '(("[^}]+?") ado-subcommand-face t)
		  '(("}") ado-constant-face)
		  ))
	  ;; Syntax 4 with exactly 4 numerical args to start with
	  (eval-when-compile
		 (make-regexps
		  '(("{") ado-constant-face)
		  "[ \t]*"
		  '((
			  "p2col"
			  ) ado-builtin-harmless-face prepend)
		  '(("\\([ \t]+\\([0-9]+\\|`[a-zA-Z0-9_`']*'\\)\\)\\{4\\}") ado-subcommand-face t)
		  "[ \t]*"
		  '((":") ado-constant-face t)
		  '(("[^}]+?") ado-subcommand-face t)
		  '(("}") ado-constant-face)
		  ))
	  ;; Syntax 3
	  ;; for the manhelp and manhelpi exceptions
	  (eval-when-compile
		 (make-regexps
		  '(("{") ado-constant-face)
		  "[ \t]*"
		  '((
			  "manhelp" "manhelpi"
			  ) ado-builtin-harmless-face prepend)
		  "[ \t]+"
		  '(("\\w*?") ado-subcommand-face)
		  "[ \t]+"
		  '(("\\w*?") ado-subcommand-face)
		  '(("}") ado-constant-face)
		  ))
	  ;; Syntax 4 exceptions
	  (eval-when-compile
		 (make-regexps
		  '(("{") ado-constant-face)
		  "[ \t]*"
		  '((
			  "manhelp" "manhelpi"
			  ) ado-builtin-harmless-face prepend)
		  "[ \t]+"
		  '(("\\w*?") ado-subcommand-face)
		  "[ \t]+"
		  '(("\\w*?") ado-subcommand-face)
		  '((":") ado-constant-face t)
		  '(("[^}]+?") ado-subcommand-face t)
		  '(("}") ado-constant-face)
		  ))
	  ;; special help syntax with double-hash
	  ;;  sheesh --- should be able to reuse with optional stuff
	  (eval-when-compile
		 (make-regexps
		  '(("{") ado-constant-face)
		  '(("help" "helpb") ado-builtin-harmless-face)
		  "[ \t]+"
		  '(("[^:# \t]*") ado-subcommand-face t)
		  '(("##") ado-constant-face t)
		  '(("\\w*?") ado-subcommand-face t)
		  '(("\\(|\\w*?\\)?") ado-subcommand-face t)
		  '(("}") ado-constant-face)
		  ))
	  (eval-when-compile
		 (make-regexps
		  '(("{") ado-constant-face)
		  '(("help" "helpb") ado-builtin-harmless-face)
		  "[ \t]+"
		  '(("[^:# \t]*") ado-subcommand-face t)
		  '(("##") ado-constant-face t)
		  '(("\\w*?") ado-subcommand-face t)
		  '(("\\(|\\w*?\\)?") ado-subcommand-face t)
		  "[ \t]*"
		  '((":") ado-constant-face)
		  '(("[^}]*?") ado-subcommand-face t)
		  '(("}") ado-constant-face)
		  ))
    
	  ;; class stuff
;;; builtin prefix operators
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("\\.") ado-function-name-face)
		  '(("Global" "Local" "Super") ado-function-name-face t)
		  '(("\\.") ado-function-name-face t)
		  ))
;;; builtin class functions and modifiers
	  (eval-when-compile
		 (make-regexps
		  "\\>"
		  '(("\\.") ado-function-name-face)
		  '((
		  "Arrdropall" "Arrdropel" "Arrpop" "Arrpush"
		  "Declare"
		  "arrindexof" "arrnels"
		  "classmv" "classname" "copy"
		  "dynamicmv"
		  "instancemv" "isa" "isofclass"
		  "new"
		  "objkey" "objtype"
		  "ref" "ref_n"
		  "superclass"
		  "uname" 
		  ) ado-function-name-face t)
		  "\\b"
		  ))
;;; class command
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("class") ado-builtin-harmless-face)
        "[ \t]+"
        '((
		  "exit"
		  ) ado-subcommand-face t)
        "\\b"
        ))
     ;; highlighting class names
     (eval-when-compile
       (make-regexps
		  "\\b"
		  '(("class") ado-builtin-harmful-face)
		  "[ \t]+"
		  '(("[_a-zA-Z][_a-zA-Z0-]*") ado-builtin-harmful-face t)
		  ))
     ;; all the different declarations
     (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "class" "classw" "classwi" "classwid" "classwide" 
		  "instance" "instances" "instancesp" "instancespe" "instancespec" 
		  "instancespeci" "instancespecif" "instancespecifi" "instancespecific" 
		  ) ado-builtin-harmless-face)
		  "[ \t]*:"
		  ))
    
	  ;; classutil stuff
	  (eval-when-compile
       (make-regexps
        "\\b"
        '(("classutil") ado-builtin-harmful-face)
        "[ \t]+"
        '((
		  "drop"
		  ) ado-subcommand-face t)
        "\\b"
        ))
	  (eval-when-compile
       (make-regexps
        "\\b"
        '(("classutil") ado-builtin-harmless-face)
        "[ \t]+"
        '((
		  "cdir"
		  "d" "de" "des" "desc" "descr" "descri" "describ" "describe"
		  "dir"
		  "which"
		  ) ado-subcommand-face t)
        "\\b"
        ))

	  ;; oh my - the creturn info!
	  (eval-when-compile
       (make-regexps
        "\\b"
        '(("cret" "cretu" "cretur" "creturn") ado-builtin-harmless-face)
        "\\W+"
        '((
		  "l" "li" "lis" "list" 
		  ) ado-subcommand-face)
        "\\b"
        ))

;;; the system 'constants' (which are not really constant) - the c() thingies
	  (eval-when-compile
       (make-regexps
        "\\b"
        '(("c(") ado-builtin-harmless-face t)
        "[ \t]*"
        '((
		  "ALPHA" "Mons" "Months" "MP" "N" "SE" "Wdays" "Weekdays"
		  "adopath" "adosize" "alpha" 
		  "born_date" "byteorder"
		  "changed" "checksum" "cmdlen" "console" "copycolor" "current_time" "current_date"
		  "dirsep" "dp"
		  "eolchar" "epsdouble" "epsfloat"
		  "fastscroll" "filedate" "filename" "flavor"
		  "graphic" 
		  "httpproxy" "httpproxyauth" "httpproxyhost" "httpproxyport" "httpproxypw" "httpproxyuser"
		  "icmap"
		  "k"
		  "level" "linegap" "linesize" "logtype"
		  "machine_type" "macrolen" 
		  "matacache" "matafavor" "matalibs" "matalnum" "matamofirst" "mataoptimize" "matastrict"
		  "matsize" 
		  "max_N_current" "max_N_theory" "max_cmdlen" 
		  "max_k_current" "max_k_theory" 
		  "max_macrolen" "max_matsize" "max_width_current" "max_width_theory" 
		  "maxbyte" "maxdb" "maxdouble" "maxfloat" "maxint" "maxiter" "maxlong" "maxstrvarlen" "maxvar" 
		  "memory"
		  "min_matsize"
		  "minbyte" "mindouble" "minfloat" "minint" "minlong"
		  "mode" "more"
		  "namelen"
		  "os" "osdtl"
		  "pagesize" "pi" "printcolor" "processors" "processors_lic" "processors_mach" "processors_max" "pwd"
		  "rc" "rmsg" "rmsg_time"
		  "scheme" "scrollbufsize" "searchdefault" "seed" "stata_version"
		  "sysdir_base" "sysdir_oldplace" "sysdir_personal" "sysdir_plus" "sysdir_site" "sysdir_stata"
		  "sysdir_updates" "timeout1" "timeout2" 
		  "trace" "tracedepth" "traceexpand" "tracehilite" "traceindent" "tracenumber" "tracesep" "type"
		  "varabbrev" "version" "virtual"
		  "width" 
		  ) ado-constant-face t)
		  "[ \t]*"
		  '((")") ado-builtin-harmless-face t)
        ))
;;; platform specific c() thingies
	  (eval-when-compile
       (make-regexps
        "\\b"
        '(("c(") ado-builtin-harmless-face t)
        "[ \t]*"
        '((
			  "autotabgraphs"
			  "dockable" "dockingguides" "doublebuffer"
			  "floatresults" "floatwindows"
			  "locksplitters"
			  "macgphengine"
			  "odbcmgr"
			  "persistfv" "persistvtopic" "piccomments" "pinnable"
			  "reventries" "revwindow"
			  "smalldlg" "smoothfonts" "smoothsize"
			  "update_interval" "update_prompt" "update_query"
			  "use_atsui_graph" "use_qd_text"
			  "varlabelpos" "varwindow"
			  "xptheme"
			  ) ado-platform-specific-face t)
		  "[ \t]*"
		  '((")") ado-builtin-harmless-face t)
        ))
;;; obsolete c() thingies
	  (eval-when-compile
       (make-regexps
        "\\b"
        '(("c(") ado-builtin-harmless-face t)
        "[ \t]*"
        '((
			  "smalldlg"
			  "xptheme"
			  ) ado-obsolete-face t)
		  "[ \t]*"
		  '((")") ado-builtin-harmless-face t)
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stuff for writing dlg files  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("VERSION") ado-builtin-harmless-face)
		  "\\W+"
		  '((
		  "8\\([.]\\(0\\|1\\|2\\)\\)?" "9\\([.]\\(0\\|00\\|1\\|2\\)\\)?"
		  ) ado-subcommand-face)
		  "\\b"
		  ))
    
	  ;; general builtins for dialogs
	  ;; here - the harmless faces define static text 
	  ;;        whereas the harmful face defines dynamic text

	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "BUTTON"
		  "CANCEL" "CHECKBOX" "COMBOBOX"
		  "DEFINE" "DIALOG"
		  "EDIT" 
		  "FILE"
		  "HELP"
		  "INCLUDE"
		  "LISTBOX"
		  "OK"
		  "RADIO" "RESET"
		  "SPINNER" "SUBMIT"
		  "VARLIST" "VARNAME"
		  "stopbox[ \t]+note"
		  "stopbox[ \t]+rusure"
		  "stopbox[ \t]+stop"
		  ) ado-builtin-harmful-face)
		  "\\b"
		  ))
    
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "BEGIN"
		  "COLOR"
		  "END" "EXP"
		  "FRAME"
		  "GROUPBOX"
		  "LIST"
		  "POSITION" "PROGRAM"
		  "SCRIPT"
		  "TEXT"
		  "allowxi"
		  "beginoptions" "by" "bysort"
		  "endoptions" "exit"
		  "ifexp" "inrange"
		  "option" "optionarg"
		  "put"
		  "require"
		  "stata"
		  "varlist"
		  "weight"
		  ) ado-builtin-harmless-face)
		  "\\b"
		  ))

	  ;; removed 	  "\\."
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "action"
		  "gaction"
		  "script"
		  "view"
		  "program"
		  ) ado-function-name-face)
		  "\\b"
		  ))

	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "call" ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "\\."
		  "action"
		  "gaction"
		  "script"
		  "view"
		  "program"
		  ) ado-subcommand-face t)
		  "\\b"
		  ))
	  ;; stata functions i.e. things which require () after them 
	  ;; obsolete functions are after this list
	  ;; finally included matrix functions
	  (eval-when-compile
		 (make-regexps
		  "\\>"
		  '(("\\.") ado-function-name-face)
		  '((
		  "contains"
		  "endswith"
		  "isdefault" "isenabled" "iseq" "iseqignorecase" "isge" "isgt" "isle" "islt" "isneq" "isvisible"
		  )
			 ado-function-name-face t)
		  "("
		  ))

	  ;; mata keywords --- won't override, because they are only active in a mata block...
	  ;;  and mata block checking has not been implemented
	  (eval-when-compile
		 (make-regexps
		  "[ \t]+"
		  '((
		  "break"
		  "colvector" "complex" "continue"
		  "do"
		  "external"
		  "for" "function"
		  "goto"
		  "if" 
		  "matrix"
		  "numeric"
		  "pointer" "pragma" 
		  "real" "return" "rowvector"
		  "scalar" "string" 
		  "transmorphic" 
		  "vector" "version" "void"
		  "while"
		  ) ado-mata-keyword-face)
		  "\\b"
		  ))

	  (eval-when-compile
		 (make-regexps
		  "[ \t]+"
		  '((
		  "aggregate" "array"
		  "boolean" "byte" 
		  "case" "catch" "class" "const"
		  "default" "delegate" "delete" "double" 
		  "else" "eltypedef" "end" "enum" "explicit" "export"
		  "float" "friend" 
		  "global"
		  "inline" "int" 
		  "local" "long" 
		  "namespace" "new" "NULL" 
		  "operator" "orgtypedef"
		  "polymorphic" "private" "protected" "public"
		  "quad"
		  "short" "signed" "static" "struct" "super" "switch"
		  "template" "this" "throw" "try" "typedef" "typename"
		  "union" "unsigned" "using" 
		  "virtual" "volatile"
		  ) ado-mata-future-keyword-face)
		  "\\b"
		  ))

	  (eval-when-compile
		 (make-regexps
		  "[ \t]+"
		  '(("pragma") ado-mata-keyword-face)
		  "[ \t]+"
		  '(("unset" "unused") ado-subcommand-face)
		  ))
    
	  (eval-when-compile
		 (make-regexps
		  "[ \t]+"
		  '(("pointer" "return") ado-mata-keyword-face)
		  "("
		  ))

	  ;; mata subcommands
	  ;;  does this run into the need for extra harmful and harmless faces?!?!
	  (eval-when-compile
		 (make-regexps
		  "[ \t]+"
		  '(("mata") ado-mata-keyword-face t)
		  "[ \t]+"
		  '((
		  "clear"
		  "d" "de" "des" "desc" "descr" "descri" "describ" "describe" 
		  "drop"
		  "end"
		  "help"
		  "matd" "matde" "matdes" "matdesc" "matdescr" "matdescri" "matdescrib" "matdescribe" 
		  "matsave" "matuse" "memory" "mlib" "mosave"
		  "query"
		  "rename"
		  "set" "stata"
		  "which"
		  ) ado-mata-keyword-face t)
		  "\\b"
		  ))
	  ;; mata subcommands
	  ;;  does this run into the need for extra harmful and harmless faces?!?!
	  (eval-when-compile
		 (make-regexps
		  "[ \t]+"
		  '(("mata") ado-mata-keyword-face t)
		  "[ \t]+"
		  '(("mlib") ado-mata-keyword-face t)
		  "[ \t]+"
		  '((
		  "add"
		  "create"
		  "index"
		  "q" "qu" "que" "quer" "query"
		  ) ado-subcommand-face t)
		  "\\b"
		  ))

	  ;; general mata set commands
	  (eval-when-compile
		 (make-regexps
		  "[ \t]+"
		  '(("mata") ado-mata-keyword-face t)
		  "[ \t]+"
		  '(("set") ado-mata-keyword-face t)
		  "[ \t]+"
		  '((
		  "matacache" "matalibs"
		  ) ado-subcommand-face t)
		  "\\b"
		  ))

	  ;; general mata set on/off commands
	  (eval-when-compile
		 (make-regexps
		  "[ \t]+"
		  '(("mata") ado-mata-keyword-face t)
		  "[ \t]+"
		  '(("set") ado-mata-keyword-face t)
		  "[ \t]+"
		  '((
		  "matamofirst" "matalnum" "mataoptimize" "matastrict"
		  ) ado-subcommand-face t)
		  "[ \t]+"
		  '(("off" "on") ado-subcommand-face t)
		  ))

	  ;; specific mata set commands
	  (eval-when-compile
		 (make-regexps
		  "[ \t]+"
		  '(("mata") ado-mata-keyword-face t)
		  "[ \t]+"
		  '(("set") ado-mata-keyword-face t)
		  "[ \t]+"
		  '((
		  "matafavor"
		  ) ado-subcommand-face t)
		  "[ \t]+"
		  '(("space" "speed") ado-subcommand-face t)
		  ))

	  ;; mata functions (durn, this is a pain in the butt)
	  ;; functions which exist for regular stata are NOT included
	  ;; these, too ended up being split
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "_chdir" "_cholesky" "_cholinv" "_cholsolve" "_collate" "_conj" "_corr"
		  "_diag"
		  "_editmissing" "_edittoint" "_edittointtol" "_edittozero" "_edittozerotol" "_editvalue"
		  "_eigensystem" "_eigenvalues"
		  "_equilc" "_equilr" "_equilrc" "_error" 
		  "_fft" "_fillmissing" 
		  "_fclose" "_fget" "_fgetmatrix" "_fgetnl" "_fopen" "_fput" "_fputmatrix" "_fread" 
		  "_fseek" "_ftell" "_ftruncate" "_fullsvd" "_fwrite"
		  "_halton" "_hqrd" "_hqrdp" "_hqrdp_la"
		  "_invfft" "_invsym"
		  "_jumble"
		  "_lefteigensystem" "_lowertriangle" "_lud" "_lud_la" "_luinv" "_luinv_la" "_lusolve" "_lusolve_la"
		  "_makesymmetric" "_matexpsym" "_matlogsym" "_matpowersym" "_mkdir" 
		  "_optimize" "_optimize_evaluate"
		  "_perhapsequilc" "_perhapsequilr" "_perhapsequilrc" "_pinv"
		  "_qrinv" "_qrsolve" "_quadrunningsum"
		  "_rmdir" "_runningsum"
		  "_solvelower" "_solveupper" "_sort" 
		  "_st_addobs" "_st_addvar" "_st_data" "_st_macroexpand" "_st_sdata" "_st_sstore" "_st_store"
		  "_st_varindex"
		  "_stata" "_strtoreal" "_substr" "_svd" "_svd_la" "_svdsv" "_svsolve" "_symeigensystem" "_symeigenvalues"
		  "_transpose" "_transposeonly"
		  "_unlink" "_uppertriangle" 
		  ) ado-mata-function-name-face t)
		  "("
		  ))

	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "C" "Corr" "Hilbert" "Im" "Re" "Toeplitz" "Vandermonde"
		  "acosh" "acosr" "adosubdir" "all" "allof" "any" "anyof" 
		  "arg" "args" "asinh" "asinr" "ascii" "assert" "asserteq" "atanh" "atanr"
		  "blockdiag" "breakkey" "breakkeyreset" 
		  "bufbfmtisnum" "bufbfmtlen" "bufbyteorder" "bufget" "bufio" "bufmissingvalue" "bufput"
		  "callersversion" "cat" "chdir" "cholsolve" "cholinv" 
		  "colmax" "colmaxabs" "colmin" "colminmax" "colmissing" "colnonmissing" "cols" "colscalefactors" "colshape" "colsum" 
		  "conj" "convolve" "correlation" "cosh" "crexternal" "cross" "crossdev" "cvpermute" "cvpermutesetup"
		  "deconvolve" "designmatrix" "dettriangular"
		  "diag" "diagonal" "dir" "direxists" "direxternal" "display" "displayas" "displayflush" "dsign"
		  "editmissing" "edittoint" "edittointtol" "edittozero" "edittozerotol" "editvalue" 
		  "eigensystem" "eigenvalues" 
		  "eltype" "epsilon" "error" "errprintf" "exit"
		  "factorial" "favorspeed" "fbufget" "fbufput" "fclose" 
		  "ferrortext" "fft" "fget" "fgetnl" "fgetmatrix" 
		  "fileexists" "findexternal" "findfile" "floatround" "fopen" "fput" "fputmatrix" 
		  "fread" "freturncode" "frombase" "fseek" "fstatus" 
		  "ftell" "ftfreqs" "ftpad" "ftperiodogram" "ftretime" "ftruncate" "ftunwrap" "ftwrap" 
		  "fullsdiag" "fullsvd" "fwrite"
		  "gamma" "ghalton" "ghk" "ghkfast" "ghkfastsetup"
		  "halton" "hqrd" "hqrdp" "hqrdmultq" "hqrdmultqlt" "hqrdq" "hqrdq1" "hqrdr" "hqrdr1"
		  "inbase" "invHilbert" "invfft" "invorder" "invtokens" "invvech"
		  "iscomplex" "isdiagonal" "isfleeting" "ispointer" "isreal" 
		  "isrealvalues" "isstring" "issymmetric" "issymmetriconly" "isview" 
		  "jumble"
		  ) ado-mata-function-name-face t)
		  "("
		  ))

	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "lefteigensystem" "liststruct" "lnnormal" "lnnormalden" "lowertriangle" "lud" "luinv" "lusolve"
		  "makesymmetric" "matexpsym" "matlogsym" "matpowersym" "maxindex"
		  "mean" "meanvariance" "minindex" "minmax" "missingof" "more"
		  "mreldifre" "mreldifsym"
		  "nameexternal" "nonmissing" "norm"
		  "optimize" "optimize_evaluate" "optimize_init" "optimize_query"
		  "order" "orgtype"
		  "panelsetup" "panelstats" "panelsubmatrix" "panelsubview"
		  "pathasciisuffix" "pathbasename" "pathisabs" "pathisurl" "pathjoin" "pathlist" 
		  "pathrmsuffix" "pathsearchlist" "pathsplit" "pathstatasuffix" "pathsubsysdir" "pathsuffix"
		  "pi" "pinv"
		  "polyadd" "polyderiv" "polydiv" "polyeval" "polyinteg" "polymult" "polyroots" "polysolve" "polytrim"
		  "printf" "pwd"
		  "range" "rangen" "rmexternal" "rowmax" "rowmissing" "rowscalefactors"
		  "qrd" "qrdp" "qrinv" "qrsolve" 
		  "quadcorrelation" "quadcross" "quadcrossdev" "quadrant" "quadcolsum" "quadrowsum" "quadrunningsum" "quadsum" "quadvariance" 
		  "querybreakintr"
		  "rank" "revorder" "rowmaxabs" "rowmin" "rowminmax" "rownonmissing" "rows" "rowshape" "rowsum" "runningsum"
		  "select"
		  "setbreakintr" "setmore" "setmoreonexit" "sinh" "sizeof" "smallestdouble"
		  "solve_tol" "solvelower" "solveupper"
		  "sort" "spline3" "spline3val" "sprintf"
		  "st_select" "stata" "statasetversion" "stataversion" 
		  "stritrim" "strltrim" "strreverse" "strrtrim" "strtoreal" "strtrim" "strlower" "strproper" "strupper"
		  "svd" "svdsv" "svsolve" "swap"
		  "symeigensystem" "symeigenvalues"
		  "timer" 
		  "tokenallowhex" "tokenallownum" "tokenget" "tokengetall" "tokeninit" "tokeninitstata" "tokenoffset" 
		  "tokenpeek" "tokenrest" "tokens" "tokenset" "tokenpchars" "tokenqchars" "tokenwchars" "transposeonly"
		  "uniqrows" "unitcircle" "uniformseed"
		  "valofexternal" "variance" "vec" "vech"
		  "unlink" "uppertriangle"
		  ) ado-mata-function-name-face t)
		  "("
		  ))
	  ;; arrrgh the mata st_ commands
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("st_") ado-mata-function-name-face t)
		  '((
		  "addobs" "addvar" 
		  "data" "dir" "dropobsif" "dropobsin" "dropvar"
		  "eclear"
		  "global"
		  "isfmt" "islmname" "isname" "isnumfmt" "isnumvar" "isstrfmt" "isstrvar"
		  "keepobsif" "keepobsin" "keepvar"
		  "local"
		  "macroexpand" "matrix" "matrixcolstripe" "matrixrowstripe"
		  "nobs" "numscalar" "nvar"
		  "rclear" "replacematrix"
		  "sclear" "sdata" "sstore" "store" "strscalar" "subview" "sview"
		  "tempfilename" "tempname" "tsrevar" 
		  "updata"
		  "varformat" "varindex" "varlabel" "varname" "varrename" "vartype" "varvaluelabel"
		  "view" "viewobs" "viewvars"
		  "vldrop" "vlexists" "vlload" "vlmap" "vlmodify" "vlsearch"
		  ) ado-mata-function-name-face t)
		  "("
		  ))
	  ;; mata optimize_init commands
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("optimize_init_") ado-mata-function-name-face t)
		  '((
			"argument"
			"constraints" "conv_maxiter" "conv_ptol"
			"evaluator"
			"narguments" "nmsimplexdeltas" "nrtol"
			"params"
			"singularHmethod"
			"technique" "tracelevel" "type"
			"valueid" "verbose" "vtol"
			"which"
		  ) ado-mata-function-name-face t)
		  "("
		  ))
	  ;; mata optimize_result commands
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("optimize_result_") ado-mata-function-name-face t)
		  '((
			  "Hessian"
			  "V" "V_oig" "V_oim" "V_robust" "Vtype"
			  "converged"
			  "errorcode"
			  "gradient"
			  "iterationlog" "iterations"
			  "params"
			  "returncode"
			  "scores"
			  "value" "value0"
			  ) ado-mata-function-name-face t)
		  "("
		  ))
	  

	  ;; all variable/macro stuff (put late so it will override)
	  ;; keep just before the obsolete commands!
	  ;; internal constants
	  (eval-when-compile
		 (make-regexps
		  "[^a-zA-Z]"
		  '(("_merge" "_n" "_pi" "_rc" "_N"
		  ) ado-constant-face t)
		  "[^a-zA-Z]"
		  ))
	  ;; some generated vars
	  ;; ... which are now out of date
	  (eval-when-compile
		 (make-regexps
		  '(("_result([1-9]+[0-9]*)"
		  ) ado-obsolete-face t)
		  ))
	  ;; global macros
	  (eval-when-compile
		 (make-regexps
		  '(("\\$[a-zA-Z_*]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
		  ))
	  ;; local macros
	  ;;   highlights *before* the macro is ended, which 
	  ;;   could lead to typos, but gets rid of recursive
	  ;;   definitions.
	  (eval-when-compile
		 (make-regexps
		  "`+\\(\\+\\+\\|--\\)?"
		  '(("[a-zA-Z_0-9]+"	
		  ) ado-variable-name-face t)
		  ))
	  (eval-when-compile
		 (make-regexps
		  "`+"
		  '(("\\(e\\|r\\|s\\)") ado-function-name-face t)
		  '(("(") ado-constant-face t)
		  '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
		  '((")") ado-constant-face t)
		  "'+"
		  ))

	  ;; obsolete mfp arguments
	  ;;  none --- looks to have been a documentation error

	  ;; what few obsolete commands I've gathered
	  ;; lfit and score moved before the matrix command so that it won't affect
	  ;; the matrix score command
	  (eval-when-compile
		 (make-regexps
		  "[ \t]+"
		  '((
			 "_huber"
			 "archlm"
			 "bgodfrey"
			 "durbina" "dwstat"
			 "greigen" 
			 "iis" "ivreg"
			 "hettest"
			 "imtest"
			 "lo" "loo" "look" "looku" "lookup"
			 "nlinit"
			 "ovtest"
			 "lstat"
			 "poisgof"
			 "stphtest" "svylc" "svytest" "szroeter"
			 "tis"
			 "varfcast" "varirf" "vce" "vif"
			 "xtcorr" "xtrchh" "xthaus" "xtpois"
			 "shelltool"
			 ) ado-obsolete-face t)
		  "\\b"
		  ))

	  ;; the datasignature commands
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
			  "datasig" "datasign" "datasigna" "datasignat" "datasignatu" "datasignatur" "datasignature") ado-builtin-harmful-face t)
		  "[ \t]+"
		  '((
			  "clear"
			  "set"
			  ) ado-subcommand-face t)
		  ))
			  
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
			  "datasig" "datasign" "datasigna" "datasignat" "datasignatu" "datasignatur" "datasignature") ado-builtin-harmless-face t)
		  "[ \t]+"
		  '((
			  "conf" "confi" "confir" "confirm" 
			  "rep" "repo" "repor" "report" 
			  ) ado-subcommand-face t)
		  "\\b"
		  ))

	  ;; the estat commands
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("estat") ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
			  "abond" "alt" "alte" "alter" "altern" "alterna" "alternat" "alternati" "alternativ" "alternative" "alternatives"
			  "anti" "archlm" 
			  "bgodfrey" "bootstrap"
			  "canontest" "clas" "class" "classfunctions" 
			  "classi" "classif" "classifi" "classific" 
			  "classifica" "classificat" "classificati" 
			  "classificatio" "classification" 
			  "classtable" 
			  "common" "compare" "con" "config" "coordinates" 
			  "cor" "corr" "corre" "correl" "correla" "correlat" "correlati" "correlatio" "correlation"
			  "correlation" "correlations" 
			  "cov" "cova" "covar" "covari" "covaria" "covarian" "covarianc" "covariance" 
			  "distances" "durbinalt" "dwatson"
			  "eff" "effe" "effec" "effect" "effects" "errorrate"
			  "factors"
			  "first" "firsts" "firstst" "firststa" "firststag" "firststage" 
			  "gof" "grdistances" "group" "grmeans" "grsummarize"
			  "hettest"
			  "ic" "imtest" "inertia"
			  "kmo"
			  "lceff" "lceffe" "lceffec" "lceffect" "lceffects" 
			  "list" "loadings"
			  "mfx" "mvreg"
			  "over" "overi" "overid" 
			  "ovtest"
			  "pairwise" "phtest" "predict" "profiles"
			  "quantiles"
			  "recovariance" "residuals" "rotate" "rotatecompare"
			  "sargan" "sd" "se" "size" "smc" "strata" "stress" "structure"
			  "su" "subinertia" "sum" "summ" "summa" "summar" 
			  "summari" "summariz" "summarize" 
			  "svyset"
			  "szroeter"
			  "table"
			  "vce" "vif"
			  "wcorrelation"
			  ) ado-subcommand-face t)
		  "\\b"
		  ))

	  ;; the estimates commands
	  ;; moved to just after the estat
	  (eval-when-compile
		 (make-regexps
       "\\b"
       '((
			 "est" "esti" "estim" "estima" "estimat" "estimate" "estimates"
			 ) ado-builtin-harmless-face t)
       "[ \t]+"
       '((
			 "ch" "cha" "chan" "chang" "change" 
			 "clear"
			 "des" "desc" "descr" "descri" "describ" "describe"
			 "dir" 
			 "drop"
			 "esample"
			 "f" "fo" "for"
			 "note" "notes" 
			 "q" "qu" "que" "quer" "query" 
			 "r" "re" 
			 "rep" "repl" "repla" "replay" 
			 "res" "rest" "resto" "restor" "restore" 
			 "save"
			 "stat" "stats" 
			 "sto" "stor" "store" 
			 "tab" "tabl" "table"
			 "title"
			 "use"
			 ) 
			ado-subcommand-face t)
       "\\b"
       ))
    ;; the extra estimates notes commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
			 "est" "esti" "estim" "estima" "estimat" "estimate" "estimates"
	  ) ado-builtin-harmless-face t)
       "[ \t]+"
       '((
			 "note" "notes"
			 )
			ado-subcommand-face t)
		 "[ \t]+*"
		 '((
			 "drop"
			 "l" "li" "lis" "list" 
			 )
			ado-subcommand-face t)
		 "\\b"
		 ))

	  ;; things which are partially obsolete
	  (eval-when-compile
		 (make-regexps
		  "^[ \t]*"
		  '((
		  "jknife"
		  "parse"
		  "whelp"
		  ) ado-obsolete-face t)
		  "\\b"
		  ))
    
	  ;; apparently obsolete window commands
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '((
		  "win" "wind" "windo" "window"
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "m" "me" "men" "menu"
		  ) ado-builtin-harmless-face)
		  "[ \t]+"
		  '((
		  "append[ \t]+popout"
		  "append[ \t]+string"
		  "popout"
		  "set"
		  ) 
			 ado-obsolete-face)
		  "\\b"
		  ))

	  ;; semi-obsolete macro commands
	  (eval-when-compile
		 (make-regexps
		  "[ \t]*"
		  '((
		  "ma" "mac" "macr" "macro"
		  "sca" "scal" "scala" "scalar" 
		  ) ado-builtin-harmful-face)
		  "[ \t]+"
		  '((
		  "de" "def" "define" 
		  )
			 ado-subcommand-face t)
		  "\\b"
		  ))
	  ;; multiword extended macro names using 'set', which are obsolete
     (eval-when-compile
       (make-regexps
        "[ \t]*"
        '((
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		  ) ado-builtin-harmless-face t)
		  "[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
		  ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
        '(("set") ado-obsolete-face t)
		  "[ \t]+"
		  '((
		  "adosize" "graphics" "level" "linesize" "logtype" "matsize" "more" "pagesize"
		  "rmsg" "trace" "type" "virtual" 
		  ) ado-obsolete-face t)
		  "\\b"
        ))



	  )))

;;; 1. need some way to highlight ^[ /t]*- lines with one background?
;;; 2. need some way to highlight ^[ /t]*= lines with another background?
;;; 3. output in plain black text
;;; skipped steps in gray text
;;; could have three modes and change the faces locally?
;;; think about method for foreground by one, background by another

;; (defun ado-set-debug-highlight ()
;;   (interactive)
;;   (setq
;;    ado-font-lock-keywords
;;    ()))

;;; now for fancy stuff: syntactic keywords
(defun ado-set-font-lock-syntactic-keywords ()
  (interactive)
  (setq font-lock-syntactic-keywords
		  (list
			'("\\(#\\)[dr]" 1 "w")
			)))



;;; Working with help files
;;; allowing for the user's name to be put into help files
(defun set-ado-signature-file ()
  (interactive)
  (setq ado-signature-file
		  (read-file-name "Set ado signature file to: "
								(file-name-directory (expand-file-name
															 (if (not ado-signature-file)
																  (if (file-exists-p "~/.ado-signature")
																		"~/.ado-signature")
																ado-signature-file)))))
  )
(defun ado-new-help-6 ()
  "Gets a boilerplate for writing help files for Stata 6 and earlier, then inserts the users name and allows editing. This is not complete, since it has trouble putting the proper signature at the bottom of the file. To make this work properly, have a variable which contains the name of the signature file or a .signature file."
  (interactive)
  (let (name)
    (setq name (read-from-minibuffer "For what program should help be written? "))
    (switch-to-buffer
     (generate-new-buffer
      (generate-new-buffer-name (concat name ".hlp"))))
    (ado-insert-boilerplate "help6.blp" t)
    (if (and ado-new-dir (y-or-n-p "Put in 'new' directory? "))
		  (cd (directory-file-name ado-new-dir)))
    (if (not ado-claim-name)
		  (setq ado-claim-name 
				  (read-from-minibuffer "Whose name should be put at the top of the file? "
												user-full-name)))
    (text-mode)
    (goto-char (point-max))
    (if ado-signature-prompt-flag
		  (progn
			 (if (not ado-signature-file)
				  (set-ado-signature-file))
			 (insert-file-contents ado-signature-file))
      (insert ado-claim-name))
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (insert (concat "^" name "^"))
    (insert (concat
				 (make-string
				  (- 81 (+ (length ado-claim-name) (current-column)))
				  (string-to-char " "))
				 ado-claim-name))
    (search-forward "Put a")
    (beginning-of-line)
    (set-fill-column 79)
    (auto-fill-mode 0)
    (recenter)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stuff for writing help files ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ado-new-help (&optional name)
  "Gets a template for writing help files, inserts the users name and allows editing. To make this work properly, have a variable which contains the name of the signature file or a .signature file."
  (interactive)
  (unless name
    (setq name (read-from-minibuffer "For what program should help be written? ")))
  (switch-to-buffer
   (generate-new-buffer
	(generate-new-buffer-name (concat name ado-help-extension))))
  (ado-insert-boilerplate "help.blp" t)
  (if (and ado-new-dir (y-or-n-p "Put in 'new' directory? "))
	  (cd (directory-file-name ado-new-dir)))
  (if (not ado-claim-name)
	  (setq ado-claim-name 
			(read-from-minibuffer "Whose name should be put at the top of the file? " user-full-name)))
  (text-mode)
  (goto-char (point-max))
  (if ado-signature-prompt-flag
	  (progn
		(if (not ado-signature-file)
			(set-ado-signature-file))
		(insert "
{title:Author}

")
		(insert-file-contents ado-signature-file))
	(insert ado-claim-name))
  (goto-char (point-min))
  (while (search-forward "XXX" nil t)
	(replace-match name t))
  (goto-char (point-min))
  (search-forward "{* Last Updated: ")
  (insert (ado-nice-current-date))
;  (search-forward "hi:help ")
;  (insert name)
  (ado-mode)
  ;; turn off indenting
  (setq ado-smart-indent-flag nil)
  (ado-save-program)
  )

(defun ado-toggle-help-extension ()
  "Toggles the extension between sthlp and hlp for those writing
help files meant to be used in Stata 7 through Stata 9."
  (interactive)
  (if (string= ado-help-extension "sthlp")
	  (setq ado-help-extension "hlp")
	(setq ado-help-extension "sthlp"))
  (message (concat "ado-help-extension is now " ado-help-extension)))

;;; useful insertions in smcl
(defun ado-help-insert-option-in-body (&optional option-name)
  (interactive)
  (if (not option-name)
      (setq option-name
				(read-from-minibuffer "What is the full name of the option? " option-name)))
  (ado-insert-with-lfd (concat "{p 0 4}{cmd:" option-name "}"))
  (ado-insert-with-lfd "{p_end}")
  (newline)
  (forward-line -2)
  (end-of-line)
  (forward-char -1)
  )

(defun ado-new-cscript (&optional desc name)
  "Gets a boilerplate for writing certification scripts, asks for the ado file being certified, and then allows editing."
  (interactive)
  (unless name
    (setq name (read-from-minibuffer "For what ado-file(s) should a cert script be written? ")))
  (unless desc
	(setq desc (read-from-minibuffer "What description would you like? (blank to match ado-file(s)) ")))
  (if (string= desc "")
	(setq desc name))
  (switch-to-buffer
   (generate-new-buffer
	(generate-new-buffer-name (concat name ".do"))))
  (insert "cscript \"" desc "\" adofile " name)
  (ado-mode)
  )


;;; Some utilities which should really be in a separate file (but which
;;;  would then cause extra installation instructions).

(defun ado-reset-value (value prompt &optional flag)
  (let (new-value value)
    (setq new-value (read-from-minibuffer (format (concat "Change " prompt " from %d to ") value)))
    (if (null new-value)
		  value
      (if (= (setq new-value (string-to-number new-value )) value)
			 (progn 
				(message (concat prompt " left unchanged."))
				value)
		  new-value
		  )
      )))

(defun ado-insert-with-lfd (junk)
  "Used to insert and indent without needed to hit the indentation key
  (usually a tab). One day, when the ado-mode is complete, the other
  functions will no longer depend on this function."
  (insert junk)
  (newline-and-indent))

(defun ado-nice-current-date ()
  "Returns the current date and time as specified by by
  `ado-date-format'. See `format-time-string' for help on setting."
  (concat 
   (if ado-lowercase-date-flag
       (downcase (format-time-string ado-date-format))
     (format-time-string ado-date-format))
   (if ado-initials-flag (concat " " ado-initials))))

(defun ado-insert-nice-current-date ()
  (interactive)
  "Inserts a the nice current date (see \\[ado-nice-current-date] for
details"
  (insert (ado-nice-current-date)))

;; (defvar calendar-month-name-array
;;   ["January" "February" "March"     "April"   "May"      "June"
;;    "July"    "August"   "September" "October" "November" "December"])
;; scunged from the calendar.el file and then changed
;; (defun ado-nice-current-date ()
;;   "Returns the current date in a nice format: month date, year @
;;   time. Called within ado-mode to timestamp files. Since 'nice' is
;;   subjective, this can be changed at whim."
;;   (interactive)
;;   (let 
;;       ((s (current-time-string))) 
;;     (concat 
;;      (aref calendar-month-name-array 
;; 	   (1-(length (member (substring s 4 7) 
;; 			      '("Dec" "Nov" "Oct" "Sep"
;; 				"Aug" "Jul" "Jun" "May"
;; 				"Apr" "Mar" "Feb" "Jan"))))) " " 
;; 				(let ((dd
;; 				       (string-to-number (substring s 8 10)))) 
;; 				  (if (< dd 10) (substring s 9 10) 
;; 				    (substring s 8 10))) ", " 
;; 				    (substring s 20 24) " @ " 
;; 				    (substring s 11 19))) 
;;   )

(defun ado-set-imenu-items ()
  (interactive)
  (setq imenu-case-fold-search nil)
  (setq imenu-generic-expression
		(list 
		   (list nil "^\\s-*pr\\(o\\|og\\|ogr\\|ogra\\|ogram\\)\\(\\s-+\\(de\\|def\\|defi\\|defin\\|define\\)?\\)\\s-+\\([a-zA-Z_][a-zA-Z_0-9]*\\)" 4))))

;;; Aquamacs emacs specifics (perhaps should be broken out?)
(if (boundp 'aquamacsxb-version)
    (define-key ado-mode-map [remap mac-key-save-file] 'ado-save-program))

(provide 'ado-mode)
;; ado-mode.el ends here
