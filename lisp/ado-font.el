;;; ado-font.el --- simple definition of font faces
;; Copyright (c) 2003,...,2007
;; Bill Rising
;;   much of this sponged from the ultex-cus.el which customizes
;;   the UltraTex mode (highly recommended) by
;; Mark Haiman, Nick Reingold, John Palmieri

;; Author:   Bill Rising
;; Maintainer: Same <brising@mac.com>
;;             URL: http://homepage.mac.com/brising
;; Keywords: ado-mode
;; Version:  0.10 of November 13, 2003

;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description:
;;
;; This file contains the base definitions of font-faces for the ado
;; mode.  If you are using Emacs 20, or an earlier version of Emacs
;; which has the customization package installed, you can change all
;; of the relevant variables here via customization.  This is
;; preferable to doing it "by hand" in your .emacs file.
;; .... unless you really really like tinkering with .emacs files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization groups
;;

(defgroup ado-font-lock nil
  "Ado mode font lock (highlighting) faces."
  :tag "Ado Fonts"
  :group 'ado)

;; default correspondence for font-lock faces:
;;  font-lock-function-face is unused as of now...

;; every one of the faces for the commands has two types: harmless and harmful
;;  the harmless faces do not change data or modify the environment
;;  the harmful faces do.
;; need to modify faces by having faces for
;;  override -- overline
;;  extras   -- underline
;;  personal -- semibold
;;  site     -- italics
;;  builtin  -- regular
;;  these are included even though there isn't currently a mechanism for 
;;    telling font-lock to use them

(defface ado-builtin-harmful-face
  '((t :inherit font-lock-keyword-face))
  "Ado mode face used to highlight builtin commands which change
data or the environment."
  :group 'ado-font-lock)

(defvar ado-builtin-harmful-face 'ado-builtin-harmful-face)

(defface ado-builtin-harmless-face
  '((t :inherit font-lock-builtin-face))
  "Ado mode face used to highlight harmless builtin ado commands"
  :group 'ado-font-lock)

(defvar ado-builtin-harmless-face 'ado-builtin-harmless-face)

(defface ado-override-harmful-face
  '((t :inherit ado-builtin-harmful-face :overline t))
  "Ado mode face used to highlight homespun ado commands which override
Stata's built-in commands and change data or the environment."
  :group 'ado-font-lock)

(defvar ado-override-harmful-face 'ado-override-harmful-face)

(defface ado-override-harmless-face
  '((t :inherit ado-builtin-harmless-face :overline t))
  "Ado mode face used to highlight harmless homespun ado commands which override
Stata's built-in commands"
  :group 'ado-font-lock)

(defvar ado-override-harmless-face'ado-override-harmless-face)

(defface ado-extras-harmful-face
  '((t :inherit ado-builtin-harmful-face :underline t))
  "Ado mode face used to highlight extra ado commands (such as those downloaded
off the net) which change data or the environment."
  :group 'ado-font-lock)

(defvar ado-extras-harmful-face 'ado-extras-harmful-face)

(defface ado-extras-harmless-face
  '((t :inherit ado-builtin-harmless-face :underline t))
  "Ado mode face used to highlight harmless extra ado commands (such 
as those downloaded off the net)."
  :group 'ado-font-lock)

(defvar ado-extras-harmless-face 'ado-extras-harmless-face)

(defface ado-personal-harmful-face
  '((t :inherit ado-builtin-harmful-face :bold t))
  "Ado mode face used to highlight personal ado commands which change
data or the environment."
  :group 'ado-font-lock)

(defvar ado-personal-harmful-face 'ado-personal-harmful-face)

(defface ado-personal-harmless-face
  '((t :inherit ado-builtin-harmless-face :bold t))
  "Ado mode face used to highlight harmless personal ado commands"
  :group 'ado-font-lock)

(defvar ado-personal-harmless-face 'ado-personal-harmless-face)

(defface ado-site-harmful-face
  '((t :inherit ado-builtin-harmful-face :slant italic))
  "Ado mode face used to highlight site-wide ado commands which change
data or the environment."
  :group 'ado-font-lock)
(defvar ado-site-harmful-face 'ado-site-harmful-face)

(defface ado-site-harmless-face
  '((t :inherit ado-builtin-harmless-face :slant italic))
  "Ado mode face used to highlight harmless site-wide ado commands"
  :group 'ado-font-lock)
(defvar ado-site-harmless-face 'ado-site-harmless-face)

(defface ado-constant-face
  '((t :inherit font-lock-constant-face))
  "Font Lock mode face used to highlight builtin constants."
  :group 'ado-font-lock)
(defvar ado-constant-face 'ado-constant-face)

(defface ado-platform-specific-face
  '((t :inherit font-lock-constant-face :bold t))
  "Ado mode face used to highlight builtin contants which exist
only on particular platforms"
  :group 'ado-font-lock)
(defvar ado-platform-specific-face 'ado-platform-specific-face)

(defface ado-string-face
  '((t :inherit font-lock-string-face))
  "Ado mode face used to highlight strings."
  :group 'ado-font-lock)

(defvar ado-string-face 'ado-string-face)

(defface ado-comment-face
  '((t :inherit font-lock-comment-face))
  "Ado mode face used to highlight comments."
  :group 'ado-font-lock)

(defvar ado-comment-face 'ado-comment-face)

(defface ado-variable-name-face
  '((t :inherit font-lock-variable-name-face))
  "Ado mode face used to highlight macro, scalar and temporary names."
  :group 'ado-font-lock)

(defvar ado-variable-name-face 'ado-variable-name-face)

(defface ado-matrix-name-face
  '((t :inherit ado-variable-name-face :underline t))
  "Ado mode face used to highlight vectors and matrices"
  :group 'ado-font-lock)

(defvar ado-matrix-name-face 'ado-matrix-name-face)

(defface ado-function-name-face
  '((t :inherit font-lock-function-name-face))
  "Ado mode face used to highlight functions in ado programs, and 
iactions in dialogs"
  :group 'ado-font-lock)

(defvar ado-function-name-face 'ado-function-name-face)

(defface ado-subcommand-face
  '((t :inherit font-lock-type-face))
  "Ado mode face used to highlight subcommands or suboptions,
and iactions which work as subcommands for dialogs."
  :group 'ado-font-lock)

(defvar ado-subcommand-face 'ado-subcommand-face)

(defface ado-obsolete-face
  '((t :inherit font-lock-warning-face))
  "Ado mode face used to highlight obsolete commands."
  :group 'ado-font-lock)

(defvar ado-obsolete-face 'ado-obsolete-face) 

(defface ado-mata-keyword-face
  '((t :inherit font-lock-preprocessor-face))
  "Ado mode face used to highlight mata keywords."
  :group 'ado-font-lock)

(defvar ado-mata-keyword-face 'ado-mata-keyword-face)

(defface ado-mata-future-keyword-face
  '((t :inherit ado-mata-keyword-face :background "linen"))
  "Ado mode face used to highlight mata keywords."
  :group 'ado-font-lock)

(defvar ado-mata-future-keyword-face 'ado-mata-future-keyword-face)

;; have another mata function name face? Start by inheriting the regular function
;;  face and then fixing it later.
(defface ado-mata-function-name-face
  '((t :inherit ado-function-name-face))
  "Ado mode face used to highlight mata functions, of all things."
  :group 'ado-font-lock)

(defvar ado-mata-function-name-face 'ado-mata-function-name-face)

(provide 'ado-font)

;;; ado-font.el ends here
