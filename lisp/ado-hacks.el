;;; ado-hacks.el --- holding spot for hacks to bridge different versions
;;;  of emacs
;; Copyright (c) 2006
;; Bill Rising

;; Author:   Bill Rising
;; Maintainer: Same <brising@mac.com>
;;             URL: http://homepage.mac.com/brising
;; Keywords: ado-mode
;; Version:  0.10 of March 24, 2006

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
;; This file contains chunks of code which are needed to get 
;;  ado mode running on versions of emacs older than version 22.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (fboundp 'line-number-at-pos)
  (defun line-number-at-pos (&optional pos)
    "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location."
    (let ((opoint (or pos (point))) start)
      (save-excursion
	(goto-char (point-min))
	(setq start (point))
	(goto-char opoint)
	(forward-line 0)
	(1+ (count-lines start (point)))))))

;;; stolen from replace.el of 22.1.1
(defun how-many (regexp &optional rstart rend interactive)
  "Print and return number of matches for REGEXP following point.
When called from Lisp and INTERACTIVE is omitted or nil, just return
the number, do not print it; if INTERACTIVE is t, the function behaves
in all respects has if it had been called interactively.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive.

Second and third arg RSTART and REND specify the region to operate on.

Interactively, in Transient Mark mode when the mark is active, operate
on the contents of the region.  Otherwise, operate from point to the
end of (the accessible portion of) the buffer.

This function starts looking for the next match from the end of
the previous match.  Hence, it ignores matches that overlap
a previously found match."

  (interactive
   (keep-lines-read-args "How many matches for (regexp): "))
  (save-excursion
    (if rstart
	(progn
	  (goto-char (min rstart rend))
	  (setq rend (max rstart rend)))
      (if (and interactive transient-mark-mode mark-active)
	  (setq rstart (region-beginning)
		rend (region-end))
	(setq rstart (point)
	      rend (point-max)))
      (goto-char rstart))
    (let ((count 0)
	  opoint
	  (case-fold-search (and case-fold-search
				 (isearch-no-upper-case-p regexp t))))
      (while (and (< (point) rend)
		  (progn (setq opoint (point))
			 (re-search-forward regexp rend t)))
	(if (= opoint (point))
	    (forward-char 1)
	  (setq count (1+ count))))
      (when interactive (message "%d occurrence%s"
				 count
				 (if (= count 1) "" "s")))
      count)))


(provide 'ado-hacks)

;;; ado-hacks.el ends here
