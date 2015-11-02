;;; Change log:

;; Sat Feb 11 17:51:15 CET 2012       Martin Stjernholm <mast@lysator.liu.se>
;;	*  Removed long-since obsolete function make-local-hook which
;;	   no longer exists in Emacs 24.  Thanks to Phil Sainty for
;;	   pointing this out.  Also dropped some more old compat code -
;;	   the baseline is now Emacs 21.
;;
;; Wed Nov 12 18:55:48 CET 2003       Martin Stjernholm <mast@lysator.liu.se>
;;	*  Added an indent-tabs-mode sensitive function ws-trim-leading.
;;
;; Thu Jun 19 01:26:51 CEST 2003      Martin Stjernholm <mast@lysator.liu.se>
;;	*  Made ws-trim-method-hook buffer local.
;;
;; Fri Oct  5 01:04:17 CEST 2001      Martin Stjernholm <mast@lysator.liu.se>
;;	*  Check if the trimming would have any effect before doing it,
;;	   to avoid getting buffers modified unnecessarily.
;;
;; Sat May 15 19:15:24 CEST 1999      Martin Stjernholm <mast@lysator.liu.se>
;;	*  Prompt for the trim method when a prefix arg is passed to
;;	   ws-trim-line, ws-trim-region and ws-trim-buffer.
;;
;; Tue Apr  8 00:28:58 MET DST 1997   Martin Stjernholm <mast@lysator.liu.se>
;;	*  First public release.

;;; Code:

(eval-when-compile
  (require 'cl))			; Some handy macros.

;;; WS Trim tools

;;;###autoload
(defvar ws-trim-method-hook '(ws-trim-leading ws-trim-trailing)
  "*The kind of trimming done by the WS Trim mode and functions.
A single or a list of functions which are run on each line that's
getting trimmed.  Supplied trim functions:

`ws-trim-trailing'        Delete trailing whitespace.
`ws-trim-leading-spaces'  Replace unnecessary leading spaces with tabs.
`ws-trim-leading-tabs'    Replace leading tabs with spaces.
`ws-trim-leading'         Replace leading tabs or spaces according to
                          `indent-tabs-mode'.  If it's nil, leading
                          tabs are replaced with spaces, otherwise
                          it's the other way around.
`ws-trim-tabs'            Replace all tabs with spaces.

This is a perfectly normal hook run by `run-hooks' and custom
functions can of course be used.  There's no inherent restriction to
just whitespace trimming either, for that matter.  Each function
should modify the current line and leave point somewhere on it.")
(make-variable-buffer-local 'ws-trim-method-hook)

;;;###autoload
(defun ws-trim-line (arg)
  "Trim whitespace on the current line.
Do this according to the hook `ws-trim-method-hook'.  With a prefix
argument, ask for the trim method to use instead."
  (interactive "*P")
  (let ((ws-trim-method-hook (if arg (ws-trim-ask-method) ws-trim-method-hook))
	(ws-trim-changed-region 'ignore)) ; ws-trim-after-change disabled now.
    (save-excursion
      (run-hooks 'ws-trim-method-hook))))

;;;###autoload
(defun ws-trim-region (arg)
  "Trim whitespace on each line in the region.
Do this according to the hook `ws-trim-method-hook'.  With a prefix
argument, ask for the trim method to use instead."
  (interactive "*P")
  (let ((ws-trim-method-hook (if arg (ws-trim-ask-method) ws-trim-method-hook)))
    (ws-trim-region-1 (mark) (point))))

;;;###autoload
(defun ws-trim-buffer (arg)
  "Trim whitespace on each line in the buffer.
Do this according to the hook `ws-trim-method-hook'.  With a prefix
argument, ask for the trim method to use instead."
  (interactive "*P")
  (ws-trim-reset-changed-region nil)
  (let ((ws-trim-method-hook (if arg (ws-trim-ask-method) ws-trim-method-hook)))
    (ws-trim-region-1 (point-min) (point-max))))

(defun ws-trim-trailing ()
  "Delete trailing whitespace on current line.
Normally used in `ws-trim-method-hook'."
  (end-of-line)
  (if (memq (preceding-char) '(?\  ?\t))
      (delete-horizontal-space)))

(defun ws-trim-leading-spaces ()
  "Replace unnecessary leading spaces with tabs on current line.
Normally used in `ws-trim-method-hook'."
  (let* ((indent-tabs-mode t)
	 (col (current-indentation))
	 (tab-col (* (/ col tab-width) tab-width)))
    (beginning-of-line)
    (skip-chars-forward "\t")
    (when (/= (current-column) tab-col)
      (d
