;;; emoicon-list.el --- 
;; 
;; Filename: emoicon-list.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: mié dic  7 16:54:37 2011 (-0300)
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; This library give functions to list emoticons. 
;; The main idea is to list all emoticons in a specialized buffer.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'emotion-iconous-mode)

					; --------------------
					; Listing emoticons


;; (defun emoicon-insert-emoticon (emoticonalist)
;;   "Insert the emoticon in the alist in the buffer."
;;   (insert (format "%s :%s\n" (car emoticonalist) (car emoticonalist)))
;;   )

(defun emoicon-insert-all-emoticons ()
  "Insert all the emoticons that exists in the alist `emoicon-emoticons-alist' in the current buffer."
  (dolist (elto emoicon-emoticons-alist)
    (emoicon-insert-emoticon (car elto))
    (insert "\n")
    )
  )

(defun emoicon-list-emoticons ()
  "Show all emoticons available in a new buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*Emoticons*")
    (emoicon-list-mode)
    (let ((inhibit-read-only t)
	  )
      (erase-buffer)
      (emoicon-insert-all-emoticons)
      )
    (goto-char 0)
    )  
  (split-window-vertically 10)
  (switch-to-buffer "*Emoticons*")
  )

(defun emoicon-list-quit ()
  "Kill emoticons buffer and restore window."
  (interactive)
  (with-current-buffer "*Emoticons*"
    (delete-window)
    (kill-buffer "*Emoticons*")
    )
  )


					; --------------------
					; Mayor mode

(defvar emoicon-list-mode-font-lock 
  '(
    ;; font-lock-keywords
    (
     )
    ;; Otros...
    )
  ;;
  "Font lock for `ej-mode'"
  )

(defvar emoicon-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'emoicon-list-quit)
    map)
  "Emoicon-list-mode keymap.")


(define-derived-mode emoicon-list-mode nil "Emoicon List"
  "Mayor mode for listing emoticons"
  
  (make-local-variable 'text-mode-variant)
  (setq text-mode-variant t)
  ;; These two lines are a feature added recently.
  (set (make-local-variable 'require-final-newline)
       mode-require-final-newline)
  (set (make-local-variable 'indent-line-function) 'indent-relative)
  ;; font lock para ej-mode
  (set (make-local-variable 'font-lock-defaults)
       emoicon-list-mode-font-lock)
  ;(set (make-local-variable 'font-lock-keywords)
  ;     ej-mode-font-lock)
  (set (make-local-variable 'buffer-read-only)
       t)
  )




(provide 'emoicon-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emoicon-list.el ends here
