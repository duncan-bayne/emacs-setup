;;; emotion-iconous-mode.el ---
;;
;; Filename: emotion-iconous-mode.el
;; Description:
;; Author: Christian
;; Maintainer:
;; Created: mié oct 26 21:11:25 2011 (-0300)
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
;; Provides a numerous functions for changing text into emoticons
;; whenever is possible.
;; Also includes a minor mode.
;;
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

;; (defgroup emotion-iconous-mode nil
;;   "Minor Mode for emoticons."

;;   )

(require 'xml)

(defvar emoicon-theme-loaded nil
  "If t the them has been loaded."
  )

(defvar emotion-iconous-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'emoicon-space-key)
    (define-key map [return] 'emoicon-enter-key)
    (define-key map "." 'emoicon-point-key)

    map
    )
  "Keymap for emotion-iconous-mode. See `emoicon-setup-keys'."
)

(defun emoicon-check-and-replace-emoticon ()
  "Check if the last word is an emoticon. If it is add the necessary properties for displaying its image!"
  (when emoicon-theme-loaded
    (unless
  (and ;; Has emoticon already?
   (let ((p (emoicon-last-word-position))) ;; `emoicon-last-word-position' can return nil...
     ;; ...nil is not adecuate as parameter for `get-text-propety'
     (if p
         (get-text-property p 'emo-icon) ;; is not nil... return last word position
       nil)) ;; And is in the `emoicon-emoticons-alist'?
   (cdr (assoc (emoicon-last-word) emoicon-emoticons-alist)))
      ;; It hasn't an emoticon and it is one!
      (emoicon-replace-last-word)
      )
    )
  )

(defun emoicon-space-key ()
  "If the last word is an emoticon listed in the alist `emoicon-emoticons-alist' change into its image if necessary."
  (interactive)

  (emoicon-check-and-replace-emoticon)
  ;; And insert space!
  (insert " ")
  )

(defun emoicon-enter-key ()
  (interactive)

  (emoicon-check-and-replace-emoticon)
  (insert ?\n)
  )

(defun emoicon-point-key ()
  (interactive)
  (emoicon-check-and-replace-emoticon)
  (insert ".")
  )


(define-minor-mode emotion-iconous-mode
  "Minor Mode for emoticons.
Emoticons appears next to characters represting that emotion."
  :group emotion-iconous
  :global nil
  :lighter " emo-icon"
  :init-value nil
  :keymap emotion-iconous-mode-map

  (if emotion-iconous-mode
      (progn
  ;; Load the emotion icons images
  (emoicon-load-emoticon-theme)

  (emoicon-buffer)
  )
    (progn
      (emoicon-erase-all-emoticons)
      )
    )
  )


;; (defun emoicon-setup-keys ()
;;   "Setup the keys to search for every emoticon and put its image."
;;   (define-key emotion-iconous-mode-map (read-kbd-macro key) 'emoicon-key-cmd)))
;;   )


(defun emicon-key-cmd ()
  "Command trigger for every key.
Put the image of the emoticon next to the text if the last word typed is an emoticon."
  (emoicon-process-last-word)
  )

(defun emoicon-insert-all-emoticon (hash-elt)
  "Change all emoticon to its image.
I need an alist element (EMOTICON . IMAGE-FILENAME)."
  (let (
  (emoticon (car hash-elt))
  (image-filename (cdr hash-elt))
  (case-fold-search nil)
  )

    (save-excursion
      (goto-char (point-min))

      (while (search-forward emoticon nil t)
  (goto-char (match-end 0))
  (emoicon-replace-last-word)
  )
      )
    )
  )

(defun emoicon-erase-all-emoticons ()
  "Erase all emoticon properties to reset the buffer as the minor mode wasn't applied."
  (interactive)

  (let ((beg (point-min))
  (end (point-max)))
    (while (and beg (< beg end))
      (setq beg (next-single-property-change beg 'emo-icon))
      (if beg
    (remove-text-properties beg (next-single-property-change beg 'emo-icon)
          (list 'display nil
          'emo-icon nil
          'help-echo nil))
  )
      )
    )
  )

(defun emoicon-buffer ()
  "Put emoticons next to each emoticon text."
  (interactive)

  ;; Reset the buffer
  (emoicon-erase-all-emoticons)

  (unless emoicon-theme-loaded
    (emoicon-load-emoticon-theme)
    )

  ;; Search for each emoticon in the buffer...
  (dolist (elto emoicon-emoticons-alist)
    (emoicon-insert-all-emoticon elto)
    )
  )

(defcustom emoicon-path-themes
  "~/.emacs.d/emoticons"
  "Path where to find themes. Each theme must be in a directory with its name."
  :group 'emotion-iconous-mode
  :type '(directory)
  )

(defcustom emoicon-theme
  "HipChat-Emoticons"
  "Actual theme for emotion-iconous-mode."
  :group 'emotion-iconous-mode
  :type '(string)
  )

(defvar emoicon-emoticons-alist nil
  "This is an alist that has each emoticon with it respectively image file name.
This alist is loaded from a file from the theme directory.

Use the function `emoicon-load-emoticon-theme' to set accordingly this variable.
"
  )

(defvar emoicon-theme-file "Emoticons.plist"
  "Name of the emoicon theme file where to find every emoticon text mapped to a image filename."
  )

(defun emoicon-file-path (file-name)
  "Create a path to a file in the theme path."
  (format "%s/%s/%s" emoicon-path-themes emoicon-theme file-name)
  )

(defun emoicon-theme-format ()
  "Return the main theme file format.
Return values are:
   pidgin
   plist
   nil   (if not recognized)
"
  (cond
      ((file-exists-p (emoicon-file-path "theme"))
       'pidgin)
      ((file-exists-p (emoicon-file-path "Emoticons.plist"))
       'plist)
      )
  )

(defun emoicon-load-emoticon-theme ()
  "Identify the theme file format and load it.
File formats can be:
 * Pidgin format
 * XML PList format

Fill the `emoicon-emoticons-alist' alist variable and set the `emoicon-theme-loaded' variable to t when finished."

  (interactive)
  (case (emoicon-theme-format)
    (pidgin
     (emoicon-load-pidgin-theme emoicon-theme)
     (message "Pidgin theme loaded... DONE :)")
     )
    (plist
     (emoicon-load-emoticon-plist-theme)
     (message "PList theme loaded... DONE :)")
     )
    )

  (setq emoicon-theme-loaded t)
  )

          ; --------------------
          ; Emoticon theme in PList XML format
          ; --------------------

(defun emoicon-load-emoticon-plist-theme ()
  "Load the 'Emoticons.plist' file wich has a map for each emoticon text into the image name.
Remember: Emoticons.plist is in XML format."
  ;; Get the XML from the file
  (setq emoicon-xml (xml-parse-file (emoicon-file-path emoicon-theme-file)))
  (emoicon-process-xml-iconsname emoicon-xml)
  (setq emoicon-theme-loaded t)
  )

(defun emoicon-process-xml-iconsname (xml-parsed)
  "Process an XML parsed from Emoticons.plist into an alist."
  (let ((emoicon-xml-buffer (get-buffer-create "**XML**")))
    (print xml-parsed emoicon-xml-buffer)
    )

  ;; Nos intereza "dict"
  (setq xml-parsed (car xml-parsed))
  (setq xml-parsed (xml-get-children xml-parsed 'dict))
  (setq xml-parsed (car xml-parsed))
  (setq xml-parsed (xml-get-children xml-parsed 'dict))

  ;; Debemos prosesar los hijos de dict...
  (setq xml-parsed (car xml-parsed))
  (setq xml-children-nodes (xml-node-children xml-parsed))

  (emoicon-process-each-keydict-entry xml-children-nodes)
  )

(defun emoicon-process-key (key-xml)
  "Take out the 'key' and 'nil' word. Just leave the name of the image file."
  (nth 2 key-xml)
  )

(defun emoicon-process-dict (dict-xml)
  "Take out everything unnecessary, just leave the emoticons texts.
Return a list of string with emoticons texts."
  (setq arr (car (xml-get-children dict-xml 'array)))
  (setq arr (xml-node-children arr))

  (setq lst-emoticons '())
  (dolist (str arr)
    (when (not(stringp str))
      (push (nth 2 str) lst-emoticons)
      )
    )
  lst-emoticons
  )

(defun emoicon-process-keydict-entry (keydict-xml)
  "For this key-dict entry change it in a sequence of alist elements like this:
 (EMOTICON-CHAR . IMAGE-FILE).
Return a list of alist elements described as above."
  (setq lst-emoticons (emoicon-process-dict (car keydict-xml)))
  (setq image-file (emoicon-process-key (car (cdr keydict-xml))))
  (setq lst '())
  (dolist (emoticon lst-emoticons)
    (setq elto (cons emoticon image-file))

    (push elto lst)

    )

  lst
  )

(defun emoicon-process-each-keydict-entry (xml)
  "For each key and then dict make an alist for that emoticon associated with the image."
  ;; Reset emoticons alist...
  (setq emoicon-emoticons-alist '())

  (while xml
    ;; Key: Insertarlo a keydict y removerlo de xml.
    (setq keydict '(""))
    (while (stringp (car keydict))
      (setq keydict
      (cons (car xml) '()))
      (setq xml (cdr xml))
      )
    ;; dict: Insertarlo en Keydict y removerlo de xml.
    (setq dictelt "")
    (while (stringp dictelt)
      (setq dictelt (car xml))
      (setq xml (cdr xml))
      )

    (push dictelt keydict)

    (let ((entries (emoicon-process-keydict-entry keydict)))
      (when entries
  ;; there are new entries... add to alist
  (dolist (elto entries)
    (push elto emoicon-emoticons-alist)
    )
  )
      )
    )
  )

(defun emoicon-last-word ()
  "Return last word no matter if they are just symbols.
If it is necessary to ignore some spaces to reach the last word it is ignored."
  (save-excursion
    (if (search-backward-regexp "[[:space:]][^[:space:]]+" nil t)
  (substring (match-string 0) 1) ;; erase the first space... return this.
      nil
      )
    )
  )


(defun emoicon-insert-last-word ()
  "Insert next to the last word from the current buffer if it is an emoticon text.
First look the last word, if it is in the `emoicon-emoticons-alist' then insert its emoticon."
  (interactive)
  ;; Get last word
  (let ((curword (emoicon-last-word)))
    ;; find emoticon in `emoicon-emoticons-alist'
    (let ((emoticon-file (cdr (assoc curword emoicon-emoticons-alist))))
      (when emoticon-file
  (emoicon-insert-emoticon-from-file emoticon-file)
  )
      )
    )
  )

(defun emoicon-insert-emoticon (emoticon-text &optional without-text)
  "Insert an emoticon-text followed by its image.
If the emoticon given by parameter doesn't exists then don't put anything and return nil.
If without-text is t, then don't write the emoticon-text."
  (interactive "MFace?")
  (let ((emoticon-file (cdr (assoc emoticon-text emoicon-emoticons-alist))))
    (when emoticon-file
      (unless without-text
  (insert emoticon-text)
  )
      (emoicon-insert-emoticon-from-file emoticon-file)
      )
    )
  )

(defun emoicon-insert-emoticon-from-file (emoticon-file)
  "Insert the file in the current position at the buffer."
  (let ((img (create-image (emoicon-file-path emoticon-file))))
    (insert-image img)
    )
  )

(defun emoicon-insert-and-replace-emoticon (emoticon-text)
  "Is simmilar to `emoicon-insert-emoticon'. Here inserts the text and put the emoticon *on* the text."
  (interactive "MEmoticon?")
  (let (
  (emoticon-file (cdr (assoc emoticon-text emoicon-emoticons-alist)))
  (begpoint (point))
  )
    (when emoticon-file
      (insert emoticon-text)
      (emoicon-replace-emoticon emoticon-file emoticon-text begpoint (point))
      )
    )
  )

(defun emoicon-last-word-position ()
  "Return the begining of the last word."
  (save-excursion
    (if (search-backward-regexp "[[:space:]][^[:space:]]+" nil t)
  (+ 1 (match-beginning 0))
      nil)
    ))



(defun emoicon-replace-last-word ()
  "Simmilar to `emoicon-insert-last-word' but this time replacing the text.
Just add the text properties nescessary for displaying an image."
  (interactive)
  (let ((emoticon-text (emoicon-last-word)))
    (let (
    (image-file (cdr (assoc emoticon-text emoicon-emoticons-alist)))
    )
      (if image-file
    (emoicon-replace-emoticon image-file emoticon-text (emoicon-last-word-position) (point))
  )
      )
    )
  )

(defun emoicon-replace-emoticon (emoticon-file emoticon-text begpoint endpoint)
  "Replace the region between begpoint and endpoint with an image file given by emoticon-file.
The filename is necessary only, the complete path is created by `emoicon-file-path'."

  (let (
  (file-path (emoicon-file-path emoticon-file))
  )
    (add-text-properties begpoint endpoint
       (list
        'display (create-image file-path)
        'emo-icon t
        'help-echo emoticon-text
        ))
    )
  )


          ; --------------------
          ; Functions to load themes in Text files
          ; --------------------


; For pidgin theme files
(defun emoicon-load-pidgin-theme (theme-name)
  "Load the \"theme\" file located in the theme given by theme-name.
Remember to copy the directory of the theme in the `emoicon-path-themes' path!

Loading the theme means to store in the variable `emoicon-emoticons-alist' all the emoticons available and its texts.
This function, also store the theme's name in `emoicon-theme' at least for now."

  ;; Set variables
  (setq emoicon-emoticons-alist '())
  (setq emoicon-theme theme-name)

  ;; Open file
  (let ((buff (find-file-noselect (emoicon-file-path "theme") t))
  )
    ;; search for section "[XMPP]"
    (with-current-buffer buff
      (search-forward "[XMPP]" nil t)
      (goto-char (match-end 0))
      (forward-line)
      (beginning-of-line)
      (emoicon-read-pidgin-entries)
      )
    (kill-buffer buff)
    )


  (setq emoicon-theme-loaded t)
  )

(defun emoicon-ignore-pidgin-comments ()
  "Ignore comments of pidgin entries.
Comments starts with \"#\" at the beginning of line.
"
  (let ((str (buffer-substring-no-properties (point) (+ 1 (point))))
  )
    (while (string=  str "#")
      (forward-line)
      (beginning-of-line)
      (setq str (buffer-substring-no-properties (point) (+ 1 (point))))
      )
    (while (or
      (string= str "!")
      (string= str " "))
      (forward-char)
      (setq str (buffer-substring-no-properties (point) (+ 1 (point))))
      )
    )
  )

(defun emoicon-read-pidgin-entry ()
  "Read only one pidgin entry from the current buffer.
This function ignore comments, and continue in the following entry.

The entry must be of the following format:

PNGFILE     TEXT     TEXT2   ...

Return an alist like the following:
 (
   (TEXT . PNGFILE)
   (TEXT2 . PNGFILE)
   ...
 )"

  (emoicon-ignore-pidgin-comments)
  (let ((lst-str nil)
  )

    ;; Luckily, `split-string' returns nil when it is given an empty line!
    (setq lst-str (split-string
       (buffer-substring-no-properties (point) (line-end-position))))

    (emoicon-convert-list-into-alist lst-str)
   )
  )

(defun emoicon-convert-list-into-alist (lst-str)
  "Converts a list like (\"PNGFILE\" \"TEXT1\" \"TEXT2\") into an alist like:
 ( (\"TEXT1\" . \"PNGFILE\")
   (\"TEXT2\" . \"PNGFILE\")
 )
"

  (let ((png-file (car lst-str))
  (rest (cdr lst-str))
  (ret '())
  )

    (dolist (elto rest)
      (push (cons elto png-file) ret)
      )
    ret
    )
  )

(defun emoicon-read-pidgin-entries ()
  "Read from the current buffer all pidgin entries.
Pidgin entries has the following format:

PNGFILE   TEXT1  TEXT2  ...
PNGFILE2  TEXT1  TEXT2  ...
"

  ;; Read entries and store them!
  (let ((entries (emoicon-read-pidgin-entry))
  )
    (while entries
      ;; Append entries given by `emoicon-read-pidgin-entry' into `emoicon-emoticons-alist'.
      (setq emoicon-emoticons-alist
      (append entries emoicon-emoticons-alist))
      (forward-line)
      ;; Next entry or nil if there isn't more...
      (setq entries (emoicon-read-pidgin-entry))
      )
    )
  )


(provide 'emotion-iconous-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emotion-iconous-mode.el ends here
