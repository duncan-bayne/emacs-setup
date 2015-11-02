;;; ps-ccrypt-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ps-ccrypt" "ps-ccrypt.el" (22071 15567 769194
;;;;;;  912000))
;;; Generated autoloads from ps-ccrypt.el
(defun auto-encryption-mode (&optional arg)
 "\
Toggle automatic file encryption and decryption.
With prefix argument ARG, turn auto encryption on if positive, else off.
Returns the new status of auto encryption (non-nil means on)."
 (interactive "P")
 (if (not (fboundp 'ps-ccrypt-installed-p))
     (progn
       (require 'ps-ccrypt)
       ;; That turned the mode on, so make it initially off.
       (toggle-auto-encryption)))
 (toggle-auto-encryption arg t))

;;;***

;;;### (autoloads nil nil ("ps-ccrypt-pkg.el") (22071 15567 793529
;;;;;;  252000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ps-ccrypt-autoloads.el ends here
