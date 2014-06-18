(setq inhibit-splash-screen t)

;; my own extensions
(add-to-list 'load-path "~/.emacs.d/duncans_emacs")
(require 'duncans_emacs)

;; hide the tool bar but show the menu bar
(if window-system
    (progn
      (tool-bar-mode 0)
      (menu-bar-mode 0)))

;; allow us to use the X-Windows clipboard
(setq x-select-enable-clipboard t)  ; as above
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; don't wait for font change; works around a Gnome bug (see https://launchpad.net/metacity/+bug/23005)
(set-default-font "-unknown-Liberation Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
(modify-frame-parameters nil '((wait-for-wm . nil)))

(custom-set-faces
 ;; make the line numbers fade into the background in console mode
 '(linum ((t (:inherit (shadow default) :background "grey" :foreground "black")))))

;; paths containing additional .el files
(duncans_emacs:add-to-load-path
 '("~/.emacs.d"
   "~/.emacs.d/auto-complete"
   "~/.emacs.d/ccrypt"
   "~/.emacs.d/cl-lib"
   "~/.emacs.d/coffee-mode"
   "~/.emacs.d/cucumber.el"
   "~/.emacs.d/diff-hl"
   "~/.emacs.d/emacs-jabber"
   "~/.emacs.d/emms"
   "~/.emacs.d/emotion-iconous-mode"
   "~/.emacs.d/find-file-in-project"
   "~/.emacs.d/geiser"
   "~/.emacs.d/haml-mode"
   "~/.emacs.d/hexrgb"
   "~/.emacs.d/inform-mode"
   "~/.emacs.d/js2-mode"
   "~/.emacs.d/markdown-mode"
   "~/.emacs.d/multiterm"
   "~/.emacs.d/org-present"
   "~/.emacs.d/php-mode"
   "~/.emacs.d/psvn"
   "~/.emacs.d/ruby-mode"
   "~/.emacs.d/sass-mode"
   "~/.emacs.d/slime"
   "~/.emacs.d/tomatinho"
   "~/.emacs.d/quack"
   "~/.emacs.d/undo-tree"
   "~/.emacs.d/vbnet-mode"
   "~/.emacs.d/ws-trim"
   "~/.emacs.d/yaml-mode"))

;; Emacs itself
(setq auto-mode-alist (cons '("emacs$" . lisp-mode) auto-mode-alist))

;; Ruby (thanks to http://stackoverflow.com/users/523044/tyler)
(global-font-lock-mode 1)
(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(duncans_emacs:set-mode
 'ruby-mode
 '("\\.erb$"
   "Gemfile$"
   "\\.gemspec$"
   "Guardfile$"
   "\\.prawn$"
   "\\.rake$"
   "Rakefile$"
   "\\.rb$"
   "\\.rhtml$"
   "\\.rsel$"))


;; HAML
(require 'haml-mode)
(duncans_emacs:set-mode
 'haml-mode
 '("\\.haml$"
   "\\.hamlc$"
   "\\.hamljs$"))

;; CSS
(duncans_emacs:set-mode
 'css-mode
 '("\\.css$"
   "\\.scss$"))

;; Coffeescript
(require 'js2-mode)
(require 'coffee-mode)
(setq coffee-tab-width 2)
(global-set-key (kbd "C-B") 'coffee-compile-region)
(duncans_emacs:set-mode 'coffee-mode '("\\.coffee$"))

;; SASS
(require 'sass-mode)
(duncans_emacs:set-mode 'sass-mode '("\\.sass$"))

;; Cucumber
(require 'feature-mode)
(autoload 'feature-mode "feature-mode" "Cucumber feature editing mode." t)
(duncans_emacs:set-mode 'feature-mode '("\\.feature$"))

;; YAML
;; http://www.emacswiki.org/emacs/YamlMode
(require 'yaml-mode)
(duncans_emacs:set-mode 'yaml-mode '("\\.yml$"))
(add-hook 'yaml-mode-hook '(lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; PHP
(require 'php-mode)

;; Inform support
(autoload 'inform-mode "inform-mode" "Inform editing mode." t)
(autoload 'inform-maybe-mode "inform-mode" "Inform/C header editing mode.")
(duncans_emacs:set-mode 'inform-maybe-mode '("\\.h\\'"))
(duncans_emacs:set-mode 'inform-mode '("\\.inf\\'"))

;; modern Emacs behaviour; see http://xahlee.org/emacs/emacs_make_modern.html
(cua-mode)
(transient-mark-mode 1)
(delete-selection-mode 1)
(global-linum-mode 1)
(column-number-mode 1)

;; highlight current line
(global-hl-line-mode 1)

;; prevent emacs from making backup files
(setq make-backup-files nil)

;; allow automagic editing of ccrypt-ed files
(require 'jka-compr-ccrypt "jka-compr-ccrypt.el")

;; tab-with-spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;; dont save a semantic.cache file in every dir (courtesy http://shreevatsa.wordpress.com/)
(setq semanticdb-default-save-directory "~/.emacs.d/semantic_cache")

;; launch some shells unless we're in Windows
(if (and
     (not (equal system-type 'windows-nt)))
    (duncans_emacs:create-terminals
     '("*tests*"
       "*server*"
       "*guard*"
       "*misc*")))

;; show full path in title bar
;; thanks to: http://www.arminsadeghi.com/slickedit_and_emacs
(setq-default frame-title-format
              (list '((buffer-file-name " %f"
                                        (dired-directory
                                         dired-directory
                                         (revert-buffer-function " %b"
                                                                 ("%b - Dir:  " default-directory)))))))

;; SLIME
(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(setq inferior-lisp-program "sbcl")

;; some Steve Yegge wisdom; see http://sites.google.com/site/steveyegge2/effective-emacs
(global-set-key (kbd "C-w")     'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)

;; https://github.com/duncan-bayne/mint-setup/issues/16 (Emacs: content-search across code files)
(global-set-key (kbd "C-x F") 'find-name-dired)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;; Interactively Do Things ( http://emacswiki.org/emacs/InteractivelyDoThings )
(require 'ido)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido)))

;; don't keep asking to follow VCS links
(setq vc-follow-symlinks t)

;; don't keep prompting about active processes on exit; see http://www.emacswiki.org/emacs/EmacsNiftyTricks
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;; find file in project, like Command-T in TextMate
(require 'find-file-in-project)
(global-set-key (kbd "C-x t") 'find-file-in-project)
(mapc '(lambda (ext) (add-to-list 'ffip-patterns ext))
      '("*"))
(setq ffip-limit 10240)
(setq ffip-find-options "-not -iwholename '.db\/*' -not -iwholename '.git'")

;; automatically revert changed files
(global-auto-revert-mode 1)

;; Pomodoro with tomatinho
(require 'tomatinho)
(global-set-key (kbd "<f12>") 'tomatinho)
(global-set-key (kbd "s-<return>") 'tomatinho-interactive-deliberate-pause)

;; trim trailing whitespace
(require 'ws-trim)
(global-ws-trim-mode t)
(set-default 'ws-trim-level 2)
(setq ws-trim-global-modes '(guess (not message-mode eshell-mode haml-mode)))

;; nice font scaling shortcuts
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; load work-specific stuff
(if (file-exists-p "~/.emacs.d/work.el")
    (load "~/.emacs.d/work.el"))

;; markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(global-set-key (kbd "C-x M-m") 'duncans_emacs:markdownify)
(global-set-key (kbd "C-x M-M") 'duncans_emacs:markdown)

;; edit as root
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.  With a prefix ARG prompt for a file to visit.  Will also prompt for a file to visit if current buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
(global-set-key (kbd "C-x C-r") 'sudo-edit)

;; Jabber client
(require 'jabber-autoloads)
(require 'emotion-iconous-mode)

;; some things - like ccrypt - don't like auto-save
(setq auto-save-default nil)

;; always find myself doing this - is that a process smell?
(global-set-key (kbd "C-x M-f") 'find-grep-dired)

;; Geiser and Quack for Racket
(load-file "~/.emacs.d/geiser/elisp/geiser.el")
(setq scheme-program-name "racket")
(defun scheme-mode-quack-hook ()
  (require 'quack)
  (setq quack-fontify-style 'emacs))
(add-hook 'scheme-mode-hook 'scheme-mode-quack-hook)

;; presentations in Emacs - yay, one more violation of the UNIX philosophy ;)
(autoload 'org-present "org-present" nil t)
(add-hook 'org-present-mode-hook
          (lambda ()
            (org-present-big)
            (org-display-inline-images)))
(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (org-present-small)
            (org-remove-inline-images)))

;; helper function to connect to HipChat rooms through jabber.el
(defun hipchat-join (room)
  (interactive "sRoom name: ")
  (jabber-groupchat-join
   (jabber-read-account)
   (concat hipchat-number "_" room "@conf.hipchat.com")
   hipchat-nickname
   t)
  (require 'jabber-emoicon))
(custom-set-variables '(jabber-auto-reconnect t))

;; beep and xmessage on Jabber message
(add-hook 'jabber-alert-message-hooks 'jabber-message-beep)

;; always-on VCS diff highlighting
(require 'diff-hl)
(global-diff-hl-mode)

(global-set-key (kbd "s-b") 'browse-url-at-point)
(add-hook 'org-mode-hook (lambda () (toggle-truncate-lines)))

