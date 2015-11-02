(setq inhibit-splash-screen t)

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                          ("marmalade" . "https://marmalade-repo.org/packages/")
                          ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(global-set-key (kbd "s-q") 'magit-status)

;; a sensible font size for a 15" 1080p LCD
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono 10"))

(when window-system
  (load-theme 'solarized t)
  (set-frame-parameter nil 'background-mode 'dark)
  (when (not (display-graphic-p (selected-frame)))
    (set-terminal-parameter (frame-terminal frame) 'background-mode mode))
  (scroll-bar-mode -1)
  (enable-theme 'solarized))

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

;; paths containing additional .el files
(duncans_emacs:add-to-load-path
  '("~/.emacs.d/auto-complete"
     "~/.emacs.d/cl-lib"
     "~/.emacs.d/coffee-mode"
     "~/.emacs.d/cucumber.el"
     "~/.emacs.d/diff-hl"
     "~/.emacs.d/editorconfig"
     "~/.emacs.d/emacs-jabber"
     "~/.emacs.d/emms"
     "~/.emacs.d/emotion-iconous-mode"
     "~/.emacs.d/find-file-in-project"
     "~/.emacs.d/geiser"
     "~/.emacs.d/go-mode"
     "~/.emacs.d/haml-mode"
     "~/.emacs.d/hexrgb"
     "~/.emacs.d/inform-mode"
     "~/.emacs.d/js2-mode"
     "~/.emacs.d/markdown-mode"
     "~/.emacs.d/multiterm"
     "~/.emacs.d/org-caldav"
     "~/.emacs.d/org-present"
     "~/.emacs.d/php-mode"
     "~/.emacs.d/psvn"
     "~/.emacs.d/sass-mode"
     "~/.emacs.d/slime"
     "~/.emacs.d/tomatinho"
     "~/.emacs.d/quack"
     "~/.emacs.d/undo-tree"
     "~/.emacs.d/ws-trim"
     "~/.emacs.d/yaml-mode"
     "~/.emacs.d/yari.el"))

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

;; golang
(require 'go-mode)
(duncans_emacs:set-mode
  'go-mode
  '("\\.go$"))

;; modern Emacs behaviour; see http://xahlee.org/emacs/emacs_make_modern.html
(transient-mark-mode 1)
(delete-selection-mode 1)
(global-linum-mode 1)
(column-number-mode 1)

;; highlight current line
(global-hl-line-mode 1)

;; tab-with-spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;; dont save a semantic.cache file in every dir (courtesy http://shreevatsa.wordpress.com/)
(setq semanticdb-default-save-directory "~/.emacs.d/semantic_cache")

;; hide line numbers for terminals, Jabber
;; no way of turning off line highlighting, grump grump grump (http://www.emacswiki.org/emacs/HighlightCurrentLine)
(defun disable-linum-mode ()
  (linum-mode 0))
(add-hook 'term-mode-hook 'disable-linum-mode)
(add-hook 'jabber-chat-mode-hook 'disable-linum-mode)

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
(slime-setup '(slime-fancy slime-asdf))

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
(setq ffip-limit 102400)
(setq ffip-find-options "-not -wholename '*.a' -not -regex '.*\/\.git\/.*' -not -regex '.*\/\.db\/.*' -not -regex '.*\/node_modules\/.*' -not -regex '.*\/tmp/.*'")

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

;; markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

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
(setq find-grep-options "-q -i")

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
    (linum-mode 0)
    (global-hl-line-mode 0)
    (org-present-big)
    (org-display-inline-images)
    (visual-line-mode)))
(add-hook 'org-present-mode-quit-hook
  (lambda ()
    (linum-mode 1)
    (global-hl-line-mode 1)
    (org-present-small)
    (org-remove-inline-images)
    (setq visual-line-mode nil)))

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

;; make buffer names unique with pathname, in reverse, for clearer navigation
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; command to turn on ANSI colour highlighting ...
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))
;; ... and assume that log files may contain ANSI colour sequences
(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))

;; CalDAV synchronisation with org-mode
(require 'org-caldav)

(global-set-key (kbd "s-b") 'browse-url-at-point)
(add-hook 'org-mode-hook (lambda () (toggle-truncate-lines)))
(setq org-startup-indented t)
(setq org-hide-leading-stars nil)

(server-start)

;; keep the same configuration as team-mates
(load "editorconfig")

;; load ALL THE MESSAGES AND SHOW ALL in Gnus; best of luck if on Usenet
(setq gnus-large-newsgroup nil)
(setq gnus-parameters
  '(("nnimap.*"
     (gnus-use-scoring nil)
     (display . all))))

;; contacts DB
(require 'bbdb)
(bbdb-initialize 'gnus)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(setq bbdb-complete-name-full-completion t)
(setq bbdb-completion-type 'primary-or-name)
(setq bbdb-complete-name-allow-cycling t)

;; better undo
(require 'undo-tree)
(global-undo-tree-mode)

;; prevent emacs from making backup or autosave files
(setq auto-save-default nil)
(setq make-backup-files nil)

;; offline Ruby documentation in Emacs
;; thanks to https://github.com/ldeck/ldeck-starter-kit for ideas
(require 'yari)
(setq yari-ri-program-name (concat (getenv "HOME") "/.rbenv/shims/ri"))
(setq yari-ruby-program-name (concat (getenv "HOME") "/.rbenv/shims/ruby"))

(defun yari-bind-key ()
  (local-set-key (kbd "C-h r") 'yari))

(add-hook 'ruby-mode-hook 'yari-bind-key)

;; courtesy http://emacsredux.com/blog/2013/06/21/eval-and-replace/
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
    (prin1 (eval (read (current-kill 0)))
      (current-buffer))
    (error (message "Invalid expression")
      (insert (current-kill 0)))))
(global-set-key (kbd "C-c e") 'eval-and-replace)

(defun my-message-mode-setup ()
  (setq fill-column 72)
  (turn-on-auto-fill))
(add-hook 'message-mode-hook 'my-message-mode-setup)

;; Cypher / Neo4j
(duncans_emacs:set-mode
  'cypher-mode
  '("\\.cql$"))

;; edit ccrypted files
(require 'ps-ccrypt)
