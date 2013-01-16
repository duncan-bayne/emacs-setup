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
(set-default-font "-misc-fixed-medium-r-normal--14-110-100-100-c-70-iso8859-1")
(modify-frame-parameters nil '((wait-for-wm . nil)))

(custom-set-faces
 ;; make the line numbers fade into the background in console mode 
 '(linum ((t (:inherit (shadow default) :background "grey" :foreground "black")))))

;; paths containing additional .el files
(duncans_emacs:add-to-load-path
 '("~/.emacs.d" 
   "~/.emacs.d/multiterm" 
   "~/.emacs.d/ruby-mode" 
   "~/.emacs.d/psvn" 
   "~/.emacs.d/ccrypt" 
   "~/.emacs.d/yaml-mode" 
   "~/.emacs.d/vbnet-mode" 
   "~/.emacs.d/cucumber.el" 
   "~/.emacs.d/php-mode" 
   "~/.emacs.d/inform-mode" 
   "~/.emacs.d/haml-mode" 
   "~/.emacs.d/sass-mode" 
   "~/.emacs.d/slime" 
   "~/.emacs.d/coffee-mode" 
   "~/.emacs.d/undo-tree"
   "~/.emacs.d/emms"
   "~/.emacs.d/find-file-in-project"))

;; Emacs itself
(setq auto-mode-alist (cons '("emacs$" . lisp-mode) auto-mode-alist))

;; Ruby (thanks to http://stackoverflow.com/users/523044/tyler)
(global-font-lock-mode 1)
(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(duncans_emacs:set-mode 
 'ruby-mode
 '("\\.rb$"
   "\\.rsel$"
   "\\.rhtml$"
   "\\.erb$" 
   "\\.prawn$"
   "\\.rake$"
   "Rakefile$"
   "Gemfile$"
   "Guardfile$"))

;; HAML
(require 'haml-mode)
(duncans_emacs:set-mode
 'haml-mode
 '("\\.haml$"
   "\\.hamlc$"
   "\\.hamljs$"))

;; Coffeescript
(require 'coffee-mode)
(duncans_emacs:set-mode 'coffee-mode '("\\.coffee$"))

;; use two spaces for tabs in Coffeescript; see https://github.com/defunkt/coffee-mode
;; commented out for now, see https://github.com/defunkt/coffee-mode/issues/68
;; (defun coffee-custom ()
;;   "coffee-mode-hook"
;;   (set (make-local-variable 'tab-width) 2))
;; (add-hook 'coffee-mode-hook
;;    '(lambda() (coffee-custom)))

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
(setq indent-tabs-mode nil)
(setq standard-indent 2)

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

;; make coffee-mode use two spaces for tabs
(setq coffee-tab-width 2)

;; find file in project, like Command-T in TextMate
(require 'find-file-in-project)
(global-set-key (kbd "C-x t") 'find-file-in-project)
(mapc '(lambda (ext) (add-to-list 'ffip-patterns ext))
      '("*.yml" "*.sass" "*.haml" "*.hamlc" "*.css" "*.rake" "Rakefile" "Gemfile" "Guardfile" "*.txt" "*.coffee" "*.feature" "*.erb" "*.ru" "*.html" "*.js" "*.json"))
(setq ffip-limit 10240)

;; automatically revert changed files
(global-auto-revert-mode 1)

