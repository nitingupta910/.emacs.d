;;; init.el --- Emacs configuration of Nitin Gupta -*- lexical-binding: t; -*-

;;; Commentary:
;; Flat configuration of Emacs

;;; Code:

;;; Package management

(setq inhibit-startup-message t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
       '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; See: https://www.emacswiki.org/emacs/ViewMode
;; I think toggling view-mode is better than toggle-read-only.
(define-key ctl-x-map "\C-q" 'view-mode)

;; show vertical bar for cursor
(setq-default cursor-type 'bar)

;; See: https://www.quora.com/profile/Stefan-Bucur/Posts/Enabling-Soft-Word-Wrap-in-Emacs
(global-visual-line-mode 1)

(show-paren-mode 1)
(setq show-paren-delay 0)

(setq password-cache-expiry nil)

;; press any key to overwrite selected text
(delete-selection-mode 1)

;; Show line numbers
(global-linum-mode 1)

;; show column number
(column-number-mode t)

;; turn-off emacs beeping.
;; Ref: http://www.emacswiki.org/emacs/AlarmBell
;;(setq visible-bell t)
(setq ring-bell-function 'ignore)

; disable autosave
(setq auto-save-default nil)

;; configure backup file creation
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Auto reload changes from disk.
;; This does not check or revert remote files,
;; because that is usually too slow.
(global-auto-revert-mode)

;; ask before exiting emacs
;(setq confirm-kill-emacs 'y-or-n-p)

;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)

;; tab defaults to 4 sparces
(setq-default indent-tabs-mode nil
              tab-width 4
              indicate-empty-lines t)

;; emacs-mac specific. Use alt as the meta key on mac to
;; match with Cocoa Emacs.
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(setq tags-revert-without-query 1)

;; Show current file name using C-x C-p
;; Source: https://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun ng/show-buffer-file-name ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

(global-set-key (kbd "C-x C-p") 'ng/show-buffer-file-name)

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
;; Also see: http://emacs.stackexchange.com/questions/169/how-do-i-reload-a-file-in-a-buffer
(defun revert-buffer-confirm-if-modified ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t))

(global-set-key (kbd "C-c r") 'revert-buffer-confirm-if-modified)

;; PuTTY fix. Ugly. Bad. But it works. (Good)
;; Even when TERM=xterm-256color on bash and
;; ~/.tmux.conf says:
;;   set-window-option -g xterm-keys on
;;   set -g default-terminal "xterm-256color"
;; still, pressing <end> key results in error:
;;   <select> is undefined
;; This hack fixes the end key. Home key already
;; worked on Linux/tmux (don't know about putty)
(define-key global-map "\M-[1~" 'beginning-of-line)
(define-key global-map [select] 'end-of-line)

;; Hide GUI elements
(when window-system
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  (menu-bar-mode -1)
  )

;; Indentation in C (Linux kernel style)
(setq c-default-style "linux")
(setq c-backspace-function 'backward-delete-char)
(add-hook 'c-mode-common-hook '(lambda ()
                                 (setq-default indent-tabs-mode t)))

(defun ng-get-ppi ()
  "Get display PPI. Do not run this function in non-graphic mode."
  (setq ng-disp-attrs (car (display-monitor-attributes-list)))

  (setq ng-mm-size (assq 'mm-size ng-disp-attrs))
  (setq ng-mm-width (nth 1 ng-mm-size))
  (setq ng-mm-height (nth 2 ng-mm-size))

  (setq ng-diag-mm (sqrt (+
                        (expt ng-mm-width 2)
                        (expt ng-mm-height 2))))

  (setq ng-diag-inches (* ng-diag-mm 0.0393701))

  (setq ng-geom (assq 'geometry ng-disp-attrs))
  (setq ng-pixel-width (nth 3 ng-geom))
  (setq ng-pixel-height (nth 4 ng-geom))

  (setq ng-diag-pixels (sqrt (+
                        (expt ng-pixel-width 2)
                        (expt ng-pixel-height 2))))

  (/ ng-diag-pixels ng-diag-inches))

(if (display-graphic-p)
    (funcall
    (lambda()
      (setq ng-ppi (floor (ng-get-ppi)))

      ; mac air has ppi of 126.x
      (if (<= ng-ppi 92)
          (setq ng-font-height 140)
        (if (<= ng-ppi 108)
            (setq ng-font-height 160)
          (if (<= ng-ppi 126)
              (setq ng-font-height 160)
            (setq ng-font-height 240))))

      (if (eq system-type 'darwin)
    (setq ng-font-face "Menlo")
        (setq ng-font-face "Monospace"))


      ; set larger font size on mac: say, 160 size becomes 232
      (if (eq system-type 'darwin)
		  (setq ng-font-height (floor (* 1.45 ng-font-height))))

      (set-face-attribute 'default nil
                          :height (symbol-value 'ng-font-height)
                          :font (symbol-value 'ng-font-face))
      )))

;; copy/paste from/to x-clipboard (for both osx and linux)
;; source:http://blog.binchen.org/posts/copypaste-in-emacs.html
(setq *is-a-mac* (eq system-type 'darwin))
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(defun copy-to-x-clipboard ()
  (interactive)
  (if (region-active-p)
      (progn
        (cond
         ((and (display-graphic-p) x-select-enable-clipboard)
          (x-set-selection 'CLIPBOARD (buffer-substring (region-beginning) (region-end))))
         (t (shell-command-on-region (region-beginning) (region-end)
                                     (cond
                                      (*cygwin* "putclip")
                                      (*is-a-mac* "pbcopy")
                                      (*linux* "xsel -ib")))
            ))
        (message "Yanked region to clipboard!")
        (deactivate-mark))
        (message "No region active; can't yank to clipboard!")))

(defun paste-from-x-clipboard()
  (interactive)
  (cond
   ((and (display-graphic-p) x-select-enable-clipboard)
    (insert (x-get-selection 'CLIPBOARD)))
   (t (shell-command
       (cond
        (*cygwin* "getclip")
        (*is-a-mac* "pbpaste")
        (t "xsel -ob"))
       1))
   ))

(defun my/paste-in-minibuffer ()
  (local-set-key (kbd "M-y") 'paste-from-x-clipboard)
  )

(add-hook 'minibuffer-setup-hook 'my/paste-in-minibuffer)


;;
;; END Generic settings
;;


;;
;; Usability related
;;
(use-package smooth-scrolling
  :ensure t
  :config (smooth-scrolling-mode 1))

;; multiple cursors (C-S-d isn't working)
(use-package multiple-cursors
  :ensure t
  :bind (("C-d" . mc/mark-next-like-this)
	 ("C-S-d" . mc/mark-previous-like-this)
	 ("C-c C-d" . mc/mark-all-like-this)))

(use-package buffer-move
  :ensure t
  :bind
  (([(ctrl shift up)] . buf-move-up)
   ([(ctrl shift down)] . buf-move-down)
   ([(ctrl shift left)] . buf-move-left)
   ([(ctrl shift right)] . buf-move-right)))

(use-package untitled-new-buffer
  :ensure t
  :bind
  (("M-n" . untitled-new-buffer-with-select-major-mode)))

;; ensure environment variables inside Emacs look the same as in the
;; user's shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (push "HISTFILE" exec-path-from-shell-variables)
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;;
;; Helm stuff
;;
(use-package helm
  :ensure    helm

  :config    (setq helm-ff-transformer-show-only-basename nil
                   helm-boring-file-regexp-list           '("\\.git$" "\\.svn$" "\\.elc$")
                   helm-yank-symbol-first                 t
                   helm-buffers-fuzzy-matching            t
                   helm-ff-auto-update-initial-value      t
                   helm-input-idle-delay                  0.1
                   helm-idle-delay                        0.1)

  :init      (progn
               (require 'helm-config)
               (helm-mode t)

               (use-package helm-ag
                 :ensure    helm-ag
                 :bind      ("C-c x" . helm-projectile-ag))

               (use-package helm-descbinds
                 :ensure    helm-descbinds
                 :bind      ("C-h b"   . helm-descbinds))

               (use-package helm-projectile
                 :ensure    helm-projectile
                 :bind      ("C-c h" . helm-projectile))

               (add-hook 'eshell-mode-hook
                         #'(lambda ()
                             (bind-key "M-p" 'helm-eshell-history eshell-mode-map)))

               (use-package helm-swoop
                 :ensure    helm-swoop
                 :bind      (("C-c o" . helm-swoop)
                             ("C-c M-o" . helm-multi-swoop)))

               (bind-key "C-c C-SPC" 'helm-ff-run-toggle-auto-update helm-find-files-map))

  :bind (("C-x r l" . helm-bookmarks)
         ("M-x" . helm-M-x)
         ("C-h i"   . helm-google-suggest)
         ("M-y"     . helm-show-kill-ring)
         ("C-h a"   . helm-apropos)
         ("C-x C-f" . helm-find-files)
         ("C-x p" .   helm-top)
         ("C-x C-b" . helm-buffers-list)
         ;;; helm: completion using <tab> in helm-find-file
         :map helm-map
         ([tab] . helm-execute-persistent-action) ; rebind tab to do persistent action
         ("C-i" . helm-execute-persistent-action) ; make TAB works in terminal
	 ("C-c g g" . helm-git-grep-from-helm)))

(use-package projectile
  :ensure t
  :commands (projectile-find-file projectile-switch-project)
  :init
  (use-package helm-projectile :ensure t)
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package helm-projectile
  :ensure t
  :bind ("M-t" . helm-projectile-find-file)
  :config
  (helm-projectile-on))

(use-package helm-gtags
  :ensure t
  :init
  (setq
    helm-gtags-ignore-case t
    ;helm-gtags-auto-update t
    helm-gtags-use-input-at-cursor t
    helm-gtags-pulse-at-cursor t
    helm-gtags-prefix-key "\C-cg"
    helm-gtags-suggested-key-mapping t)
  :bind
  (("C-c g a" . helm-gtags-tags-in-this-function)
   ("C-c g d" . helm-gtags-find-tag)
   ("M-s" . helm-gtags-select)
   ("M-." . helm-gtags-dwim)
   ("M-," . helm-gtags-pop-stack)
   ("C-c <" . helm-gtags-previous-history)
   ("C-c >" . helm-gtags-next-history))
  :config
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

; Prevent C-d to be bound to c-electric-delete-forward
(defun my-c-mode-hook ()
  (local-set-key (kbd "C-d") 'mc/mark-next-like-this))

(defun my-c++-mode-hook ()
  (local-set-key (kbd "C-d") 'mc/mark-next-like-this))

(use-package helm-git-grep
  :ensure t
  :bind
  (("C-c g x" . helm-git-grep)
   :map isearch-mode-map
   ("C-c g x" . helm-git-grep)))

(use-package monokai-theme
  :ensure t
  :config (load-theme 'monokai t))

(use-package markdown-mode
  :ensure t
  :config (flyspell-mode))

(use-package magit
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package company
  :ensure t)

;;
;; Rust stuff
;;
(use-package rust-mode
  :ensure t
  :config

  (add-hook 'before-save-hook
            #'(lambda ()
                (when (eq major-mode 'rust-mode)
                  (rust-format-buffer))))

  (use-package flycheck-rust
    :ensure t
    :config

    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package racer
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'company-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package cargo :ensure t)
(use-package toml-mode :ensure t)

;;
;; Elm stuff
;;
(use-package elm-mode
  :ensure t
  :init
  (setq
   elm-tags-on-save t
   elm-format-on-save t)
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-elm-setup)

  (add-hook 'elm-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '(company-elm))
            (local-set-key (kbd "M-.") 'elm-mode-goto-tag-at-point)
            (local-set-key (kbd "M-,") 'pop-tag-mark)
            (company-mode)
            (flycheck-mode)))

  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion))

(use-package flycheck-elm
  :ensure t)

;;
;; Elixir stuff
;;
(use-package alchemist
  :ensure t
  :config
  (add-hook 'elixir-mode-hook
	    (lambda()
	      (company-mode)
	      ))
  :init
  (setq
   alchemist-mix-env "prod"
   alchemist-hooks-compile-on-save nil))


;;
;; Javascript and HTML stuff
;;
(use-package web-mode
  :disabled t
  :ensure t
  :mode (("\\.html$" . web-mode)))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\(?:on\\)?\\'"
  :config
  (progn
    (setq js2-strict-missing-semi-warning nil)
    (setq-default js2-basic-offset 2)))

(use-package scss-mode
  :ensure t
  :config
  (add-hook 'scss-mode-hook
	    (lambda()
	      (flycheck-mode)
	      (setq css-indent-offset 2)
	      (company-mode)
	      (company-css)
	      )))

;;
;; Haskell stuff
;;
(use-package hindent :ensure t)
(use-package intero :ensure t)
(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook (lambda ()
                               (intero-mode)
                               (hindent-mode)
                               (setq hindent-reformat-buffer-on-save t))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-split-window-function (quote split-window-horizontally))
 '(package-selected-packages
   (quote
	(helm use-package smooth-scrolling projectile popup helm-core)))
 '(split-height-threshold 200)
 '(split-width-threshold 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
