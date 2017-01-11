;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;;
;; BEGIN Generic settings
;;

;; show vertical bar for cursor
(setq-default cursor-type 'bar)

;; projectile doesn't work on welcome screen (projectile bug)
(setq inhibit-startup-screen t)

(menu-bar-mode -1)

;; See: https://www.quora.com/profile/Stefan-Bucur/Posts/Enabling-Soft-Word-Wrap-in-Emacs
(global-visual-line-mode 1)

(show-paren-mode 1)
(setq show-paren-delay 0)

(setq password-cache-expiry nil)

;; for smooth scrolling and disabling the automatic
;; recentering of emacs when moving the cursor
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; press any key to overwrite selected text
(delete-selection-mode 1)

;; Show line numbers
(global-linum-mode 1)

; disable autosave
(setq auto-save-default nil)

;; ask before exiting emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)

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

;;
;; END Generic settings
;;

;; for moving buffers (buffer-move package)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; set font size
;; Color definitions: https://github.com/morhetz/gruvbox
;(setq gruvbox-fg0 "#fbf1c7")
;(setq gruvbox-fg1 "#ebdbb2")
;(setq gruvbox-fg2 "#d5c4a1")
;(set-face-attribute 'default nil :height 180 :foreground gruvbox-fg1)

(defun ng-get-ppi ()
  "Get display PPI. Do not run this function in non-graphic mode."
  (setq ng-disp-attrs (car (display-monitor-attributes-list)))

  (setq ng-mm-size (assq 'mm-size (symbol-value 'ng-disp-attrs)))
  (setq ng-mm-width (nth 1 (symbol-value 'ng-mm-size)))
  (setq ng-mm-height (nth 2 (symbol-value 'ng-mm-size)))

  (setq ng-diag-mm (sqrt (+
                        (expt (symbol-value 'ng-mm-width) 2)
                        (expt (symbol-value 'ng-mm-height) 2))))

  (setq ng-diag-inches (* (symbol-value 'ng-diag-mm) 0.0393701))

  (setq ng-geom (assq 'geometry (symbol-value 'ng-disp-attrs)))
  (setq ng-pixel-width (nth 3 (symbol-value 'ng-geom)))
  (setq ng-pixel-height (nth 4 (symbol-value 'ng-geom)))

  (setq ng-diag-pixels (sqrt (+
                        (expt (symbol-value 'ng-pixel-width) 2)
                        (expt (symbol-value 'ng-pixel-height) 2))))

  (/ (symbol-value 'ng-diag-pixels)
     (symbol-value 'ng-diag-inches))
  )

(if (display-graphic-p)
    (funcall
     (lambda()
       (setq ng-ppi (ng-get-ppi))

       (if (< (symbol-value 'ng-ppi) 108)
           (setq ng-font-height 140)
         (if (< (symbol-value 'ng-ppi) 128)
             (setq ng-font-height 220)
           (setq ng-font-height 240)))

       (if (eq system-type 'darwin)
           (setq ng-font-face "Menlo")
         (setq ng-font-face "Monospace"))

       (set-face-attribute 'default nil
                           :height (symbol-value 'ng-font-height)
                           :font (symbol-value 'ng-font-face))
       )))

;(set-face-attribute 'region nil
;                    :background "lightblue"
;                    :foreground "black")

;; Hide GUI elements
(when window-system
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  (menu-bar-mode -1)
  )

;; helm
(helm-mode 1)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

;(set-face-attribute 'helm-selection nil
;		    :background "purple"
;		    :foreground "black")

;;; helm: completion using <tab> in helm-find-file
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action)

;; projectile
;;
;; HACK: projectile causes slowdown when editing files over sshfs
;; See: https://github.com/bbatsov/projectile/issues/657
(projectile-global-mode)
(setq projectile-mode-line "foo")
;; In large projects, caching can significantly speedup file and
;; directory listings, making it display instantly.
;; See: http://tuhdo.github.io/helm-projectile.html
(setq projectile-enable-caching t)

;; helm-dash
(setq helm-dash-browser-func 'eww)

;; helm-gtags (does not seems to work correct over tramp)
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

;; without this explicit require, I get this error:
;; Symbol's value as variable is void: helm-gtags-mode-map
;; during init.el loading during emacs startup
(require 'helm-gtags)

;(set-face-attribute 'helm-selection nil
;                    :background "yellow"
;                    :foreground "black")

;; start-face is all gets displayed in terminal mode
;; for ref. the other face is: pulse-higlight-face
;; To get list of all face available:
;;    M-x helm-colors RET
(set-face-attribute 'pulse-highlight-start-face nil
                    :background "yellow"
                    :foreground "black")

;; Treat .h files as C++ files unless overridden by dir specific config
;; To use c-mode for .h files for a particular project (dir), create
;; .dir-locals.el in that dir with this line:
;;   ((c++-mode . ((mode . c))))
;; (Source: http://stackoverflow.com/questions/3312114/how-to-tell-emacs-to-open-h-file-in-c-mode)
;; (See second answer)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;;(define-key helm-gtags-mode-map (kbd "f6") 'helm-gtags-find-files)
(global-set-key [f6] 'helm-gtags-find-files)


;; helm-etags (etags is much slower than global/gtags)
;;(global-set-key (kbd "M-.") 'helm-etags-select)
;;(global-set-key (kbd "M-,") 'pop-tag-mark)

;; ggtags (ggtags doesn't have good candidate selection method c.f. helm-gtags)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;               (ggtags-mode 1))))


;; indentation (linux kernel style)
(setq c-default-style "linux")
(setq c-backspace-function 'backward-delete-char)
(add-hook 'c-mode-common-hook '(lambda ()
                                 (setq-default indent-tabs-mode t)))

;; helm-git-grep
(global-set-key (kbd "C-c g") 'helm-git-grep)
;; Invoke `helm-git-grep' from isearch.
(define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
;; Invoke `helm-git-grep' from other helm.
(eval-after-load 'helm
  '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))

;; company mode
;; helm-gtags-find-files generates incorrect _local_ filename when
;; working on a remote project with tramp. This results in company
;; modes like company-clang to fail initiailization which hangs
;; the emacs. So, don't enable globally on init.
;;
;;(add-hook 'after-init-hook 'global-company-mode)

;;----------------------
;; golang
(add-hook 'before-save-hook 'gofmt-before-save)

;(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
			  (set (make-local-variable 'company-backends) '(company-go))
			  (company-mode)))
(add-hook 'go-mode-hook 'flycheck-mode)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-gometalinter-setup))

(defun my-go-mode-hook ()
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark))

(add-hook 'go-mode-hook 'my-go-mode-hook)

;; fix environment variables from launching emacs GUI
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "PATH"))

;; ==================
;; Haskell
;; ==================
;(setq hindent-style "gibiansky")
;(add-hook 'haskell-mode-hook (lambda ()
;                               (set (make-local-variable 'company-backends) ;'(company-ghc))
;                               (interactive-haskell-mode)
;                               (hindent-mode)
;                               (company-mode)
;                               (flycheck-mode)))

(add-hook 'haskell-mode-hook (lambda ()
                               (intero-mode)
                               (hindent-mode)))

;; SCSS
(add-hook 'scss-mode-hook
          (lambda()
            (flycheck-mode)
            (setq css-indent-offset 2)
            (company-mode)
            (company-css)
            ))

;; remote path
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; set theme
;;(if (display-graphic-p)
;;    (load-theme 'monokai t))
;; neotree displays correctly on dark background terminal
;; with this theme
(load-theme 'monokai t)
;(load-theme 'darktooth t)
;(load-theme 'adwaita t)
;(load-theme 'deeper-blue t)
;(load-theme 'wombat t)
;(load-theme 'dichromacy t)
;(load-theme 'gruvbox t)

;; show column number
(column-number-mode t)

;; turn-off emacs beeping.
;; Ref: http://www.emacswiki.org/emacs/AlarmBell
;;(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; file/dir tree sidebar
(global-set-key [f8] 'neotree-toggle)

;; make indentation commands use space only (never tab character)
(setq-default indent-tabs-mode nil) ; emacs 23.1, 24.2, default to t

;; configure backup file creation
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; multiple cursors (C-S-d isn't working))
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-d") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-d") 'mc/mark-all-like-this)

;; defining my own minor mode for key overrides
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-key my-keys-minor-mode-map (kbd "C-d") 'mc/mark-next-like-this)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

(defun indent-buffer ()
  "Indent current buffer according to major mode."
  (interactive)
  (indent-region (point-min) (point-max)))

;; js2-mode
;;(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
;;(add-to-list 'auto-mode-alist '("\\.tsx\\'" . js2-mode))

;; typescript
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

;; typescript: tslint flycheck integration
(require 'flycheck)
(flycheck-define-checker tslint
  "Use tslint to flycheck TypeScript code."
  :command ("tslint"
            "-f" source
            "-c" (eval (projectile-expand-root "tslint.json"))
            "-t" "prose")
  :error-patterns ((warning (file-name) "[" line ", " column "]: " (message)))
  :modes typescript-mode)
(add-to-list 'flycheck-checkers 'tslint)


;; ============
;; Protobuf
;; ============
(add-hook 'protobuf-mode-hook (lambda()
                                (flycheck-mode)))

;; ============
;; C++
;; ============
;;
;; style I want to use in c++ mode
;; (source: http://www.emacswiki.org/emacs/CPlusPlusMode)
(c-add-style "my-style"
             '("stroustrup"
               (indent-tabs-mode . nil)        ; use spaces rather than tabs
               (c-basic-offset . 2)            ; indent by four spaces
               (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                                   (brace-list-open . 0)
                                   (statement-case-open . +)))))

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(require 'flycheck-irony)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

(defun my-company-irony-setup ()
  (setq company-backends (delete 'company-semantic company-backends))
  (eval-after-load 'company
    '(add-to-list
      'company-backends 'company-irony)))

(setq irony-additional-clang-options '("-std=c++14"))

(defun my-c++-mode-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (irony-mode)
  (flycheck-mode)
  (company-mode)
  (local-set-key (kbd "C-c f") #'clang-format-buffer)
  (my-company-irony-setup)
  (c-toggle-hungry-state 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; =============
;; CMake
;; =============
(cmake-ide-setup)

;; =============
;; Elm
;; =============
;;
(require 'elm-mode)

(setq elm-tags-on-save t)
(add-hook 'flycheck-mode-hook 'flycheck-elm-setup)
(add-hook 'elm-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '(company-elm))
            (local-set-key (kbd "M-.") 'elm-mode-goto-tag-at-point)
            (local-set-key (kbd "M-,") 'pop-tag-mark)
            (company-mode)
            (flycheck-mode)))

;;(with-eval-after-load 'company
;;  (add-to-list 'company-backends 'company-elm))
(add-hook 'elm-mode-hook #'elm-oracle-setup-completion)

(setq elm-format-on-save t)

;;-----------------
;; Rust
(require 'rust-mode)

(add-hook 'rust-mode-hook
          (lambda ()
            (racer-mode)
            (cargo-minor-mode)
            (flycheck-mode)
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; Org mode
(setq org-support-shift-select 1)

;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (variable-pitch-mode t)
;;             (custom-set-faces
;;              '(default ((t (:inherit nil :stipple nil :background "white smoke" :foreground "dim gray" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 143 :width normal :foundry "DAMA" :family "Ubuntu Mono"))))
;;              )))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("3fd0fda6c3842e59f3a307d01f105cce74e1981c6670bb17588557b4cebfe1a7" default)))
 '(ediff-merge-split-window-function (quote split-window-horizontally))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(font-use-system-font t)
 '(org-babel-load-languages
   (quote
    ((sh . t)
     (python . t)
     (js . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (awk . t)
     (C . t))))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

 ;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;'(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 160 :width normal))))
 ;'(ediff-fine-diff-A ((t (:background "brightblack"))))
 ;'(ediff-fine-diff-B ((t (:background "color-24"))))
 ;'(org-level-1 ((t (:inherit variable-pitch :foreground "tan" :height 1.0))))
 ;'(org-level-2 ((t (:inherit variable-pitch :foreground "#A6E22E" :height 1.0)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;'(ediff-current-diff-C ((t (:background "dark slate gray"))))
 ;'(ediff-even-diff-C ((t (:background "DeepSkyBlue4"))))
 ;'(ediff-odd-diff-A ((t (:background "dark olive green"))))
 ;'(ediff-odd-diff-B ((t (:background "dark olive green"))))
 ;'(ediff-odd-diff-C ((t (:background "dark olive green"))))
 )
