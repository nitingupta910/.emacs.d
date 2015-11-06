(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; projectile doesn't work on welcome screen (projectile bug)
(setq inhibit-startup-screen t)

;; for smooth scrolling and disabling the automatic
;; recentering of emacs when moving the cursor
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(set-face-attribute 'region nil
                    :background "lightblue"
                    :foreground "black")

;; helm
(helm-mode 1)
(set-face-attribute 'helm-selection nil
		    :background "purple"
		    :foreground "black")
;;; helm: completion using <tab> in helm-find-file
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action)

;; projectile
(projectile-global-mode)

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
(add-hook 'c-mode-common-hook
           (lambda ()
             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
               (setq c-default-style "linux"))))

;;(setq c-default-style "linux")

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

;; golang
(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
			  (set (make-local-variable 'company-backends) '(company-go))
			  (company-mode)))
(add-hook 'go-mode-hook 'flycheck-mode)

(defun my-go-mode-hook ()
  ; Godef jump key binding                                                      
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; fix environment variables from launching emacs GUI
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "PATH"))
  
;; set font size
(set-face-attribute 'default nil :height 180)

;; remote path
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; set theme
;;(if (display-graphic-p)
;;    (load-theme 'monokai t))
;; neotree displays correctly on dark background terminal
;; with this theme
(load-theme 'monokai t)


;; turn-off emacs beeping.
;; Ref: http://www.emacswiki.org/emacs/AlarmBell
;;(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; file/dir tree sidebar
(global-set-key [f8] 'neotree-toggle)

;; make indentation commands use space only (never tab character)
(setq-default indent-tabs-mode nil) ; emacs 23.1, 24.2, default to t

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

;; configure backup file creation
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
