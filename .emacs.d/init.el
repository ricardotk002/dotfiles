;; -*- lexical-binding: t -*-

(setq inhibit-startup-message t
      message-log-max t
      load-prefer-newer t)

(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; package config
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(package-initialize)

;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(require 'use-package)

;; yes/no to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; default to UTF-8
(prefer-coding-system 'utf-8)

(use-package diminish
  :ensure t)

(use-package ido
  :init (ido-mode 1))

(use-package ido-vertical-mode
  :ensure t
  :init (progn
          (ido-vertical-mode 1)
          (setq ido-vertical-indicator "↣")))

(use-package evil
  :ensure t
  :init (progn
          (evil-mode 1)))

(use-package magit
  :ensure t
  :bind (("C-c s" . 'magit-status))
  :diminish auto-revert-mode)

(use-package evil-magit
  :ensure t)

(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode 1)
  :bind (("C-x u" . 'undo)))

(use-package rjsx-mode
  :ensure t
  :mode "\\.jsx\\'")

(use-package atom-one-dark-theme
  :ensure t
  :config (load-theme 'atom-one-dark t))

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Monaco")
  (set-face-attribute 'default nil :height 140))

;; ;; Added by Package.el.  This must come before configurations of
;; ;; installed packages.  Don't delete this line.  If you don't want it,
;; ;; just comment it out by adding a semicolon to the start of the line.
;; ;; You may delete these explanatory comments.
;; (package-initialize)

;; (require 'exec-path-from-shell)
;; (exec-path-from-shell-initialize)

;; (add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
;; (require 'init-better-defaults)
;; (require 'init-theme)
;; (require 'init-font)
;; (require 'init-packages)
;; (require 'init-evil)
;; (require 'init-magit)
;; (require 'init-web-mode)
;; (require 'init-helm)
;; (require 'init-ido)

;; (require 'flycheck)
;; (add-hook 'prog-mode 'flycheck-mode)
;; (global-flycheck-mode)
;; (setq-default flycheck-disabled-checkers '(ruby-reek))

;; (require 'rspec-mode)
;; (eval-after-load 'rspec-mode
;;   '(rspec-install-snippets))

;; (require 'evil-surround)

;; ;; rvm.el activates the right Ruby version
;; (add-hook 'ruby-mode-hook
;;           (lambda () (rvm-activate-corresponding-ruby)))

;; (require 'powerline)
;; (powerline-center-theme)

;; (display-time-mode)

;; (require 'diminish)
;; (diminish 'jiggle-mode)

;; (require 'ox-reveal)

;; ;; start magit in a full-screen buffer
;; ;; (setq magit-display-buffer-function 'switch-to-buffer)

;; (setq inhibit-splash-screen t)
;; (setq package-list '(better-defaults
;;                      web-mode
;;                      neotree
;;                      evil
;;                      evil-magit
;;                      atom-one-dark-theme
;;                      helm
;;                      helm-projectile
;;                      helm-ag
;;                      ruby-electric
;;                      seeing-is-believing
;;                      rvm
;;                      robe
;;                      evil-commentary
;;                      ido-vertical-mode
;;                      linum-relative
;;                      column-marker))
;; (evil-commentary-mode)

;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))

;; (setq js-indent-level 2)

;; (global-set-key (kbd "M-x") #'helm-M-x)
;; ;; TODO: helm-ag isn't working with system's ag
;; (global-set-key (kbd "s-f") #'helm-projectile-ag)
;; (global-set-key (kbd "s-t") #'helm-projectile-find-file-dwim)

;; (global-set-key (kbd "C-c s") 'magit-status)

;; (global-set-key (kbd "C-c j") 'robe-jump)
;; (global-set-key (kbd "C-c b") 'pop-tag-mark)

;; (defun set-ido-keys ()
;;   (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
;;   (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
;;   (define-key ido-completion-map (kbd "<left>") 'ido-delete-backward-updir)
;;   (define-key ido-completion-map (kbd "<right>") 'ido-exit-minibuffer)
;;   )

;; (add-hook 'ido-setup-hook #'set-ido-keys)

;; ;; (add-hook 'abg-code-modes-hook
;; ;;           (lambda () (linum-mode 1)))

;; ;; (add-hook 'ruby-mode-hook
;; ;;           (lambda () (run-hooks 'abg-code-modes-hook)))

;; ;; Line and column numbers
;; ;; (linum-on)
;; (setq column-number-mode t)
;; (require 'linum-relative)
;; (global-linum-mode)
;; (with-eval-after-load 'linum
;;   (linum-relative-toggle))
;; ;; (linum-relative-mode)

;; ;; Move lines up and down
;; (defun move-line-up ()
;;   "Move up to the current line."
;;   (interactive)
;;   (transpose-lines 1)
;;   (forward-line -2)
;;   (indent-according-to-mode))

;; (defun move-line-down ()
;;   "Move up to the current line."
;;   (interactive)
;;   (forward-line 1)
;;   (transpose-lines 1)
;;   (forward-line -1)
;;   (indent-according-to-mode))

;; (global-set-key [(meta shift up)] 'move-line-up)
;; (global-set-key [(meta shift down)] 'move-line-down)

;; (add-hook 'ruby-mode-hook 'robe-mode)
;; (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
;;   (rvm-activate-corresponding-ruby))

;; (rvm-use-default)
;; ;; (setq seeing-is-believing-prefix "C-0")
;; ;; (add-hook 'ruby-mode-hook 'seeing-is-believing)
;; ;; (require 'seeing-is-believing)

;; (setq custom-file "~/.emacs.d/custom.el")
;; (load custom-file)
