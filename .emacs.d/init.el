;; -*- lexical-binding: t -*-

(setq inhibit-startup-message t
      message-log-max t
      load-prefer-newer t)

(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

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

;; TODO: use-package's map binding doesn't work
(defun set-ido-keys ()
  (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
  (define-key ido-completion-map (kbd "<left>") 'ido-delete-backward-updir)
  (define-key ido-completion-map (kbd "<right>") 'ido-exit-minibuffer)
  )
(add-hook 'ido-setup-hook #'set-ido-keys)

(use-package ido
  :init (progn (ido-mode 1)))

(use-package ido-vertical-mode
  :ensure t
  :init (progn
	  (ido-vertical-mode 1)
	  (setq ido-vertical-indicator "↣")))

(use-package evil
  :ensure t
  :init (progn
	  (evil-mode 1))
  :config (progn
	    (setq evil-move-cursor-back nil)))

(use-package evil-commentary
  :ensure t
  :init (progn
	  (evil-commentary-mode 1)))

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

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-mini)))

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

;; web-mode
(use-package web-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;; Ruby config
(use-package rvm
  :ensure t)

(use-package ruby-mode
  :mode "\\.rb\\'"
  :hook (ruby-mode . rvm-activate-corresponding-ruby))

;; Flycheck
(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :config (setq-default flycheck-disabled-checkers '(ruby-reek)))

;; ;; rvm.el activates the right Ruby version
;; (add-hook 'ruby-mode-hook
;;           (lambda () (rvm-activate-corresponding-ruby)))

;; (use-package atom-one-dark-theme
;;   :ensure t
;;   :config (load-theme 'atom-one-dark t))

(use-package dracula-theme
  :ensure t
  :config (load-theme 'dracula t))

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Monaco")
  (set-face-attribute 'default nil :height 140))

;; TODO
;;   rspec-mode
;;   linum-relative
;;   robe (seeing is believing)
;;   js-indent-level
;;   evil-surround
