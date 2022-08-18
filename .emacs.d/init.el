;; -*- lexical-binding: t -*-

;; always load newest byte code
(setq load-prefer-newer t)

(setq inhibit-startup-message t)
(setq message-log-max t)

(setq default-directory "~/")
(setq command-line-default-directory "~/")

(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; revert buffers automatically when underlying files are changed
;; (global-auto-revert-mode t)

;; package config
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(package-initialize)

;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; default to UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package diminish
  :ensure t)

(use-package ido
  ;; :bind (:map ido-completion-map
  ;;             ("<up>" . ido-prev-match)
  ;;             ("<down>" . ido-next-match)
  ;;             ("<left>" . ido-delete-backward-updir)
  ;;             ("<right>" . ido-exit-minibuffer))
  :config (progn
            (ido-mode 1)
            (ido-everywhere 1)
            (setq ido-enable-flex-matching 1)))

(use-package ido-vertical-mode
  :ensure t
  :config (progn
            (ido-vertical-mode 1)
            (setq ido-vertical-indicator "↣"
                  ido-vertical-define-keys 'C-n-C-p-up-and-down)))

(use-package evil
  :ensure t
  :init (progn
          (setq evil-want-integration t
                evil-want-keybinding nil
                evil-move-cursor-back nil
                evil-undo-system 'undo-tree))
  :config (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init))

(use-package evil-commentary
  :ensure t
  :config (evil-commentary-mode 1))

(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode 1))

(use-package magit
  :ensure t
  :bind (("C-c s" . 'magit-status)
         ([remap quit-window] . magit-mode-bury-buffer))
  :diminish auto-revert-mode
  :config (progn
            (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1
                  magit-section-visibility-indicator nil
                  magit-process-finish-apply-ansi-colors t)))

(when (eq system-type 'darwin)
  ;; Lefthook outputs an extra carriage returns on Darwin.
  (defun me/magit--darwin-process-filter (orig-fun proc string)
    (funcall orig-fun proc (replace-regexp-in-string "\r\n" "\n" string)))
  (advice-add 'magit-process-filter :around #'me/magit--darwin-process-filter))

;; (use-package evil-magit
;;   :ensure t)

;; (use-package moody
;;   :ensure t
;;   :config
;;   (setq x-underline-at-descent-line t)
;;   (setq moody-slant-function #'moody-slant-apple-rgb)
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode 1)
  :bind (("C-x u" . 'undo)))

(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'")

(use-package prettier-js
  :ensure t
  :hook ((rjsx-mode . prettier-js-mode)
         (js2-mode . prettier-js-mode)))

(use-package add-node-modules-path
  :ensure t
  :hook ((js-mode . add-node-modules-path)
         (js2-mode . add-node-modules-path)))

;; (eval-after-load 'rjsx-mode
;;   '(add-hook 'js-mode-hook #'add-node-modules-path))

;; (eval-after-load 'js2-mode
;;   '(add-hook 'js2-mode-hook #'add-node-modules-path))

;; (setq-default tab-width 2)
(setq js-indent-level 2)

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

(setq-default indent-tabs-mode nil) ;; don't use tabs to indent
(setq-default tab-width 2)          ;; but maintain the correct appearance

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)

  ;; (setq web-mode-markup-indent-offset 2)
  ;; (setq web-mode-markup-indent-offset 2)
  ;; (setq web-mode-code-indent-offset 2)
  ;; (setq web-mode-css-indent-offset 2)
  ;; (setq web-mode-enable-auto-pairing t)
  ;; (setq web-mode-enable-auto-expanding t)
  ;; (setq web-mode-enable-css-colorization t)
  ;; (setq indent-tabs-mode nil)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;; Ruby config
(use-package rvm
  :ensure t)

(use-package ruby-mode
  :mode "\\.rb\\'"
  :hook (ruby-mode . rvm-activate-corresponding-ruby))

;; (defun rspec-switch-to-buffer ()
;;   (other-window (get-buffer-window "*rspec-compilation*"))
;; )

(use-package rspec-mode
  :ensure t
  :hook ((ruby-mode . rspec-mode)
         (dired-mode . rspec-dired-mode))
  :diminish rspec-mode
  :init (add-hook 'after-init-hook 'inf-ruby-switch-setup))
;; :init (add-hook 'rspec-before-verification-hook 'rspec-switch-to-buffer))

(use-package minitest
  :ensure t)

;; Flycheck
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :config (setq-default flycheck-disabled-checkers '(ruby-reek)))

(use-package nlinum-relative
  :ensure t
  :config (progn
            (nlinum-relative-setup-evil)
            (add-hook 'prog-mode-hook 'nlinum-relative-mode)
            (setq nlinum-relative-redisplay-delay 0
                  nlinum-relative-current-symbol " ~ "
                  nlinum-format "%3d ")))

;; UI
(use-package spaceline
  :ensure t)
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(setq powerline-image-apple-rgb t)
;; (set-face-attribute 'powerline-active1 :background "#282a36" :foreground "#ff79c6")
;; (set-face-attribute 'powerline-active2 :background "#282a36" :foreground "#ff79c6")
;; (set-face-attribute 'powerline-inactive1 :background "#373844" :foreground "#bd93f9")
;; (set-face-attribute 'powerline-inactive2 :background "#373844" :foreground "#bd93f9")

;; (use-package atom-one-dark-theme
;;   :ensure t
;;   :config (load-theme 'atom-one-dark t))

(use-package dracula-theme
  :ensure t
  :config (load-theme 'dracula t))

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Monaco" :height 140))

(use-package go-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(use-package robe
  :ensure t
  :bind (:map robe-mode-map
              ("C-c j" . robe-jump)
              ("C-c b" . pop-tag-mark))
  :hook (ruby-mode . robe-mode))

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

(use-package flyspell
  :ensure t)

(setq flyspell-issue-message-flg nil)
(add-hook 'enh-ruby-mode-hook
          (lambda () (flyspell-prog-mode)))

(add-hook 'web-mode-hook
          (lambda () (flyspell-prog-mode)))

(use-package restclient
  :ensure t)

(use-package evil-multiedit
  :ensure t
  :bind (:map evil-normal-state-map
              ("M-d" . evil-multiedit-match-and-next)))

;; (define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)

(use-package deadgrep
  :ensure t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(show-paren-mode t)

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))

(use-package rust-mode
  :ensure t)

;; Disable backup files
;;(setq make-backup-files nil
;;      vc-make-backup-files nil
;;      auto-save-default nil
;;      auto-save-list-file-prefix nil
;;      create-lockfiles nil)

;; TODO
;;   rspec-mode
;;   linum-relative
;;   robe (seeing is believing)
;;   js-indent-level
;;   evil-surround
