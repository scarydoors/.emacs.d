;;; init.el --- My personal initialization file for GNU Emacs
;;; -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2022  Alan Strauhs

;; Author: Alan Strauhs <scarydoorsyeah@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file sets up Emacs with better default settings and external packages
;; for programming and other uses. 

;;; Code:

;;; straight.el setup

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)
(setq use-package-always-ensure t)

;;; sane defaults for Emacs
(use-package emacs
  :ensure nil
  :init
  (setq inhibit-startup-screen t
        initial-scratch-message nil
        ring-bell-function 'ignore
        sentence-end-double-space nil
        use-dialog-box nil
        use-short-answers t)
  
  ;; split vertically by default 
  (setq split-width-threshold 160
        split-height-threshold nil)

  (setq user-full-name "Alan Strauhs"
        user-mail-address "scarydoorsyeah@gmail.com")

  ;; default to utf-8
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  (delete-selection-mode +1)

  (recentf-mode +1)

  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  (setq make-backup-files nil
        auto-save-default nil
        create-lockfiles nil)

  (setq vc-follow-symlinks t)

  (winner-mode +1)

  (setq-default indent-tabs-mode nil)
  (setq-default truncate-lines 1)
  
  (setq-default fill-column 80)
  (global-display-fill-column-indicator-mode +1)
  
  ;; better scrolling
  (setq scroll-preserve-screen-position 1
        scroll-step 1
        scroll-margin 4
        scroll-conservatively 10000)

  ;; setup line numbers
  (global-display-line-numbers-mode +1)
  (setq-default display-line-numbers-width 4)

  (column-number-mode +1))

;; garbage collector hack
(use-package gcmh
  :demand t
  :config
  (gcmh-mode +1))

;;; user interface

(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-sourcerer t)
  (doom-themes-org-config))

(set-face-attribute 'default nil
                    :family "Hack"
                    :height 120)
(set-face-attribute 'variable-pitch nil
                    :family "IBM Plex Sans"
                    :weight 'normal
                    :height 120)
(set-face-attribute 'mode-line nil :inherit 'variable-pitch)
(set-face-attribute 'mode-line-inactive nil :inherit 'variable-pitch)

(use-package which-key
  :defer 0.1
  :config
  (which-key-mode +1))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 40))

(use-package all-the-icons
  :demand
  :if (display-graphic-p))

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

;;; editing utilities

(use-package expand-region
  :defer 0.1
  :bind ("C-=" . er/expand-region))

(use-package smartparens
  :defer 0.1
  :bind ("M-k" . sp-forward-sexp)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode +1))

;;; lsp

(use-package lsp-mode
  :init
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-keymap-prefix "C-c l"
        lsp-idle-delay 0.1)

  (setq-default lsp-lens-enable nil)
  :hook (((c++-mode c-mode rust-mode js-mode web-mode css-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred)

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1)
  (defadvice company-in-string-or-comment (around company-in-string-or-comment-hack activate)
    (if (memq major-mode '(php-mode html-mode nxml-mode))
        (setq ad-return-value nil)
      ad-do-it)))

(use-package yasnippet
  :defer 0.1
  :config
  (yas-global-mode +1))

;;; selectrum completion engine + extras

(use-package selectrum
  :demand t
  :config
  (selectrum-mode +1))

(use-package prescient
  :demand t
  :config (prescient-persist-mode +1))

(use-package selectrum-prescient
  :init (selectrum-prescient-mode +1)
  :after selectrum)

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                  ;; orig. next-matching-history-element
         ("M-r" . consult-history)))                ;; orig. previous-matching-history-element

(use-package marginalia
  :demand t
  :config
  (marginalia-mode +1)
  :bind (:map minibuffer-local-completion-map
              ("M-A" . marginalia-cycle)
              ("C-i" . marginalia-cycle-annotators)))


;;; rust
(use-package rustic)

;;; meson

(use-package meson-mode)

;;; javascript
(use-package rjsx-mode
  :mode ("\\.js\\'"
         "\\.jsx\\'")
  :config
  (setq js2-mode-show-parse-errors nil
        js2-mode-show0strict-warnings nil
        js2-basic-offset 2
        js-indent-level 2))

(use-package add-node-modules-path
  :hook (((js2-mode rjsx-mode) . add-node-modules-path)))

;;; web
(use-package web-mode
  :mode
  (("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)      
  ("\\.jsp\\'" . web-mode)            
  ("\\.as[cp]x\\'" . web-mode)        
  ("\\.erb\\'" . web-mode)            
  ("\\.mustache\\'" . web-mode)       
  ("\\.djhtml\\'" . web-mode)         
  ("\\.jst.ejs\\'" . web-mode)        
  ("\\.html?\\'" . web-mode))

  :init
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-comment-keywords t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)   
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-comment-style 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq css-indent-level 2)
  (setq css-indent-offset 2))


;;; init.el ends here