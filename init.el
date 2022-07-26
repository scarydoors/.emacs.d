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
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :init
  (setq inhibit-startup-screen t
        initial-scratch-message nil
        ring-bell-function 'ignore
        sentence-end-double-space nil
        use-dialog-box nil
        use-short-answers t)

  (setq load-prefer-newer t)
  
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
        doom-themes-enable-italic t
        doom-themes-treemacs-theme "doom-colors")
  (load-theme 'doom-sourcerer t)
  (doom-themes-org-config)
  (doom-themes-treemacs-config))

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

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'bitmap))

(use-package dashboard
  :demand t
  :config
  (setq dashboard-center-content t
        dashboard-banner-logo-title "bowow")
  (dashboard-setup-startup-hook))

(use-package treemacs
  :bind (:map global-map
              ("M-0" . treemacs-select-window)
              ("C-x t 1" . treemacs-delete-other-windows)
              ("C-x t t" . treemacs)
              ("C-x t d" . treemacs-select-directory)
              ("C-x t B" . treemacs-bookmark)
              ("C-x t C-t" . treemacs-find-file)
              ("C-x t M-t" . treemacs-find-tag))
  :config
  (treemacs-follow-mode +1)
  (treemacs-filewatch-mode +1)
  (treemacs-fringe-indicator-mode 'always))

(use-package dirvish
  :defer 0.1
  :config
  (dirvish-override-dired-mode +1))

;;; editing utilities

(use-package expand-region
  :defer 0.1
  :bind ("C-=" . er/expand-region))

(use-package smartparens
  :defer 0.1
  :bind ("M-n" . sp-forward-sexp)
  :config
  (require 'smartparens-config)

  (defun indent-between-pair (&rest _ignored)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET")))
  
  (smartparens-global-mode +1))

(use-package avy
  :config
  (avy-setup-default)
  (global-set-key (kbd "C-c C-j") 'avy-resume)
  :bind
  (("M-g w" . avy-goto-word-1)
   ("M-g g" . avy-goto-line)
   ("C-'" . avy-goto-char-2)
   ("C-c C-'" . avy-pop-mark)))

(use-package ace-window
  :bind ("M-o" . ace-window))

;;; utilities

(use-package magit
  :defer 0.1)

(use-package hydra
  :defer 0.1)

;;; lsp

(use-package lsp-mode
  :init
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-keymap-prefix "C-c l"
        lsp-idle-delay 0.1)
  (setq-default lsp-lens-enable nil)
  (setq lsp-completion-provider :none)
  :hook (((c++-mode c-mode rust-mode js-mode web-mode css-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (unbind-key "M-n" lsp-signature-mode-map)
  :commands lsp lsp-deferred)

(use-package corfu
  :demand t
  :init
  (setq corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 1
        corfu-quit-no-match 'separator)
  (setq tab-always-indent 'complete)
  :config
  (global-corfu-mode))

(use-package cape
  :demand t
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package corfu-doc
  :hook (corfu-mode . corfu-doc-mode)
  :bind
  (
   :map corfu-map
        ("M-p" . corfu-doc-scroll-down)
        ("M-n" . corfu-doc-scroll-up)
        ("M-d" . corfu-doc-toggle)
   ))

(use-package kind-icon
  :demand t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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
        js2-mode-show-strict-warnings nil
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

;;; c/c++
(use-package cc-mode
  :ensure nil
  :defer t
  :config
  (setq c-basic-offset 4
        c-backspace-function #'delete-backward-char)

  ;; TODO: Refine this style
  (c-add-style
   "scary" '((c-comment-only-line-offset . 0)
             (c-hanging-braces-alist (brace-list-open)
                                     (brace-entry-open)
                                     (substatement-open after)
                                     (block-close . c-snug-do-while)
                                     (namespace-open after)
                                     (extern-lang-open after))
             (c-cleanup-list brace-else-brace
                             brace-catch-brace)
             (c-offsets-alist
              (knr-argdecl-intro . 0)
              (defun-close . 0)
              (defun-open . 0)
              (innamespace . 0)
              (substatement-open . 0)
              (substatement-label . 0)
              (statement-cont . +)
              (case-label . +)
              (brace-list-entry . 0)
              (brace-list-intro . +)
              (brace-list-close . 0)
              (arglist-intro . +)
              (arglist-close . 0)
              (inline-open . 0)
              (inlambda . 0)
              (access-label . -)
              (inclass . +)
              (label . 0))))

    (when (listp c-default-style)
    (setf (alist-get 'other c-default-style) "scary")))

;;; init.el ends here
