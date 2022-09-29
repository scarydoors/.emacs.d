;;; early-init.el --- Early Initialization for GNU Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Alan Strauhs

;; Author: Alan Strauhs(setq package-enable-at-startup nil) <sundog@doors>
;; Keywords: 

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

;; This file runs before init.el.

;;; Code:

(setq gc-cons-threshold most-positive-fixnum)
(let ((old-file-name-handler-alist file-name-handler-alist))
  (setq-default file-name-handler-alist nil)
  (defun sd-reset-file-handler-alist ()
    (setq file-name-handler-alist
          (delete-dups (append file-name-handler-alist
                               old-file-name-handler-alist))))
  (add-hook 'emacs-startup-hook #'sd-reset-file-handler-alist 101))

(setq-default inhibit-redisplay t
              inhibit-message t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil))
          (redisplay))

(setq native-comp-async-report-warnings-errors 'silent)

;;; early-init.el ends here
