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

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
