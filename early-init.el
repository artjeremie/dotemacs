;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 artjeremie

;; Author: artjeremie <artjeremie@gmail.com>
;; URL: https://github.com/artjeremie

;;; Commentary:

;; This file is NOT part of GNU Emacs. Since Emacs 27, an early configuration
;; file early-init.el can be provided to handle initialization to be done
;; before init.el is loaded.

;;; Code:

(tool-bar-mode -1)
(scroll-bar-mode -1)

(dolist (var '(default-frame-alist initial-frame-alist))
  (add-to-list var '(width . (text-pixels . 1280)))
  (add-to-list var '(height . (text-pixels . 900)))
  (add-to-list var '(font . "Iosevka-14")))

(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)

(setq inhibit-splash-screen t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-buffer-menu t)
(setq frame-title-format '("%b"))

(setq-default load-prefer-newer t)
(setq-default package-check-signature nil)
(setq-default native-comp-async-report-warnings-errors 'silent)

(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))

(load "uistyle-init")
(load "vivendi-init")

;;; early-init.el ends here
