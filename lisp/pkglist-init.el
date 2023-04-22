;;; pkglist-init.el --- Package lists -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 artjeremie

;; Author: artjeremie <artjeremie@gmail.com>
;; URL: https://github.com/artjeremie

;;; Commentary:

;; This file contains initial package setup and function to tangle
;; Org literate configuration file.

;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(setq package-archive-column-width 12)
(setq package-version-column-width 28)

(setq-default package-selected-packages
              '(cape
                consult
                corfu
                diff-hl
                dired-narrow
                dired-sidebar
                dired-subtree
                elfeed
                emojify
                flyspell-correct
                helpful
                lua-mode
                magit
                marginalia
                markdown-mode
                no-littering
                olivetti
                orderless
                org-appear
                powershell
                rainbow-mode
                rg
                toc-org
                vertico
                yasnippet))

(defun art-package-update ()
  "Update package content and install or uninstall packages."
  (interactive)
  (package-refresh-contents)
  (package-autoremove)
  (package-install-selected-packages 'noconfirm))

(defconst art-source-path
  (locate-user-emacs-file "README.org")
  "Path to Org literate configuration file.")

(defconst art-target-path
  (locate-user-emacs-file "artconfig.el")
  "Path to Elisp version of Org literate configuration file.")

(autoload 'org-babel-tangle-file "ob-tangle")

(defun art-tangle-and-compile ()
  "Tangle Org literate configuration file."
  (interactive)
  (when (file-newer-than-file-p art-source-path art-target-path)
    (org-babel-tangle-file art-source-path
                           art-target-path)))

(add-hook 'kill-emacs-hook 'art-tangle-and-compile)

(defun art-find-org-configuration ()
  "Open Org literate configuration file."
  (interactive)
  (find-file art-source-path))

(global-set-key (kbd "C-c u") 'art-package-update)
(global-set-key (kbd "C-c f") 'art-find-org-configuration)

;;; pkglist-init.el ends here