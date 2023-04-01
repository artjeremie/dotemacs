;;; package-init-modules.el --- Packages setup -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 hoaxdream

;; Author: hoaxdream <hoaxdream@gmail.com>
;; URL: https://github.com/hoaxdream

;;; Commentary:

;; This file contains initial package setup and function to tangle
;; the Org literate configuration file.

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
                rg
                toc-org
                vertico
                yasnippet))

(defun tlj-rebuild-pkg ()
  "Refresh packages contents then install or uninstall missing packages."
  (interactive)
  (package-refresh-contents)
  (package-autoremove)
  (package-install-selected-packages 'noconfirm))

(defconst tlj-source-path
  (locate-user-emacs-file "README.org")
  "Path to the Org literate configuration file.")

(defconst tlj-target-path
  (locate-user-emacs-file "userconfig.el")
  "Path to the Elisp version of Org literate configuration.")

(autoload 'org-babel-tangle-file "ob-tangle" nil t)
(defun tlj-tangle-and-compile ()
  "Tangle Org literate configuration file."
  (interactive)
  (when (file-newer-than-file-p tlj-source-path tlj-target-path)
    (require 'org)
    (require 'ob)
    (org-babel-tangle-file tlj-source-path
                           tlj-target-path
                           (rx string-start
                               (or "emacs-lisp" "elisp")
                               string-end))
    (byte-compile-file tlj-target-path)))

(add-hook 'kill-emacs-hook 'tlj-tangle-and-compile)

(defun tlj-find-org-config ()
  "Open Org literate configuration file."
  (interactive)
  (find-file tlj-source-path))

(global-set-key (kbd "C-c u") 'tlj-rebuild-pkg)
(global-set-key (kbd "C-c f") 'tlj-find-org-config)

;;; package-init-modules.el ends here