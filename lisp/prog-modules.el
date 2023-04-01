;;; prog-modules.el --- Prog-mode -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 hoaxdream

;; Author: hoaxdream <hoaxdream@gmail.com>
;; URL: https://github.com/hoaxdream

;;; Commentary:

;; This file contains `prog-mode' defaults.

;;; Code:

(defun tlj-prog-mode ()
  "Default modes for `prog-mode'."
  (display-line-numbers-mode)
  (flymake-mode))

(add-hook 'prog-mode-hook 'tlj-prog-mode)

(defun tlj-elisp-flymake-byte-compile (old-function &rest arguments)
  "Change `load-path' OLD-FUNCTION ARGUMENTS."
  (let ((elisp-flymake-byte-compile-load-path
         (append elisp-flymake-byte-compile-load-path load-path)))
    (apply old-function arguments)))

(advice-add 'elisp-flymake-byte-compile :around 'tlj-elisp-flymake-byte-compile)

(with-eval-after-load 'flymake
  (when (boundp 'flymake-mode-map)
    (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
    (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)))

;;; prog-modules.el ends here