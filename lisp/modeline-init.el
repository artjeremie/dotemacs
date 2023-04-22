;;; modeline-init.el --- Modeline -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 artjeremie

;; Author: artjeremie <artjeremie@gmail.com>
;; URL: https://github.com/artjeremie

;;; Commentary:

;; This file contains `mode-line' configuration.

;;; Code:

(defvar hidden-minor-modes
  '(abbrev-mode
    eldoc-mode
    rainbow-mode
    yas-minor-mode))

(defun art-purge-minor-modes ()
  "Hide some minor modes in `mode-line'."
  (dolist (x hidden-minor-modes nil)
    (let ((trg (cdr (assoc x minor-mode-alist))))
      (when trg (setcar trg "")))))

(add-hook 'after-change-major-mode-hook 'art-purge-minor-modes)

(defun art-modeline-padding ()
  "Padding for `mode-line'."
  (let ((r-length
         (length (format-mode-line mode-line-end-spaces))))
    (propertize " "
                'display `(space :align-to (- right ,r-length)))))

(setq mode-line-end-spaces
      '(""
        mode-line-modes
        (vc-mode vc-mode)
        "    "))

(setq-default mode-line-format
              '("%e "
                " [%*] "
                "%m: "
                "%I "
                "%b "
                "%l:%c "
                (:eval (art-modeline-padding))
                mode-line-end-spaces))

;;; modeline-init.el ends here