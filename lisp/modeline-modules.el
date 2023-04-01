;;; modeline-modules.el --- Mode-line -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 hoaxdream

;; Author: hoaxdream <hoaxdream@gmail.com>
;; URL: https://github.com/hoaxdream

;;; Commentary:

;; This file contains `mode-line' configuration.

;;; Code:

;; (setq-default header-line-format
;;               (list
;;                `(vc-mode vc-mode)
;;                mode-line-modes))

(defvar hidden-minor-modes
  '(abbrev-mode
    eldoc-mode
    yas-minor-mode))

(defun tlj-purge-minor-modes ()
  "Hide some minor modes in modeline."
  ;; (interactive)
  (dolist (x hidden-minor-modes nil)
    (let ((trg (cdr (assoc x minor-mode-alist))))
      (when trg
        (setcar trg "")))))

(add-hook 'after-change-major-mode-hook 'tlj-purge-minor-modes)

(defun tlj-modeline-padding ()
  "Padding for modeline."
  (let ((r-length
         (length
          (format-mode-line mode-line-end-spaces))))
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
                (:eval (tlj-modeline-padding))
                mode-line-end-spaces))

;;; modeline-modules.el ends here