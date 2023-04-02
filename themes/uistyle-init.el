;;; uistyle-init.el --- UI style -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 artjeremie

;; Author: artjeremie <artjeremie@gmail.com>
;; URL: https://github.com/artjeremie

;;; Commentary:

;; This file contains configuration to Emacs UI.

;;; Code:

(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 40)))

(setq window-divider-default-right-width 2)
(setq window-divider-default-bottom-width 1)

(window-divider-mode t)

(add-hook 'before-make-frame-hook 'window-divider-mode)

;;; uistyle-init.el ends here