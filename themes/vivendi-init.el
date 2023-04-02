;;; vivendi-init.el --- Modus vivendi -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 artjeremie

;; Author: artjeremie <artjeremie@gmail.com>
;; URL: https://github.com/artjeremie

;;; Commentary:

;; This file contains configuration to `modus-vivendi' theme.

;;; Code:

(setq-default org-todo-keyword-faces
              '(("DONE" :foreground "#44df44")
                ("STOP" :foreground "purple")))

(setq-default org-priority-faces '((65 . "#ff5f5f")
                                   (66 . "#338fff")
                                   (67 . "#efef00")
                                   (68 . "#ff66ff")
                                   (69 . "#caa6df")
                                   (70 . "#9ac8e0")))

;; Load theme in `early-init.el' so we avoid that ugly white screen startup.
(load-theme 'modus-vivendi)

(add-to-list 'default-frame-alist '(background-color . "#1d2021"))
(add-to-list 'default-frame-alist '(foreground-color . "#c2c2c2"))

;; Recommended to enable `variable-pitch-mode'.
(custom-set-faces
 '(org-block ((t (:background "#282828"))))
 '(org-block-begin-line ((t (:inherit variable-pitch))))
 '(org-block-end-line ((t (:inherit org-block-begin-line))))
 '(fringe ((t (:background nil))))
 '(diff-hl-insert ((t (:inherit default :foreground "#237f3f"))))
 '(diff-hl-change ((t (:inherit default :foreground "#8a7a00"))))
 '(diff-hl-delete ((t (:inherit default :foreground "#b81a1f"))))
 '(line-number ((t (:background "#1d2021"))))
 '(line-number-current-line ((t (:background "#1d2021"))))
 '(org-headline-done ((t (:foreground "#4c4c4c"))))
 '(org-agenda-done ((t (:foreground "#4c4c4c"))))
 '(org-agenda-current-time ((t (:foreground "#44bc44")))))

;;; vivendi-init.el ends here