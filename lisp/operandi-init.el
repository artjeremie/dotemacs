;;; operandi-init.el --- Modus operandi -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 artjeremie

;; Author: artjeremie <artjeremie@gmail.com>
;; URL: https://github.com/artjeremie

;;; Commentary:

;; This file contains configuration to `modus-operandi' theme.

;;; Code:

(setq-default org-todo-keyword-faces
              '(("DONE" :foreground "#008900")
                ("STOP" :foreground "purple")))

(setq-default org-priority-faces '((65 . "#d00000")
                                   (66 . "#7c318f")
                                   (67 . "#808800")))

;; Recommended to enable `variable-pitch-mode'.
(custom-set-faces
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-block-begin-line ((t (:inherit variable-pitch))))
 '(org-block-end-line ((t (:inherit org-block-begin-line))))
 '(fringe ((t (:background nil))))
 '(diff-hl-insert ((t (:inherit default :foreground "#6cc06c"))))
 '(diff-hl-change ((t (:inherit default :foreground "#d7c20a"))))
 '(diff-hl-delete ((t (:inherit default :foreground "#d84a4f"))))
 '(org-headline-done ((t (:foreground "#a6a6a6"))))
 '(org-agenda-done ((t (:foreground "#a6a6a6"))))
 '(org-agenda-current-time ((t (:foreground "#006800")))))

;;; operandi-init.el ends here