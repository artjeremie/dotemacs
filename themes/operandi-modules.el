;;; operandi-modules.el --- Modus operandi -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 hoaxdream

;; Author: hoaxdream <hoaxdream@gmail.com>
;; URL: https://github.com/hoaxdream

;;; Commentary:

;; This file contains configuration to `operandi' theme.

;;; Code:

(setq-default org-todo-keyword-faces
              '(("DONE" :foreground "#008900")
                ("STOP" :foreground "purple")))

(setq-default org-priority-faces '((65 . "#d00000")
                                   (66 . "#0000ff")
                                   (67 . "#808800")
                                   (68 . "#dd22dd")
                                   (69 . "#7c318f")
                                   (70 . "#005077")))

;; Load theme in `early-init.el' so we avoid that ugly white screen startup.
(load-theme 'modus-operandi)

;; Recommended to enable `variable-pitch-mode'.
(custom-set-faces
 '(org-block ((t (:inherit fixed-pitch))))
 '(fringe ((t (:background nil))))
 '(diff-hl-insert ((t (:inherit default :foreground "#6cc06c"))))
 '(diff-hl-change ((t (:inherit default :foreground "#d7c20a"))))
 '(diff-hl-delete ((t (:inherit default :foreground "#d84a4f"))))
 '(org-block-begin-line ((t (:inherit variable-pitch))))
 '(org-block-end-line ((t (:inherit org-block-begin-line))))
 '(org-headline-done ((t (:foreground "#a6a6a6"))))
 '(org-agenda-current-time ((t (:foreground "#006800")))))

;;; operandi-modules.el ends here