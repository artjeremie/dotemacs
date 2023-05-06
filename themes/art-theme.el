;;; art-theme.el --- Art theme -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 artjeremie

;; Author: artjeremie <artjeremie@gmail.com>
;; URL: https://github.com/artjeremie

;;; Commentary:

;; This file contains `art-theme'.

;;; Code:

(setq-default org-priority-faces '((65 . "#f7768e")
                                   (66 . "#bb9af7")
                                   (67 . "#9ece6a")))

(deftheme art
  "My personal theme for Emacs.")

(custom-theme-set-faces
 'art
 '(default ((t (:background "#282c34" :foreground "#c0caf5"))))
 '(border ((t (:background "#1d2021" :foreground "#292e42"))))
 '(fringe ((t (:background "#282c34"))))
 '(region ((t (:background "#414868"))))
 '(selection ((t (:background "#414868"))))
 '(highlight ((t (:background "#3b4261" :foreground "#c0caf5"))))
 '(show-paren-match ((t (:background nil :foreground "#f7768e" :underline t :bold t))))
 '(line-number-current-line ((t (:foreground "#7aa2f7"))))
 '(mode-line ((t (:background "#21252b" :foreground "#9da5b4"))))
 '(mode-line-inactive ((t (:background "#181a1f" :foreground "#3e4451"))))
 '(font-lock-comment-face ((t (:foreground "#565f89"))))
 '(font-lock-constant-face ((t (:foreground "#ff9e64"))))
 '(font-lock-keyword-face ((t (:foreground "#bb9af7"))))
 '(font-lock-warning-face ((t (:foreground "#e0af68"))))
 '(font-lock-doc-face ((t (:foreground "#7c819b"))))
 '(font-lock-type-face ((t (:foreground "#c0caf5"))))
 '(font-lock-builtin-face ((t (:foreground "#f7768e"))))
 '(font-lock-string-face ((t (:foreground "#9ece6a"))))

 ;; Org
 '(org-block ((t (:foreground "#a0aad2"))))
 '(org-level-1 ((t (:foreground "#7aa2f7"))))
 '(org-level-2 ((t (:foreground "#bb9af7"))))
 '(org-level-3 ((t (:foreground "#9aa5ce"))))
 '(org-table ((t (:foreground "#9aa5ce"))))
 '(org-verbatim ((t (:foreground "#73daca"))))
 '(org-document-info-keyword ((t (:foreground "#9099c0"))))
 '(org-todo ((t (:foreground "#73daca"))))
 '(org-done ((t (:foreground "#9099c0"))))
 '(org-headline-done ((t (:foreground "#9099c0"))))
 '(org-agenda-structure ((t (:foreground "#bb9af7"))))
 '(org-agenda-date ((t (:foreground "#2ac3de"))))
 '(org-agenda-date-weekend ((t (:foreground "#2ac3de"))))
 '(org-agenda-date-weekend-today ((t (:foreground "#2ac3de" :bold t))))
 '(org-agenda-date-today ((t (:foreground "#2ac3de" :bold t))))
 '(org-scheduled-previously ((t (:foreground "#c0caf5"))))
 '(org-imminent-deadline ((t (:foreground "#f19999"))))
 '(org-upcoming-deadline ((t (:foreground "#8c93b2"))))

 ;; Calendar
 '(holiday ((t (:background nil :foreground "#f7768e"))))

 ;; Habit
 '(org-habit-clear-future-face ((t (:background "#565f89" :foreground "#13141c"))))
 '(org-habit-clear-face ((t (:background "#7aa2f7" :foreground "#13141c"))))
 '(org-habit-alert-future-face ((t (:background "#ff9e64" :foreground "#13141c"))))
 '(org-habit-overdue-future-face ((t (:background "#f7768e" :foreground "#13141c"))))
 '(org-habit-ready-face ((t (:background "#9ece6a" :foreground "#13141c"))))
 '(org-habit-alert-face ((t (:background "#e0af68" :foreground "#13141c"))))

 ;; Diff-hl
 '(diff-hl-insert ((t (:inherit default :foreground "#9ece6a"))))
 '(diff-hl-change ((t (:inherit default :foreground "#e0af68"))))
 '(diff-hl-delete ((t (:inherit default :foreground "#f7768e")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'art)

;;; art-theme.el ends here