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
 '(cursor ((t (:background "#b4f9f8"))))
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
 '(org-block-begin-line ((t (:foreground "#565f89"))))
 '(org-block-end-line ((t (:foreground "#565f89"))))
 '(org-link ((t (:foreground "#b4f9f8"))))
 '(org-table ((t (:foreground "#9aa5ce"))))
 '(org-verbatim ((t (:foreground "#73daca"))))
 '(org-document-info-keyword ((t (:foreground "#9099c0"))))
 '(org-todo ((t (:foreground "#73daca" :underline t))))
 '(org-done ((t (:foreground "#9099c0"))))
 '(org-date ((t (:foreground "#e0af68" :underline t))))
 '(org-checkbox-statistics-todo ((t (:foreground "#e0af68"))))
 '(org-headline-done ((t (:foreground "#9099c0"))))
 '(org-agenda-structure ((t (:foreground "#bb9af7"))))
 '(org-agenda-date ((t (:foreground "#2ac3de" :underline t))))
 '(org-agenda-date-weekend ((t (:foreground "#2ac3de" :underline t))))
 '(org-agenda-date-weekend-today ((t (:foreground "#2ac3de" :underline t :bold t))))
 '(org-agenda-date-today ((t (:foreground "#2ac3de" :underline t :bold t))))
 '(org-scheduled-previously ((t (:foreground "#c0caf5"))))
 '(org-imminent-deadline ((t (:foreground "#f7768e"))))
 '(org-upcoming-deadline ((t (:foreground "#8c93b2"))))
 '(org-agenda-current-time ((t (:foreground "#ff9e64"))))

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
 '(diff-hl-delete ((t (:inherit default :foreground "#f7768e"))))

 ;; Rainbow-Delimiters
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#7aa2f7"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#bb9af7"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#73daca"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#9aa5ce"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#2ac3de"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#7aa2f7"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#bb9af7"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#73daca"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#9aa5ce"))))
 '(rainbow-delimiters-base-face ((t (:inherit default))))
 '(rainbow-delimiters-base-error-face ((t (:inherit default :foreground "#ff9e64"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "#ff9e64"))))
 '(rainbow-delimiters-mismatched-face ((t (:inherit rainbow-delimiters-unmatched-face))))

 ;; Vc
 '(vc-edited-state ((t (:foreground "#f7768e")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'art)

;;; art-theme.el ends here