;;; typography-init.el --- Org variable-pitch -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 artjeremie

;; Author: artjeremie <artjeremie@gmail.com>
;; URL: https://github.com/artjeremie

;;; Commentary:

;; This file contains configuration faces for Org `variable-pitch-mode'.

;;; Code:

(custom-set-faces
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight light :height 0.8))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-agenda-structure-secondary ((t (:weight bold :height 0.8))))
 '(org-headline-done ((t (:strike-through t)))))

;;; typography-init.el ends here