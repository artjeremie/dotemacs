;;; init.el --- Init File -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 hoaxdream

;; Author: hoaxdream <hoaxdream@gmail.com>
;; URL: https://github.com/hoaxdream

;;; Commentary:

;; This file is NOT part of GNU Emacs.

;;; Code:

(let ((file-name-handler-alist nil)
      (gc-cons-percentage .6)
      (gc-cons-threshold most-positive-fixnum)
      (read-process-output-max (* 1024 1024)))

  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

  (load "modeline-modules" nil t)
  (load "prog-modules" nil t)
  (load "package-init-modules" nil t)

  (let ((user-settings "~/.emacs.d/userconfig.el"))
    (when (file-exists-p user-settings)
      (load user-settings nil t)))

  (cd "c:/Users/hoaxdream/")
  (garbage-collect)

  (add-hook 'emacs-startup-hook
            (lambda ()
              (message
               "Loaded in %s with %d garbage-collected."
               (format
                "%.2f seconds"
                (float-time
                 (time-subtract after-init-time before-init-time)))
               gcs-done))))

;;; init.el ends here