;;; early-init.el --- Jonathan's Emacs configuration

;;; Commentary:

;; This is my personal Emacs configuration.  You probably don't want any.

;;; Code:

(require 'package)
(setq package-archives
      (nconc package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                                ("melpa" . "https://melpa.org/packages/"))))

;;; early-init.el ends here
