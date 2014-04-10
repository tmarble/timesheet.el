;; custom.el
;; General emacs customizations

(message ";;; custom.el - user customizations")

;; special file while rapid prototyping the timesheet package
;; OPTIONAL: only useful during development!
(load "timesheet-dev.el")

(unless (require 'timesheet nil t)
  (message "package 'timesheet not yet installed..."))


;; Make sure TEXINPUTS is set to
;; export TEXINPUTS=.:$HOME/.emacs.d/elpa/auctex-11.87.4/latex:
(unless (require 'latex nil t)
  (message "MELPA package 'latex not yet installed..."))

;; --------- auto save stuff below -------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(timesheet-invoice-number 100))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-hide ((t (:foreground "black")))))
