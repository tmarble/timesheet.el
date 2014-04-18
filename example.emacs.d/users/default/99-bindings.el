;; bindings.el

(message (concat ";;; bindings.el - bindings for user: " user-login-name))

;; example timesheet bindings
(require 'timesheet)

(define-prefix-command 'outline-map)
(global-set-key (kbd "<f10>") 'outline-map)
(define-key outline-map (kbd "c") 'timesheet-clock-update-timeclock)
(define-key outline-map (kbd "t") 'timesheet-calc-today) ; today
(define-key outline-map (kbd "y") 'timesheet-calc-yesterday) ; yesterday
(define-key outline-map (kbd "T") 'timesheet-calc-at-point)
(define-key outline-map (kbd "w") 'timesheet-weekly-this) ; this week
(define-key outline-map (kbd "Y") 'timesheet-weekly-last) ; last week
(define-key outline-map (kbd "W") 'timesheet-weekly-at-point)
(define-key outline-map (kbd "4") 'timesheet-invoice-this) ; this month
(define-key outline-map (kbd "$") 'timesheet-invoice-last) ; last moint

(define-key outline-map (kbd "S") 'wipe-scratch) ; demo binding to a function in my-elisp.el
