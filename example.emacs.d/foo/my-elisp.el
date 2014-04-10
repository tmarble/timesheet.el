;; my-elisp.el
;; example of a user specific load file

(message ";;; USER/my-elisp.el - user specific config example")

(defun wipe-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*")
  (let* ((beg (point-min)))
    (point-max)
    (delete-region beg (point))
    ))
