;; my-elisp.el
;; example of a user specific load file

(defun wipe-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*")
  (let* ((beg (point-min)))
    (point-max)
    (delete-region beg (point))
    ))
