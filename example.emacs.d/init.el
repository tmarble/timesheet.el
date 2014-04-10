;;; init.el --- Where all the magic begins
(message ";;; init.el")

;; load files in .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

;; load MELPA packages
(load "melpa.el")

(setq hostname (system-name)
      melpa-installed nil)

;; load required MELPA libraries here
;; NOTE before MELPA is installed this will fail.. which is OK if
;; system name returns a FQDN the first time through
(when (require 's nil t)
  (setq hostname (car (s-split "\\." hostname))
        melpa-installed t))

;; customization file
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

(unless melpa-installed
  (message "MELPA packages not yet installed...")
  (package-refresh-contents)
  (package-install 's))

(when melpa-installed
  ;; load any system and user specific filessy
  (setq system-specific-config (concat dotfiles-dir hostname ".el")
        user-specific-config (concat dotfiles-dir user-login-name ".el")
        user-specific-dir (concat dotfiles-dir user-login-name))
  (add-to-list 'load-path user-specific-dir)

  (if (file-exists-p system-specific-config) (load system-specific-config))
  (if (file-exists-p user-specific-config) (load user-specific-config))
  (if (file-exists-p user-specific-dir)
      (mapc #'load (directory-files user-specific-dir nil ".*el$"))))

;; end
