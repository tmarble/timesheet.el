;;; init.el --- Where all the magic begins
(message ";;; init.el") ;; DEBUG

;; Directory structure... in load order:
;; HOME/
;;  .emacs.d/
;;    init.el
;;    melpa.el - MELPA configuration
;;    elpa/ - MELPA packages
;;    hosts/default/*.el - system default configuration
;;    hosts/HOSTNAME/*.el - system specific configuration
;;    users/default/*.el - user default configuration
;;    users/USERNAME/*.el - user specific configuration
;;    custom.el - simple customizations

(setq init-verbose t ;; message before each loaded file
      dotfiles-dir (file-name-directory (or (buffer-file-name)
                                            load-file-name))
      hosts-dir (file-name-as-directory (expand-file-name "hosts" dotfiles-dir))
      host-default-dir (file-name-as-directory (expand-file-name "default" hosts-dir))
      hostname (system-name)
      ;; wait until hostname is simplified before constructing host-specific-dir
      users-dir (file-name-as-directory (expand-file-name "users" dotfiles-dir))
      user-default-dir (file-name-as-directory (expand-file-name "default" users-dir))
      user-specific-dir (file-name-as-directory (expand-file-name user-login-name users-dir)))

(add-to-list 'load-path dotfiles-dir)

(defun init-load (filename &optional noerror)
  "Load FILENAME and provide message when init-verbose passing in optional NOERROR."
  (when init-verbose
    (message (format ";; LOAD: %s -------------------------" filename)))
  (load filename noerror))

(init-load "melpa.el")

;; Check if MELPA is installed
(unless (require 's nil t)
  ;; MELPA not present.. do first initialization
  (message "Installing MELPA packages...")
  (package-refresh-contents)
  (package-install 's)
  (require 's))

(setq hostname (car (s-split "\\." hostname))
      host-specific-dir (file-name-as-directory (expand-file-name hostname hosts-dir)))

;; load any system and user specific files
(dolist (dir (list host-default-dir host-specific-dir user-default-dir user-specific-dir))
  (when (file-exists-p dir)
    (add-to-list 'load-path dir)
    (mapc #'load (directory-files dir nil ".*el$"))))

;; customization file
(init-load "custom.el" 'noerror)

;; end
