;; timesheet-dev.el
;; Example of emacs package debugging tools..
;; This allows specifying a minimum version (and removing old vesions)

(message ";;; timesheet-dev.el - timesheet.el rapid prototyping tools")

;; debug load of timesheet.el

(setq elpa-dir (concat dotfiles-dir "elpa"))

;; note: package can either be string or a symbol
(defun package-versions (package)
  "return a list of package versions for package"
  (interactive)
  (let* ((pkg (if (symbolp package) package (make-symbol package)))
	 (pkgname (symbol-name pkg))
	 (pkgname- (concat pkgname "-")))
    (mapcar (lambda (str)
	      (replace-regexp-in-string pkgname- "" str t t))
	    (directory-files elpa-dir nil pkgname-))))

;; note: package can either be string or a symbol
;; note: version can either be string or a version-list
(defun package-update (package version install-dir)
  "update package to version (if necessary) from install-dir"
  (interactive)
  (let* ((ver (if (stringp version) version (package-version-join version)))
	 (ver-list (version-to-list ver))
	 (pkg (if (symbolp package) package (make-symbol package)))
	 (pkgname (symbol-name pkg))
	 (pkgname-el (concat pkgname ".el"))
	 (pkgname-ver-tar (concat pkgname "-" ver ".tar"))
	 ;; (install-filename (concat install-dir "/" pkgname-el "/" pkgname-el))
	 ;; (install-filename (concat install-dir "/" pkgname-el "/" pkgname-ver-tar))
	 (install-filename (concat install-dir "/" pkgname-ver-tar))
	 (load-filename (concat dotfiles-dir "elpa/" pkgname "-" version "/" pkgname-el)))
    ;; (message (format "pkgname: %s ver-list: %s install-filename: %s load-filename: %s pkgname-ver-tar: %s" pkgname ver-list install-filename load-filename pkgname-ver-tar))
    (if (not (file-exists-p install-filename))
	(message (format "updated package file not found: %s" install-filename))
      (unless (package-installed-p pkg ver-list)
	(message (format "updating package %s to version %s ..." pkgname ver))
	(when (package-installed-p pkg)
	  ;; delete old versions
	  (dolist (oldver (package-versions pkg))
	    (message (format "deleting obsolete version of %s: %s" pkgname oldver))
	    (package-delete pkgname oldver)))
	(package-install-file install-filename)
	;; must re-load to take timesheet into account...
	(load load-filename)))))

(package-update 'timesheet "0.2.27" "/var/tmp")
