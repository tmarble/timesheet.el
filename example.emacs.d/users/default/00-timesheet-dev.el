;; timesheet-dev.el
;; Example of emacs package debugging tools..
;; This allows specifying a minimum version (and removing old vesions)

;; debug load of timesheet.el

(setq elpa-dir (file-name-as-directory (expand-file-name "elpa" dotfiles-dir)))

;; note: package can either be string or a symbol
(defun package-versions (package)
  "Return a list of package versions for PACKAGE."
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
  "Update PACKAGE to VERSION from INSTALL-DIR (if necessary)."
  (interactive)
  (let* ((ver (if (stringp version) version (package-version-join version)))
	 (ver-list (version-to-list ver))
	 (pkg (if (symbolp package) package (make-symbol package)))
	 (pkgname (symbol-name pkg))
	 (pkgname-el (concat pkgname ".el"))
         (pkgname-ver (concat pkgname "-" ver))
	 (pkgname-ver-tar (concat pkgname-ver ".tar"))
	 (install-filename (expand-file-name pkgname-ver-tar install-dir))
         (elpa-pkg-dir (file-name-as-directory (expand-file-name pkgname-ver elpa-dir)))
         (elpa-pkgname-el (expand-file-name pkgname-el elpa-pkg-dir)))
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
	(load elpa-pkgname-el)))))

(package-update 'timesheet "0.2.31" "/var/tmp")
