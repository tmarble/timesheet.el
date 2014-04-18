;; melpa.el
;; load MELPA per http://melpa.milkbox.net/#/getting-started

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; NOTE: this is not tested for EMACS < 24
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; end
