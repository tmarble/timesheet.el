;;; timesheet.el --- Timesheet management add-on for org-mode

;; Copyright © 2014 Informatique, Inc.

;; Author: Tom Marble
;; URL: https://github.com/tmarble/timesheet.el
;; Version: 0.3.0
;; Created: 2015-08-31
;; Keywords: org timesheet
;; Package-Requires: ((s "1") (org "7") (auctex "11"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Debian-depends: emacs24 make gawk sed git tar rubber texlive-latex-extra texlive-fonts-recommended texlive-fonts-extra evince
;;
;; This library adds timetracking and invoice generation to org-mode
;; and relies heavily on
;; org clocking http://orgmode.org/org.html#Clocking-work-time
;; and TODO items http://orgmode.org/org.html#TODO-Items
;; and org spreadsheets http://orgmode.org/org.html#The-spreadsheet
;;
;; This library attempts to conform to packaging conventions:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Packaging.html
;; Bugs, enhancements welcome!

;;; Usage

;; Ensure TEXINPUTS is set to (in your ~/.bashrc)
;; export TEXINPUTS=.:$HOME/.emacs.d/elpa/auctex-11.87.4/latex:
;;
;; Start by creating an example client...
;;   M-x timesheet-example
;;   You will be viewing the buffer yoyodyne.org that already has some example
;;   time entries... Create an invoice with
;;   M-x timesheet-invoice-this
;;
;; Next steps...
;; - customize your name (in defs.tex) and logo (in logo.pdf).
;; - update some time entries.
;;
;; Example key bindings
;;  see example.emacs.d/foo/bindings.el

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 's)
(require 'org)
(require 'org-clock)
(require 'org-table)

;; vars

;; timesheet-version should match the Version comment above
(defconst timesheet-version "0.3.0")

(defconst timesheet-path (file-name-directory (or load-file-name (buffer-file-name))))

(defun timesheet-home-dir ()
  "Return the user's HOME directory."
  (file-name-as-directory (expand-file-name "~")))

;; customizations

(defgroup timesheet nil
  "Timesheet functions"
  :tag "Timesheet"
  :group 'org)

(defcustom timesheet-invoice-number 100
  "Next invoice number."
  :type '(integer)
  :group 'timesheet)

(defcustom timesheet-invoice-script
  (expand-file-name "timesheet-invoice" (expand-file-name "bin" timesheet-path))
  "Script to create a timesheet invoice."
  :type 'string
  :group 'timesheet)

(defcustom timesheet-company-dir
  (expand-file-name "Timesheet" (timesheet-home-dir))
  "Parent directory for timesheet invoices."
  :type 'string
  :group 'timesheet)

;; get the next invoice number (and increment the counter)
;; if the argument is given.. set the next invoice number
;;;###autoload
(defun timesheet-next-invoice (&optional invoice)
  "Get next invoice number (following `timesheet-invoice-number' or INVOICE if present)."
  (interactive)
  (let* ((next-invoice (if invoice invoice timesheet-invoice-number))
         (arg (list 'timesheet-invoice-number (1+ next-invoice))))
    (custom-set-variables arg)
    (custom-save-all)
    next-invoice))

;; debug functions
;; these will probably be removed from a future version...

(defvar timesheet-debug-msg-delay 3)

;;;###autoload
(defun timesheet-debug-msg (&rest msgs)
  "Display some debug MSGS."
  (interactive)
  (message (apply 'concat msgs))
  (redisplay t)
  (sleep-for timesheet-debug-msg-delay))

;;;###autoload
(defun timesheet-debug-time (time &rest msgs)
  "Display TIME with some debug MSGS."
  (interactive)
  (let ((at-time (if time time (current-time))))
    (apply 'timesheet-debug-msg (cons (format-time-string "%Y-%m-%d %a %H:%M:%S" at-time) msgs))))

;;;###autoload
(defun timesheet-debug-time-cal (time-cal &rest msgs)
  "Display TIME-CAL with some debug MSGS."
  (interactive)
  (apply 'timesheet-debug-time (cons (apply 'encode-time time-cal) msgs)))

;; functions

(defun timesheet-template-files ()
  "Return a list of pathnames for timesheet template files."
  (let ((share-dir (expand-file-name "share" timesheet-path)))
    (directory-files share-dir t (concat "\\." (regexp-opt '("tex" "pdf" "org"))  "\\'"))))

(defun timesheet-round-time-down (tl)
  "Round the time in TL (timelist format) to the previous quarter hour."
  (let* ((s (car tl))
         (m (nth 1 tl))
         (h (nth 2 tl))
         (rest (nthcdr 3 tl)))
    (cons s
          (cons (* (/ m 15) 15)
                (cons h rest)))))

(defun timesheet-round-time-up (tl)
  "Round the time in TL (timelist format) do the next quarter hour."
  (let* ((s (car tl))
         (m (nth 1 tl))
         (h (nth 2 tl))
         (rest (nthcdr 3 tl))
         (md (nth 1 (timesheet-round-time-down tl)))
         (mu (* (min 4 ;; most chunks possible is 4 * 15 = 60
                     (/ (+ m 10) 15)) ;; within 5 min of 15 min boundary
                15)) ; 15 min chunks
         tup)
    (if (> mu md)
        (setq m mu)
      (setq m md))
    (when (= m 60)
      (setq m 0)
      (setq h (+ h 1))) ;; BUG! overflow days!
    (setq tup (cons s (cons m (cons h rest))))
    tup))

(defun timesheet-get-heading-path ()
  "Return the full heading path."
  (append (org-get-outline-path) (list (nth 4 (org-heading-components)))))

;;;###autoload
(defun timesheet-clock-update-timeclock (&optional withpath)
  "If this is a CLOCK line, update /round it and return t.
Otherwise, return nil.  Optionally using WITHPATH."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (skip-chars-forward " \t")
    (when (looking-at org-clock-string)
      (let ((re (concat "[ \t]*" org-clock-string
			" *[[<]\\([^]>]+\\)[]>]\\(-+[[<]\\([^]>]+\\)[]>]"
			"\\([ \t]*=>.*\\)?\\)?"))
	    ts te h m s neg dts dte ets ete secs fh)
	(cond
	 ((not (looking-at re))
	  nil)
	 ((not (match-end 2))
	  (when (and (equal (marker-buffer org-clock-marker) (current-buffer))
		     (> org-clock-marker (point))
		     (<= org-clock-marker (point-at-eol)))
	    ;; The clock is running here
	    (setq org-clock-start-time
		  (apply 'encode-time
			 (org-parse-time-string (match-string 1))))
	    (org-clock-update-mode-line)))
	 (t
	  (and (match-end 4) (delete-region (match-beginning 4) (match-end 4)))
	  (end-of-line 1)
	  (setq ts (match-string 1)
		te (match-string 3))
          (setq dts (timesheet-round-time-down (org-parse-time-string ts))
                dte (timesheet-round-time-up (org-parse-time-string te)))
          (setq ets (apply 'encode-time dts)
                ete (apply 'encode-time dte))
	  (setq ts (format-time-string "%Y-%m-%d %a %H:%M" ets)
                te (format-time-string "%Y-%m-%d %a %H:%M" ete))
	  (setq s (- (org-float-time ete)
		     (org-float-time ets))
		neg (< s 0)
		s (abs s)
                secs s
                fh (/ secs 3600.0)
		h (floor (/ s 3600))
		s (- s (* 3600 h))

		m (floor (/ s 60))
		s (- s (* 60 s)))
	  (insert " => " (format "%s -- %s @ %5.2f" ts te fh))
          (if withpath
              (list ets ete fh (timesheet-get-heading-path))
            (list ets ete fh))
          ))))))

(defun timesheet-same-day-p (t1 t2)
  "Return true of T1 and T2 (timelist format) are on the same day."
  (let* ((dt1 (decode-time t1))
         (dt2 (decode-time t2)))
    (and (= (nth 5 dt1) (nth 5 dt2))
         (= (nth 4 dt1) (nth 4 dt2))
         (= (nth 3 dt1) (nth 3 dt2)))))

(defun timesheet-midnight (day-time)
  "Round DAY-TIME to midnight on that day."
  (let* ((day-time-cal (decode-time day-time))
         (day-cal (append '(0 0 0)
                          (nthcdr 3 day-time-cal)))
         (day (apply 'encode-time day-cal)))
    day))

(defun timesheet-add-days (time days)
  "Offset TIME by a positive (or negative) number of DAYS."
  (let* ((day (* 60 60 24))
         (d (abs days))
         (time-func (if (< days 0) 'time-subtract 'time-add))
         (time2 (funcall time-func time (seconds-to-time (* d day)))))
    time2))

;;;###autoload
(defun timesheet-today ()
  "Date for calculating timesheet: today."
  (interactive)
  (let* ((now (current-time))
         (today (timesheet-midnight now)))
    today))

;;;###autoload
(defun timesheet-yesterday ()
  "Date for calculating timesheet: yesterday."
  (interactive)
  (timesheet-add-days (timesheet-today) -1))

;;;###autoload
(defun timesheet-at-point ()
  "Date for calculating timesheet: current clock line."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (skip-chars-forward " \t")
    (when (looking-at org-clock-string)
      (let ((re (concat "[ \t]*" org-clock-string
			" *[[<]\\([^]>]+\\)[]>]\\(-+[[<]\\([^]>]+\\)[]>]"
			"\\([ \t]*=>.*\\)?\\)?")))
        (when (looking-at re)
          (timesheet-midnight (apply 'encode-time (org-parse-time-string (match-string 1)))))))))

(defun timesheet-heading ()
  "Goto (or create) the Timesheet heading."
  (let* ((timesheet (org-find-exact-headline-in-buffer "Timesheet" nil t)))
    (when timesheet
      (goto-char timesheet))
    (unless timesheet
      (message "adding a Timesheet...")
      (goto-char (point-max))
      (insert "\n* Timesheet"))
    (beginning-of-line)))

;; BUG: on creating the month heading for a new month... it ends up AFTER
;; the summary (it should be before!)
(defun timesheet-heading-month (day)
  "Goto (or create) the Timesheet/month heading for DAY."
  (timesheet-heading)
  (let* ((yyyy-mm (format-time-string "%Y-%m" day))
         (m-head (org-goto-first-child))
         found
         firstmonth)
    (unless m-head
      (setq firstmonth t))
    (while m-head
      (let* ((m (nth 4 (org-heading-components)))
             (prev (point)))
        (if (string= m yyyy-mm)
            (progn
              (setq m-head nil)
              (setq found t))
          (setq m-head (org-get-next-sibling)))
        (unless m-head ;; we have no children
          (goto-char prev)
          (end-of-line))
        )
      )
    (unless found
      (if firstmonth
          (org-insert-heading)
        (org-insert-heading-after-current))
      (insert yyyy-mm)))
  (beginning-of-line))

;; FIX: insert in order
(defun timesheet-heading-day (day)
  "Goto (or create) the Timesheet/month/day heading for DAY."
  (timesheet-heading-month day)
  ;; (timesheet-debug-msg "timesheet-heading-day BEGIN")
  ;; (timesheet-debug-msg "timesheet-heading-day BEGIN 2")
  ;; (timesheet-debug-msg "timesheet-heading-day BEGIN 3")
  (let* ((dday (format-time-string "%Y-%m-%d %a" day))
         (d-head (org-goto-first-child))
         found
         firstday)
    ;; (timesheet-debug-msg "looking for " dday)
    (unless d-head ;; no days yet
      (setq firstday t))
    (while d-head
      (let* ((d (nth 4 (org-heading-components)))
             (prev (point)))
        ;; (timesheet-debug-msg "checking " d)
        (if (s-starts-with? dday d)
            (progn
              ;; (timesheet-debug-msg "FOUND it")
              (setq d-head nil)
              (setq found t))
          (setq d-head (org-get-next-sibling)))
        (unless d-head ;; no more kids
          ;; (timesheet-debug-msg "no more kids")
          (goto-char prev)
          (end-of-line))
        )
      )
    (unless found
      ;; (timesheet-debug-msg "NOT found")
      (if firstday
          (org-insert-heading)
        (org-insert-heading-after-current))
      (insert dday)
      (when firstday
        (org-do-demote))
      )
    ))

(defun timesheet-cmp-task (atask btask)
  "Compare two tasks and return t if ATASK < BTASK."
  (let* ((apath (car atask))
         (bpath (car btask))
         (alen (length apath))
         (blen (length bpath))
         cmp)
    (if (> alen blen)
        (setq cmp t)
      (when (= alen blen)
        ;; return true of the components of apath are lexicographically after bpath
        (while apath
          (let* ((a (pop apath))
                 (b (pop bpath)))
            (cond ((string< b a)
                   (setq cmp t)
                   (setq apath nil)) ;; we know a < b, stop
                  ((string= b a)
                   nil) ; keep comparing along apath
                  (t
                   (setq apath nil))) ;; we know a > b, stop
            ))
        ))
    cmp))

(defun timesheet-paths-same-p (a b)
  "Return t if paths A and B are the same."
  (when (and (listp a)
             (listp b)
             (= (length a) (length b)))
    (let* ((cmp t)
           (a0 (pop a))
           (b0 (pop b)))
      (while a0
        (if (string= a0 b0)
            (progn
              (setq a0 (pop a))
              (setq b0 (pop b)))
          (progn
            (setq a0 nil)
            (setq cmp nil))))
      cmp
      )))

(defun timesheet-rollup-times (clocks)
  "Sort and rollup CLOCKS."
  (let* (clock
         task
         tasks
         rtasks
         (day-hours 0.0)
         (project-hours 0.0)
         (goal-hours 0.0)
         (task-hours 0.0)
         prev-task-path
         prev-goal-path
         prev-project-path)
    (dolist (clock clocks)
      (setq task (list (cdr (nth 3 clock)) (car clock) (nth 1 clock) (nth 2 clock)))
      (push task tasks))
    ;; sort tasks by path: longest, descending
    (setq rtasks (sort tasks 'timesheet-cmp-task))
    ;; go through.. when path changes, make roll up entry (path start stop total-hours)
    (setq tasks nil)
    (dolist (task rtasks)
      (let* ((task-path (car task))
             (goal-path (butlast task-path))
             (project-path (butlast goal-path))
             (hours (nth 3 task))
             (p (length task-path)))
        ;; if the path is not the same, then make rollup for previous
        (unless (timesheet-paths-same-p task-path prev-task-path)
          (unless (= task-hours 0.0)
            (push (list prev-task-path nil nil task-hours) tasks)
            (setq task-hours 0.0))
          (unless (timesheet-paths-same-p goal-path prev-goal-path)
            (unless (= goal-hours 0.0)
              (push (list prev-goal-path nil nil goal-hours) tasks)
              (setq goal-hours 0.0))
            (unless (timesheet-paths-same-p project-path prev-project-path)
              (unless (= project-hours 0.0)
                (push (list prev-project-path nil nil project-hours) tasks)
                (setq project-hours 0.0)))))
        ;; add this leaf task
        (push task tasks)
        ;; add totals
        (when (> p 0)
          (when (> p 1)
            (when (> p 2)
              (setq task-hours (+ task-hours hours)))
            (setq goal-hours (+ goal-hours hours)))
          (setq project-hours (+ project-hours hours)))
        (setq day-hours (+ day-hours hours))
        (setq prev-task-path task-path)
        (setq prev-goal-path goal-path)
        (setq prev-project-path project-path)
        ))
    (unless (= task-hours 0.0)
      (push (list prev-task-path nil nil task-hours) tasks))
    (unless (= goal-hours 0.0)
      (push (list prev-goal-path nil nil goal-hours) tasks))
    (unless (= project-hours 0.0)
      (push (list prev-project-path nil nil project-hours) tasks))
    (push (list nil nil nil day-hours) tasks)
    tasks
    ))

(defun timesheet-calc (day)
  "Calculate timesheet for the given DAY."
  (let* ((clocks (timesheet-clocks day (timesheet-add-days day 1)))
         (tasks (timesheet-rollup-times clocks))
         (day-hours (nth 3 (pop tasks)))
         task
         prevpath)
    (timesheet-heading-day day)
    ;; delete contents of heading
    (org-cut-subtree)
    ;; insert updated data for the day
    (insert (format "*** %s = %3.2f hours\n" (format-time-string "%Y-%m-%d %a" day) day-hours))
    (forward-line -1)
    (end-of-line)
    (while (setq task (pop tasks))
      (let* ((path (car task))
             (p (length path))
             (start (nth 1 task))
             (stop (nth 2 task))
             (hours (nth 3 task)))
        (unless start ;; do NOT print clock entries
          (insert "\n***")
          (while (> p 0)
            (insert "*")
            (setq p (1- p)))
          (insert (format " %s = %3.2f hours" (car (last path)) hours))
          )
        (setq prevpath path)
        ))
    ))

(defun timesheet-clocks (start-time end-time)
  "Return a list of clocks for the time interval given by START-TIME and END-TIME."
  (save-excursion
    (save-restriction
      (let (day-times sehp)
        (goto-char (point-max))
        (beginning-of-line 1)
        (while (not (bobp))
          (setq sehp (timesheet-clock-update-timeclock t)) ;; start end hours path
          (when (and sehp
                     (listp sehp)
                     (time-less-p start-time (car sehp))
                     (time-less-p (car sehp) end-time))
            (push sehp day-times))
          (beginning-of-line 0))
        day-times
        ))))

;;;###autoload
(defun timesheet-calc-today ()
  "Calculate timesheet for today."
  (interactive)
  (timesheet-calc (timesheet-today)))

;;;###autoload
(defun timesheet-calc-yesterday ()
  "Calculate timesheet for yesterday."
  (interactive)
  (timesheet-calc (timesheet-yesterday)))

;;;###autoload
(defun timesheet-calc-at-point ()
  "Calculate timesheet for the date on this line."
  (interactive)
  (let ((day (timesheet-at-point)))
    (if day
        (timesheet-calc day)
      (message (concat "no " org-clock-string " at point!")))))

;;;###autoload
(defun timesheet-weekly-at-point ()
  "Calculate week for the date on this line."
  (interactive)
  (let ((day (timesheet-at-point)))
    (if day
        (timesheet-weekly (timesheet-week-time day))
      (message (concat "no " org-clock-string " at point!")))))

(defun timesheet-cmp-string-lists (asl bsl)
  "Compare two string lists and return t if ASL < BSL."
  (let ((i 0)
        (cmp 0)) ;; -1 <, 0 =, 1 >
    (while (and (= cmp 0) (> (length asl) i) (> (length bsl) i))
      (let ((a (nth i asl))
            (b (nth i bsl)))
        (if (string< a b)
            (setq cmp -1)
          (if (not (string= a b))
              (setq cmp 1)))
        )
      (setq i (1+ i)))
    (when (and (= cmp 0) (< (length asl) (length bsl)))
      (setq cmp -1))
    (= cmp -1)))

(defun timesheet-project-times ()
  "Get list of project times for the given week."
  (save-excursion
    (save-restriction
      (timesheet-heading)
      (let* ((heading (timesheet-get-heading-path))
             (first (car heading)) ; Timesheet
             (prev first)
             (prevheading heading)
             project-times)
        (while (string= first prev) ; while we are in Timesheet
          (unless (or (/= (length heading) 4)
                      (timesheet-paths-same-p heading prevheading))
            (push (cdr heading) project-times))
          (forward-line)
          (setq prevheading heading)
          (setq heading (timesheet-get-heading-path))
          (setq prev first)
          (setq first (car heading))
          )
        (sort project-times 'timesheet-cmp-string-lists)
        ))))

(defun timesheet-goto-weekly ()
  "Goto (or create) the Weekly heading."
  (let* ((weekly (org-find-exact-headline-in-buffer "Weekly" nil t)))
    (when weekly
      (goto-char weekly))
    (unless weekly
      (message "adding a Weekly...")
      (goto-char (point-max))
      (insert "\n* Weekly"))
    (beginning-of-line)))

;; FIX: insert in order
(defun timesheet-heading-week (week &optional delete-existing-week)
  "Goto (or create) the Timesheet WEEK heading.
If DELETE-EXISTING-WEEK is set then the old heading is removed."
  (timesheet-goto-weekly)
  (recenter-top-bottom 1)
  (let* ((week-sunday (timesheet-add-days week 6))
         (week-label (format "%s week #%d: %s - %s"
                             (format-time-string "%Y" week)
                             (timesheet-week-number week)
                             (format-time-string "%B %d" week)
                             (format-time-string "%B %d" week-sunday)))
         (w-head (org-goto-first-child))
         firstweek
         found)
    (end-of-line)
    (unless w-head
      (setq firstweek t))
    (while w-head
      (let* ((w (nth 4 (org-heading-components)))
             (prev (point)))
        (if (string= w week-label)
            (progn
              (setq w-head nil)
              (setq found t))
          (setq w-head (org-get-next-sibling)))
        (unless w-head ;; we have no children
          (goto-char prev)
          (end-of-line))
        )
      )
    (when (and found delete-existing-week)
      (org-insert-heading)
      (org-cut-subtree)
      (forward-line -1))
    (unless found
      (org-insert-heading-after-current)
      (insert week-label)
      (when firstweek
        (org-demote-subtree)))
    (beginning-of-line)
    week-label
    ))

;;;###autoload
(defun timesheet-table-goto (top col row)
  "In the table given at TOP move to a position COL ROW."
  (interactive)
  (goto-char top)
  (forward-line row)
  (org-table-goto-column col))

(defun timesheet-weekly (week)
  "Calculate weekly timesheet for the given WEEK."
  (let* ((all-project-times (timesheet-project-times))
         (week-label (timesheet-heading-week week t))
         project-times ;; day project hours
         dates-cols ;; date to column alist
         projects
         project-rows ;; project to row alist
         table-top) ;; point at the beginning of the table
    (end-of-line)
    (insert "\n")
    ;; setup dates-cols alist
    (dotimes (i 7)
      (let* ((day (timesheet-add-days week i)))
        ;; (when (= i 6)
        ;;   (setq week-sunday day))
        (push (cons (format-time-string "%Y-%m-%d" day) (+ i 2)) dates-cols)))
    (dolist (pt all-project-times)
      (let* ((day-total (nth 1 pt))     ;; 2013-06-11 Tue = 6.50 hours
             (project-total (nth 2 pt)) ;; SuperProject = 6.50 hours
             day
             project
             hours)
        (when (string-match "^\\([0-9\-]+\\) ... = \\([0-9\.]+\\) hours" day-total)
          (setq day (match-string 1 day-total)) ;; don't use match 2
          (when (string-match "^\\(.+\\) = \\([0-9\.]+\\) hours" project-total)
            (setq project (match-string 1 project-total))
            (setq hours (match-string 2 project-total))))
        (when (and day project (assoc day dates-cols))
          (unless (assoc project project-rows)
            (push (cons project 0) project-rows))
          (push (list project day hours) project-times))
        ))
    ;; now begin to construct the table
    (setq table-top (point))
    (insert "#+BEGIN: columnview :hlines 1 :id global\n")
    (insert "| /Project/ | Mon | Tue | Wed | Thu | Fri | Sat | Sun | /Total/ |\n")
    (insert "|-----------+-----+-----+-----+-----+-----+-----+-----+---------|\n")
    (insert "|           |     |     |     |     |     |     |     |         |\n")
    (insert "|-----------+-----+-----+-----+-----+-----+-----+-----+---------|\n")
    (insert "| /Daily/   |     |     |     |     |     |     |     |         |\n")
    (insert "#+TBLFM: @2$9..@-1$9=vsum($2..$8);%.2f;::@>$2..@>$9='(format \"%3.2f\" (apply '+ '(@2..@-1)));N;\n")
    (insert "#+END:")
    ;; sort by project
    (dolist (p project-rows)
      (push (car p) projects))
    (setq projects (sort projects 'string<)) ;; sort is destructive :(
    (setq project-rows nil)
    (let* ((row 3))
      (dolist (p projects)
        (push (cons p row) project-rows)
        (when (> row 3)
          (timesheet-table-goto table-top 1 (1- row))
          (org-table-insert-row t))
        (timesheet-table-goto table-top 1 row)
        (insert p)
        (setq row (1+ row))
        ))
    (goto-char table-top)
    (dolist (pt project-times)
      (timesheet-table-goto table-top
                  (cdr (assoc (nth 1 pt) dates-cols))
                  (cdr (assoc (car pt) project-rows)))
      (insert (nth 2 pt))
      )
    ;; compute formulae in table
    (org-table-iterate)
    (org-table-align)
    ))

(defun timesheet-week-time (time)
  "Round TIME to beginning of the week."
  (let* ((time-cal (decode-time time))
         (dow (nth 6 time-cal)) ;; 0 == Sunday
         (day (* 60 60 24))) ; in seconds
    (when (= dow 0)
      (setq dow 7))
    (timesheet-midnight (time-subtract time (seconds-to-time (* day (1- dow)))))))

(defun timesheet-week-number (time)
  "Calculate the ISO week number for this TIME."
  (let* ((w-str (format-time-string "%W" time))
         (w (string-to-number w-str)))
    (1+ w)))

;;;###autoload
(defun timesheet-this-week ()
  "Date for calculating timesheet: today."
  (interactive)
  (let* ((now (current-time))
         (week (timesheet-week-time now)))
    week))

;;;###autoload
(defun timesheet-last-week ()
  "Date for calculating timesheet: yesterday."
  (interactive)
  (let* ((this (timesheet-this-week))
         (day (* 60 60 24)) ; in seconds
         (last (time-subtract this (seconds-to-time (* 7 day)))))
    last))

;;;###autoload
(defun timesheet-weekly-this ()
  "Calculate timesheet this week."
  (interactive)
  (timesheet-weekly (timesheet-this-week)))

;;;###autoload
(defun timesheet-weekly-last ()
  "Calculate timesheet last week."
  (interactive)
  (timesheet-weekly (timesheet-last-week)))

;; NOTE: this is not handled particularly well... and it needs a better user interface
;; (defun timesheet-overlap (day)
;;   "calculate timesheet overlap for today"
;;   (interactive)
;;   (let* ((clocks (timesheet-clocks day))
;;          (sorted-clocks (sort clocks (lambda (a b) (time-less-p (car a) (car b)))))
;;          times overlap
;;          last-start last-end
;;          start end
;;          path
;;          )
;;     (goto-char (point-max))
;;     (insert "\n* Overlap\n")
;;     (dolist (sehp sorted-clocks)
;;       (setq start (car sehp))
;;       (setq end (nth 1 sehp))
;;       (setq path (nth 3 sehp))
;;       (cond ((and last-end (time-less-p start last-end)) ;; overlap
;;              (setq overlap t)
;;              )
;;             ((and last-start (timesheet-same-day-p start last-start)) ;; same day
;;              nil
;;              )
;;             (last-start ;; new day (save previous day)
;;              (setq overlap nil)))
;;       (when overlap
;;         (insert (format "OVERLAP at %s in %s\n"
;;                         (format-time-string "%Y-%m-%d %a %H:%M" end)
;;                         path)))
;;       (setq last-start start)
;;       (setq last-end end)
;;       )
;;     (message "check overlap")
;;     ))

;; (defun timesheet-overlap-today ()
;;   "calculate timesheet for today"
;;   (interactive)
;;   (timesheet-overlap (timesheet-today)))

;; (defun timesheet-overlap-yesterday ()
;;   "calculate timesheet for yesterday"
;;   (interactive)
;;   (timesheet-overlap (timesheet-yesterday)))

;; BUG: this is USD centric :(
(defun timesheet-currency (v)
  "Return currency value for V."
  (let* ((fv (format "$%3.2f" (or v 0)))
         (len (length fv)))
    (cond ((>= v 1000000.00)
           (concat (substring fv 0 (- len 9)) ","
                   (substring fv (- len 9) (- len 6)) ","
                   (substring fv (- len 6))))
          ((>= v 1000.00)
           (concat (substring fv 0 (- len 6)) ","
                   (substring fv (- len 6))))
          (t
           fv))))

(defun timesheet-month-time (&optional time)
  "Round `current-time' (or TIME if given) to beginning of the month."
  (let* ((at-time (if time time (current-time)))
         (time-cal (decode-time at-time))
         (first-cal (list (car time-cal)
                          (nth 1 time-cal)
                          (nth 2 time-cal)
                          1 ;; first day of month
                          (nth 4 time-cal)
                          (nth 5 time-cal))))
    (timesheet-midnight (apply 'encode-time first-cal))))

;;;###autoload
(defun timesheet-this-month ()
  "Date for calculating timesheet: this month."
  (interactive)
  (timesheet-month-time))

(defun timesheet-days-in-month (year month)
  "How many days in the month given by YEAR MONTH."
  (let* ((jason '(0 31 28 31 30 31 30 31 31 30 31 30 31))
         (days (nth month jason)))
    (if (and (= month 2) (date-leap-year-p year))
        (1+ days)
      days)))

;;;###autoload
(defun timesheet-last-month ()
  "Date for calculating timesheet: last month."
  (interactive)
  (let* ((this-month (timesheet-this-month))
         (secs-per-day (* 60 60 24))) ; in seconds
    (timesheet-month-time (time-subtract this-month (seconds-to-time secs-per-day)))))

;;;###autoload
(defun timesheet-last-day-in-month (&optional time)
  "Return the date for the last day in this month.
Current month or month for TIME if present."
  (interactive)
  (let* ((at-time (if time time (current-time)))
         (time-cal (decode-time at-time))
         (month (nth 4 time-cal))
         (year (nth 5 time-cal))
         (last-cal (list 0 ;(car time-cal)
                         0 ; (nth 1 time-cal)
                         0 ;  (nth 2 time-cal)
                         (timesheet-days-in-month year month) ;; last day of month
                         month
                         year)))
    (apply 'encode-time last-cal)
    ))

;;;###autoload
(defun timesheet-first-day-next-month (&optional time)
  "Return the date for the first day in the next month.
Current month or month for TIME if present."
  (interactive)
  (let* ((last-day (timesheet-last-day-in-month time))
         (secs-per-day (* 60 60 24))) ; in seconds
    (timesheet-month-time (time-add last-day (seconds-to-time secs-per-day)))))

;;;###autoload
(defun timesheet-invoice-this ()
  "Calculate invoice this month."
  (interactive)
  (timesheet-invoice (timesheet-this-month)))

;;;###autoload
(defun timesheet-invoice-at-point ()
  "Calculate invoice at point (a CLOCK line)."
  (interactive)
  (let ((day (timesheet-at-point)))
    (if day
        (timesheet-invoice day)
      (message (concat "no " org-clock-string " at point!")))))

;;;###autoload
(defun timesheet-invoice-last ()
  "Calculate invoice last month."
  (interactive)
  (timesheet-invoice (timesheet-last-month)))

;; MUST leave point at end of line so inserting subtrees works as expected
;;;###autoload
(defun timesheet-goto-invoices ()
  "Goto (or create) the Invoices heading."
  (interactive)
  (let* ((invoices (org-find-exact-headline-in-buffer "Invoices" nil t)))
    (when invoices
      (goto-char invoices))
    (unless invoices
      (message "adding a Invoices heading...")
      (goto-char (point-max))
      (insert "\n* Invoices\n")
      (forward-line -1))
    (end-of-line)
    ))

;;;###autoload
(defun timesheet-goto-invoice (month)
  "Goto the invoice for the MONTH."
  (interactive)
  (timesheet-goto-invoices)  ;; 2013-06 Invoice #430
  (let* ((yyyy-mm (format-time-string "%Y-%m" month))
         (m-head (org-goto-first-child))
         invoice
         found
         firstinvoice)
    (unless m-head
      (setq firstinvoice t))
    (while m-head
      (let* ((ym (nth 4 (org-heading-components)))
             (prev (point)))
        (if (s-starts-with? yyyy-mm ym)
            (progn
              (setq m-head nil)
              (setq found t))
          (setq m-head (org-get-next-sibling)))
        (unless m-head ;; we have no children
          (goto-char prev)
          (end-of-line))
        )
      )
    (when found
      (when (org-goto-first-child)
        (let ((invoice-str (org-entry-get nil "Invoice")))
          (when (stringp invoice-str)
            (setq invoice (string-to-number invoice-str))
            )
          )
        (outline-up-heading 1)
        )
      (end-of-line)
      (insert "\n** old")
      (org-cut-subtree)
      (forward-line -1)
      (end-of-line))
    (unless invoice
      (setq invoice (timesheet-next-invoice)))
    (unless found
      (if firstinvoice
          (org-insert-heading)
        (org-insert-heading-after-current))
      (insert (concat yyyy-mm " Invoice #" (int-to-string invoice)))
      (when firstinvoice
        (org-do-demote)
        )
      )
    (org-insert-heading)
    (org-do-demote)
    (insert "Header")
    (org-set-property "Invoice" (int-to-string invoice))
    (org-insert-heading-after-current)
    (insert "Detail")
    (outline-up-heading 1) ;; go back to the invoice heading
    (end-of-line)
    (message (concat "Invoice " (int-to-string invoice)))
    invoice
    )
  )

(defun timesheet-american-month (month)
  "Using MONTH return Month DD, YYYY."
  (let* ((mname (format-time-string "%B" month))
         (m (nth 3 (decode-time month)))
         (space (if (< m 10) "" " "))
         (dd-yyyy (format-time-string "%e, %Y" month)))
    (concat mname space dd-yyyy)))

(defun timesheet-invoice (month)
  "Prepare invoice for the given MONTH."
  ;; if this is a new invoice, get the next invoice number
  ;; else preserve the existing number
  (let* ((yyyy-mm (format-time-string "%Y-%m" month))
         (customer (org-table-get-constant "customer"))
         (invoice-dir
          (expand-file-name yyyy-mm
                            (expand-file-name "Invoices"
                                              (expand-file-name customer
                                                                timesheet-company-dir))))
         (next-month (timesheet-first-day-next-month month))
         (invoice (timesheet-goto-invoice month))
         (invoice-str (int-to-string invoice))
         (all-project-times (timesheet-project-times))
         header-top
         detail-top
         row
         total-hours amount-due)
    (make-directory invoice-dir t)
    (org-goto-first-child) ;; Header
    (org-set-property "BillDate" (timesheet-american-month next-month))
    (org-set-property "DueDate" (timesheet-american-month (timesheet-last-day-in-month next-month)))
    (org-set-property "TotalHours" "0.00")
    (org-set-property "AmountDue" "0.00")
    (org-set-property "TABLE_EXPORT_FILE" (expand-file-name "header.tsv" invoice-dir))
    (org-set-property "TABLE_EXPORT_FORMAT" "orgtbl-to-tsv")
    (org-set-property "PDF" (concat "file://" invoice-dir "/Invoice-" invoice-str ".pdf"))
    ;; move below the properties
    (org-get-next-sibling)
    (forward-line -1)
    (org-cycle)
    (org-get-next-sibling)
    (forward-line -1)
    (end-of-line 1)
    (org-return)
    (setq header-top (point))
    (insert "#+BEGIN:\n")
    (insert "|---------------+--------------+-------------|\n")
    (insert "| /Remit To/    | /Date/       | /Invoice #/ |\n")
    (insert "|---------------+--------------+-------------|\n")
    (insert "|               |              |             |\n")
    (insert "|               |              |             |\n")
    (insert "|               |              |             |\n")
    (insert "|---------------+--------------+-------------|\n")
    (insert "| /Bill To/     | /Terms/      | /Due Date/  |\n")
    (insert "|---------------+--------------+-------------|\n")
    (insert "|               |              |             |\n")
    (insert "|               |              |             |\n")
    (insert "|               |              |             |\n")
    (insert "|---------------+--------------+-------------|\n")
    (insert "| /Total Hours/ | /Amount Due/ | /Enclosed/  |\n")
    (insert "|---------------+--------------+-------------|\n")
    (insert "|               |              |             |\n")
    (insert "|---------------+--------------+-------------|\n")
    (insert "#+TBLFM: @2$1='(concat \"$PROP_RemitTo1\");::@2$2='(concat \"$PROP_BillDate\");::@2$3=$PROP_Invoice;N::@3$1='(concat \"$PROP_RemitTo2\");::@4$1='(concat \"$PROP_RemitTo3\");::@6$1='(concat \"$PROP_BillTo1\");::@6$2='(concat \"$PROP_Terms\");::@6$3='(concat \"$PROP_DueDate\");::@7$1='(concat \"$PROP_BillTo2\");::@8$1='(concat \"$PROP_BillTo3\");::@10$1='(concat \"$PROP_TotalHours\");%3.2f::@10$2='(timesheet-currency $PROP_AmountDue);::\n")
    (insert "#+END:")
    (org-get-last-sibling) ; NOT (outline-previous-visible-heading)
    (forward-line)
    (org-cycle)
    (goto-char header-top)
    (forward-line)
    (org-get-next-sibling) ;; Detail
    (org-set-property "TABLE_EXPORT_FILE" (expand-file-name "detail.tsv" invoice-dir))
    (org-set-property "TABLE_EXPORT_FORMAT" "orgtbl-to-tsv")
    ;; move below the properties
    (org-get-next-sibling)
    (forward-line -1)
    (org-cycle)
    (org-get-next-sibling)
    (forward-line -1)
    (end-of-line 1)
    (org-return)
    (setq detail-top (point))
    (insert "#+BEGIN:\n")
    (insert "|----------+-------------+----------+--------+-----------|\n")
    (insert "| Date     | Description | Quantity |   Rate |    Amount |\n")
    (insert "|----------+-------------+----------+--------+-----------|\n")
    (insert "|          |             |          |        |           |\n")
    (insert "|----------+-------------+----------+--------+-----------|\n")
    (insert "| /Month/  |             |     0.00 |        |      0.00 |\n")
    (insert "|----------+-------------+----------+--------+-----------|\n")
    (insert "#+TBLFM: $4=$rate;%3.2f;::$5=$3*$rate;%3.2f;::@>$3=vsum(@2$3..@-1$3);%3.2f;::@>$4=string(\"/Total/\");::@>$5=vsum(@2$5..@-1$5);%3.2f::\n")
    (insert "#+END:")
    (org-get-last-sibling)
    (forward-line)
    (org-cycle)
    (setq row 4)
    (timesheet-table-goto detail-top 1 row)
    ;; insert project times
    (let (prev-day prev-projects prev-hours)
      (dolist (pt all-project-times)
        (let* ((ym (car pt)) ;; 2013-06
               (day-total (nth 1 pt))     ;; 2013-06-11 Tue = 6.50 hours
               (project-total (nth 2 pt)) ;; SuperProject = 6.50 hours
               day project hours)
          (when (string= ym yyyy-mm) ;; this month
            (message (format "entry %s == %s" ym yyyy-mm))
            (when (string-match "^\\([0-9\-]+\\) ... = \\([0-9\.]+\\) hours" day-total)
              (setq day (match-string 1 day-total))
              (setq hours (match-string 2 day-total))
              (when (string-match "^\\(.+\\) = \\([0-9\.]+\\) hours" project-total)
                (setq project (match-string 1 project-total)))
              (when (and prev-day (not (string= day prev-day))) ;; emit prev-day
                (insert prev-day)
                (org-table-next-field)
                (insert prev-projects)
                (org-table-next-field)
                (insert prev-hours)
                (org-table-insert-row t)
                (setq row (1+ row))
                (setq prev-projects nil))
              (if prev-projects ;; add project to this days project list
                  (setq prev-projects (concat prev-projects ", " project))
                (setq prev-projects project))
              (setq prev-day day)
              (setq prev-hours hours)
              ))))
      ;; emit last day (if there has been at least one day)
      (when (and prev-day prev-projects prev-hours)
        (insert prev-day)
        (org-table-next-field)
        (insert prev-projects)
        (org-table-next-field)
        (insert prev-hours))
      )
    ;; compute formulae in table
    (org-table-iterate)
    (org-table-align)
    (org-table-export)
    (timesheet-table-goto detail-top 3 (+ row 2))
    (setq total-hours (s-trim (substring-no-properties (caar (org-table-copy-region (point) (point))))))
    (timesheet-table-goto detail-top 5 (+ row 2))
    (setq amount-due (s-trim (substring-no-properties (caar (org-table-copy-region (point) (point))))))
    (org-get-last-sibling) ; Detail
    (org-get-last-sibling) ; Header
    ;; set properties
    (org-set-property "TotalHours" total-hours)
    (org-set-property "AmountDue" amount-due)
    (timesheet-table-goto header-top 2 16)
    ;; update table
    (org-table-iterate)
    (org-table-align)
    (org-table-export)
    (timesheet-run timesheet-invoice-script "-d" "-v" "-i" invoice-dir "-p")
    (message (concat yyyy-mm " Invoice #" invoice-str))
    ))

(defun timesheet-run (script &rest args)
  "Run a company specific SCRIPT (with optional ARGS) to generate the timesheet."
  (let ((buffer-name "*timesheet-run*"))
    (unless (file-executable-p script)
      (user-error "The script does not exist: %s" script))
    (with-output-to-temp-buffer buffer-name
      (let ((rv (apply 'call-process (append (list script nil buffer-name script) args))))
        (if (= rv 0)
            (message (format "%s successful" script ))
          (message (format "%s failed with %d" script rv)))
        rv))))

;;;###autoload
(defun timesheet-example ()
  "Setup a timesheet example with a customer called Yoyodyne."
  (interactive)
  (let* ((org-file "yoyodyne.org")
         (customer "Yoyodyne")
         (share-dir (file-name-as-directory (expand-file-name "share" timesheet-company-dir)))
         (customer-dir (file-name-as-directory (expand-file-name customer timesheet-company-dir)))
         (customer-org (expand-file-name org-file customer-dir)))
    (message (format "Making timesheet example with customer: %s" customer))
    (make-directory share-dir t)
    (make-directory customer-dir t)
    (dolist (f (timesheet-template-files))
      (if (s-ends-with? ".org" f)
          (copy-file f customer-org t)
        (copy-file f share-dir t)))
    ;; open a buffer with customer-org
    (find-file customer-org)))

(provide 'timesheet)
;;; timesheet.el ends here
