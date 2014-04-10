# timesheet.el

Timesheet management add-on for Emacs org-mode

## Usage

See the example [.emacs.d](example.emacs.d) configuration files for tips
on configuring Emacs for [MELPA](http://melpa.milkbox.net/#/getting-started).

Ensure TEXINPUT is set to at least (in your ~/.bashrc)
* ````export TEXINPUTS=.:/home/tmarble/.emacs.d/elpa/auctex-11.87.4/latex:````

Start by creating an example client...
* ````M-x timesheet-example````

You will be viewing the buffer yoyodyne.org that already has some example time entries. Create an invoice with
* ````M-x timesheet-invoice-this````

Next steps...
* Customize your "company" directory where invoices are generated, your next invoice number, etc.
  in ````M-x customize```` then search for ````timesheet````.
* Customize your name (in defs.tex) and logo (in logo.pdf).
* Learn more about org-mode!
  * org clocking http://orgmode.org/org.html#Clocking-work-time
  * TODO items http://orgmode.org/org.html#TODO-Items
  * org spreadsheets http://orgmode.org/org.html#The-spreadsheet

## Dependencies

This program depends on ...
* The following emacs packages (MELPA): ````s org auctex````
* The following Debian packages
  ```apt-get install emacs24 make gawk sed git tar rubber texlive-latex-extra texlive-fonts-recommended texlive-fonts-extra evince```

## Bugs

There are several known bugs:

* Only tested on GNU/Linux
* Entering the first time entry for a month results in the Month heading after the new entry (instead of before)
* Add a function to create an invoice for any clock time at point (not just this month or last month)

## License: GPLv3+

This work is Copyright © 2014 Informatique, Inc.

and is licensed under the [GPL](LICENSE) version 3 or any later version.
