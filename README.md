# timesheet.el

Timesheet management add-on for Emacs org-mode

NOTE: the latest stable version is `0.4.1` which should

correspond to this MELPA version: [![MELPA](https://melpa.org/packages/timesheet-badge.svg)](https://melpa.org/#/timesheet)

## Super Quick Example

If you want to see how **timesheet.el** works quickly without changing your
current configuration try the following in a test account (i.e. a user that
does NOT have `~/.emacs.d`).

1. Make sure you have all the required Dependencies (see below)
2. Copy the example Emacs configuration <br/>
   `cp -a src/github/tmarble/timesheet.el/example.emacs.d/ .emacs.d/`
3. Start Emacs <br/>
   `emacs -nw --debug-init`
4. Create an example timesheet <br/>
   `M-x timesheet-example`
5. Move the cursor to the first clock entry <br/>
`CLOCK: [2014-04-01 Tue 08:30]--[2014-04-01 Tue 11:00] => 2014-04-01 Tue 08:30 -- 2014-04-01 Tue 11:00 @  2.50`
6. Create an invoice (the example binding of `<f10> I` is to 'timesheet-invoice-at-point). You should see the invoice for Yoyodyne in USD. <br/>
  `<f10> I`
7. Copy the GBP and EUR examples (`yoyodyne-uk.org`, `yoyodyne-fr.org`) to the home directory: <br/>
`cp .emacs.d/elpa/timesheet-*/share/yoyodyne-*.org ~/`
8. Visit yoyodyne-uk.org, move the cursor to first clock entry, create an invoice
for Yoyodyne England: <br/>
  `<f10> I`
9. Visit yoyodyne-fr.org, move the cursor to first clock entry, create an invoice
for Yoyodyne France: <br/>
  `<f10> I`

*voila!*

## Usage

See the example [.emacs.d](example.emacs.d) configuration files for tips
on configuring Emacs for [MELPA](http://melpa.milkbox.net/#/getting-started).

Ensure TEXINPUT is set. Typically all you need to do is
`(require 'preview)` as auctex will set this for you.

**timesheet.el** expects that you'll use one file per client to be
invoiced, with client-related metadata stored in file-level properties
(see below). The file can contain any number of project-related
headings, but all TODOs and associated time-clocks are expected to be
found at level four (see example files).

A typical workflow involves clocking in and out of tasks during the
course of the day, and then calling `timesheet-calc-today` at the end
of the day. This will automatically generate a top-level heading
called "Timesheet", containing a breakdown of time spent on various
tasks. If it ever becomes necessary to update work done on previous
days, simply put point on the updated CLOCK line and call
`timesheet-calc-at-point`.

When it comes time to create an invoice, call the command
`timesheet-invoice-this`, which creates or finds a top-level heading
called "Invoices", and creates an invoice for the current month, using
the data from the "Timesheet" heading. You can also use
`timesheet-invoice-at-point` to re-create past invoices.

**timesheet.el** can also create weekly breakdowns of time spent
per-project, using `timesheet-weekly-this` or
`timesheet-weekly-at-point`. See the example files for possible
keybindings.

Next steps...
* Customize your "company" directory where invoices are generated, your next invoice number, etc.
  in ````M-x customize```` then search for ````timesheet````.
* Customize your name (in defs.tex) and logo (in logo.pdf).
* Learn more about org-mode!
  * org clocking http://orgmode.org/org.html#Clocking-work-time
  * TODO items http://orgmode.org/org.html#TODO-Items
  * org spreadsheets http://orgmode.org/org.html#The-spreadsheet

## Customizing each client

You can customize certain constants and properties for each
client by defining the following at thte the top of the org file:

The properties are all used to fill in common parts of the invoice.

The constants are:
* customer: used to determine the directory to store the invoice
* rate: hourly rate (NOTE: this could be abused to be a daily rate if you record one "hour" to represent quantity one at this rate)
* roundmin: the number of minutes to round to. By default this is 15 minutes. The value could be between [1..60] inclusive.

```
#+CONSTANTS: customer=Yoyodyne rate=20.00 roundmin=10
#+PROPERTY: RemitTo1 Emacs Consultants LLC
#+PROPERTY: RemitTo2 123 Any Street
#+PROPERTY: RemitTo3 Minneapolis, MN 55401
#+PROPERTY: Terms Net 30
#+PROPERTY: BillTo1 Yoyodyne, Inc.
#+PROPERTY: BillTo2 451 Franklin Street
#+PROPERTY: BillTo3 Boston, MA 02110-1301 USA
```


## Currency

An initial attempt has been made to make **timesheet.el** aware of different
currencies. Currently **timesheet.el** understands USD, GBP and EUR.
Specify a currency by adding a proprty at the top of your org file:
`#+PROPERTY: Currency GBP`

See the `yoyodyne-uk.org` and `yoyodyne-fr.org` files for examples.

*NOTE:* using EUR requires that main.tex include `\usepackage{eurosym}`
(which is now part of the default main.tex template).

*NOTE:* no effort has been made to localize the use and location of
commas and periods in currency strings.

For developers interesting in thinking more about this I have found
the following links:

* Regarding currency conversions
 + https://openexchangerates.org/
 + https://oxr.readme.io/docs/supported-currencies
 + http://www.verona.se/projects/curconv.html
* Regarding commas and periods
 + http://stackoverflow.com/questions/35661173/how-to-format-table-fields-as-currency-in-org-mode
 + http://emacs.stackexchange.com/questions/15076/how-to-add-dollar-amounts-in-org-mode
* Regarding units (leveraging calc)
 + https://www.gnu.org/software/emacs/manual/html_node/calc/Basic-Operations-on-Units.html
 + https://www.gnu.org/software/emacs/manual/html_node/calc/The-Units-Table.html
* Related to numbers and emacs
 + https://www.emacswiki.org/emacs/SpellNumber
* LaTex special characters
 + https://en.wikibooks.org/wiki/LaTeX/Special_Characters
 + http://library.caltech.edu/etd/symbols-a4.pdf

## Dependencies

This program depends on ...
* The following emacs packages (MELPA): ````s org auctex````
* The following Debian packages
  ```apt-get install emacs24 make gawk sed git tar rubber texlive-latex-extra texlive-fonts-recommended texlive-fonts-extra evince```

## Bugs

There are several known bugs:

* Only tested on GNU/Linux

## License: GPLv3+

This work is Copyright © 2014-2016 Informatique, Inc.

and is licensed under the [GPL](LICENSE) version 3 or any later version.
