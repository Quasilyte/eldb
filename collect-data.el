;; This code will no be needed when API and storage format are
;; stabilized. We are not there yet, so this provided a way
;; to quickly re-load Emacs core packages into database.

(require 'eldb)

(eldb-data-reset)

(setq emacs-home (read-directory-name "Emacs home dir: "))

(setq lisp-dir (concat emacs-home "lisp/"))

(defun emacs-lisp/ (file) (concat lisp-dir "emacs-lisp/" file))

(dolist (pkg '(advice
               avl-tree
               backquote
               benchmark
               bindat
               bytecomp
               byte-opt
               cconv
               chart
               check-declare
               checkdoc
               cl
               cl-extra
               cl-generic
               cl-indent
               cl-lib
               cl-loaddefs
               cl-macs
               cl-preloaded
               cl-seq
               copyright
               crm
               cursor-sensor
               debug
               derived
               disass
               easymenu
               easy-mmode
               edebug
               eieio-base
               eieio-compat
               eieio-core
               eieio-custom
               eieio-datadebug
               eieio
               eieio-opt
               eieio-speedbar
               eldoc
               elint
               elp
               ert
               ert-x
               ewoc
               find-func
               generator
               generic
               gv
               helper
               inline
               let-alist
               lisp-mnt
               lisp-mode
               macroexp
               map
               nadvice
               package
               package-x
               pcase
               pp
               re-builder
               regexp-opt
               regi
               ring
               rx
               seq
               shadow
               smie
               subr-x
               syntax
               tabulated-list
               testcover
               thunk
               timer
               tq
               trace
               unsafep
               warnings
               ))
  (eldb-insert-file pkg
                    (emacs-lisp/ (format "%s.el" pkg))))

(eldb-insert-file 'lisp-float-type (emacs-lisp/ "float-sup.el"))

(eldb-data-save)
