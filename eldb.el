;;; eldb --- Emacs Lisp code corpus database

;; Author: Iskander Sharipov <quasilyte@gmail.com>
;; Keywords: lisp, corpus

;;; Commentary:

;; This package comes with `eldb--data-filename' file,
;; which contains structured Emacs Lisp code corpus.
;;
;; Public functions give you a set of operations
;; that can be performed on that corpus.
;;
;; The most valueable part is the corpus itself, of course.
;;
;; For documetation, visit upstream repository:
;;   https://github.com/Quasilyte/eldb

;;; Code:

;; To learn how exactly data is stored,
;; see documentation that is available at `eldb' github repo.
(defconst eldb--data-filename
  (expand-file-name
   "data.el"
   (file-name-directory (buffer-file-name)))
  "Database file name.")

(defconst eldb--max-context 128
  "Max query context length in chars.")

(defvar eldb--data nil
  "Database state object.")

;; Public:

(defun eldb-data-load ()
  "Load database file into Emacs, if it is not already loaded.
Should be called before any operations over corpus."
  (unless (eldb-data-loaded-p)
    (eldb-data-reload)))

(defun eldb-data-reload ()
  "Forced load of database file."
  (load-file eldb--data-filename))

(defun eldb-data-loaded-p ()
  "Return non-nil if database file is loaded into Emacs."
  (not (not eldb--data)))

(defun eldb-data-reset ()
  "Reset database object state.  Last saved database file is left untouched.
To make changes permanent, call `eldb-data-save' after this."
  (when (y-or-n-p "Reset database object?")
    (setq eldb--data (make-hash-table :test 'eq))))

(defun eldb-data-save ()
  "Write current database state to the filesystem."
  (when (and (eldb-data-loaded-p)
             (not (hash-table-p eldb--data)))
    (error "`eldb--data' is corrupted"))
  (with-temp-file eldb--data-filename
    ;; This code avoids big string allocation.
    ;; `prin1' writes dirrectly to the buffer.
    (insert "(defconst eldb--data ")
    (prin1 eldb--data (current-buffer))
    (insert ")")))

(defun eldb-insert-forms (pkg forms)
  "Associate PKG symbol with FORMS.
FORMS must be a list of package forms with `progn' head.
For example, '(progn (list 1 2) (+ 1 2)) is valid."
  (unless (symbolp pkg)
    (error "PKG must be a symbol, %S given" (type-of pkg)))
  (unless (listp forms)
    (error "FORMS must be a list, %S given" (type-of forms)))
  (unless (eq 'progn (car forms))
    (error "FORMS head should be `progn', got %S" (car forms)))
  (let ((text (eldb--minify-text
               (prin1-to-string
                (eldb--minify-forms forms)))))
    ;; Add space padding at the both string sides.
    ;; This makes taking a substring with context pad simpler
    ;; as there is no corner cases.
    (setq text (concat (make-string eldb--max-context ? )
                       text
                       (make-string eldb--max-context ? )))
    (puthash pkg text eldb--data)))

(defun eldb-insert-text (pkg text)
  "Associate PKG symbol with TEXT.
TEXT may have multiple Emacs Lisp forms.
Result will be wrapped in `progn' form."
  (unless (stringp text)
    (error "TEXT must be string, %S given" (type-of text)))
  (eldb-insert-forms pkg (read (format "(progn %s)" text))))

(defun eldb-query-package (pkg-regexp regexp &optional context)
  "For packages that match PKG-REGEXP, collect REGEXP matches with CONTEXT pad."
  (let ((matches nil))
    (maphash
     (lambda (pkg text)
       (when (string-match-p pkg-regexp (symbol-name pkg))
         (push (eldb--query-text text regexp context)
               matches)))
     eldb--data)
    (apply 'append matches)))

;; Private:

;; STUB: will be implemented later.
(defun eldb--minify-forms (forms)
  "Make FORMS more compact without information loss.
Transformations are defined in `eldb' package description."
  forms)

;; STUB: will be implemented later.
(defun eldb--minify-text (text)
  "Apply compressions to TEXT.
Transformations are defined in `eldb' package description."
  text)

(defsubst eldb--context-size (context)
  "Return corrected CONTEXT argument."
  (if (numberp context)
      (min context eldb--max-context)
    ;; Default value.
    20))

(defun eldb--query-text (text regexp &optional context)
  "Collect lines from TEXT that match REGEXP with CONTEXT pad.
TEXT is expected to be package source text."
  (setq context (eldb--context-size context))
  (let ((pos 0)
        (matches nil))
    (while (setq pos (string-match regexp text pos))
      (push (substring text
                       (- pos context)
                       (+ (match-end 0) context))
            matches)
      (setq pos (match-end 0)))
    (nreverse matches)))

(provide 'eldb)

;;; eldb.el ends here
