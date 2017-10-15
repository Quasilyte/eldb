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

;; Unexported, compile-time only definitions.
;; Makes compiled package cleaner,
;; and private API is better encapsulated.
(eval-when-compile
  ;; `pkg' type is a pending package that is not indexed.
  (defmacro eldb--make-pkg (code)
    `(vector ,code))
  (defmacro eldb--pkg-code (pkg)
    `(aref ,pkg 0))
  ;; `info' type is a package metadata unit.
  (defmacro eldb--make-info (id)
    `(vector ,id))
  (defmacro eldb--info-id (info)
    `(aref ,info 0))
  ;; Utils.
  (defmacro eldb--dohash (key-val-hash &rest body)
    (declare (indent defun))
    (let ((key (nth 0 key-val-hash))
          (val (nth 1 key-val-hash))
          (hash (nth 2 key-val-hash)))
      `(maphash (lambda (,key ,val) ,@body) ,hash)))
  )

(defconst eldb--data-dir (file-name-directory (buffer-file-name))
  "Data tarball directory.")

(defconst eldb--info-filename
  (format "%s/data/info.el" eldb--data-dir)
  "Info data filename.")

(defconst eldb--code-filename
  (format "%s/data/code.el" eldb--data-dir)
  "Code data filename.")

(defconst eldb--max-context 128
  "Max query context length in chars.")

(defvar eldb--packages (make-hash-table :test 'eq)
  "Map that contains freshly added packages.
These can not be queried until `eldb-data-save' is called.")

(defvar eldb--infodata nil
  "Mapping of package symbols to `info' objects.")

(defvar eldb--codedata nil
  "Vector of `read'-able strings.
Each string represents single package source code.")

;; Public:

(defun eldb-data-load ()
  "Load database files into Emacs, if they are not already loaded.
Should be called before any operations over corpus."
  (unless (eldb-data-loaded-p)
    (eldb-data-reload)))

(defun eldb-data-reload ()
  "Forced load of database files."
  (eldb--check-tar-bz)
  (shell-command
   (format "tar -jxf data.tar.bz2 -C \"%s\""
           eldb--data-dir))
  (mapc #'load-file
        (list eldb--info-filename
              eldb--code-filename)))

(defun eldb-data-loaded-p ()
  "Return non-nil if database files are loaded into Emacs."
  (not (not eldb--infodata)))

(defun eldb-data-reset ()
  "Reset database state objects.  Last saved database files are left untouched.
To make changes permanent, call `eldb-data-save' after this."
  (when (y-or-n-p "Reset database objects?")
    (setq eldb--packages (make-hash-table :test 'eq)
          eldb--infodata (make-hash-table :test 'eq)
          eldb--codedata [])))

(defun eldb-data-save ()
  "Write current database state to the filesystem.
Also updates loaded data state: makes newly added packages
available for queries."
  (eldb--check-tar-bz)
  (let ((infodata (make-hash-table :test 'eq))
        (codedata [])
        (code-list nil)
        (id 0))
    ;; Collect new data.
    (eldb--dohash (sym pkg eldb--packages)
      (puthash sym
               (eldb--make-info id)
               infodata)
      (push (eldb--pkg-code pkg) code-list)
      (setq id (1+ id)))
    ;; Merge old data.
    (eldb--dohash (sym info eldb--infodata)
      (unless (gethash sym infodata)
        (puthash sym
                 (eldb--make-info id)
                 infodata)
        (push (aref eldb--codedata (eldb--info-id info)) code-list)
        (setq id (1+ id))))
    ;; Prepare data.
    (setq codedata (vconcat (nreverse code-list)))
    ;; Write data files.
    (with-temp-file eldb--info-filename
      (insert "(setq eldb--infodata ")
      (prin1 infodata (current-buffer))
      (insert ")"))
    (with-temp-file eldb--code-filename
      (insert "(setq eldb--codedata ")
      (prin1 codedata (current-buffer))
      (insert ")"))
    ;; Combine data files into tarball.
    (shell-command
     (format "tar -cjf data.tar.bz2 data/info.el data/code.el -C \"%s\""
             eldb--data-dir))
    ;; Update state variables.
    (setq eldb--packages (make-hash-table :test 'eq)
          eldb--infodata infodata
          eldb--codedata codedata)))

(defun eldb-insert-forms (sym forms)
  "Associate SYM with FORMS.
FORMS must be a list of package forms with `progn' head.
For example, '(progn (list 1 2) (+ 1 2)) is valid.

Packages that were inserted, but not written to
filesystem yet are not available for querying.
To flush pending packages, call `eldb-data-save'."
  (unless (symbolp sym)
    (error "SYM must be a symbol, %S given" (type-of sym)))
  (unless (listp forms)
    (error "FORMS must be a list, %S given" (type-of forms)))
  (unless (eq 'progn (car forms))
    (error "FORMS head should be `progn', got %S" (car forms)))
  (let ((code (eldb--minify-code
               (prin1-to-string
                (eldb--minify-forms forms)))))
    ;; Add space padding at the both string sides.
    ;; This makes taking a substring with context pad simpler
    ;; as there is no corner cases.
    (setq code (concat (make-string eldb--max-context ? )
                       code
                       (make-string eldb--max-context ? )))
    (puthash sym
             (eldb--make-pkg code)
             eldb--packages)))

(defun eldb-insert-code (sym code)
  "Associate SYM with CODE.
CODE may have multiple Emacs Lisp forms.
Result will be wrapped in `progn' form.
See `eldb-insert-forms' for additional details."
  (unless (stringp code)
    (error "CODE must be string, %S given" (type-of code)))
  ;; Newlines are needed to avoid issues with leading
  ;; and trailing comments.
  (eldb-insert-forms sym (read (format "(progn \n%s\n)" code))))

(defun eldb-insert-files (pkg files)
  "Associate PKG symbol with text collected from FILES.
Concatenates file contents and calls `elds-insert-code'."
  (dolist (f files)
    (unless (file-exists-p f)
      (error "file `%s' does not exist" f)))
  (eldb-insert-code pkg
                    (with-temp-buffer
                      (dolist (f files)
                        (insert-file-contents f)
                        (insert "\n"))
                      (buffer-string))))

(defun eldb-insert-file (pkg file)
  "Associate PKG symbol with FILE contents.
Convenience wrapper around `eldb-insert-files' for single file."
  (eldb-insert-files pkg (list file)))

(defun eldb-query-corpus (regexp &optional context)
  "Collect REGEXP matches with CONTEXT pad.  Scans entire corpus."
  (let ((matches nil))
    (dotimes (i (length eldb--codedata))
      (push (eldb--query-code (aref eldb--codedata i) regexp context)
            matches))
    (apply 'append matches)))

;; Private:

;; STUB: will be implemented later.
(defun eldb--minify-forms (forms)
  "Make FORMS more compact without information loss.
Transformations are defined in `eldb' package description."
  forms)

(defun eldb--minify-code (code)
  "Apply compressions to CODE.
Transformations are defined in `eldb' package description."
  ;; Escape newlines.
  (replace-regexp-in-string "\n" "\\n" code nil t))

(defsubst eldb--context-size (context)
  "Return corrected CONTEXT argument."
  (if (numberp context)
      (min context eldb--max-context)
    ;; Default value.
    20))

(defun eldb--query-code (code regexp &optional context)
  "Collect lines from CODE that match REGEXP with CONTEXT pad.
CODE is expected to be package source text."
  (setq context (eldb--context-size context))
  (let ((pos 0)
        (matches nil))
    (while (setq pos (string-match regexp code pos))
      (push (substring code
                       (- pos context)
                       (+ (match-end 0) context))
            matches)
      (setq pos (match-end 0)))
    (nreverse matches)))

(defun eldb--check-tar-bz ()
  "Check that both `tar' and `bzip2' are available."
  (unless (and (executable-find "bzip2")
               (executable-find "tar"))
    (error "`bzip2' and `tar' are required")))

(provide 'eldb)

;;; eldb.el ends here
