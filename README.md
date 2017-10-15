# eldb
Emacs Lisp corpus. Code collected from many-many projects for you to query it!

## Terminology

**Code** - textual/printed Emacs Lisp representation that is suitable
for `read` function.

**Form** - Emacs Lisp expression.

**Package** - an object which holds code and associated metadata. Usually
used to refer to newly added packages that were not saved to the database yet.

**Database** - a set of all packages that are stored at filesystem,
can be loaded into memory.

**Corpus** - combined (concatenated) code from entire database.

**Package info** - package metadata that is used to associate
package symbol with it's data.

**Package code** - like `code`, but may be preprocessed/compressed.
In particular, multi-line strings may be moved to `text` component.

**Package text** - long strings that are extracted from `Package code`.
