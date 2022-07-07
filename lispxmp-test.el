;;; lispxmp-test.el --- test code for lispxmp.el     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Tomita Keisuke

;; Author: ril <fenril.nh@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((cort "7.2.0") (paredit "20191121.2328"))
;; Keywords:tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'lispxmp)
(require 'paredit)
(require 'cort)

(defun lispxmp-to-string (no-properties-p from)
  "Test function for `lispxmp.el'.
It inserts the FROM to the temp-buffer, run lispxmp, gets the
annotation string ang retuns.  If NO-PROPERTIES-P is non-nil,
lispxmp retuns the value with priperties of strings in
annotation,"
  (let ((lispxmp-string-no-properties no-properties-p))
    (with-temp-buffer
      (insert from)
      (lispxmp)
      (buffer-string))))

;;; unit test
(cort-deftest-generate lispxmp-test/string-without-properties :string=
  '(
    ((lispxmp-to-string t "\"a\\nb\" ; => ") "\"a\\nb\" ; => \"a\\nb\"
")
    ((lispxmp-to-string t "(propertize \"aaaa\" 'face 'match) ; => ") "(propertize \"aaaa\" 'face 'match) ; => \"aaaa\"
")
    ((lispxmp-to-string t "
 (list (propertize \"a\" 'face 'match)
 (propertize \"b\" 'face 'match)) ; => ")
     "
 (list (propertize \"a\" 'face 'match)
 (propertize \"b\" 'face 'match)) ; => (\"a\" \"b\")
")))

(cort-deftest-generate lispxmp-test/string-with-properties :string=
  '(((lispxmp-to-string nil "\"a\\nb\" ; => ") "\"a\\nb\" ; => \"a\\nb\"
")
    ((lispxmp-to-string nil "(propertize \"aaaa\" 'face 'match) ; => ") "(propertize \"aaaa\" 'face 'match) ; => #(\"aaaa\" 0 4 (face match))
")
    ((lispxmp-to-string nil "
 (list (propertize \"a\" 'face 'match)
 (propertize \"b\" 'face 'match)) ; => ")
     "
 (list (propertize \"a\" 'face 'match)
 (propertize \"b\" 'face 'match)) ; =>\
 (#(\"a\" 0 1 (face match)) #(\"b\" 0 1 (face match)))
")))

(cort-deftest-generate lispxmp-test/destructive-annotation-test :string=
  '(((lispxmp-to-string t "
         (setq l (list 1 2)) ; =>
         (setcar l 100)      ; =>
         l                   ; =>
")
     "
         (setq l (list 1 2)) ; => (1 2)
         (setcar l 100)      ; => 100
         l                   ; => (100 2)
")
    ((lispxmp-to-string t "
         (setq s (copy-sequence \"abcd\")) ; =>
         (aset s 0 ?A)                     ; =>
         s                                 ; =>
")
     "
         (setq s (copy-sequence \"abcd\")) ; => \"abcd\"
         (aset s 0 ?A)                     ; => 65
         s                                 ; => \"Abcd\"
")
    ((lispxmp-to-string t "
         (setq c (cons 1 2)) ; =>
         (setcar c 100)      ; =>
         c                   ; =>
")
     "
         (setq c (cons 1 2)) ; => (1 . 2)
         (setcar c 100)      ; => 100
         c                   ; => (100 . 2)
")))
(cort-deftest-generate lispxmp-test/pp-test :string=
  '(((lispxmp-to-string t "'((\"a\") \"b\" (\"c\"))
;;; =>
")
     "'((\"a\") \"b\" (\"c\"))
;;; => ((\"a\")
;;;     \"b\"
;;;     (\"c\"))
")
    ((lispxmp-to-string t "'((\"a\") \"b\" (\"c\"))
;; =>
")
     "'((\"a\") \"b\" (\"c\"))
;; => ((\"a\")
;;     \"b\"
;;     (\"c\"))
" )
    ((lispxmp-to-string nil "'((\"a\") \"b\" (\"c\"))
;;; =>
")
     "'((\"a\") \"b\" (\"c\"))
;;; => ((\"a\")
;;;     \"b\"
;;;     (\"c\"))
")
    ((lispxmp-to-string nil "'((\"a\") \"b\" (\"c\"))
;; =>
")
     "'((\"a\") \"b\" (\"c\"))
;; => ((\"a\")
;;     \"b\"
;;     (\"c\"))
"
     )
    ((lispxmp-to-string t "'a
;;; =>
")
     "'a
;;; => a
"
     )
    ((lispxmp-to-string t "'(\"a\")
;;; =>
")
     "'(\"a\")
;;; => (\"a\")
")))

(cort-deftest-generate lispxmp-test/pp-reexecute :string=
  '(((lispxmp-to-string t "'((\"a\") \"b\" (\"c\"))
;;; => ((\"a\")
;;;     \"b\"
;;;     (\"c\"))
")
     "'((\"a\") \"b\" (\"c\"))
;;; => ((\"a\")
;;;     \"b\"
;;;     (\"c\"))
"
     )
    ((lispxmp-to-string t "1
;;; => 1
;;; 2
;;;    3
")
     "1
;;; => 1
;;; 2
;;;    3
" )))

(provide 'lispxmp-test)
;;; lispxmp-test.el ends here
