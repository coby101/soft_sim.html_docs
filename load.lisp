;;;===========================================================================
;;; file:   src/generators/web-docs/load.lisp
;;; auth:   Coby Beck
;;; date:   2021-07-02
;;;
;;;---------------------------------------------------------------------------
;;;   load file for code associated with generating a application web-docs
;;;---------------------------------------------------------------------------  
;;;
;;;  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package simian)

(defpackage :html-docs
  (:nicknames :web-docs)
  (:use
   :cl
   :software-simian
   :documentation
   :interrogation
   :foundation
   :utilities
   :unparser
   :attribute
   :formula
   :interface
   :attribute
   :entity
   :relationship)
  (:export #:generate))

(in-package :web-docs)

(let ((sb-ext:*muffled-warnings* 'style-warning))
  (load-unparser (or (ignore-errors (natural-language foundation:*application*)) "english"))
  (load-unparser "html")
  (load-unparser "dot")
  (load (merge-pathnames "general.lisp" *load-truename*))
  (format t "~% loaded ~a" (merge-pathnames "general.lisp" *load-truename*))
  (load (merge-pathnames "er-diagrams.lisp" *load-truename*))
  (format t "~% loaded ~a" (merge-pathnames "er-diagrams.lisp" *load-truename*))
  (load (merge-pathnames "relationships.lisp" *load-truename*))
  (format t "~% loaded ~a" (merge-pathnames "relationships.lisp" *load-truename*))
  (load (merge-pathnames "entities.lisp" *load-truename*))
  (format t "~% loaded ~a" (merge-pathnames "entities.lisp" *load-truename*))
  (load (merge-pathnames "attributes.lisp" *load-truename*))
  (format t "~% loaded ~a" (merge-pathnames "attributes.lisp" *load-truename*))
  (load (merge-pathnames "interface.lisp" *load-truename*))
  (format t "~% loaded ~a" (merge-pathnames "interface.lisp" *load-truename*))
  (if (probe-file (merge-pathnames "local.lisp" *load-truename*))
      (load (merge-pathnames "local.lisp" *load-truename*))))

(format t "~&To generate full documentation at any time run (html-docs:generate)")

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
