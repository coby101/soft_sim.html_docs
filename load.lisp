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

(defpackage "WEB-DOCS"
  (:use "SIMIAN" "CL"))

(in-package :web-docs)

(let ((sb-ext:*muffled-warnings* 'style-warning))
  (load-unparser "english")
  (load-unparser "html")
  (load-unparser "dot")
  (load (merge-pathnames "general.lisp" *load-truename*))
  (load (merge-pathnames "er-diagrams.lisp" *load-truename*))
  (load (merge-pathnames "relationships.lisp" *load-truename*))
  (load (merge-pathnames "entities.lisp" *load-truename*))
  (load (merge-pathnames "attributes.lisp" *load-truename*))
  (load (merge-pathnames "interface.lisp" *load-truename*))
  (if (probe-file (merge-pathnames "local.lisp" *load-truename*))
      (load (merge-pathnames "local.lisp" *load-truename*))))

(format t "~&To generate full documentation at any time run (web-docs:write-html-docs)")


;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
