;;;===========================================================================
;;; file:   src/generators/web-docs/attributes.lisp
;;; auth:   Coby Beck
;;; date:   2021-07-02
;;;
;;;---------------------------------------------------------------------------
;;;   general functions for generating web documentation of attribute objects
;;;---------------------------------------------------------------------------  
;;;
;;;  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :web-docs)

#|
user-attributes
calculated-attribute
inherited-attribute
summary-attribute
multi-valued-attribute
audit-attribute
db-key
summary-attribute
|#

(defmethod document :before ((att attribute) (clarity (eql :detailed))
                             (format (eql :html)) &optional stream)
  (write-html-header stream
          :title (format nil "~a ~a Attribute Technical Specifications"
                         (short-name (my-entity att)) (short-name att))
          :meta (list :file-source
                      "write-html-docs in soft-sim/src/generators/web-docs/attributes.lisp")
          :style *documentation-stylesheet*)
  (format stream (html:heading 1 (format nil "~a.~a" (name (my-entity att)) (name att)))))

(defmethod write-blurb ((att attribute) &optional stream)
  (let* ((blurb (make-links-to-object-reference (my-entity att) (blurb att)
                      :preamble "the " :postscript " table")))
    (write-synopsis blurb stream)))

(defmethod write-blurb ((att summary-attribute) &optional stream)
  (let* ((blurb (make-links-to-object-reference (my-entity att) (blurb att)))
         (sum-source (source att)))
    (write-synopsis
     (make-links-to-object-reference (my-entity sum-source) blurb
        :name-string (short-name (my-entity sum-source)) :postscript " records")
     stream)))

(defmethod write-blurb ((att foreign-key) &optional stream)
  (let* ((blurb (make-links-to-object-reference (my-entity att) (blurb att)
                                                :postscript " table" :preamble "the "))
         (target (my-entity (source att))))
    (write-synopsis
     (make-links-to-object-reference target blurb
        :name-string (short-plural target) :preamble "the " :postscript " table")
     stream)))

(defmethod write-blurb ((att arc-key) &optional stream)
  (let* ((blurb (make-links-to-object-reference (my-entity att) (blurb att)
                                                :postscript " table" :preamble "the ")))
;;         (target (my-entity (source att))))
    (write-synopsis blurb
;;     (make-links-to-object-reference target blurb
;;       :name-string (short-plural target) :preamble "the " :postscript " table")
     stream)))

(defmethod write-blurb ((att multi-valued-attribute) &optional stream)
  (let* ((blurb (make-links-to-object-reference (my-entity att) (blurb att)))
         (target (child-entity att)))
    (write-synopsis
     (make-links-to-object-reference target blurb
        :name-string (short-plural target) :preamble "the " :postscript " table")
     stream)))

(defmethod document ((att attribute) (clarity (eql :detailed))
                     (format (eql :html)) &optional stream)
  (write-blurb att stream)
  (format stream "<p>")
  (write-designation-section att stream)
  (format stream "<br>")
  (write-descriptor-section att stream)

  
  (write-table-section stream "Data Sheet" 2
          (datasheet-headings)
          (datasheet-rows att))
  )

(defmethod datasheet-rows ((att attribute))
  (list (list "Logical Type" (name (logical-type att)))
        (list "Default Value" (typecase (default-value att)
                                (null "no default value")
                                (formula (english:unparse-expression (default-value att)))
                                (attribute (format nil "~a from the related ~a record"
                                                   (name (default-value att))
                                                   (name (my-entity (default-value att)))))
                                (t (formatted-value (logical-type att) (default-value att)))))
        (list "My Entity"
              (make-links-to-object-reference
               (my-entity att) (short-plural (my-entity att))))
        (list "Indexed?" (if (indexed? att) "yes" "no"))
        (list "Read Only?" (if (or (read-only? (my-entity att)) (read-only? att)) "yes" "no"))
        (list "Data Type" (symbol-name (data-type att)))
        (list "Domain Properties"
              (typecase (domain att)
                (referential-enumeration
                 (format-values-source att))
                (static-enumeration
                 (format nil "values are one of ~a"
                         (format-acceptable-value-list (legal-values (domain att)))))
                (t "restricted by datatype")))
        (list "Other Constraints" (if (constraints att)
                                      (format nil "~{~a~^<br>~}"
                                              (mapcar #'english:unparse-expression
                                                      (mapcar #'formula (constraints att))))
                                      "no other constraints"))))

(defmethod format-values-source ((att attribute))
  (format nil "values come from the ~a field in the ~a table"
          (make-links-to-object-reference
           (data-source (domain att)) (name (data-source (domain att))))
          (make-links-to-object-reference
           (my-entity (data-source (domain att)))
           (short-plural (my-entity (data-source (domain att)))))))

(defmethod format-values-source ((att arc-key))
  (format nil "values come from the primary key field in the ~a table"
          ;; (make-links-to-object-reference
          ;;  (my-entity (data-source (domain att)))
          ;;  (short-plural (my-entity (data-source (domain att)))))
          (format-english-or-list (mapcar #'short-plural (mapcar #'my-entity (source att))))))

(defmethod datasheet-rows ((att calculated-attribute))
  (list (list "Logical Type" (name (logical-type att)))
        (list "Formula" (format nil "~a" (english:unparse (formula att))))
        (list "My Entity"
              (make-links-to-object-reference
               (my-entity att) (short-plural (my-entity att))))
        (list "Indexed?" (if (indexed? att) "yes" "no"))
        (list "Data Type" (symbol-name (data-type att)))))

(defmethod document :after ((att attribute) (clarity (eql :detailed))
                            (format (eql :html)) &optional stream)
  (format stream "~%<br>~%")
  (format stream (home-page-link "Back to Top Level"))
  (write-html-footer stream))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
