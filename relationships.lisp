;;;===========================================================================
;;; file:   src/generators/web-docs/relationships.lisp
;;; auth:   Coby Beck
;;; date:   2021-07-16
;;;
;;;---------------------------------------------------------------------------
;;;   general functions for generating web documentation of relationship objects
;;;---------------------------------------------------------------------------  
;;;
;;;  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :web-docs)


(defmethod document :before ((rel binary-relationship) (clarity (eql :detailed))
                             (format (eql :html)) &optional stream)
  (write-html-header stream
          :title (format nil "~a Relationship Technical Details"
                         (short-name rel))
          :meta (list :file-source
                      "write-html-docs in soft-sim/src/generators/relationships.lisp")
          :style *documentation-stylesheet*)
  (format stream (html::heading 1 (format nil "~a" (short-name rel)))))


(defmethod document :after ((rel binary-relationship) (clarity (eql :detailed))
                            (format (eql :html)) &optional stream)
  (write-html-footer stream))


(defmethod write-blurb ((rel binary-relationship) &optional stream)
  (let* ((blurb (make-links-to-object-reference (entity (rhs rel))
                  (blurb rel) :name-string (short-name (entity (rhs rel)))
                  :postscript " records")))
    (write-synopsis (make-links-to-object-reference (entity (lhs rel))
                       blurb :name-string (short-name (entity (lhs rel)))
                       :preamble "parent ")
                    stream)))

(defmethod document ((rel binary-relationship) (clarity (eql :detailed))
                     (format (eql :html)) &optional stream)
  (write-blurb rel stream)
  (format stream "<p>")
  (write-designation-section rel stream)
  (format stream "<br>")
;  (write-descriptor-section rel stream))
  
  (write-table-section stream (format nil "~a Data Sheet" (short-name (lhs rel))) 2
                       (datasheet-headings)
                       (datasheet-rows (lhs rel)))  
  (write-table-section stream (format nil "~a Data Sheet" (short-name (rhs rel))) 2
                       (datasheet-headings)
                       (datasheet-rows (rhs rel))))

(defun describe-dependency (relation)
  (let ((my-plural (short-plural relation))
        relationship relationship-name my-relation-singular my-relation-plural)
    (if (typep relation 'shared-relation)
        (progn
          (setf relationship (car (relationships relation)))
          (setf relationship-name (short-name relationship))
          (setf my-relation-singular
                (format-english-or-list
                 (mapcar #'short-name (my-relations relation))))
          (setf my-relation-plural
                (format-english-or-list
                 (mapcar #'short-plural (my-relations relation)))))
        (progn
          (setf relationship (my-relationship relation))
          (setf relationship-name (short-name relationship))
          (setf my-relation-singular (short-name (my-relation relation)))
          (setf my-relation-plural (short-plural (my-relation relation)))))
    (if (typep relationship 'specialization)
      (cond
        ((eq relation (rhs relationship))
         (format nil "~a can not exist outside of the ~a relationship" my-plural relationship-name))
        ((not (complete? relationship))
         (format nil "~a are independent from ~a" my-plural my-relation-plural))
        (t (format nil "~a need to have a subclass record but it need not be ~a"
                         my-plural (with-article my-relation-singular))))
      (ecase (dependency relation)
        (:independent (format nil "~a are independent from ~a" my-plural my-relation-plural))
        (:changeable (format nil "~a need ~a but can move from one to another"
                             my-plural (with-article my-relation-singular)))
        (:complete (format nil "~a can not exist outside of the ~a relationship"
                           my-plural relationship-name))
        (:dependent (format nil "~a are required to be in a ~a relationship"
                            my-plural relationship-name))))))


(defmethod datasheet-rows ((rel relation))
  (list (list "Entity"
              (make-links-to-object-reference
               (entity rel) (short-plural (entity rel))))
        (list "Multiplicity" (format nil "~a" (multiplicity rel)))
        (list "Dependency" (describe-dependency rel))
        (list "Navigable?"
              (if (navigable? rel)
                  (format nil "Users can get to ~a from ~a"
                          (with-article (format-english-or-list (mapcar #'short-name (my-relations rel))))
                          (with-article (short-name rel)))
                  (format nil "~a are not reachable from ~a"
                          (format-english-list (mapcar #'short-plural (my-relations rel)))
                          (short-plural rel))))
        (list "Ordering" (or (ordering rel) (format nil "~a have no sorting order" (short-plural rel))))
        (list "Constraints"
              (if (constraints rel)
                  (format nil "~{~a~^<br>~}"
                          (mapcar #'english::unparse-expression (constraints rel)))
                  "there are no constraints on this side of the relationship"))))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
