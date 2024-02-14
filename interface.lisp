;;;===========================================================================
;;; file:   src/generators/web-docs/interface.lisp
;;; auth:   Coby Beck
;;; date:   2021-10-08
;;;
;;;---------------------------------------------------------------------------
;;;   general functions and parameters for generating
;;;   web documentation of application iterfaces
;;;---------------------------------------------------------------------------  
;;;
;;;  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :web-docs)


(defmethod document ((view view) (clarity (eql :detailed)) (format (eql :html)) &optional stream)
  (write-html-header stream
    :title (format nil "~a View Technical Specifications"
                   (short-name view))
    :meta (list :file-source
                "document method in soft-sim/src/generators/web-docs/interface.lisp")
    :link (list :rel "stylesheet" :type "text/css"
                :href (strcat "../" (file-namestring (documentation-css-filepath)))))
  (format stream (html:heading 1 (format nil "Technical Detail for the ~a View (~a)"
                                         (short-name view) (name view))))

  (format stream
          "<p align=\"center\">The ~a view is one of ~d end user views ~
           in the ~a application.~a<hr border=1px>" (short-name view)
           (length (views *application*))
           (html:link (long-name *application*) "../application.html")
           (describe-source view (natural-language *application*)))

  (write-synopsis (blurb view (natural-language *application*)) stream)
  (format stream "<br>")
  (write-designation-section view stream)
  (format stream "<br>")
  (write-descriptor-section view stream)
  (format stream "<br>")
  (insert-er-graph view stream)
  (write-table-section stream "Accessible Entities" 2
          (aspect-table-headings)
          (aspect-table-rows view))
  (write-html-footer stream))

(defun aspect-table-headings ()
  (html:tag
   "tr"
     (with-nesting
       (strcat (html:tag "th" "Entity Aspect") (line-feed)
               (html:tag "th" "Allowed Actions") (line-feed)
               (html:tag "th" "Record Filters") (line-feed)
               (html:tag "th" "Sorting") (line-feed)
               (html:tag "th" "Viewable Attributes") (line-feed)
               (html:tag "th" "Modifiable Attributes") (line-feed)))))

(defmethod aspect-table-rows ((view view))
  (mapcar #'(lambda (asp)
              (with-nesting
                (list (list (html:link (long-name (entity asp))
                                        (strcat "../" (document-link asp)))
                            :align "left")
                      (list (format nil "~(~a~)" (format-english-list (operations asp)))
                            :align "left")
                      (list (unparse (filters asp) :english) :align "left")
                      (list (unparse (ordering asp) :english) :align "left")
                      (list (format-english-list (mapcar #'(lambda (att) (unparse-expression att :english))
                                                         (accessible-attributes asp :read)))
                            :aligh "left")
                      (list (format-english-list (mapcar #'name (accessible-attributes asp :write)))
                            :aligh "left"))))
          (reverse (aspects view))))

(defmethod document ((aspect aspect) (clarity (eql :detailed)) (format (eql :html)) &optional stream)
  (write-html-header stream
    :title (format nil "Technical Specifications of the ~a Aspect of ~a"
                   (long-name (view aspect)) (short-plural (entity aspect)))
    :meta (list :file-source
                "document method in soft-sim/src/generators/web-docs/interface.lisp")
    :link (list :rel "stylesheet" :type "text/css"
                :href (strcat "../" (file-namestring (documentation-css-filepath)))))
  (format stream (html:heading 1 (format nil "Technical Detail for the ~a aspect of the ~a entity"
                                         (long-name (view aspect)) (name (entity aspect)))))

  (format stream
          "<p align=\"center\">The ~s aspect of the ~a entity is one of ~d defined aspects ~
           in the ~a application.~a<hr border=1px>" (long-name (view aspect))
           (name (entity aspect)) (apply #'+ (mapcar #'length (mapcar #'aspects (views *application*))))
           (html:link (long-name *application*) "../application.html")
           (describe-source (view aspect) (natural-language *application*)))
  
  (write-synopsis (blurb aspect (natural-language *application*)) stream)
  (format stream "<br>")
  (write-table-section stream "Data Sheet" 2
          (datasheet-headings)
          (datasheet-rows aspect))
  (format stream "<br>")
  (write-code-section aspect stream)
  (format stream (home-page-link "Back to Top Level"))
  (write-html-footer stream))

(defmethod datasheet-rows ((asp aspect))
  (list (list "My View" (make-links-to-object-reference (view asp) (long-name (view asp))))
        (list "Entity" (make-links-to-object-reference (entity asp) (short-plural (entity asp))))
        (list "Filters" (unparse (filters asp) :english))
        (list "Default Sort Order" (unparse (ordering asp) :english))
        (list "Allowed Operations" (format-english-list (operations asp)))
        (list "List Layout" (if (list-panel asp) (unparse (list-panel asp) :html) "no layout defined"))
        (list "Detail Layout" (if (detail-panel asp) (unparse (detail-panel asp) :html) "no layout defined"))
        (list "Edit Record Layout" (if (edit-panel asp) (unparse (edit-panel asp) :html) "no layout defined"))
        (list "Create Record Layout" (if (add-panel asp) (unparse (add-panel asp) :html) "no layout defined"))
        (list "Context Layout" (if (context-panel asp) (unparse (context-panel asp) :html) "no layout defined"))
        (list "Search Layout" (if (search-panel asp) (unparse (search-panel asp) :html) "no layout defined"))))

(defmethod write-code-section ((aspect aspect) &optional stream)
  (let ((content (get-code-examples aspect)))
    (if content
      (write-table-section stream "Generated Implementation Code" 2
         (code-sample-table-headings)
         (code-sample-rows content))
      (warn "no code samples to display"))))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
