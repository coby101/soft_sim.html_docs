;;;===========================================================================
;;; file:   src/generators/web-docs/entities.lisp
;;; auth:   Coby Beck
;;; date:   2021-07-02
;;;
;;;---------------------------------------------------------------------------
;;;   general functions and parameters for generating web documentation
;;;---------------------------------------------------------------------------  
;;;
;;;  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :web-docs)

(defmethod document :before ((ent entity) (clarity (eql :detailed))
                             (format (eql :html)) &optional stream)
  (write-html-header stream
          :title (format nil "~a Entity Technical Specifications"
                         (short-name ent))
          :meta (list :file-source
                      "write-html-docs in soft-sim/src/generators/web-docs/entities.lisp")
          :link (list :rel "stylesheet" :type "text/css"
                      :href (strcat "../" (file-namestring (documentation-css-filepath)))))
  (format stream (html::heading 1 (format nil "Technical Detail for the ~a Entity "
                                         (name ent)))))

(defmethod document :after ((ent entity) (clarity (eql :detailed))
                            (format (eql :html)) &optional stream)
  (write-html-footer stream))

(defmethod write-blurb ((ent audit-entity) &optional stream)
  (let ((blurb (make-links-to-object-reference
                (find-audited-table ent) (blurb ent)
                :name-string (short-plural (find-audited-table ent)))))
    (write-synopsis blurb stream)))

(defmethod write-blurb ((ent weak-entity) &optional stream)
  (let ((blurb (if (keep-history? ent)
                   (make-links-to-object-reference
                    (or (find-audit-table ent) ent) (blurb ent)
                    :name-string (short-plural (or (find-audit-table ent) ent)))
                   (blurb ent))))
    (write-synopsis
     (make-links-to-object-reference (owner ent) blurb :preamble "the " :postscript " entity"
                                     :name-string (name (owner ent)))
     stream)))

(defmethod write-blurb :before ((ent entity) &optional stream)
  (format stream
          "<p align=\"center\">The ~a entity is ~a (not including internal system entities) ~
           in the ~a application.~a<hr border=1px>" (short-plural ent)
           (html::link (format nil "one of ~d entities" (length (application-entities *application*)))
                       "../entities.html")
           (home-page-link)
           (describe-source ent)))

(defmethod write-blurb ((ent entity) &optional stream)
  (let ((blurb (if (keep-history? ent)
                   (make-links-to-object-reference
                    (or (find-audit-table ent) ent) (blurb ent) :name-string (short-plural (or (find-audit-table ent) ent)))
                   (blurb ent)))
        (seed-data (ignore-errors (seed-data ent))))
    (when seed-data
      (setf blurb
            (strcat blurb
                    (format nil "<br><br>~s was defined with seed data, which can be accessed ~a ~
                                or viewed at the botom of this page in table format."
                            (short-name ent) (html::link "here in CSV form" (strcat (name ent) ".csv"))))))
    (write-synopsis blurb stream)))

(defun describe-entity-constraint-context (event)
  (format nil "(checked on ~(~a~))" (or event "create and update")))

(defmethod write-constraint-section ((ent entity) &optional stream)
  (let ((content
         (if (constraints ent)
             (format nil "~{<tr><td>~a</td></tr>~}"
                     (mapcar #'(lambda(c)
                                 (format nil "~a ~a" (english::unparse-expression (formula c))
                                         (if (typep c 'constraint)
                                             (describe-entity-constraint-context (event c))
                                             "")))
                             (constraints ent)))
             "<tr><td width=\"800px\">There are no record level constraints</tr></td>")))
    (format stream
            (write-data-section
             "Constraints:" (apply #'html::tag "table" content *plain-table-attributes*)))))

(defun state-headings ()
    (html::tag "tr"
     (strcat
      (html::tag "td" "<b>State</b>" :align "center")
      (html::tag "td" "<b>Predicate</b>" :align "center"))))

(defmethod write-state-section ((ent entity) &optional stream)
  (let ((content
         (if (states ent)
             (format nil "~{<tr>~a</tr>~}"
                     (list* (state-headings)
                            (mapcar #'(lambda(st)
                                        (format nil "~a ~a"
                                                (html::tag "td" (short-name st))
                                                (html::tag "td" (english::unparse-expression (predicate st)))))
                                    (states ent))))
             "<tr><td colspan=2 width=\"1100px\">There are no user defined entity states</tr></td>")))
    (format stream
            (write-data-section
             "Entity States:" (apply #'html::tag "table" content *plain-table-attributes*)))))

(defmethod document ((ent entity) (clarity (eql :detailed)) (format (eql :html)) &optional stream)
  (write-blurb ent stream)
  (insert-er-graph ent stream)
  (format stream "<br>")
  (write-designation-section ent stream)
  (format stream "<br>")
  (write-descriptor-section ent stream)
  (format stream "<br>")
  (write-constraint-section ent stream)
  (format stream "<br>")
  (write-state-section ent stream)

  (write-table-section stream "Relationships" 2
          (relationship-role-table-headings)
          (relationship-role-table-rows ent))     
  (write-table-section stream "Stored Attributes" 2
          (user-attribute-table-headings)
          (user-defined-stored-attribute-rows ent))
  (write-table-section stream "Multivalued Stored Attributes" 2
          (multivalued-attribute-table-headings)
          (multivalued-attribute-rows ent))
  (write-table-section stream "Derived Attributes" 2
          (derived-attribute-table-headings)
          (user-defined-derived-attribute-rows ent))
  (write-table-section stream "Summary Attributes" 2
          (summary-attribute-table-headings)
          (summary-attribute-rows ent))
  (write-table-section stream "Database Key Fields" 2
          (db-key-table-headings)
          (db-key-rows ent))
  (write-table-section stream "Meta-data" 2
          (audit-attribute-table-headings)
          (audit-attribute-rows ent))
  (format stream "<br>")
  (write-code-section ent stream)
  (when (ignore-errors (seed-data ent))
    (write-seed-data ent stream)))

(defmethod write-seed-data ((ent entity) &optional (stream t))
  (let ((data (seed-data ent)))
    (write-table-section stream "Seed Data" 2
        (format nil "<tr>~{<th>~a</th>~}</tr>"
                (mapcar #'(lambda (col)
                            (name (find-field (keywordify col) (id ent))))
                        (car data)))
         (cdr data))))


(defmethod document ((ent audit-entity) (clarity (eql :detailed)) (format (eql :html)) &optional stream)
  (write-blurb ent stream)
  ;; (constraints)
  (format stream "<br>")
  (write-designation-section ent stream)
  (format stream "<br>")
  (write-descriptor-section ent stream)
     
  (write-table-section stream "Auditable Attributes" 2
          (user-attribute-table-headings)
          (auditable-attribute-rows ent))
  (write-table-section stream "Database Key Fields" 2
          (db-key-table-headings)
          (db-key-rows ent))
  (write-table-section stream "Meta-data" 2
          (audit-attribute-table-headings)
          (audit-attribute-rows ent)))

(defmethod write-code-section ((ent entity) &optional stream)
  (let ((content (get-code-examples ent)))
    (if content
      (write-table-section stream "Generated Implementation Code" 2
         (code-sample-table-headings)
         (code-sample-rows content))
      (warn "no code samples to display"))))

(defun code-sample-table-headings ()
  (html::tag
   "tr"
     (with-nesting
       (strcat (html::tag "th" "Description") (line-feed)
               (html::tag "th" "Generated Code") (line-feed)))))

(defun code-sample-rows (content)
  (mapcar #'(lambda (code-chunk)
              (with-nesting
                (list (list (car code-chunk) :valign "top" :align "left")
                      (list (html::tag
                             "pre"
                             (html::tag "code" (cadr code-chunk)))
                            :align "left"))))
          content))

(defun relationship-role-table-headings ()
  (html::tag
   "tr"
     (with-nesting
       (strcat (html::tag "th" "Relationship Name") (line-feed)
               (html::tag "th" "My Role") (line-feed)
               (html::tag "th" "Role Description") (line-feed)
               (html::tag "th" "Associative Type") (line-feed)
               (html::tag "th" "Multiplicity") (line-feed)
               (html::tag "th" "Constraints") (line-feed)))))

(defmethod relationship-role-table-rows ((ent entity))
  (append (mapcar #'(lambda (role)
                      (let ((my-relationship (or (ignore-errors (my-relationship role)) (car (relationships role)))))
                        (with-nesting
                            (list (list (html::link (long-name my-relationship)
                                                    (strcat "../" (document-link my-relationship)))
                                        :align "left")
                                  (if (string-equal (name role) (name (entity role)))
                                      (list "(not named)" :align "center")
                                      (list (long-name role) :align "left"))
                                  (list (describe-my-role role) :align "left")
                                  (list (type-of my-relationship) :align "left")
                                  (list (multiplicity role) :align "center")
                                  (list (format-english-list
                                         (mapcar #'english::unparse-expression
                                                 (mapcar #'formula (append (constraints role)
                                                                           (apply #'append
                                                                                  (mapcar #'constraints (my-relations role)))))))
                                        :align "left")))))
                  (my-roles ent))
          (super-class-table-rows ent #'relationship-role-table-rows 6)))

(defmethod super-class-table-rows ((ent entity) row-func cols)
  nil)
(defmethod super-class-table-rows ((ent specialized-entity) row-func cols)
  (let* ((super (super ent))
         (super-rows (list*
                      (list
                       (list (format nil "<em>(inherited from the ~a parent class)</em>"
                                     (make-links-to-object-reference
                                      super (short-plural super)))
                             :colspan cols :align "center"))
                      (remove-if #'(lambda (row)
                                     (member 'specialization row :key #'car))
                                 (funcall row-func super)))))
    (unless (= 1 (length super-rows))
      super-rows)))

(defun multivalued-attribute-table-headings ()
  (html::tag
   "tr"
     (with-nesting
       (strcat (html::tag "th" "Internal Name") (line-feed)
               (html::tag "th" "UI Short Name") (line-feed)
               (html::tag "th" "UI Descriptive Name") (line-feed)
               (html::tag "th" "Data Repository") (line-feed)
               (html::tag "th" "Components") (line-feed)
               (html::tag "th" "Additional Description") (line-feed)))))

(defmethod multivalued-attribute-rows ((ent entity))
  (append (mapcar #'(lambda (att)
                      (with-nesting
                        (list (list (html::link (name att) (strcat "../" (document-link att))) :align "left")
                              (list (short-name att) :align "left")
                              (list (long-name att) :align "left")
                              (list (make-links-to-object-reference
                                     (child-entity att) (short-plural (child-entity att)))
                                    :align "center")
                              (list
                               (format-english-list
                                (mapcar #'(lambda (a)
                                            (format nil "\"~a\" (~a field)" (name a)
                                                    (designation-with-article
                                                     (logical-type a) 'name nil)))
                                        (user-attributes (child-entity att))))
                               :align "left")
                              (list (or (description att) "") :align "left"))))
                  (sort (copy-list (multi-valued-attributes ent)) #'string-lessp :key #'name))
          (super-class-table-rows ent #'multivalued-attribute-rows 6)))

(defun audit-attribute-table-headings ()
  (html::tag
   "tr"
     (with-nesting
       (strcat (html::tag "th" "Internal Name") (line-feed)
               (html::tag "th" "UI Short Name") (line-feed)
               (html::tag "th" "UI Descriptive Name") (line-feed)
               (html::tag "th" "Use Type") (line-feed)
               (html::tag "th" "Data Type") (line-feed)
               (html::tag "th" "Additional Description") (line-feed)))))

(defmethod audit-attribute-rows ((ent entity))
  (mapcar #'(lambda (att)
              (with-nesting
                (list (list (html::link (name att) (strcat "../" (document-link att))) :align "left")
                      (list (short-name att) :align "left")
                      (list (long-name att) :align "left")
                      (list (name (logical-type att)) :align "center")
                      (list (data-type att) :align "center")
                      (list (or (description att) "") :align "left"))))
          (sort (copy-list (audit-attributes ent)) #'string-lessp :key #'name)))

(defun db-key-table-headings ()
  (html::tag
   "tr"
     (with-nesting
       (strcat (html::tag "th" "Internal Name") (line-feed)
               (html::tag "th" "UI Short Name") (line-feed)
               (html::tag "th" "UI Descriptive Name") (line-feed)
               (html::tag "th" "Target Entity") (line-feed)
               (html::tag "th" "Use Type") (line-feed)
               (html::tag "th" "Mandatory?") (line-feed)
               (html::tag "th" "Additional Description") (line-feed)))))

(defmethod db-key-rows ((ent entity))
  (append (mapcar
           #'(lambda (att)
               (with-nesting
                   (list (list (html::link (name att) (strcat "../" (document-link att))) :align "left")
                         (list (short-name att) :align "left")
                         (list (long-name att) :align "left")
                         (list (typecase att
                                 (primary-key (short-plural (my-entity att)))
                                 (arc-key (format-english-or-list
                                           (mapcar #'short-plural (mapcar #'my-entity (source att)))))
                                 (foreign-key (make-links-to-object-reference
                                               (my-entity (source att))
                                               (short-plural (my-entity (source att))))))
                               :align "left")
                         (list (name (logical-type att)) :align "center")
                         (list (if (nullable? att) "" "mandatory") :align "center")
                         (list (or (description att) "") :align "left"))))
                  (list* (primary-key ent)
                         (sort (copy-list (foreign-keys ent)) #'string-lessp :key #'name)))
          (super-class-table-rows ent #'db-key-rows 7)))

(defun user-attribute-table-headings ()
  (html::tag
   "tr"
     (with-nesting
       (strcat (html::tag "th" "Internal Name") (line-feed)
               (html::tag "th" "UI Short Name") (line-feed)
               (html::tag "th" "UI Descriptive Name") (line-feed)
               (html::tag "th" "Use Type") (line-feed)
               (html::tag "th" "Data Type") (line-feed)
               (html::tag "th" "Unique?") (line-feed)
               (html::tag "th" "Mandatory?") (line-feed)
               (html::tag "th" "Other Constraints") (line-feed)
               (html::tag "th" "Additional Description") (line-feed)))))

(defmethod user-defined-stored-attribute-rows ((ent entity))
  (append (mapcar #'(lambda (att)
                      (with-nesting
                        (list (list (html::link (name att) (strcat "../" (document-link att))) :align "left")
                              (list (short-name att) :align "left")
                              (list (long-name att) :align "left")
                              (list (name (logical-type att)) :align "center")
                              (list (data-type att) :align "center")
                              (list (or (uniqueness att) "") :align "center")
                              (list (if (nullable? att) "" "mandatory") :align "center")
                              (list (if (constraints att)
                                        (format nil "~{~a~^<br>~}"
                                                (mapcar #'english::unparse-expression
                                                        (mapcar #'formula (constraints att))))
                                        "no other constraints"))
                              (list (or (description att) "") :align "left"))))
                  (sort (copy-list (user-attributes ent)) #'string-lessp :key #'name))
          (super-class-table-rows ent #'user-defined-stored-attribute-rows 8)))

(defmethod auditable-attribute-rows ((ent entity))
  (mapcar #'(lambda (att)
              (with-nesting
                (list (list (html::link (name att) (strcat "../" (document-link att))) :align "left")
                      (list (short-name att) :align "left")
                      (list (long-name att) :align "left")
                      (list (name (logical-type att)) :align "center")
                      (list (data-type att) :align "center")
                      (list (or (uniqueness att) "") :align "center")
                      (list (if (nullable? att) "" "mandatory") :align "center")
                      (list (or (description att) "") :align "left"))))
          (sort (copy-list (auditable-attributes ent)) #'string-lessp :key #'name)))

(defun summary-attribute-table-headings ()
  (html::tag
   "tr"
     (with-nesting
       (strcat (html::tag "th" "Internal Name") (line-feed)
               (html::tag "th" "UI Short Name") (line-feed)
               (html::tag "th" "UI Descriptive Name") (line-feed)
               (html::tag "th" "Summary Type") (line-feed)
               (html::tag "th" "Use Type") (line-feed)
               (html::tag "th" "Source Data") (line-feed)
               (html::tag "th" "Additional Description") (line-feed)))))

(defmethod summary-attribute-rows ((ent entity))
  (append (mapcar #'(lambda (att)
                      (with-nesting
                        (list (list (html::link (name att) (strcat "../" (document-link att))) :align "left")
                              (list (short-name att) :align "left")
                              (list (long-name att) :align "left")
                              (list (string-downcase (format nil "~a" (summary-type att))) :align "center")
                              (list (name (logical-type att)) :align "center")
                              (list (html::link
                                     (format nil "~a.~a" (name (my-entity (source att))) (name (source att)))
                                               (strcat "../" (document-link (source att))))
                                    :align "center")
                              (list (or (description att) "") :align "left"))))
                  (sort (copy-list (summary-attributes ent)) #'string-lessp :key #'name))
          (super-class-table-rows ent #'summary-attribute-rows 7)))

(defun derived-attribute-table-headings ()
  (html::tag
   "tr"
     (with-nesting
       (strcat (html::tag "th" "Internal Name") (line-feed)
               (html::tag "th" "UI Short Name") (line-feed)
               (html::tag "th" "UI Descriptive Name") (line-feed)
               (html::tag "th" "Use Type") (line-feed)
               (html::tag "th" "Formula") (line-feed)
               (html::tag "th" "Additional Description") (line-feed)))))

(defmethod user-defined-derived-attribute-rows ((ent entity))
  (append (mapcar #'(lambda (att)
                      (with-nesting
                        (list (list (html::link (name att) (strcat "../" (document-link att))) :align "left")
                              (list (short-name att) :align "left")
                              (list (long-name att) :align "left")
                              (list (name (logical-type att)) :align "center")
                              (list (english::unparse (formula att)) :align "left")
                              (list (or (description att) "") :align "left"))))
                  (sort (copy-list (calculated-attributes ent)) #'string-lessp :key #'name))
          (super-class-table-rows ent #'user-defined-derived-attribute-rows 6)))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
