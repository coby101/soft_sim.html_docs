;;;===========================================================================
;;; file:   src/generators/web-docs/general.lisp
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

;; former th backgroud #6C7DD5;

;; PCMD logo green is 47D7AC
;; complementary colors: https://color.adobe.com/create/color-wheel
(defparameter *documentation-stylesheet*

#|.collapsible {
  background-color: #47D7AC;
  color: black;
  cursor: pointer;
  padding: 18px;
  width: 50%;
  border: none;
  text-align: left;
  outline: none;
  font-size: 15px;
}

.active, .collapsible:hover {
  background-color: #25a780;
}

.content {
  padding: 0 18px;
  display: none;
  overflow: hidden;
  background-color: #f1f1f1;
}
|#
"
body {background-color: lightyellow;
      margin-left: 20px}

table {border: 1px solid black;
       border-collapse: collapse;
       background-color: #FEEECD;
       margin-right: 55px;
       margin-left: 25px;}

td    {border: 1px solid black; border-collapse: collapse}

th {border: 1px solid black;
    background-color: #47D7AC; 
    padding-top: 12px;
    padding-bottom: 12px;
    color: #8A2C12;}

tr:nth-child(even){background-color: #f2f2f2;}

tr:hover {background-color: #ddd;}

hr   {border: 3px solid green;
      border-radius: 5px;}

p {margin-right: 10%;
   margin-left: 20px;}

h1 {color: navy;
    text-decoration: underline;
    text-align: center;}

h2 {text-decoration: underline;
    margin-left: 60px;}

ul {column-count: 3;
    column-gap: 20px;
    border-left: 10px solid #FBA90A;
    list-style-type: disc;
    padding: 10px 40px;}

a:hover {font-weight: bold;}

img {   max-width: 100%;
;;      display: block;
;;      margin-left: auto;
;;      margin-right: auto;
        min-width: 20%;
;;      height: auto;
;;      box-shadow: 0 4px 8px 2px rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
     }

;; img {border-radius: 50px;
;;      max-width: 90%;
;;      display: block;
;;      margin-left: auto;
;;      margin-right: auto;
;;      min-width: 20%;
;;      height: auto;
;;      box-shadow: 0 4px 8px 2px rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
;;     }
 ")

(defparameter *documentation-directory* (pathname "~/public_html/docs/"))

(defparameter *plain-table-attributes*
  '(:style "bgcolor:white; border:1;"))

(defparameter *plain-td-padding*  ;; not used anywhere yet
  "padding: 1px 5px 1px 5px;")

(defmethod formula ((str string))
  str)

(defun document-file-path (subdir name type)
  (merge-pathnames (make-pathname :name name :type type)
                   (documentation-directory subdir)))

(defun documentation-directory (&rest sub-dirs)
  (ensure-directories-exist
   (merge-pathnames
    (make-pathname :directory (cons :relative (list* *application-name* sub-dirs)))
    *documentation-directory*)))

(defun documentation-css-filepath ()
  (document-file-path "" "doc" "css"))

(defun write-documentation-css-file ()
  (when *documentation-stylesheet*
    (with-open-file (css (documentation-css-filepath)
                         :direction :output :if-exists :supersede)
      (format css *documentation-stylesheet*))))


(defun write-data-section (label content)
  (strcat
   (html::open-tag "table" :style "border:0")
   (html::open-tag "tr")
   (html::tag "td" label :width "160px" :style "font-size:large; font-weight:bold; border:0")
   (html::tag "td" content :style "border:0")
   (html::close-tag "tr")
   (html::close-tag "table")))

(defmethod write-designation-section ((obj named-object) &optional stream)
  (let ((content (with-output-to-string (str)
                   (document (designation obj) :detailed :html str))))
    (format stream (write-data-section "Designations:" content))))

(defmethod write-descriptor-section ((obj described-object) &optional stream)
  (let ((content
         (with-output-to-string (str)
           (if (descriptor obj)
               (document (descriptor obj) :detailed :html str)
               (format str "There is no documentation")))))
    (format stream (write-data-section "Documentation" content))))

(defmethod make-links-to-object-reference ((ent entity) str &key name-string
                                                (preamble "") (postscript ""))
  (replace-all str
      (strcat preamble (or name-string (short-plural ent)) postscript)
      (strcat preamble (linked-table-name ent name-string) postscript)))

(defmethod make-links-to-object-reference ((att attribute) str &key name-string
                                                (preamble "") (postscript ""))
  (replace-all str
      (strcat preamble (or name-string (short-name att)) postscript)
      (strcat preamble (linked-attribute-name att name-string) postscript)))

(defmethod make-links-to-object-reference ((view view) str &key name-string
                                               (preamble "") (postscript ""))
  (replace-all str
      (strcat preamble (or name-string (long-name view)) postscript)
      (strcat preamble (linked-view-name view name-string) postscript)))

(defun linked-view-name (view &optional link-text)
  (html::link (or link-text (long-name view))
             (strcat "../" (document-link view))))

(defun linked-table-name (ent &optional link-text)
  (html::link (or link-text (short-plural ent))
             (strcat "../" (document-link ent))))

(defun linked-attribute-name (att &optional link-text)
  (html::link (or link-text (short-name att))
             (strcat "../" (document-link att))))

(defmethod document-link ((rel binary-relationship))
  (strcat "relationships/" (file-namestring
     (document-file-path "relationships" (unambiguous-name rel) "html"))))

(defmethod document-link ((view view))
  (strcat "interface/" (file-namestring
     (document-file-path "interfaces" (unambiguous-name view) "html"))))

(defmethod document-link ((aspect aspect))
  (strcat "aspects/" (file-namestring
     (document-file-path "aspects" (strcat (name (view aspect)) "." (name (entity aspect))) "html"))))

(defun diagram-link (name version) 
  (strcat "diagrams/" (file-namestring
     (document-file-path "diagrams" (strcat name "-" version) "gif"))))

(defmethod document-link ((ent entity))
  (strcat "entities/" (file-namestring (document-file-path "entities" (name ent) "html"))))

(defmethod document-link ((att attribute))
  (let ((name (strcat (name (my-entity att)) "." (name att))))
    (strcat "attributes/" (file-namestring (document-file-path "entities" name "html")))))

(defun entity-rank (ent)
  (+ (typecase ent
       (control-table 800)
       (lookup-table 700)
       (internal-entity 400)
       (weak-entity 900)
       (application-entity 1000)
       (framework-entity 600)
       (audit-entity 500)
       (t 0))
     (length (relationships ent))))

(defun ranked-entities (&optional (app *application*))
  (sort (copy-list (schema app)) #'> :key #'entity-rank))

(defun ranked-entities (&optional (app *application*))
  (sort (copy-list (schema app)) #'string-lessp :key #'name))

(defun write-html-docs (&optional (app *application*))
  (declare (ignorable app))
  (if (null *application*)
      "no application object defined, nothing written"
      (let ((*nesting-level* 0))
        (let ((total-entites (length (schema app)))
              (total-attributes (length (apply #'append (mapcar #'attributes (schema app)))))
              (total-relationsips (length (find-all-relationships)))
              (total-views (length (views app)))
              (total-aspects (length (apply #'append (mapcar #'aspects (views app))))))
          (write-documentation-css-file)
          (with-open-file (main (document-file-path "" "application" "html")
                                :direction :output :if-exists :supersede)
            (write-html-header main
                               :title (format nil "Technical Specifications for project ~a"
                                              (ignore-errors (short-name app)))
                               :meta '(:file-source "write-html-docs in soft-sim/src/documentation.lisp")
                               :link (list :rel "stylesheet" :type "text/css"
                                           :href (file-namestring (documentation-css-filepath))))
            (format main (html::heading 1 (format nil "~a Technical Documentation" (long-name app))))
            (format main "
This page provides technical documentation for the ~a web application. The ~a application is owned by ~
~a and this material is for internal use. All intellectual property contained herein remains ~a's ~
own.  Below you can find links to further detail." (long-name app) (id app) (client app) (client app))
            (format main (html::heading 3 (html::link "Application Views" "interface.html")))
            (format main (html::heading 3 (html::link "Entity Definitions" "entities.html")))
            (format main (html::heading 3 (html::link "Entity Relationships" "relationships.html")))
            (format main (html::heading 3 (html::link "Application Subsystems" "subsystems.html")))

            (with-open-file (intf (document-file-path "" "interface" "html")
                                  :direction :output :if-exists :supersede)
              (write-interface-page app intf))
            
            (with-open-file (ents (document-file-path "" "entities" "html")
                                  :direction :output :if-exists :supersede)
              (write-entity-page app ents))

            (with-open-file (rels (document-file-path "" "relationships" "html")
                                  :direction :output :if-exists :supersede)
              (write-relationship-page app rels))

            (with-open-file (mods (document-file-path "" "subsystems" "html")
                                  :direction :output :if-exists :supersede)
              (write-modules-page app mods))

            (write-html-footer main (doc-timestamp)))
           (format t "wrote pages for ~a entites, ~a relationships, ~
                      ~a attributes, ~a views and ~a aspects (~a total pages)"
                   total-entites total-relationsips total-attributes total-views total-aspects
                   (+ total-relationsips total-attributes total-entites total-views total-aspects))
           (format t "~%~%see documentation at ~a" (document-file-path "" "application" "html"))))))

(defun write-entity-page (app stream)
  (write-html-header stream
   :title (format nil "Entity Specifications for project ~a"
                       (ignore-errors (short-name app)))
   :meta (list :file-source "write-html-docs in soft_sim/generators/web-docs/general.lisp")
   :link (list :rel "stylesheet" :type "text/css"
               :href (file-namestring (documentation-css-filepath))))
  (format stream (html::link "Documentation Home" "application.html"))
  (let ((core-ents (remove-if #'(lambda(table)
                                 (or (typep table 'lookup-table)
                                     (typep table 'attribute-table)))
                             (application-entities app)))
        (sup-ents (remove-if-not #'(lambda(table)
                                     (or (typep table 'lookup-table)
                                         (typep table 'attribute-table)))
                                 (application-entities app)))
        (assoc-ents (remove-if-not #'(lambda(table)
                                       (typep table 'associative-entity))
                                   (append (application-entities app)
                                           )))
        (frame-ents (framework-entities app)))
    (write-entity-section
     "Core Entities"
     (sort (copy-list core-ents) #'string-lessp :key #'name) stream)

    (write-entity-section "Supporting Entities"
     (sort (copy-list sup-ents) #'string-lessp :key #'name)
     stream)
    (write-entity-section "Associative Entities"
     (sort (copy-list assoc-ents) #'string-lessp :key #'name)
     stream)
    (write-entity-section "Framework Entities"
     (sort (copy-list frame-ents) #'string-lessp :key #'name)
     stream)
    (format stream (html::link "Documentation Home" "application.html"))
    (write-html-footer stream (doc-timestamp))))

(defun write-entity-section (name entities stream)
  (format stream (html::tag "h1" name))
  (format stream "~%<ul>~%")
  (dolist (ent entities)
    (unless (typep ent 'audit-entity)
      (format stream "~%<li>~a</li>"
              (html::link (name ent) (document-link ent))))
    (create-entity-graphs ent)
    (when (ignore-errors (seed-data ent))
      (with-open-file (seed (document-file-path "entities" (name ent) "csv")
                            :direction :output :if-exists :supersede)
        (mapcar #'(lambda(row)
                    (format seed "~{~s~^,~}~%" row))
                (seed-data ent))))
    (with-open-file (ent-doc (document-file-path "entities" (name ent) "html")
                             :direction :output :if-exists :supersede)
      (document ent :detailed :html ent-doc))
    (dolist (att (attributes ent))
      (with-open-file (att-doc (document-file-path "attributes"
                                                   (strcat (name ent) "." (name att)) "html")
                               :direction :output :if-exists :supersede)
        (document att :detailed :html att-doc))))
  (format stream "~%</ul>~%"))

(defun write-relationship-page (app stream)
  (write-html-header stream
   :title (format nil "Relationship Definitions for project ~a"
                       (ignore-errors (short-name app)))
   :meta (list :file-source "write-html-docs in soft_sim/generators/web-docs/general.lisp")
   :link (list :rel "stylesheet" :type "text/css"
               :href (file-namestring (documentation-css-filepath))))
  (format stream (html::link "Documentation Home" "application.html"))
  (format stream (html::tag "h1" "Relationships"))
  (format stream "~%<ul>~%")
  (dolist (rel (find-all-relationships))
    (format stream "~%<li>~a : <small><em>(~a)</em></small></li>"
            (html::link (short-name rel) (document-link rel))
            (ascii-depiction rel))
    (with-open-file (doc (document-file-path "relationships" (unambiguous-name rel) "html")
                         :direction :output :if-exists :supersede)
      (document rel :detailed :html doc)))
  (format stream "~%</ul>~%")
  (format stream (html::link "Documentation Home" "application.html"))
  (write-html-footer stream (doc-timestamp)))

(defun ascii-depiction (rel)
  (let ((l-arrow (if (> (or (multiplicity-max (lhs rel)) 2) 1) "<-" "-"))
        (r-arrow (if (> (or (multiplicity-max (rhs rel)) 2) 1) "->" "-")))
    (format nil "~a~a~a~a" (name (entity (lhs rel))) l-arrow r-arrow (name (entity (rhs rel))))))

(defun list-contents (space stream)
  (with-nesting
      (format stream (html::tag "li"
                        (html::tag "b"
                           (format nil "~a (~s)" (long-name space) (name space)))))
    (when (or (views space) (sub-spaces space))
      (format stream "~%~a~a~%" (html::make-indent)
                  (html::open-tag "ul" :style "column-count: 1; border-left: 0px"))
      (with-nesting
          (when (views space)
            (dolist (view (views space))
              (format stream "~%~a<li>~a</li>" (html::make-indent)
                      (html::link (long-name view) (document-link view)))))
        (when (sub-spaces space)
          (dolist (spc (sub-spaces space))
            (list-contents spc stream))))
      (format stream "~%~a</ul>~%" (html::make-indent)))))


(defun unparse-space (space &optional stream)
  (let ((views (views space))
        (sub-spaces (sub-spaces space)))
    (format stream "~%~a~a" (html::make-indent)
            (html::button (long-name space) :class "collapsible"))
    (format stream "~%~a~a" (html::make-indent)
            (html::div
             (if (and (null views) (null sub-spaces))
                 "(this space has no content)"
                 (with-output-to-string (str)
                   (when views
                     (format str
                             (html::ltag nil
                                (loop for view in views
                                      collect
                                      (html::link (long-name view) (document-link view)))
                                :style "column-count: 1; border-left: 0px")))
                   (when sub-spaces
                     (with-nesting
                         (dolist (sub sub-spaces)
                           (unparse-space sub str))))))
             :class "content"))))

;; non-css-collabsible version
(defun unparse-space (space &optional stream)
  (let ((views (views space))
        (sub-spaces (sub-spaces space)))
    (format stream (html::open-tag "ul" :style "border-left: 0px #FFFFE0; column-count: 1"))
    (format stream "~%~a~a~a" (html::make-indent) (html::open-tag "li") (long-name space))
    (format stream "~%~a~a" (html::make-indent)
            (if (and (null views) (null sub-spaces))
                 "(this space has no content)"
                 (with-output-to-string (str)
                   (when views
                     (format str
                             (html::ltag nil
                                (loop for view in views
                                      collect
                                      (html::link (long-name view) (document-link view)))
                                :style "column-count: 1; border-left: 0px")))
                   (when sub-spaces
                     (with-nesting
                         (dolist (sub sub-spaces)
                           (unparse-space sub str)))))))
    (format stream "~%~a~a" (html::make-indent) (html::close-tag "li"))
    (format stream (html::close-tag "ul"))))

(defun write-view-menu (app stream)
  (dolist (space (remove-if #'parent-space (spaces app)))
    (unparse-space space stream))
  (format stream "
<script>
var coll = document.getElementsByClassName(\"collapsible\");
var i;

for (i = 0; i < coll.length; i++) {
  coll[i].addEventListener(\"click\", function() {
    this.classList.toggle(\"active\");
    var content = this.nextElementSibling;
    if (content.style.display === \"block\") {
      content.style.display = \"none\";
    } else {
      content.style.display = \"block\";
    }
  });
}
</script>"))

(defun write-modules-page (app stream)
  (write-html-header stream
   :title (format nil "Application Subsystems for project ~a"
                       (ignore-errors (short-name app)))
   :meta (list :file-source "write-html-docs in soft_sim/generators/web-docs/general.lisp")
   :link (list :rel "stylesheet" :type "text/css"
               :href (file-namestring (documentation-css-filepath))))
  (format stream (html::link "Documentation Home" "application.html"))
  (format stream (html::tag "h1" "Application Subsystems"))
  (apply #'create-module-graph "Full Schema" (schema-entities app)) 
  (apply #'create-module-graph "Application Entities" (application-entities app)) 
  (apply #'create-module-graph "Application Framework" (application-framework-entities app)) 
  (apply #'create-module-graph "Calendar Entities" (calendar-entities)) 

  (let ((subsystems (append *sub-systems*
                            (list (list* "Calendar Entities" (calendar-entities))
                                  (list* "Framework Entities" (framework-entities app))
                                  (list* "Application Framework" (application-framework-entities app))
                                  (list* "Full Schema" (schema-entities app))))))
    (dolist (sub subsystems)
      (let ((name (car sub)) (entities (cdr sub)))
        (apply #'create-module-graph name entities)
        (format stream "~%<p>~a~%~a</p>" (html::heading 2 name)
                (html::image (strcat (replace-all name " " "-") ".gif"))))))

  (format stream (html::p (html::link "Documentation Home" "application.html")))
  (write-html-footer stream (doc-timestamp)))

(defun write-interface-page (app stream)
  (write-html-header stream
   :title (format nil "Application Interfaces for project ~a"
                       (ignore-errors (short-name app)))
   :meta (list :file-source "write-html-docs in soft_sim/generators/web-docs/general.lisp")
   :link (list :rel "stylesheet" :type "text/css"
               :href (file-namestring (documentation-css-filepath))))
  (format stream (html::link "Documentation Home" "application.html"))
  (format stream (html::tag "h1" "Application Views and Navigation Structure"))
  (write-view-menu app stream)
  (dolist (view (views app))
    (create-view-graphs view)
    (with-open-file (doc (document-file-path "interface" (unambiguous-name view) "html")
                         :direction :output :if-exists :supersede)
      (ignore-errors
       (document view :detailed :html doc)))
    (dolist (asp (aspects view))
      (with-open-file (asp-doc (document-file-path "aspects"
                                                   (strcat (name view) "." (name (entity asp))) "html")
                               :direction :output :if-exists :supersede)
        (ignore-errors
         (document asp :detailed :html asp-doc)))))
  (format stream (html::p (html::link "Documentation Home" "application.html")))
  (write-html-footer stream (doc-timestamp)))

(defun write-synopsis (content &optional stream)
  (format stream (html::open-tag "table" :style "border:0"))
    (format stream "<tr style=\"border:0\">~a~a</tr>"
            (html::tag "td" "Synopsis:" :width "180px"
                         :style "font-size:large; font-weight:bold; border:0")
            (html::tag "td" content :width "900px" :style "border:0"))
    (format stream (html::close-tag "table")))

(defun write-table-section (stream title hlevel headings rows)
  (format stream (html::heading hlevel title))
  (if rows
      (progn
        (format stream "<table>~a" (line-feed))
        (with-nesting
          (format stream headings)
          (format stream "~a" (apply #'table-rows nil rows)))
        (format stream "</table>~a" (line-feed)))
      (format stream
         (html::p
             (html::tag "em" (format nil "There are no ~a" title))))))

(defun datasheet-headings ()
  (html::tag
   "tr"
     (with-nesting
       (strcat (html::tag "th" "Property") (line-feed)
               (html::tag "th" "Value") (line-feed)))))

(defun make-table-cells (&rest cells)
  (let ((frmt-str (format nil "~~{~~a~a~~}" (line-feed))))
    (format nil frmt-str
            (mapcar #'(lambda (cell)
                        (etypecase cell
                          (symbol (html::tag "td" (symbol-name cell)))
                          (number (html::tag "td" cell))
                          (string (html::tag "td" cell))
                          (list (apply #'html::tag "td" (car cell) (cdr cell)))))
                    cells))))

(defun table-rows (attributes &rest rows)
  (let ((format-string (format nil "~~a~~{~a~a~~a</tr>~~}~a"
                               (line-feed) (apply #'html::open-tag "tr" attributes) (line-feed))))
    (format nil format-string
            (line-feed)
            (with-nesting
              (mapcar #'(lambda(r)
                          (apply #'make-table-cells r))
                      rows)))))

(defun write-html-header (stream &key title style meta link)
  (format stream "<!DOCTYPE html>~%<html>~%")
  (format stream "~a~%" (html::open-tag "head"))
  (when title (format stream (html::tag "title" title)))
  (when style (format stream (html::tag "style" style)))
  (when meta (format stream "~%~a" (apply #'html::open-tag "meta" meta)))
  (when link (format stream "~%~a~%" (apply #'html::open-tag "link" link)))
  (format stream "~a~%" (html::close-tag "head"))
  (format stream "~a~%" (html::open-tag "body")))

(defun write-html-footer (stream &optional timestamp)
  (format stream "~%~a~%~a~%~a~%"
          (or timestamp "") (html::close-tag "body") (html::close-tag "html")))

;;; this is crap, it assumes we are one level down when it is called. no time to fix now
(defun home-page-link (&optional link-text)
  (html::link (or link-text (long-name *application*)) "../application.html"))

(defun doc-timestamp ()
  (html::p
     (html::tag "small"
        (html::tag "em"
           (format nil "(this page was generated on ~a)"
                   (local-time:format-timestring nil (local-time:now)
                        :format *documentation-timestamp-format*))))))

(defmethod document ((des designation) (clarity t) (format (eql :html)) &optional stream)
  (format stream "~a~a" (apply #'html::open-tag "table" *plain-table-attributes*) (line-feed))
  (with-nesting
    (format stream
            (table-rows
             nil
             (list (list "" :width "140px")
                   (list (html::tag "b" "Singular") :width "400px" :align "center")
                   (list (html::tag "b" "Plural") :width "400px" :align "center"))
             (list (list (html::tag "b" "Base Name:") :align "right")
                   (name des)
                   (plural des))
             (list (list (html::tag "b" "Abbreviated Name:") :align "right")
                   (if (string-equal (short-name des) (name des)) "&nbsp" (short-name des))
                   (if (string-equal (short-plural des) (plural des)) "&nbsp" (short-plural des)))
             (list (list (html::tag "b" "Descriptive Name:") :align "right")
                   (if (string-equal (short-name des) (long-name des)) "&nbsp" (long-name des))
                   (if (string-equal (short-plural des) (long-plural des)) "&nbsp" (long-plural des))))))
  (format stream "~a~a" (html::close-tag "table") (line-feed)))


(defmethod document ((desc descriptor) (clarity t) (format (eql :html)) &optional stream)
  (format stream "~a~a" (apply #'html::open-tag "table" *plain-table-attributes*) (line-feed))
  (with-nesting
    (format stream
            (table-rows
             nil
             (list (list (html::tag "b" "User Summary:") :align "right")
                   (list (or (user-summary desc) "") :width "800px"))
             (list (list  (html::tag "b" "User Detail:") :align "right")
                   (or (user-detail desc) ""))
             (list (list (html::tag "b" "Technical Summary:") :align "right")
                   (or (tech-summary desc) ""))
             (list (list (html::tag "b" "Technical Detail:") :align "right")
                   (or (tech-detail desc) "")))))
  (format stream "</table>~a" (line-feed)))
;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
