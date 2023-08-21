;;;===========================================================================
;;; file:   src/generators/web-docs/er-diagrams.lisp
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

(defun create-view-graphs (view)
  (create-view-graph view :basic)
  (create-view-graph view :logical)
  (create-view-graph view :complete))

(defun create-entity-graphs (entity)
  (create-entity-graph entity :basic)
  (create-entity-graph entity :logical)
  (create-entity-graph entity :complete))

(defmethod create-view-graph (view &optional (extent :basic))
  (pipe-command
   (list "echo" (dot:unparse-view view extent))
   (list "dot" "-Gfontname=fixed" "-Tgif" "-o"
         (pathname->shell-filename
          (document-file-path
           "diagrams"
           (strcat (name view) "-view-" (string-downcase (symbol-name extent)))
           "gif")))))

(defmethod create-entity-graph (entity &optional (extent :logical))
  (pipe-command
   (list "echo" (dot:unparse-entity entity extent))
   (list "dot" "-Gfontname=fixed" "-Tgif" "-o"
         (pathname->shell-filename
          (document-file-path
           "diagrams"
           (strcat (name entity) "-" (string-downcase (symbol-name extent)))
           "gif")))))

(defun create-module-graph (name &rest entities)
  (let ((entity-set (remove nil entities)))
    (when entity-set
      (let ((path (pathname->shell-filename
                   (document-file-path "" (replace-all name " " "-") "gif"))))
        (pipe-command
         (list "echo" (dot:unparse-entity-cluster name entity-set))
         (list "dot" "-Gfontname=fixed" "-Tgif" "-o"
               path))
        path))))

(defmethod er-details ((view view))
  (let* ((entities (mapcar #'entity (aspects view)))
	 (complete (apply #'append (mapcar #'(lambda(entity)
					       (extended-relationships entity :complete))
					   entities)))
         (logical (apply #'append (mapcar #'(lambda(entity)
					      (extended-relationships entity :logical))
					  entities)))
         (basic (apply #'append (mapcar #'(lambda(entity)
					    (extended-relationships entity :basic))
					entities))))
    (format nil "~a~a"
            (if (set-difference complete logical)
                (format nil "<br>(see ~a for fully extended relationships)"
                        (html:link "here" (strcat "../" (diagram-link (name view) "view-complete"))))
                "")
            (if (set-difference logical basic)
                (format nil "<br>(see ~a for extended relationships)"
                        (html:link "here" (strcat "../" (diagram-link (name view) "view-logical"))))
                ""))))

(defmethod er-details ((entity entity))
  (let* ((complete (extended-relationships entity :complete))
         (logical (extended-relationships entity :logical))
         (basic (extended-relationships entity :basic)))
    (format nil "~a~a"
            (if (set-difference complete logical)
                (format nil "<br>(see ~a for extended relationships)"
                        (html:link "here" (strcat "../" (diagram-link (name entity) "complete"))))
                "")
            (if (set-difference logical basic)
                (format nil "<br>(see ~a for basic relationships)"
                        (html:link "here" (strcat "../" (diagram-link (name entity) "basic"))))
                ""))))

(defmethod insert-er-graph ((entity entity) &optional stream)
  (format stream (html:open-tag "table" :style "border:0"))
    (format stream "<tr style=\"border:0\">~a~a</tr>"
            (html:tag "td" "ER Diagram"
                 :width "180px" :style "font-size:large; font-weight:bold; border:0")
            (html:tag
             "td" (strcat (html:open-tag "img"
			     :src (strcat "../" (diagram-link (name entity) "logical")))
                          (er-details entity))
             :align "center" :width "1100px" :style "border:0"))
  (format stream (html:close-tag "table")))

(defmethod insert-er-graph ((view view) &optional stream)
  (format stream (html:open-tag "table" :style "border:0"))
    (format stream "<tr style=\"border:0\">~a~a</tr>"
            (html:tag "td" "Relationships of Accessible Entities"
                 :width "180px" :style "font-size:large; font-weight:bold; border:0")
            (html:tag
             "td" (strcat (html:open-tag "img"
			     :src (strcat "../" (diagram-link (name view) "view-basic")))
                          (er-details view))
             :align "center" :width "1100px" :style "border:0"))
  (format stream (html:close-tag "table")))



;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
