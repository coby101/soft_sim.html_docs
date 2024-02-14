;;;===========================================================================
;;; file:   lib/tests/web-docs.lisp
;;; auth:   Coby Beck
;;; date:   2021-07-13
;;;
;;;---------------------------------------------------------------------------
;;;
;;;  tests for generating web documentation
;;;  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :simian)

;;nothing needs the generator yet this is documentation.lisp code for now
(load-generator "web-docs")
(define-test :documentation "user attribute general value descriptions all sound fine"
  (with-new-schema
      (define-lookup-table ("AnyType") :with-code? t
          :attributes (("Rate" :type money)))
      (define-entity ("Anything")
          :attributes (:note
                       (:entity-code)
                       ("Type" :controller AnyType
                               :other-attributes ((Rate :storage inherit)
                                                  (Name :storage cached)))
                       ("Quantity" :type quantity :nullable? nil)
                       ("Cost" :type money :formula ($multiply Quantity Rate))))
      (resolve-constraint-objects nil)
      (resolve-referential-enumerations nil)
      (resolve-attribute-derivations nil)
      (resolve-default-value-expressions nil)
      (mapcar #'blurb
              (user-attributes (find-entity :anything))))
  '("The Rate field is of class USER-ATTRIBUTE and belongs to the Anythings table. \"Rate\" is an optional field with a default taken from the \"Rate\" field in the \"Any Types\" table and is not required to be unique. \"Rate\" is FLOAT data that repesents a \"Money\" value with zero additional user defined constraints."
    "The Code field (\"Anything Code\") is of class USER-ATTRIBUTE and belongs to the Anythings table. \"Anything Code\" is a mandatory field with no default value and is unique. \"Anything Code\" is STRING data that repesents a \"User-Key\" value with one additional user defined constraint."
    "The Note field (\"Miscellaneous Notes\") is of class USER-ATTRIBUTE and belongs to the Anythings table. \"Miscellaneous Notes\" is an optional field with no default value and is not required to be unique. \"Miscellaneous Notes\" is STRING data that repesents a \"Memo\" value with zero additional user defined constraints."
    "The Quantity field is of class USER-ATTRIBUTE and belongs to the Anythings table. \"Quantity\" is a mandatory field with no default value and is not required to be unique. \"Quantity\" is FLOAT data that repesents a \"Quantity\" value with zero additional user defined constraints."))

(define-test :documentation "derived attribute general value descriptions all sound fine"
  (with-new-schema
      (define-lookup-table ("AnyType")
          :attributes (("Rate" :type money)))
      (define-entity ("Anything")
          :attributes (:note
                       (:entity-code)
                       ("Type" :controller AnyType
                               :other-attributes ((Rate :storage inherit)
                                                  (Name :storage cached)))
                       ("Quantity" :type quantity :nullable? nil)
                       ("Cost" :type money :formula ($multiply Quantity Rate))))

      (resolve-constraint-objects nil)
      (resolve-referential-enumerations nil)
      (resolve-attribute-derivations nil)
      (resolve-default-value-expressions nil)
      (mapcar #'blurb
              (derived-attributes (find-entity :anything))))
  ' ("The AnyTypeName field is of class CACHED-INHERITANCE and though stored in Anythings, its data belongs to the Any Types table. \"Any Type Name\" can be null. \"Any Type Name\" is STRING data that repesents a \"Short-Text\" value with zero additional user defined constraints."
    "The Cost field is of class CALCULATED-ATTRIBUTE and belongs to the Anythings table. \"Cost\" is FLOAT data that repesents a \"Money\" value and is derived from Anything Quantity times Anything Rate."))

(define-test :documentation "system attribute general value descriptions all sound fine"
  (with-new-schema
      (define-lookup-table ("AnyType")
          :attributes (("Rate" :type money)))
      (define-entity ("Anything")
          :attributes (:note
                       (:entity-code)
                       ("Type" :controller AnyType
                               :other-attributes ((Rate :storage inherit)))
                       ("Quantity" :type quantity :nullable? nil)
                       ("Cost" :type money :formula ($multiply Quantity Rate))))

      (resolve-constraint-objects nil)
      (resolve-referential-enumerations nil)
      (resolve-attribute-derivations nil)
      (resolve-default-value-expressions nil)
      (mapcar #'blurb
              (system-attributes (find-entity :anything))))
  '("The TypeController field (\"Type\") is a foreign key in the Anythings table that links records to the Any Types table."
    "The SysChangeCount field is a wholly system managed field storing modification data for each record in the Anythings table. At each modification, the current SysChangeCount value is stored in a new record in the Anything Audits table."
    "The SysChangeOrigin field is a wholly system managed field storing modification data for each record in the Anythings table. At each modification, the current SysChangeOrigin value is stored in a new record in the Anything Audits table."
    "The SysEditor field is a wholly system managed field storing modification data for each record in the Anythings table. At each modification, the current SysEditor value is stored in a new record in the Anything Audits table."
    "The SysModTime field is a wholly system managed field storing modification data for each record in the Anythings table. At each modification, the current SysModTime value is stored in a new record in the Anything Audits table."
    "The SysCreator field is a wholly system managed field storing modification data for each record in the Anythings table. At each modification, the current SysCreator value is stored in a new record in the Anything Audits table."
    "The SysAddTime field is a wholly system managed field storing modification data for each record in the Anythings table. At each modification, the current SysAddTime value is stored in a new record in the Anything Audits table."
    "The Anything field (\"Anything Internal Key\") is the primary key of the Anythings table. It is fully system managed with implementation specific properties"))

(define-test :documentation "summary attribute general value descriptions all sound fine"
  (with-new-schema
      (define-lookup-table ("AnyType")
          :attributes (("Rate" :type money)))
      (define-entity ("Anything")
          :attributes (:note
                       (:entity-code)
                       ("Type" :controller AnyType
                               :other-attributes ((Rate :storage inherit)))
                       ("Quantity" :type quantity :nullable? nil)
                       ("Cost" :type money :formula ($multiply Quantity Rate))))
      (define-summary "Expenditures"
          :source Cost
          :summary-paths ((AnyType Anything)))
      (resolve-constraint-objects nil)
      (resolve-referential-enumerations nil)
      (resolve-attribute-derivations nil)
      (resolve-default-value-expressions nil)
      (mapcar #'blurb
              (summary-attributes (find-entity :anytype))))
  '("The Expenditures field is of class SUMMARY-ATTRIBUTE and belongs to the Any Types table. \"Expenditures\" is an optional field whose value is the SUM of Costs in related Anything records and is calculated when accessed. \"Expenditures\" is FLOAT data that repesents a \"Money\" value with zero additional user defined constraints.<br><br>Data in this field is read only."
    "The Anythings field is of class SUMMARY-ATTRIBUTE and belongs to the Any Types table. \"Anythings\" is an optional field whose value is the COUNT of Anything Internal Keys in related Anything records and is calculated when accessed. \"Anythings\" is INTEGER data that repesents a \"Count\" value with one additional user defined constraint.<br><br>Data in this field is read only."))


;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
