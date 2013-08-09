#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- JIRA REST API Client
  Copyright (c) 2013 Dirk Esser

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
|#

(in-package "DARTS.LIB.JIRA-API")

(defclass json-value-type (value-type) ()
  (:default-initargs
    :display-name "JSON Value"
    :lisp-type 't))

(defclass string-value-type (value-type) ()
  (:default-initargs
    :identifier "string"
    :display-name "String"
    :lisp-type 'string))

(defclass integer-value-type (value-type) ()
  (:default-initargs
    :display-name "Integer"
    :lisp-type 'integer))

(defclass number-value-type (value-type) ()
  (:default-initargs
    :identifier "number"
    :display-name "Number"
    :lisp-type 'number))

(defclass boolean-value-type (value-type) ()
  (:default-initargs
    :identifier "boolean"
    :display-name "Boolean"
    :lisp-type 't))

(defclass uri-value-type (value-type) () 
  (:default-initargs
    :display-name "URI"
    :lisp-type 'uri))

(defclass timestamp-value-type (value-type) ()
  (:default-initargs
    :identifier "datetime"
    :display-name "Timestamp"
    :lisp-type 'timestamp))

(defclass date-value-type (value-type) ()
  (:default-initargs
    :identifier "date"
    :display-name "Date"
    :lisp-type 'timestamp))

(defparameter +json+ (make-instance 'json-value-type))
(defparameter +string+ (make-instance 'string-value-type))
(defparameter +integer+ (make-instance 'integer-value-type))
(defparameter +number+ (make-instance 'number-value-type))
(defparameter +boolean+ (make-instance 'boolean-value-type))
(defparameter +uri+ (make-instance 'uri-value-type))
(defparameter +timestamp+ (make-instance 'timestamp-value-type))
(defparameter +date+ (make-instance 'date-value-type))


(defgeneric parse-json-value* (value type state session path))

(defmethod parse-json-value* ((value t) (type json-value-type) state session path)
  (declare (ignore state session path))
  value)

(defmethod parse-json-value* (value type state session path)
  (declare (ignore state session))
  (parser-error value type path))

(defmethod parse-json-value* ((value string) (type string-value-type) state session path)
  (declare (ignore state session path))
  value)

(defmethod parse-json-value* ((value string) (type uri-value-type) state session path)
  (declare (ignore state session path))
  (handler-case (parse-uri value)
    (error () (call-next-method))))

(defmethod parse-json-value* ((value integer) (type integer-value-type) state session path)
  (declare (ignore state session path))
  value)

(defmethod parse-json-value* ((value number) (type number-value-type) state session path)
  (declare (ignore state session path))
  value)

(defmethod parse-json-value* ((value t) (type boolean-value-type) state session path)
  (declare (ignore state session path))
  (cond 
    ((eq value :true) t)
    ((eq value :false) nil)
    (t (call-next-method))))

(defmethod parse-json-value* ((value string) (type timestamp-value-type) state session path)
  (declare (ignore state session path))
  (or (parse-timestring value :fail-on-error nil)
      (call-next-method)))

(defmethod parse-json-value* ((value string) (type date-value-type) state session path)
  (declare (ignore state session path))
  (or (parse-timestring value :fail-on-error nil :allow-missing-time-part t)
      (call-next-method)))



(defun parse-json-value (value type
                         &key (required t) (nullable (not required)) (default nil) 
                              (path nil) (session *default-session*) (state nil))
  (labels 
      ((parse-value (value)
         (if (eq value :null)
             (if nullable 
                 (values default :null)
                 (parser-error value type path))
             (values (parse-json-value* value type state session path)
                     nil)))
       (parser-error-p (value) (typep value 'parser-error))
       (format-path (path)
         (and path
              (with-output-to-string (stream)
                (labels
                    ((recurr (path)
                         (if (null path) (write-string "result" stream)
                             (progn (recurr (cdr path))
                                    (cond
                                      ((integerp (car path)) (format stream "[~D]" (car path)))
                                      ((stringp (car path)) 
                                       (if (scan "^[a-zA-Z$_][a-zA-Z0-9$_]*$" (car path))
                                           (format stream ".~A" (car path))
                                           (format stream "[~S]" (car path))))
                                      (t (format stream "<~S>" (car path))))))))
                  (recurr path)))))
       (report-restart (stream message)
         (format stream "~A ~S~@[ at ~A~]" 
                 message value (format-path path)))
       (read-replacement-value ()
         (format *query-io* "~&Enter an expression: ")
         (list (eval (read *query-io*)))))
    (loop
      (restart-case (multiple-value-bind (answer indicator) (parse-value value)
                      (return-from parse-json-value (values answer indicator)))
        (skip-offending-value ()
          :test (lambda (condition) (and (not required) (parser-error-p condition)))
          :report (lambda (stream) (report-restart stream "Skip offending value"))
          (return-from parse-json-value (values default :error)))
        (use-value (replacement)
          :test parser-error-p
          :interactive read-replacement-value
          :report (lambda (stream) (report-restart stream "Provide a replacement to parse instead of"))
          (setf value replacement))))))
          


;;; Helper macros for the complex parsers below

(defmacro registering (type-name (identifier &key (state 'state) (session 'session)) &body body)
  `(register-session-object ,state ,session ',type-name ,identifier 
                            (lambda () ,@body)))
           

;;; Derived field types.

(defclass array-value-type (value-type) 
  ((element-type
     :type value-type :initarg :element-type
     :reader array-value-type-element-type)
   (element-required
     :type boolean :initform 't :initarg :element-required 
     :reader array-value-type-element-required-p)
   (element-nullable
     :type boolean :initarg :element-nullable
     :reader array-value-type-element-nullable-p)
   (element-default
     :type boolean :initform 'nil :initarg :element-default
     :reader array-value-type-element-default))
  (:default-initargs
    :lisp-type 'sequence))


(defmethod initialize-instance :after ((object array-value-type) &key &allow-other-keys)
  (unless (value-type-display-name object)
    (let ((etp (array-value-type-element-type object)))
      (setf (slot-value object 'display-name)
            (format nil "Array Of ~A" (or (value-type-display-name etp)
                                          (value-type-identifier etp)
                                          (value-type-lisp-type etp)))))))


(defmethod parse-json-value* ((object cons) (type array-value-type) state session path)
  (if (not (eq (car object) :array))
      (call-next-method)
      (let ((etype (array-value-type-element-type type))
            (required (array-value-type-element-required-p type))
            (nullable (array-value-type-element-nullable-p type))
            (default (array-value-type-element-default type)))
        (loop
           :with head := nil :and tail := nil
           :for element :in (cdr object)
           :for index :upfrom 0
           :for new-path := (cons index path)
           :do (multiple-value-bind (parsed fail) (parse-json-value element etype 
                                                                    :path new-path :session session :state state
                                                                    :required required
                                                                    :nullable nullable
                                                                    :default default)
                 (unless (eq fail :error)
                   (let ((link (cons parsed nil)))
                     (if (null head)
                         (setf head (setf tail link))
                         (setf tail (setf (cdr tail) link))))))
           :finally (return (coerce head (value-type-lisp-type type)))))))


(defun make-array-value-type (element-type 
                              &key (element-required t) (element-nullable (not element-required))
                                   (element-default nil))
  (make-instance 'array-value-type
                 :element-type element-type :element-required element-required
                 :element-nullable element-nullable :element-default element-default))


(defclass subset-value-type (value-type) 
  ((container
     :type string :initarg :container
     :reader subset-value-type-container)
   (element-type
     :type value-type :initarg :element-type
     :reader subset-value-type-element-type)
   (element-required
     :type boolean :initform 't :initarg :element-required 
     :reader subset-value-type-element-required-p)
   (element-nullable
     :type boolean :initarg :element-nullable
     :reader subset-value-type-element-nullable-p)
   (element-default
     :type boolean :initform 'nil :initarg :element-default
     :reader subset-value-type-element-default))
  (:default-initargs
    :lisp-type 'subset))


(defmethod initialize-instance :after ((object subset-value-type) &key &allow-other-keys)
  (unless (value-type-display-name object)
    (let ((etp (subset-value-type-element-type object)))
      (setf (slot-value object 'display-name)
            (format nil "Subset Of ~A" (or (value-type-display-name etp)
                                          (value-type-identifier etp)
                                          (value-type-lisp-type etp)))))))


(defmethod parse-json-value* ((object cons) (type subset-value-type) state session path)
  (if (not (eq (car object) :object))
      (call-next-method)
      (let ((container (subset-value-type-container type))
            (etype (subset-value-type-element-type type))
            (required (subset-value-type-element-required-p type))
            (nullable (subset-value-type-element-nullable-p type))
            (default (subset-value-type-element-default type))
            (offset 0) (limit 0) (total 0)
            (elements nil))
        (loop
           :for (key value) :on (cdr object) :by #'cddr
           :for new-path := (cons key path)
           :do (string-case (key)
                 ("maxResults" (setf limit (parse-json-value value +integer+ :required nil :default 0)))
                 ("startAt" (setf offset (parse-json-value value +integer+ :required nil :default 0)))
                 ("total" (setf total (parse-json-value value +integer+ :required nil :default 0)))
                 (t (cond 
                      ((string/= container key) nil)
                      ((eq value :null) nil)
                      ((not (consp value)) (parser-error value 'subset-elements new-path))
                      ((not (eq (car value) :array)) (parser-error value 'subset-elements new-path))
                      (t (loop
                            :with head := nil :and tail := nil
                            :for element :in (cdr value)
                            :for index :upfrom 0
                            :for new-path-2 := (cons index new-path)
                            :do (multiple-value-bind (parsed fail) (parse-json-value element etype 
                                                                                     :path new-path-2 :session session 
                                                                                     :state state :required required
                                                                                     :nullable nullable :default default)
                                  (unless (eq fail :error)
                                    (let ((link (cons parsed nil)))
                                      (if (null head)
                                          (setf head (setf tail link))
                                          (setf tail (setf (cdr tail) link))))))
                            :finally (setf elements head)))))))
        (make-instance 'subset :offset offset :limit limit 
                       :total total :elements elements))))


(defun make-subset-value-type (element-type 
                               &key (container "elements") (element-required t) 
                                    (element-nullable (not element-required))
                                    (element-default nil))
  (make-instance 'subset-value-type :container container
                 :element-type element-type :element-required element-required
                 :element-nullable element-nullable :element-default element-default))



;;; Complex field types and parsers. 

(defclass avatar-value-type (value-type) ()
  (:default-initargs
    :display-name "Avatar List"
    :lisp-type 'list))

(defparameter +avatar+ (make-instance 'avatar-value-type))

(defmethod parse-json-value* ((object cons) (type avatar-value-type) state session path)
  (if (not (eq (car object) :object))
      (call-next-method)
      (loop
         :with avatars := ()
         :for (key value) :on (cdr object) :by #'cddr
         :do (multiple-value-bind (match groups) (scan-to-strings "^\\s*([0-9]+)\\s*[xX]\\s*([0-9]+)\\s*$" key)
               (when match
                 (let ((width (parse-integer (aref groups 0)))
                       (height (parse-integer (aref groups 1))))
                   (multiple-value-bind (uri fail) 
                       (parse-json-value value +uri+ 
                                         :required nil :nullable nil 
                                         :path (cons key path)
                                         :state state :session session)
                     (when (null fail)
                       (push (make-instance 'avatar 'uri uri :width width :height height)
                             avatars))))))
         :finally (return avatars))))


(defclass user-value-type (value-type) ()
  (:default-initargs
    :identifier "user"
    :display-name "User"
    :lisp-type 'user))

(defparameter +user+ (make-instance 'user-value-type))

(defmethod parse-json-value* ((object cons) (type user-value-type) state session path)
  (if (not (eq (car object) :object))
      (call-next-method)
      (let (uri name display-name email avatars)
        (loop
           :for (key value) :on (cdr object) :by #'cddr
           :for new-path := (cons key path)
           :do (macrolet ((parse (type &rest options)
                            `(parse-json-value value ,type :path new-path :state state :session session 
                                               ,@options)))
                 (string-case (key)
                   ("self" (setf uri (parse +uri+ :required t)))
                   ("name" (setf name (parse +string+ :required t)))
                   ("displayName" (setf display-name (parse +string+ :required nil)))
                   ("emailAddress" (setf email (parse +string+ :required nil)))
                   ("avatarUrls" (setf avatars (parse +avatar+ :required nil)))
                   (t nil))))
        (registering user (name)
          (make-instance 'user 
                         'uri uri :name name :display-name display-name
                         :email-address email :avatars avatars)))))
        
                   

(defclass descriptor-based-value-type (value-type) ())

(defmethod parse-json-value* ((object cons) (type descriptor-based-value-type) state session path)
  (if (not (eq (car object) :object))
      (call-next-method)
      (let (uri id name description icon-uri)
        (loop
           :for (key value) :on (cdr object) :by #'cddr
           :for new-path := (cons key path)
           :do (macrolet ((parse (type &rest options)
                            `(parse-json-value value ,type :path new-path :state state :session session 
                                               ,@options)))
                 (string-case (key)
                   ("self" (setf uri (parse +uri+ :required t)))
                   ("id" (setf id (parse +string+ :required t)))
                   ("name" (setf name (parse +string+ :required nil)))
                   ("description" (setf description (parse +string+ :required nil)))
                   ("iconUrl" (setf icon-uri (parse +uri+ :required nil)))
                   (t nil))))
        (register-session-object state session (value-type-lisp-type type) id
                                 (lambda ()
                                   (make-instance (value-type-lisp-type type)
                                                  'uri uri :id id :name name 
                                                  :description description
                                                  'icon-uri icon-uri))))))



(defclass resolution-value-type (descriptor-based-value-type) ()
  (:default-initargs
    :identifier "resolution"
    :display-name "Resolution"
    :lisp-type 'resolution))

(defparameter +resolution+ (make-instance 'resolution-value-type))

(defclass status-value-type (descriptor-based-value-type) ()
  (:default-initargs
    :identifier "status"
    :display-name "Status"
    :lisp-type 'status))

(defparameter +status+ (make-instance 'status-value-type))

(defclass priority-value-type (descriptor-based-value-type) ()
  (:default-initargs
    :identifier "priority"
    :display-name "Priority"
    :lisp-type 'priority))

(defparameter +priority+ (make-instance 'priority-value-type))

(defclass issue-type-value-type (descriptor-based-value-type) ()
  (:default-initargs
    :identifier "issuetype"
    :display-name "Issue Type"
    :lisp-type 'issue-type))

(defparameter +issue-type+ (make-instance 'issue-type-value-type))

(defclass component-value-type (descriptor-based-value-type) ()
  (:default-initargs
    :display-name "Component"
    :lisp-type 'component))

(defparameter +component+ (make-instance 'component-value-type))




(defclass issue-link-type-value-type (value-type) ()
  (:default-initargs
    :display-name "Issue Link Type"
    :lisp-type 'issue-link-type))

(defparameter +issue-link-type+ (make-instance 'issue-link-type-value-type))

(defmethod parse-json-value* ((object cons) (type issue-link-type-value-type) state session path)
  (if (not (eq (car object) :object))
      (call-next-method)
      (let (uri id name inwards outwards)
        (loop
           :for (key value) :on (cdr object) :by #'cddr
           :for new-path := (cons key path)
           :do (macrolet ((parse (type &rest options)
                            `(parse-json-value value ,type :path new-path :state state :session session 
                                               ,@options)))
                 (string-case (key)
                   ("self" (setf uri (parse +uri+ :required t)))
                   ("id" (setf id (parse +string+ :required t)))
                   ("name" (setf name (parse +string+ :required nil)))
                   ("inwards" (setf inwards (parse +string+ :required nil)))
                   ("outwards" (setf outwards (parse +string+ :required nil)))
                   (t nil))))
        (registering issue-link-type (id) 
          (make-instance 'issue-link-type
                         'uri uri :id id :name name
                         :inwards inwards
                         :outwards outwards)))))



(defclass comment-value-type (value-type) ()
  (:default-initargs
    :display-name "Comment"
    :lisp-type 'comment))

(defparameter +comment+ (make-instance 'comment-value-type))

(defmethod parse-json-value* ((object cons) (type comment-value-type) state session path)
  (if (not (eq (car object) :object))
      (call-next-method)
      (let (uri id body author editor created edited)
        (loop
           :for (key value) :on (cdr object) :by #'cddr
           :for new-path := (cons key path)
           :do (macrolet ((parse (type &rest options)
                            `(parse-json-value value ,type :path new-path :state state :session session 
                                               ,@options)))
                 (string-case (key)
                   ("self" (setf uri (parse +uri+ :required t)))
                   ("id" (setf id (parse +string+ :required t)))
                   ("body" (setf body (parse +string+ :required nil)))
                   ("author" (setf author (parse +user+ :required nil)))
                   ("updateAuthor" (setf editor (parse +user+ :required nil)))
                   ("created" (setf created (parse +timestamp+ :required nil)))
                   ("updated" (setf edited (parse +timestamp+ :required nil)))
                   (t nil))))
        (registering comment (id) 
          (make-instance 'comment
                         'uri uri :id id 
                         :body body :author author :editor editor
                         :created created :edited edited)))))


(defclass project-value-type (value-type) ()
  (:default-initargs
    :display-name "Project"
    :lisp-type 'project))

(defparameter +project+ (make-instance 'project-value-type))

(defparameter +array-of-component+ 
  (make-instance 'array-value-type
                 :lisp-type 'list
                 :element-type +component+
                 :element-required nil
                 :element-nullable nil))

(defparameter +array-of-issue-type+ 
  (make-instance 'array-value-type
                 :lisp-type 'list
                 :element-type +issue-type+
                 :element-required nil
                 :element-nullable nil))

(defparameter +array-of-status+ 
  (make-instance 'array-value-type
                 :lisp-type 'list
                 :element-type +status+
                 :element-required nil
                 :element-nullable nil))

(defparameter +array-of-priority+ 
  (make-instance 'array-value-type
                 :lisp-type 'list
                 :element-type +priority+
                 :element-required nil
                 :element-nullable nil))

(defparameter +array-of-resolution+ 
  (make-instance 'array-value-type
                 :lisp-type 'list
                 :element-type +resolution+
                 :element-required nil
                 :element-nullable nil))


(defmethod parse-json-value* ((object cons) (type project-value-type) state session path)
  (if (not (eq (car object) :object))
      (call-next-method)
      (let (uri id rkey name description components issue-types lead avatars)
        (loop
           :for (key value) :on (cdr object) :by #'cddr
           :for new-path := (cons key path)
           :do (macrolet ((parse (type &rest options)
                            `(parse-json-value value ,type :path new-path :state state :session session 
                                               ,@options)))
                 (string-case (key)
                   ("self" (setf uri (parse +uri+ :required t)))
                   ("id" (setf id (parse +string+ :required t)))
                   ("key" (setf rkey (parse +string+ :required t)))
                   ("name" (setf name (parse +string+ :required nil)))
                   ("description" (setf description (parse +string+ :required nil)))
                   ("components" (setf components (parse +array-of-component+ :required nil)))
                   ("issueTypes" (setf issue-types (parse +array-of-issue-type+ :required nil)))
                   ("lead" (setf lead (parse +user+ :required nil)))
                   ("avatarUrls" (setf avatars (parse +avatar+ :required nil)))
                   (t nil))))
        (registering project (id) 
          (make-instance 'project
                         'uri uri :id id :key rkey 
                         :name name :description description
                         :components components :issue-types issue-types
                         :lead lead :avatars avatars)))))

(defclass schema-field-value-type (value-type) ()
  (:default-initargs
    :display-name "Schema Field"
    :lisp-type 'schema-field))

(defparameter +schema-field+ (make-instance 'schema-field-value-type))

(defparameter +array-of-schema-field+
  (make-instance 'array-value-type
                 :element-type +schema-field+
                 :element-required nil
                 :element-nullable nil))

(defmethod parse-json-value* ((object cons) (type schema-field-value-type) state session path)
  (if (not (eq (car object) :object))
      (call-next-method)
      (let (id name type custom-id plugin orderable searchable navigable)
        (loop
           :for (key value) :on (cdr object) :by #'cddr
           :for new-path := (cons key path)
           :do (macrolet ((parse (value type &rest options)
                            `(parse-json-value ,value ,type :path new-path :state state :session session 
                                               ,@options)))
                 (string-case (key)
                   ("id" (setf id (parse value +string+)))
                   ("name" (setf name (parse value +string+ :required nil)))
                   ("orderable" (setf orderable (parse value +boolean+ :required nil)))
                   ("navigable" (setf navigable (parse value +boolean+ :required nil)))
                   ("searchable" (setf searchable (parse value +boolean+ :required nil)))
                   ("schema" (if (not (and (consp value) (eq (car value) :object)))
                                 (parser-error value 'schema-field-schema new-path)
                                 (loop
                                    :for (key value-2) :on (cdr value) :by #'cddr
                                    :for new-path-2 := (cons key new-path)
                                    :do (string-case (key)
                                          ("type" (setf type (parse value-2 +string+)))
                                          ("custom" (setf plugin (parse value-2 +string+ :required nil)))
                                          ("customId" (setf custom-id (parse value-2 +integer+ :required nil)))
                                          (t nil)))))
                   (t nil))))
        (make-instance 'schema-field 
                       :id id :name name :type type
                       :orderable orderable :searchable searchable
                       :navigable navigable :plugin plugin
                       :custom-id custom-id))))





(defparameter *base-value-types*
  (loop
     :with tree := (attribute-tree)
     :for type :in (list +string+ +boolean+ +timestamp+ +date+ +priority+
                         +status+ +resolution+ +number+ +issue-type+ +user+)
     :for ident := (value-type-identifier type)
     :do (assert (not (null ident)))
         (setf tree (wbtree-update ident type tree))
     :finally (return tree)))

(defparameter *field-overrides*
  (attribute-tree
    "attachment" (make-array-value-type +json+)  ;; FIXME
    "comment" (make-subset-value-type +comment+ :container "comments")
    "components" +array-of-component+
    "project" +project+))
    


(defclass mapped-field ()
  ((key
     :type string :initarg :key
     :reader mapped-field-key)
   (definition
     :type (or null schema-field) :initarg :definition :initform nil
     :reader mapped-field-definition)
   (value-type
     :type value-type :initarg :value-type
     :reader mapped-field-value-type)))

(defmethod print-object ((object mapped-field) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S~@[ ~A~]"
            (mapped-field-key object)
            (mapped-field-value-type object)
            (if (mapped-field-definition object) nil
                '(synthetic)))))


(defmethod initialize-instance :after ((object mapped-field) &key &allow-other-keys)
  (let ((definition (mapped-field-definition object)))
    (when definition
      (let ((fid (schema-field-id definition))
            (ftype (schema-field-type definition)))
        (unless (slot-boundp object 'key) (setf (slot-value object 'key) fid))
        (unless (slot-boundp object 'value-type)
          (let ((rtype (or (wbtree-find fid *field-overrides*)
                           (wbtree-find ftype *base-value-types*))))
            (when rtype
              (setf (slot-value object 'value-type) rtype)))))))
  (unless (slot-boundp object 'value-type)
    (setf (slot-value object 'value-type) +json+)))

(defun mapped-field-id (field)
  (let ((definition (mapped-field-definition field)))
    (and definition (schema-field-id definition))))

(defun mapped-field-name (field)
  (let ((definition (mapped-field-definition field)))
    (and definition (schema-field-name definition))))

(defun mapped-field-searchable-p (field)
  (let ((definition (mapped-field-definition field)))
    (and definition (schema-field-searchable-p definition))))

(defun mapped-field-orderable-p (field)
  (let ((definition (mapped-field-definition field)))
    (and definition (schema-field-orderable-p definition))))

(defun mapped-field-navigable-p (field)
  (let ((definition (mapped-field-definition field)))
    (and definition (schema-field-navigable-p definition))))

(defun mapped-field-custom-id (field)
  (let ((definition (mapped-field-definition field)))
    (and definition (schema-field-custom-id definition))))


(defparameter +key-pseudo-field+
  (make-instance 'mapped-field
                 :key "KEY" :definition nil
                 :value-type +string+))

(defparameter +id-pseudo-field+
  (make-instance 'mapped-field
                 :key "ID" :definition nil
                 :value-type +string+))


(defun compile-mapped-field-index (schema-fields custom-index)
  (loop
     :with index := (attribute-tree)
     :for field :in schema-fields
     :do (let* ((custom-id (schema-field-custom-id field))
                (override (and custom-id 
                               ;; If this is a custom field, then the actual type declaration
                               ;; in the schema-field doesn't even come close the reality sometimes,
                               ;; so treat all custom fields as unparsable, unless the client
                               ;; application gives us a proper parser for the field.
                               (or (wbtree-find custom-id custom-index)
                                   (make-instance 'mapped-field :definition field :value-type +json+))))
                (actual (or override (make-instance 'mapped-field :definition field)))
                (key (schema-field-id field)))
           (setf index (wbtree-update key actual index)))
     :finally (return index)))


(defun session-issue-field-index (&key (session *default-session*)
                                       (allow-loading t) (default nil))
  (let ((cached (session-attribute session 'issue-field-index)))
    (cond
      (cached (values cached t))
      ((not allow-loading) (values default nil))
      (t (with-json-result (body "field" :session session)
           (let* ((actual-fields (parse-json-value body +array-of-schema-field+ 
                                                   :session session :state nil :path nil
                                                   :nullable nil :required t))
                  (custom-index (session-custom-fields session))
                  (field-map (compile-mapped-field-index actual-fields custom-index)))
             (setf (session-attribute session 'issue-field-index) field-map)
             field-map))))))
             

(defclass issue-value-type (value-type) ()
  (:default-initargs
    :display-name "Issue"
    :lisp-type 'issue))

(defparameter +issue+ (make-instance 'issue-value-type))

(defparameter +subset-of-issue+
  (make-subset-value-type +issue+ :container "issues"))

(defmethod parse-json-value* ((object cons) (type issue-value-type) state session path)
  (if (not (eq (car object) :object))
      (call-next-method)
      (let ((attributes (attribute-tree))
            (field-index (session-issue-field-index :session session))
            uri rkey id)
        (loop
           :for (key value) :on (cdr object) :by #'cddr
           :for new-path := (cons key path)
           :do (string-case (key)
                 ("self" (setf uri (parse-json-value value +uri+)))
                 ("id" (setf id (parse-json-value value +string+)))
                 ("key" (setf rkey (parse-json-value value +string+)))
                 ("fields"
                  (cond
                    ((eq value :null) nil)
                    ((not (consp value)) (parser-error value 'issue-field-set new-path))
                    ((not (eq (car value) :object)) (parser-error value 'issue-field-set new-path))
                    (t (loop
                          :for (fkey fvalue) :on (cdr value) :by #'cddr
                          :for new-path-2 := (cons fkey new-path)
                          :for definition := (wbtree-find fkey field-index)
                          :for effective := (or definition (make-instance 'mapped-field :key fkey :value-type +json+))
                          :for type := (mapped-field-value-type effective)
                          :for name := (mapped-field-key effective)
                          :do (multiple-value-bind (parsed fail) (parse-json-value fvalue type
                                                                                   :session session :state state
                                                                                   :path new-path-2 :required nil
                                                                                   :nullable t :default nil)
                                (unless fail
                                  (setf attributes (wbtree-update name (cons parsed effective) attributes))))))))
                 (t nil)))
        (registering issue (id)
          (make-instance 'issue 
                         'uri uri :id id :key rkey
                         :fields attributes)))))



(defun find-issue (key-or-id &key (session *default-session*) 
                                  (fields "*all,-comment,-attachment,-worklog")
                                  (expand nil) (if-does-not-exist :error)
                                  (default nil) (object-cache nil) 
                                  (include-json nil))
  "Obtains information about the issue identified by `key-or-id' from
   the Jira backend referenced by `session'. The value of `fields'
   determines, which data fields should be included in the result.

   If there is no matching issue available on the server, the result
   depends on `if-does-not-exist':
  
   - `:error' the function will raise a transport-error condition
   - `:default' the function returns the value of `default' instead

   The `object-cache' value is passed down as the caching state to 
   the parser functions called to intepret the result."
  (flet ((param (include name value)
           (and include
                (list (cons name (format nil "~A" value))))))
    (let ((parameters (nconc (param t "fields" fields)
                             (param expand "expand" expand))))
      (handler-case (with-json-result (body (format nil "issue/~A" key-or-id) 
                                            :session session 
                                            :parameters parameters)
                      (values (parse-json-value body +issue+ 
                                                :required t :session session :state object-cache
                                                :path nil)
                              t (and include-json body)))
        (transport-error (condition)
          (if (eql (transport-error-status condition) 404)
              (ecase if-does-not-exist
                ((:error) (error condition))
                ((:default) (values default nil (and include-json (transport-error-body condition)))))))))))


(defun search-issues (query &key (session *default-session*) 
                                 (fields "*navigable,summary")
                                 (expand nil) (offset 0) (limit nil)
                                 (object-cache nil))
  "Returns a list of all issues matching the given `query' string;
   at most `limit' elements are returned, starting with the element 
   at the logical offset `offset' in the full result set. The value 
   of `fields' determines, which data fields should be included in 
   the result.

   The `object-cache' value is passed down as the caching state to 
   the parser functions called to intepret the result."
  (flet ((param (include name value)
           (and include
                (list (cons name (format nil "~A" value))))))
    (let ((parameters (nconc (param t "jql" query)
                             (param t "fields" fields)
                             (param expand "expand" expand)
                             (param (plusp offset) "startAt" offset)
                             (param limit "maxResults" limit))))
      (with-json-result (body "search" 
                              :session session 
                              :parameters parameters)
        (let ((result (parse-json-value body +subset-of-issue+ 
                                        :session session :state object-cache :path nil
                                        :required nil)))
          (if result
              (values (subset-elements result) (subset-total result))
              (values nil 0)))))))


(defun find-project (key-or-id 
                     &key (session *default-session*) 
                          (if-does-not-exist :error) (default nil) 
                          (object-cache nil))
  (handler-case (with-json-result (body (format nil "project/~A" key-or-id) :session session)
                  (values (parse-json-value body +project+ 
                                            :required t :session session :state object-cache
                                            :path nil)
                          t))
    (transport-error (condition)
      (if (eql (transport-error-status condition) 404)
          (ecase if-does-not-exist
            ((:error) (error condition))
            ((:default) (values default nil)))))))


(defun find-user (username
                  &key (session *default-session*) 
                       (if-does-not-exist :error) (default nil) 
                       (object-cache nil))
  (handler-case (with-json-result (body "user" :session session :parameters `(("username" . ,username)))
                  (values (parse-json-value body +user+ 
                                            :required t :session session :state object-cache
                                            :path nil)
                          t))
    (transport-error (condition)
      (if (eql (transport-error-status condition) 404)
          (ecase if-does-not-exist
            ((:error) (error condition))
            ((:default) (values default nil)))))))


(defun list-issue-types (&key (session *default-session*) object-cache)
  (with-json-result (body "issuetype")
    (parse-json-value body +array-of-issue-type+ 
                      :required nil :session session :state object-cache)))

(defun list-states (&key (session *default-session*) object-cache)
  (with-json-result (body "status")
    (parse-json-value body +array-of-status+ 
                      :required nil :session session :state object-cache)))

(defun list-priorities (&key (session *default-session*) object-cache)
  (with-json-result (body "priority")
    (parse-json-value body +array-of-priority+ 
                      :required nil :session session :state object-cache)))

(defun list-resolutions (&key (session *default-session*) object-cache)
  (with-json-result (body "resolution")
    (parse-json-value body +array-of-resolution+ 
                      :required nil :session session :state object-cache)))





(defun make-custom-field (id 
                          &key (navigable nil) (searchable nil) (orderable nil)
                               (plugin nil) (key (format nil "customfield_~D" id))
                               (name "Custom Field") (type "custom") value-type)
  (let ((base-field (make-instance 'schema-field 
                                            :id key :name name :orderable orderable
                                            :searchable searchable :navigable navigable
                                            :plugin plugin :custom-id id :type type)))
    (if (not value-type)
        (make-instance 'mapped-field :definition base-field :key key)
        (make-instance 'mapped-field :definition base-field :key key :value-type value-type))))
                       
                 
                 
                                            
