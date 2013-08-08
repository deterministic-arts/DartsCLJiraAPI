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

;; Forward declaration
(declaim (ftype (function (session) attribute-tree) session-schema-field-index))

(defvar +ignore+ (gensym "IGNORE-VALUE")
  "If this value is returned by a parser function, then the
   key/value pair (or array element) will be ignored by the
   caller, and not be added to the object currently under
   construction.")

(defun ignorep (value)
  "True, if `value' refers to the special `+ignored+' value,
   and false otherwise."
  (eq value +ignore+))


(defgeneric intern-session-object (state key constructor)
  (:documentation "Tests, whether there is already an object with
    a given `key' in cache `state'. If so, returns the existing
    object, otherwise, calls factory to construct a new instance,
    and adds it to `state'. The parser calls this function after
    it has successfully parsed a resource. The key is always a
    cons pair whose car is the name of the class, and whose cdr
    is a (class-dependend) identifier.    

    This function has only a single default implementation,
    specializing the `state' argument on type `null'. This method
    will simply always call the constructor. 

    This function is provided for client applications, which want
    to perform caching for all (or at least some sensible subset
    of) the resources being read from the Jira server. This is
    interesting in particular for resource types like issue-type,
    priority, etc., but outside of the scope of this API."))

(defmethod intern-session-object ((state null) key constructor)
  (funcall constructor))


(defmacro interning (state key &body forms)
  (let ((svar (gensym)))
    `(let ((,svar ,state))
       (if (not ,svar) (progn ,@forms)
           (intern-session-object ,svar ,key 
             (lambda () ,@forms))))))


(defun parser-error-p (condition)
  (typep condition 'parser-error))


(defun read-replacement-value ()
  (format *query-io* "~&Enter an expression: ")
  (list (eval (read *query-io*))))


(defun invoke-with-parser-restarts (function value state path
                                    &key (nullable nil) (default +ignore+))
  (tagbody
   retry
     (return-from invoke-with-parser-restarts
       (restart-case (funcall function value state path)
         (skip-offending-value ()
           :test (lambda (condition) (and (member nullable '(t :error)) (parser-error-p condition)))
           :report (lambda (stream) (format stream "Skip the offending value~@[ at ~S~]" path))
           default)
         (use-value (replacement)
           :test parser-error-p
           :report (lambda (stream) (format stream "Provide a replacement for the value~@[ at ~S~]" path))
           :interactive read-replacement-value
           (setf value replacement)
           (go retry))))))


(defun wrap-parser-function (function)
  (lambda (value state path)
    (invoke-with-parser-restarts function value state path)))


(defun json-object-p (value)
  (and (consp value) (eq :object (car value))))

(defun json-array-p (value)
  (and (consp value) (eq :array (car value))))


(defun null-parser (value state path &key (session nil) (nullable t) (default +ignore+))
  (declare (ignore state session))
  (if (eq value :null)
      (case nullable 
        ((t :null) default)
        (otherwise (parser-error value 'anything path)))
      (cond
        ((eq value :true) t)
        ((eq value :false) nil)
        ((stringp value) value)
        ((numberp value) value)
        ((consp value) value)
        (t (error "internal error: bad JSON value ~S" value)))))


;;; A parser function is a function 
;;;
;;;   (lambda (value state path &key nullable default) ...)
;;;
;;; which takes
;;;
;;; - `value' the value to parse
;;; - `state' an application supplied state used for caching
;;; - `path' a list indicating the path to `value' in the graph being parsed
;;;
;;; and returns the parsed representation of `value' or (if the
;;; caller allows it) `+ignore+'.
;;;
;;; Accepted values for `nullable' are
;;;
;;; - `nil' if the value is required 
;;; - `:error' if it may be ignored if parsing fails
;;; - `:null' if it may be ignored if it was explicitly null in the input
;;; - `t' if the value is always optional
;;;
;;; The value of `default' is returned by the parser, if the input
;;; value was `:null' and `nullable' did allow for this, or if
;;; a parse error occurred, `nullable' allows skipping the value
;;; in that situation, and the `skip-offending-value' restart was
;;; invoked.
;;;
;;; Each parser function defined using the macro below provides
;;; a few default restarts, which are active in case of parser-errors.
;;; These restarts may be used to recover from errors either from
;;; the debugger or programmatically by calling the appropriate 
;;; restart function.
;;;
;;; The restart named `skip-offending-value' is active, if the
;;; caller of the parser function specified, that a null value is
;;; acceptable in case of errors (i.e., by supplying `nullable'
;;; as either `t' or `:error'). If it is invoked, the parser
;;; function will return the `default' value.
;;;
;;; The restart `use-value' is always active. It can be used to
;;; provide a replacement value, which will then be parsed instead
;;; of the original offending value.
;;;
;;; FIXME: the interface should better be
;;;
;;;   (lambda (value &key state path nullable default) ...)
;;;
;;; which would make the parser functions nicer to use in a stand-
;;; alone fashion.

(defmacro object-field-dispatch (((key value path) object &optional old-path) &body clauses)
  (let ((rkey (gensym))
        (obvar (gensym))
        (ptvar (gensym)))
    `(loop
        :with ,obvar := ,object :and ,ptvar := ,old-path
        :for (,rkey ,value) :on (cdr ,obvar) :by #'cddr
        :for ,key := ,rkey
        :for ,path := (cons ,rkey ,ptvar)
        :do (string-case (,rkey)
              ,@clauses))))


(defmacro with-parser ((state path &optional (name 'parse)) &body body)
  (let ((value (gensym))
        (type (gensym))
        (nullable (gensym))
        (default (gensym))
        (uname (gensym)))
    `(macrolet ((,name (,value ,type &key ((:nullable ,nullable) ':error) ((:default ,default) '+ignore+))
                  (let ((,uname (intern (format nil "PARSE-JSON-~A" (symbol-name ,type)) *package*)))
                    (list ,uname ,value ',state ',path :nullable ,nullable :default ,default))))
       ,@body)))


(defmacro ensuring-object ((object form &optional (path 'nil) (type ''resource)) &body body)
  (let ((bvar (gensym)))
    `(let ((,bvar ,form))
       (if (not (json-object-p ,bvar))
           (parser-error ,bvar ,type ,path)
           (let ((,object ,bvar))
             ,@body)))))
  


(defmacro define-json-parser (type-name (value-var state-var path-var &optional session-var) &body body)
  (let* ((package *package*)
         (base-name (symbol-name type-name))
         (name (intern (format nil "PARSE-JSON-~A" base-name) package))
         (nullable (gensym "NULLABLE-"))
         (default (gensym "DEFAULT-"))
         (vvar (gensym "JSON-VALUE-"))
         (svar (gensym "STATE-"))
         (pvar (gensym "PATH-"))
         (evar (or session-var (gensym "SESSION-"))))
    `(defun ,name (,vvar ,svar ,pvar 
                   &key ((:session ,evar) *default-session*)
                        ((:nullable ,nullable) nil)
                        ((:default ,default) +ignore+))
       ,@(unless session-var `((declare (ignore ,evar))))
       (invoke-with-parser-restarts (lambda (,vvar ,svar ,pvar)
                                      (if (eq ,vvar ':null)
                                          (case ,nullable
                                            ((t :null) ,default)
                                            (otherwise
                                              (parser-error ,vvar '(:strict ,type-name) 
                                                            ,pvar)))
                                          (let ((,value-var ,vvar)
                                                (,state-var ,svar)
                                                (,path-var ,pvar))
                                            ,@body)))
                                   ,vvar ,svar ,pvar
                                   :nullable ,nullable
                                   :default ,default))))


(defmacro define-parser-for-array-of (inner-type
                                      &key (array-nullable 't)
                                           (array-default 'nil)
                                           (element-nullable ':error)
                                           (element-default '+ignore+))
  (let* ((base-name (symbol-name inner-type))
         (type-name (intern (format nil "ARRAY-OF-~A" base-name) *package*))
         (parser-name (intern (format nil "PARSE-JSON-~A" type-name) *package*))
         (session (gensym "SESSION-"))
         (nullable (gensym "NULLABLE-"))
         (default (gensym "DEFAULT-"))
         (inner-parser-name (intern (format nil "PARSE-JSON-~A" base-name) *package*)))
    `(defun ,parser-name (object state path
                          &key ((:session ,session) *default-session*)
                               ((:nullable ,nullable) ,array-nullable)
                               ((:default ,default) ,array-default))
       (invoke-with-parser-restarts (lambda (object state path)
                                      (if (not (json-array-p object))
                                          (parser-error object ',type-name path)
                                          (loop
                                             :for element :in (cdr object)
                                             :for index :upfrom 0
                                             :for new-path := (cons index path)
                                             :for parsed := (,inner-parser-name element state new-path 
                                                                                :session ,session
                                                                                :nullable ,element-nullable 
                                                                                :default ,element-default)
                                             :unless (ignorep parsed) :collect parsed)))
                                    object state path
                                    :nullable ,nullable
                                    :default ,default))))


(defmacro define-parser-for-container-of (inner-type elements
                                          &key (container-nullable 't)
                                               (container-default '+ignore+)
                                               (element-nullable ':error)
                                               (element-default '+ignore+))
  (let* ((base-name (symbol-name inner-type))
         (type-name (intern (format nil "CONTAINER-OF-~A" base-name) *package*))
         (parser-name (intern (format nil "PARSE-JSON-~A" type-name) *package*))
         (element-parser (intern (format nil "PARSE-JSON-ARRAY-OF-~A" inner-type) *package*))
         (session (gensym "SESSION-"))
         (nullable (gensym "NULLABLE-"))
         (default (gensym "DEFAULT-")))
    `(defun ,parser-name (object state path
                          &key ((:session ,session) *default-session*)
                               ((:nullable ,nullable) ,container-nullable)
                               ((:default ,default) ,container-default))
       (invoke-with-parser-restarts (lambda (object state path)
                                      (cond
                                        ((eq object :null)
                                         (case ,nullable
                                           ((t :null) ,default)
                                           (otherwise (parser-error object '(:strict ,type-name) path))))
                                        ((not (json-object-p object)) (parser-error object ',type-name path))
                                        (t (loop
                                              :with offset := 0 :and limit := nil :and total := 0 
                                                :and elements := nil
                                              :for (key value) :on (cdr object) :by #'cddr
                                              :for new-path := (cons key path)
                                              :do (cond
                                                    ((string= key "startAt") (setf offset (parse-json-integer value state new-path)))
                                                    ((string= key "maxResults") (setf limit (parse-json-integer value state new-path)))
                                                    ((string= key "total") (setf total (parse-json-integer value state new-path)))
                                                    ((string= key ,elements) (setf elements (,element-parser value state new-path
                                                                                                             :session ,session
                                                                                                             :nullable ,element-nullable
                                                                                                             :default ,element-default))))
                                              :finally (return (make-instance 'container :offset offset :limit limit :total total
                                                                              :elements elements))))))
                                    object state path 
                                    :nullable ,nullable
                                    :default ,default))))


(define-json-parser string (value state path)
  (declare (ignore state))
  (if (stringp value)
      value
      (parser-error value 'string path)))


(define-json-parser boolean (value state path)
  (declare (ignore state))
  (cond
    ((eq value :true) t)
    ((eq value :false) nil)
    (t (parser-error value 'boolean path))))


(define-json-parser integer (value state path)
  (declare (ignore state))
  (cond
    ((integerp value) value)
    (t (parser-error value 'integer path))))


(define-json-parser number (value state path)
  (declare (ignore state))
  (cond
    ((numberp value) value)
    (t (parser-error value 'number path))))


(define-json-parser uri (value state path)
  (declare (ignore state))
  (cond
    ((stringp value) 
     (handler-case (parse-uri value)
       (error () (parser-error value 'uri path))))
    (t (parser-error value 'uri path))))


(define-json-parser timestamp (value state path)
  (declare (ignore state))
  (cond
    ((stringp value)
     (let ((parsed (parse-timestring value :fail-on-error nil)))
       (or parsed
           (parser-error value 'timestamp path))))
    (t (parser-error value 'timestamp path))))


(define-json-parser date (value state path)
  (declare (ignore state))
  (cond
    ((stringp value)
     (let ((parsed (parse-timestring value 
                                     :fail-on-error nil 
                                     :allow-missing-date-part nil
                                     :allow-missing-time-part t)))
       (or parsed
           (parser-error value 'date path))))
    (t (parser-error value 'date path))))


(define-json-parser avatar-list (value state path)
  (if (not (json-object-p value))
      (parser-error value 'avatar-list path)
      (loop
         :with avatars := ()
         :for (key value) :on (cdr value) :by #'cddr
         :do (multiple-value-bind (match groups) (scan-to-strings "^\\s*([0-9]+)\\s*[xX]\\s*([0-9]+)\\s*$" key)
               (when match
                 (let ((width (parse-integer (aref groups 0)))
                       (height (parse-integer (aref groups 1)))
                       (uri (parse-json-uri value state (cons key path))))
                   (push (make-instance 'avatar 'uri uri :width width :height height)
                         avatars))))
         :finally (return avatars))))


(define-json-parser user (object state path)
  (ensuring-object (object object path 'user)
    (let (name display-name uri email-address avatars)
      (with-parser (state new-path)
        (object-field-dispatch ((key value new-path) object path)
          ("self" (setf uri (parse value uri)))
          ("name" (setf name (string value)))
          ("displayName" (setf display-name (parse value string :nullable t :default nil)))
          ("emailAddress" (setf email-address (parse value string :nullable t :default nil)))
          ("avatarUrls" (setf avatars (parse value avatar-list :nullable t :default nil)))
          (t nil)))
      (interning state (cons 'user name)
        (make-instance 'user 
                       :uri uri :name name
                       :display-name display-name
                       :email-address email-address
                       :avatars avatars)))))


(define-json-parser priority (object state path)
  (if (not (json-object-p object))
      (parser-error object 'priority path)
      (let (uri name id description icon-uri)
        (loop
           :for (key value) :on (cdr object) :by #'cddr
           :for new-path := (cons key path)
           :do (string-case (key)
                 ("self" (setf uri (parse-json-uri value state new-path)))
                 ("id" (setf id (parse-json-string value state new-path)))
                 ("name" (setf name (parse-json-string value state new-path)))
                 ("description" (setf description (parse-json-string value state new-path :nullable t :default nil)))
                 ("iconUrl" (setf icon-uri (parse-json-uri value state new-path :nullable t :default nil)))
                 (t nil)))
        (interning state (cons 'priority id)
          (make-instance 'priority
                         :id id 'uri uri 
                         :name name
                         :description description
                         'icon-uri icon-uri)))))


(define-json-parser status (object state path)
  (if (not (json-object-p object))
      (parser-error object 'status path)
      (let (uri name id description icon-uri)
        (loop
           :for (key value) :on (cdr object) :by #'cddr
           :for new-path := (cons key path)
           :do (string-case (key)
                 ("self" (setf uri (parse-json-uri value state new-path)))
                 ("id" (setf id (parse-json-string value state new-path)))
                 ("name" (setf name (parse-json-string value state new-path)))
                 ("description" (setf description (parse-json-string value state new-path :nullable t :default nil)))
                 ("iconUrl" (setf icon-uri (parse-json-uri value state new-path :nullable t :default nil)))
                 (t nil)))
        (interning state (cons 'status id)
          (make-instance 'status
                         :id id 'uri uri 
                         :name name
                         :description description
                         'icon-uri icon-uri)))))


(define-json-parser resolution (object state path)
  (if (not (json-object-p object))
      (parser-error object 'resolution path)
      (let (uri name id description icon-uri)
        (loop
           :for (key value) :on (cdr object) :by #'cddr
           :for new-path := (cons key path)
           :do (string-case (key)
                 ("self" (setf uri (parse-json-uri value state new-path)))
                 ("id" (setf id (parse-json-string value state new-path)))
                 ("name" (setf name (parse-json-string value state new-path)))
                 ("description" (setf description (parse-json-string value state new-path :nullable t :default nil)))
                 ("iconUrl" (setf icon-uri (parse-json-uri value state new-path :nullable t :default nil)))
                 (t nil)))
        (interning state (cons 'resolution id)
          (make-instance 'resolution
                         :id id 'uri uri 
                         :name name
                         :description description
                         'icon-uri icon-uri)))))


(define-json-parser issue-link-type (object state path)
  (if (not (json-object-p object))
      (parser-error object 'issue-link-type path)
      (let (uri id name inwards outwards)
        (loop
           :for (key value) :on (cdr object) :by #'cddr
           :for new-path := (cons key path)
           :do (string-case (key)
                 ("self" (setf uri (parse-json-uri value state new-path)))
                 ("id" (setf id (parse-json-string value state new-path)))
                 ("name" (setf name (parse-json-string value state new-path :nullable t :default nil)))
                 ("inwards" (setf inwards (parse-json-string value state new-path :nullable t :default nil)))
                 ("outwards" (setf outwards (parse-json-string value state new-path :nullable t :default nil)))
                 (t nil)))
        (interning state (cons 'issue-link-type id)
          (make-instance 'issue-link-type
                         :id id 'uri uri 
                         :name name :inwards inwards
                         :outwards outwards)))))


(define-json-parser issue-type (object state path)
  (if (not (json-object-p object))
      (parser-error object 'issue-type path)
      (let (uri name id description icon-uri)
        (loop
           :for (key value) :on (cdr object) :by #'cddr
           :for new-path := (cons key path)
           :do (string-case (key)
                 ("self" (setf uri (parse-json-uri value state new-path)))
                 ("id" (setf id (parse-json-string value state new-path)))
                 ("name" (setf name (parse-json-string value state new-path)))
                 ("description" (setf description (parse-json-string value state new-path :nullable t :default nil)))
                 ("iconUrl" (setf icon-uri (parse-json-uri value state new-path :nullable t :default nil)))
                 (t nil)))
        (interning state (cons 'issue-type id)
          (make-instance 'issue-type
                         :id id 'uri uri 
                         :name name
                         :description description
                         'icon-uri icon-uri)))))

(define-parser-for-array-of issue-type)



(define-json-parser comment (object state path)
  (if (not (json-object-p object))
      (parser-error object 'comment path)
      (let (uri id body author editor created edited)
        (loop
           :for (key value) :on (cdr object) :by #'cddr
           :for new-path := (cons key path)
           :do (string-case (key)
                 ("self" (setf uri (parse-json-uri value state new-path)))
                 ("id" (setf id (parse-json-string value state new-path)))
                 ("body" (setf body (parse-json-string value state new-path :nullable t)))
                 ("author" (setf author (parse-json-user value state new-path :nullable t)))
                 ("updateAuthor" (setf author (parse-json-user value state new-path :nullable t)))
                 ("created" (setf created (parse-json-timestamp value state new-path :nullable t)))
                 ("updated" (setf edited (parse-json-timestamp value state new-path :nullable t)))
                 (t nil)))
        (interning state (cons 'comment id)
          (make-instance 'comment
                         :id id 'uri uri 
                         :body body :author author
                         :editor editor :created created
                         :edited edited)))))


(define-parser-for-array-of comment)
(define-parser-for-container-of comment "comments")


(define-json-parser component (object state path)
  (if (not (json-object-p object))
      (parser-error object 'component path)
      (let (uri name id description icon-uri)
        (loop
           :for (key value) :on (cdr object) :by #'cddr
           :for new-path := (cons key path)
           :do (string-case (key)
                 ("self" (setf uri (parse-json-uri value state new-path)))
                 ("id" (setf id (parse-json-string value state new-path)))
                 ("name" (setf name (parse-json-string value state new-path)))
                 ("description" (setf description (parse-json-string value state new-path :nullable t :default nil)))
                 ("iconUrl" (setf icon-uri (parse-json-uri value state new-path :nullable t :default nil)))
                 (t nil)))
        (interning state (cons 'component id)
          (make-instance 'component
                         :id id 'uri uri 
                         :name name
                         :description description
                         'icon-uri icon-uri)))))


(define-parser-for-array-of component)


(define-json-parser project (object state path)
  (if (not (json-object-p object))
      (parser-error object 'project path)
      (let (uri name id rkey description avatars issue-types
            components lead)
        (loop
           :for (key value) :on (cdr object) :by #'cddr
           :for new-path := (cons key path)
           :do (string-case (key)
                 ("self" (setf uri (parse-json-uri value state new-path)))
                 ("id" (setf id (parse-json-string value state new-path)))
                 ("key" (setf rkey (parse-json-string value state new-path)))
                 ("name" (setf name (parse-json-string value state new-path)))
                 ("description" (setf description (parse-json-string value state new-path :nullable t :default nil)))
                 ("components" (setf components (parse-json-array-of-component value state new-path)))
                 ("issueTypes" (setf issue-types (parse-json-array-of-issue-type value state new-path)))
                 ("lead" (setf lead (parse-json-user value state new-path)))
                 ("avatarUrls" (setf avatars (parse-json-avatar-list value state new-path)))
                 (t nil)))
        (interning state (cons 'project id)
          (make-instance 'project
                         'uri uri :id id :key rkey 
                         :name name :description description
                         :components components :issue-types issue-types
                         :lead lead :avatars avatars)))))


(define-json-parser issue (object state path session)
  (if (not (json-object-p object))
      (parser-error object 'issue-type path)
      (let ((attrs (attribute-tree)) uri id rkey field-index)
        (flet ((remember (kw value)
                 (unless (ignorep value)
                   (setf attrs (wbtree-update kw value attrs))
                   value)))
          (macrolet ((pushattr (kw parser)
                       `(remember ',kw (,parser value state new-path :nullable t))))
            (loop
               :for (key value) :on (cdr object) :by #'cddr
               :for new-path := (cons key path)
               :do (string-case (key)
                     ("self" (setf uri (parse-json-uri value state new-path)))
                     ("id" (setf id (parse-json-string value state new-path)))
                     ("key" (setf rkey (parse-json-string value state new-path)))
                     ("fields"
                      (cond 
                        ((eq value :null) nil)
                        ((not (json-object-p value)) (parser-error value 'issue-fields new-path))
                        (t (loop
                              :with base-path := new-path
                              :for (key value) :on (cdr value) :by #'cddr
                              :for new-path := (cons key base-path)
                              :do (string-case (key)
                                    ("reporter" (pushattr :reporter parse-json-user))
                                    ("assignee" (pushattr :assignee parse-json-user))
                                    ("created" (pushattr :created parse-json-timestamp))
                                    ("updated" (pushattr :updated parse-json-timestamp))
                                    ("status" (pushattr :status parse-json-status))
                                    ("issuetype" (pushattr :issue-type parse-json-issue-type))
                                    ("resolution" (pushattr :resolution parse-json-resolution))
                                    ("priority" (pushattr :priority parse-json-priority))
                                    ("summary" (pushattr :summary parse-json-string))
                                    ("description" (pushattr :description parse-json-string))
                                    ("components" (pushattr :components parse-json-array-of-component))
                                    ("resolutiondate" (pushattr :resolution-date parse-json-date))
                                    ("duedate" (pushattr :due-date parse-json-date))
                                    ("comment" (pushattr :comment parse-json-container-of-comment))
                                    ("project" (pushattr :project parse-json-project))
                                    ("issuelinks" (pushattr :issue-links parse-json-array-of-issue-link))
                                    (t (let* ((index (or field-index
                                                         (and session
                                                              (setf field-index (session-schema-field-index session)))))
                                              (field (and index (wbtree-find key index)))
                                              (parser (or (and field (schema-field-parser field)) #'null-parser)))
                                         (let ((stored (format nil "X-~A" key))
                                               (parsed (funcall parser value state new-path
                                                                :nullable t :default +ignore+
                                                                :session session)))
                                           (unless (ignorep parsed)
                                             (setf attrs (wbtree-update stored parsed attrs)))))))))))
                     (t nil)))))
        (interning state (cons 'issue id)
          (make-instance 'issue
                         'uri uri 
                         :id (or id "") 
                         :key (or rkey "")
                         :fields attrs)))))


(define-json-parser issue-link (object state path)
  (if (not (json-object-p object))
      (parser-error object 'issue-link path)
      (let (uri id type inwards outwards)
        (loop
           :for (key value) :on (cdr object) :by #'cddr
           :for new-path := (cons key path)
           :do (string-case (key)
                 ("self" (setf uri (parse-json-uri value state new-path)))
                 ("id" (setf id (parse-json-string value state new-path)))
                 ("type" (setf type (parse-json-issue-link-type value state new-path)))
                 ("inwardIssue" (setf inwards (parse-json-issue value state new-path)))
                 ("outwardIssue" (setf outwards (parse-json-issue value state new-path)))
                 (t nil)))
        (interning state (cons 'issue-link id)
          (make-instance 'issue-link
                         'uri uri :id id
                         :type type :inward-issue inwards
                         :outward-issue outwards)))))


(define-parser-for-array-of issue-link)
(define-parser-for-array-of issue)
(define-parser-for-array-of status)
(define-parser-for-array-of priority)
(define-parser-for-array-of resolution)


(define-json-parser search-result (object state path)
  (if (not (json-object-p object))
      (parser-error object 'search-result path)
      (loop
         :with issues := nil :and total := 0
         :for (key value) :on (cdr object) :by #'cddr
         :for new-path := (cons key path)
         :do (string-case (key)
               ("total" (setf total (parse-json-integer value state new-path :nullable t :default 0)))
               ("issues" (setf issues (parse-json-array-of-issue value state new-path)))
               (t nil))
         :finally (return (cons total issues)))))


(defun plist-ref (plist indicator &key (default nil) (test #'eql))
  (loop
     :for (key value) :on plist :by #'cddr
     :when (funcall test key indicator)
     :do (return (values value t))
     :finally (return (values default nil))))



(define-json-parser custom-string (value state path session)
  (cond
    ((eq value :true) "true")
    ((eq value :false) "false")
    ((integerp value) (format nil "~D" value))
    ((numberp value) (format nil "~F" value))
    ((stringp value) value)
    ((consp value)
     (if (eq (car value) :object)
         (let ((vtag (plist-ref (cdr value) "value" :test #'string=)))
           (if vtag 
               (parse-json-custom-string vtag state (cons "value" path) :session session)
               (format nil "~S" value)))
         (format nil "~S" value)))
    (t (format nil "~S" value))))



(defparameter *parser-table*
  (attribute-tree 
    "string" #'parse-json-custom-string
    "datetime" #'parse-json-timestamp
    "date" #'parse-json-date
    "priority" #'parse-json-priority
    "issuetype" #'parse-json-issue-type
    "resolution" #'parse-json-resolution
    "status" #'parse-json-status
    "project" #'parse-json-project
    "user" #'parse-json-user))



(define-json-parser schema-field (object state path) 
  (if (not (json-object-p object))
      (parser-error object 'schema-field path)
      (let (id name type custom-id plugin orderable searchable navigable)
        (loop
           :for (key value) :on (cdr object) :by #'cddr
           :for new-path := (cons key path)
           :do (string-case (key)
                 ("id" (setf id (parse-json-string value state new-path :nullable nil)))
                 ("name" (setf name (parse-json-string value state new-path :nullable t :default nil)))
                 ("orderable" (setf orderable (parse-json-boolean value state new-path :nullable t :default nil)))
                 ("navigable" (setf navigable (parse-json-boolean value state new-path :nullable t :default nil)))
                 ("searchable" (setf searchable (parse-json-boolean value state new-path :nullable t :default nil)))
                 ("schema" 
                  (if (not (json-object-p value))
                      (parser-error value 'schema-field-schema new-path)
                      (loop
                         :for (key value-2) :on (cdr value) :by #'cddr
                         :for new-path-2 := (cons key new-path)
                         :do (string-case (key)
                               ("type" (setf type (parse-json-string value-2 state new-path-2 :nullable nil)))
                               ("custom" (setf plugin (parse-json-string value-2 state new-path-2 :nullable t :default nil)))
                               ("customId" (setf custom-id (parse-json-integer value-2 state new-path-2 :nullable t :default nil)))
                               (t nil)))))
                 (t nil)))
        (make-instance 'schema-field 
                       :id id :name name :type type
                       :orderable orderable :searchable searchable
                       :navigable navigable :plugin plugin
                       :custom-id custom-id))))


(define-parser-for-array-of schema-field)


(defmethod slot-unbound (class (object schema-field) (slot (eql 'parser)))
  (declare (ignore slot class))
  (let ((parser (wbtree-find (schema-field-type object) *parser-table* #'null-parser)))
    (setf (slot-value object 'parser) parser)))


(defun list-schema-fields (&key (session *default-session*) (object-cache nil))
  (with-json-result (body "field" :session session)
    (parse-json-array-of-schema-field body object-cache nil :session session)))


(defun session-schema-field-index (session)
  (let ((cached-index (session-attribute session 'schema-field-index)))
    (or cached-index
        (let ((all (list-schema-fields :session session))
              (tree (attribute-tree)))
          (loop
             :for field :in all
             :do (setf tree (wbtree-update (schema-field-id field) field tree)))
          (setf (session-attribute session 'schema-field-index) tree)
          tree))))
                   

(defun find-issue (key-or-id &key (session *default-session*) 
                                  (fields "*all,-comment,-attachment,-worklog")
                                  (expand nil) (if-does-not-exist :error)
                                  (default nil) (object-cache nil) (include-json nil))
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
                      (values (parse-json-issue body object-cache nil :session session)
                              t (and include-json body)))
        (transport-error (condition)
          (if (eql (transport-error-status condition) 404)
              (ecase if-does-not-exist
                ((:error) (error condition))
                ((:default) (values default nil (and include-json (transport-error-body condition)))))))))))


(defun find-user (username &key (session *default-session*)
                                (if-does-not-exist :error) (default nil)
                                (object-cache nil))
  "Obtains information about the user, whose username is `username'
   from the server being referenced by `session'. 

   If there is no matching user available on the server, the result
   depends on `if-does-not-exist':
  
   - `:error' the function will raise a transport-error condition
   - `:default' the function returns the value of `default' instead

   The `object-cache' value is passed down as the caching state to 
   the parser functions called to intepret the result."
  (let ((parameters `(("username" . ,username))))
    (handler-case (with-json-result (body "user" :session session :parameters parameters)
                    (values (parse-json-user body object-cache nil :session session) t))
      (transport-error (condition)
        (if (eql (transport-error-status condition) 404)
            (ecase if-does-not-exist
              ((:error) (error condition))
              ((:default) (values default nil))))))))


(defun find-project (id-or-key &key (session *default-session*)
                                    (if-does-not-exist :error) (default nil)
                                    (object-cache nil) (include-json nil))
  (handler-case (with-json-result (body (format nil "project/~A" id-or-key) :session session)
                  (values (parse-json-project body object-cache nil :session session) t
                          (and include-json body)))
      (transport-error (condition)
        (if (eql (transport-error-status condition) 404)
            (ecase if-does-not-exist
              ((:error) (error condition))
              ((:default) (values default 
                                  nil
                                  (and include-json
                                       (transport-error-body condition)))))))))


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
        (let ((result (parse-json-search-result body object-cache nil :session session)))
          (values (cdr result) (car result)))))))


(defun list-resolutions (&key (session *default-session*) (object-cache nil))
  (with-json-result (body "resolution" :session session)
    (parse-json-array-of-resolution body object-cache nil :session session)))


(defun list-states (&key (session *default-session*) (object-cache nil))
  (with-json-result (body "status" :session session)
    (parse-json-array-of-status body object-cache nil :session session)))


(defun list-priorities (&key (session *default-session*) (object-cache nil))
  (with-json-result (body "priority" :session session)
    (parse-json-array-of-priority body object-cache nil :session session)))


(defun list-issue-types (&key (session *default-session*) (object-cache nil))
  (with-json-result (body "issuetype" :session session)
    (parse-json-array-of-issue-type body object-cache nil :session session)))
