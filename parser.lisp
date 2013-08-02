
(in-package "DARTS.LIB.JIRA-API")


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


(defmacro define-json-parser (type-name (value-var state-var path-var) &body body)
  (let* ((package *package*)
         (base-name (symbol-name type-name))
         (name (intern (format nil "PARSE-JSON-~A" base-name) package))
         (nullable (gensym "NULLABLE-"))
         (default (gensym "DEFAULT-"))
         (vvar (gensym "JSON-VALUE-"))
         (svar (gensym "STATE-"))
         (pvar (gensym "PATH-")))
    `(defun ,name (,vvar ,svar ,pvar 
                   &key ((:nullable ,nullable) nil)
                        ((:default ,default) +ignore+))
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
                                      &key (nullable ':error)
                                           (default '+ignore+))
  (let* ((base-name (symbol-name inner-type))
         (type-name (intern (format nil "ARRAY-OF-~A" base-name) *package*))
         (parser-name (intern (format nil "PARSE-JSON-~A" type-name) *package*))
         (inner-parser-name (intern (format nil "PARSE-JSON-~A" base-name) *package*)))
    `(defun ,parser-name (object state path)
       (if (not (json-array-p object))
           (parser-error object ',type-name path)
           (loop
              :for element :in (cdr object)
              :for index :upfrom 0
              :for new-path := (cons index path)
              :for parsed := (,inner-parser-name element state new-path :nullable ,nullable :default ,default)
              :unless (ignorep parsed) :collect parsed)))))




(define-json-parser string (value state path)
  (declare (ignore state))
  (if (stringp value)
      value
      (parser-error value 'string path)))


(define-json-parser boolean (value state path)
  (declare (ignore state))
  (cond
    ((eq value :true) t)
    ((eq value :nil) nil)
    (t (parser-error value 'boolean path))))


(define-json-parser integer (value state path)
  (declare (ignore state))
  (cond
    ((integerp value) value)
    (t (parser-error value 'boolean path))))


(define-json-parser number (value state path)
  (declare (ignore state))
  (cond
    ((numberp value) value)
    (t (parser-error value 'boolean path))))


(define-json-parser uri (value state path)
  (declare (ignore state))
  (cond
    ((stringp value) 
     (handler-case (parse-uri value)
       (error () (parser-error value 'uri path))))
    (t (parser-error value 'boolean path))))


(define-json-parser timestamp (value state path)
  (declare (ignore state))
  (cond
    ((stringp value)
     (let ((parsed (parse-timestring value :fail-on-error nil)))
       (or parsed
           (parser-error value 'timestamp path))))
    (t (parser-error value 'timestamp path))))


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
  (if (not (json-object-p object))
      (parser-error object 'user path)
      (let (name display-name uri email-address avatars)
        (loop
           :for (key value) :on (cdr object) :by #'cddr
           :for new-path := (cons key path)
           :do (string-case (key)
                 ("self" (setf uri (parse-json-uri value state new-path)))
                 ("name" (setf name (parse-json-string value state new-path)))
                 ("displayName" (setf display-name (parse-json-string value state new-path :nullable t :default nil)))
                 ("emailAddress" (setf email-address (parse-json-string value state new-path :nullable t :default nil)))
                 ("avatarUrls" (setf avatars (parse-json-avatar-list value state new-path :nullable t :default nil)))
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



(define-json-parser issue (object state path)
  (if (not (json-object-p object))
      (parser-error object 'issue-type path)
      (let ((attrs (attribute-tree)) uri id rkey)
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
                                    (t nil))))))
                     (t nil)))))
        (interning state (cons 'issue id)
          (make-instance 'issue
                         'uri uri 
                         :id (or id "") 
                         :key (or rkey "")
                         :fields attrs)))))


(define-parser-for-array-of issue)
(define-parser-for-array-of issue-type)
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
                   

(defun find-issue (key-or-id &key (session *default-session*) 
                                  (fields "*all,-comment,-attachment,-worklog")
                                  (expand nil) (if-does-not-exist :error)
                                  (default nil) (object-cache nil))
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
                      (values (parse-json-issue body object-cache nil)
                              t))
        (transport-error (condition)
          (if (eql (transport-error-status condition) 404)
              (ecase if-does-not-exist
                ((:error) (error condition))
                ((:default) (values default nil)))))))))


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
                    (values (parse-json-user body object-cache nil) t))
      (transport-error (condition)
        (if (eql (transport-error-status condition) 404)
            (ecase if-does-not-exist
              ((:error) (error condition))
              ((:default) (values default nil))))))))


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
        (let ((result (parse-json-search-result body object-cache nil)))
          (values (cdr result) (car result)))))))


(defun list-resolutions (&key (session *default-session*) (object-cache nil))
  (with-json-result (body "resolution" :session session)
    (parse-json-array-of-resolution body object-cache nil)))


(defun list-states (&key (session *default-session*) (object-cache nil))
  (with-json-result (body "status" :session session)
    (parse-json-array-of-status body object-cache nil)))


(defun list-priorities (&key (session *default-session*) (object-cache nil))
  (with-json-result (body "priority" :session session)
    (parse-json-array-of-priority body object-cache nil)))


(defun list-issue-types (&key (session *default-session*) (object-cache nil))
  (with-json-result (body "issuetype" :session session)
    (parse-json-array-of-issue-type body object-cache nil)))
