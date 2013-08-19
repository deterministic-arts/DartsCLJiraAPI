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


(define-wbtree attribute-tree string<)
(define-wbtree integer-tree <)


(defclass credentials () ()
  (:documentation "Abstract login credentials. Right now, we only
    support username/password. Instances of this class are immutable
    after construction any may be shared across multiple clients."))


(defclass connector ()
  ((base-uri
     :type uri :initarg :base-uri 
     :reader connector-base-uri))
  (:documentation "Basic connection infos. This class provides the
    basic connection information, which we need, and which are not
    dependent on the actual user credentials. Instances of this class
    are immutable after construction and may be shared across 
    multiple clients."))


(defclass session (annotatable)
  ((connector
     :type connector :initarg :connector
     :reader session-connector)
   (credentials
     :type credentials :initarg :credentials
     :reader session-credentials)
   (cookie-jar
     :type cookie-jar :initform (make-instance 'cookie-jar)
     :reader session-cookie-jar)
   (custom-fields 
     :type integer-tree :initarg custom-fields
     :reader session-custom-fields)
   (attributes 
     :type attribute-tree :initform (attribute-tree)
     :accessor session-attributes))
  (:documentation "Jira API session. Instances of this class keep
    track of the state during interactions with the Jira server and
    provide caches for frequently used (but infrequently changed) 
    information like details about priorities, issue types, etc.

    Each session combines a connector, which provides the basic 
    connection parameters, with a set of authentication credentials,
    and a bunch of state information.

    Session instances are not thread-safe and should not be shared
    across concurrently executing threads. They are, however, not
    thread hostile, i.e., if the application provides a proper locking
    disclipline, it is possible to share sessions across threads."))


(defclass username-password (credentials annotatable) 
  ((username
     :type (or null string) :initform nil :initarg :username
     :reader credentials-username)
   (password
     :type (or null string) :initform nil :initarg :password
     :reader credentials-password))
  (:documentation "The default (and currently: only supported) concrete
    implementation of `credentials'. Simply a pair of a username and
    associated password."))


(defvar *default-session* nil
  "The default session, which is used by most API functions, if
   no instance is passed explicitly. Bind this variable to a session
   in order to provide the default.")
  


(defmethod initialize-instance :after ((object session) &key custom-fields &allow-other-keys)
  (unless (slot-boundp object 'custom-fields)
    (loop
       :with index := (integer-tree)
       :for field :in custom-fields
       :do (setf index (wbtree-update (mapped-field-custom-id field) field index))
       :finally (setf (slot-value object 'custom-fields) index))))





(defmethod print-object ((object username-password) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S~@[ ~S~]"
            (or (credentials-username object) "(anonymous)")
            (and (credentials-password object) "***"))))


(defmethod print-object ((object session) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S as ~S"
            (connector-base-uri (session-connector object))
            (session-credentials object))))


(defmethod print-object ((object connector) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S"
            (connector-base-uri object))))



(defun session-attribute (session key &optional default)
  "Returns the value of the session attribute identified by `key'
   as found in `session', of `default', if there is no matching 
   attribute. 

   Session attributes are like session annotations, in that they 
   allow arbitrary information to be associated with sessions. 
   Unlike annotations, attributes are an implementation detail of 
   this library and reserved for use by the library, and subclasses
   of the `session' class, whereas the annotations plist is reserved 
   for use by the client application alone.

   This function returns two values, the attribute value found
   (if any), and a generalized boolean value, which indicates,
   whether the attribute was found or not.

   This function is `setf'-able."
  (wbtree-find key (session-attributes session) default))

(defun (setf session-attribute) (new-value session key &optional default)
  (declare (ignore default))
  (setf (session-attributes session) (wbtree-update key new-value (session-attributes session)))
  new-value)


(defgeneric reset-session (session)
  (:method ((session session))
    (setf (session-attributes session) (attribute-tree))
    (setf (slot-value session 'cookie-jar) (make-instance 'cookie-jar))
    session)
  (:documentation "Remove all cached information from `session'. 
    After this function returns, the given `session' instance is 
    essentially in the same state as right after construction.

    Subclasses of `session' might want to add methods on this
    function to reset additional state they introduce. If so,
    methods should make sure, that the base method is actually
    called via `call-next-method'."))


(defgeneric register-session-object (state session type identifier constructor))

(defmethod register-session-object (state session type identifier constructor)
  (declare (ignore state session type identifier))
  (funcall constructor))


(defgeneric expand-resource-uri (uri base)
  (:method (uri (base uri)) (merge-uris uri base))
  (:method (uri (base connector)) (merge-uris uri (connector-base-uri base)))
  (:method (uri (base session)) (expand-resource-uri uri (session-connector base)))
  (:documentation "Merge the resource-specific relative uri `uri' with
    the base URI designated by `base'."))


(defun generate-auth-header (session)
  (let* ((credentials (session-credentials session))
         (username (or (credentials-username credentials) ""))
         (password (or (credentials-password credentials) ""))
         (combination (concatenate 'string username ":" password))
         (octets (string-to-utf-8-bytes combination))
         (encoded (usb8-array-to-base64-string octets)))
    (concatenate 'string "Basic " encoded)))


(defun send-json-request (session full-uri auth-header 
                          &key (method :get) (content nil) (content-length nil)
                               (parameters nil))
  (let ((*text-content-types* (cons (cons "application" "json") *text-content-types*))
        #-(and) (*header-stream* *standard-output*))
    (http-request full-uri
                  :method method
                  :parameters parameters
                  :cookie-jar (session-cookie-jar session)
                  :additional-headers (list (cons "Authorization" auth-header))
                  :accept "application/json"
                  :content-type "application/json"
                  :content-length content-length
                  :content content
                  :want-stream t 
                  :external-format-in :utf-8
                  :external-format-out :utf-8
                  :close t)))


(defun invoke-with-json-result (function uri
                                &key (method :get) (content nil) (content-length nil)
                                     (parameters nil) (session *default-session*))
  (labels 
      ((collect-body (stream)
         (let ((buffer (make-array 128 :element-type 'character :adjustable t :fill-pointer 0)))
           (setf (flexi-streams:flexi-stream-external-format stream) '(:iso-8859-1))
           (loop
              :for char := (read-char stream nil)
              :while char :do (vector-push-extend char buffer)
              :finally (return buffer))))
       (process-value (stream status)
         (if (<= 200 status 299)
             (funcall function (read-json-value stream))
             (transport-error session status (collect-body stream)))))
    (let ((full-uri (expand-resource-uri uri session))
          (auth-header (generate-auth-header session)))
      (multiple-value-bind (stream status)
          (send-json-request session full-uri auth-header 
                             :method method
                             :content content 
                             :content-length content-length
                             :parameters parameters)
        (unwind-protect (process-value stream status)
          (close stream))))))
  

(defun oneway-request (uri &key (method :delete) (content nil) (content-length nil)
                                (parameters nil) (session *default-session*))
  (labels 
      ((process-value (status)
         (unless (<= 200 status 299)
           (transport-error session status))))
    (let ((full-uri (expand-resource-uri uri session))
          (auth-header (generate-auth-header session)))
      (multiple-value-bind (stream status)
          (send-json-request session full-uri auth-header 
                             :method method
                             :content content 
                             :content-length content-length
                             :parameters parameters)
        (unwind-protect (process-value status)
          (close stream))))))


(defmacro with-json-result ((var uri &rest options) &body body)
  `(invoke-with-json-result (lambda (,var) ,@body)
                            ,uri ,@options))

