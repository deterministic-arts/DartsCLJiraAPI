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
  (let ((*text-content-types* (cons (cons "application" "json") *text-content-types*)))
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
      ((read-all (stream)
         (let ((result (make-array 128 :element-type 'character :fill-pointer 0 :adjustable t))
               (buffer (make-array 1024 :element-type 'character)))
           (loop
              :for chars-read := (read-sequence buffer stream)
              :while (plusp chars-read)
              :do (loop
                     :for k :upfrom 0 :below chars-read
                     :do (vector-push-extend (char buffer k) result))
              :finally (return result))))               
       (process-value (stream status)
         (if (/= 200 status)
             (transport-error session status (read-json-value stream))
             (funcall function (read-json-value stream)))))
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
  

(defmacro with-json-result ((var uri &rest options) &body body)
  `(invoke-with-json-result (lambda (,var) ,@body)
                            ,uri ,@options))

