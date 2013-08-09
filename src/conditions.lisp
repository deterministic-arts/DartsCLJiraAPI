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


(define-condition jira-error (error) ()
  (:documentation "Base class for all conditions signalled by this library
    due to run-time errors."))


(define-condition transport-error (jira-error)
  ((session
     :initform nil :initarg :session
     :reader transport-error-session)
   (status
     :initform 500 :initarg :status
     :reader transport-error-status)
   (body
     :initform nil :initarg :body
     :reader transport-error-body))
  (:report (lambda (condition stream)
             (format stream "the server did respond with status code ~S~@[~%~<~S~:>~]"
                     (transport-error-status condition)
                     (transport-error-body condition))))
  (:documentation "A condition if this type is signalled, if the server
    responds with a non-200 (a.k.a., success) status code. The body of the
    response can be obtained by reading the `body' slot of the condition.
    The value is already parsed as json value."))


(defun transport-error (session status &optional body)
  "Signals a non-continuable error of type `transport-error', using the
   values provided to initialize the condition instance."
  (error 'transport-error 
         :session session :status status 
         :body body))
           

(define-condition parser-error (jira-error)
  ((datum 
     :initform nil :initarg :datum
     :reader parser-error-datum)
   (expected-type
     :initform nil :initarg :expected-type
     :reader parser-error-expected-type)
   (path
     :initform nil :initarg :path
     :reader parser-error-path))
  (:report (lambda (condition stream)
             (format stream "the value ~S~@[ found at ~S~] cannot be interpreted as ~S"
                     (parser-error-datum condition)
                     (parser-error-path condition)
                     (parser-error-expected-type condition))))
  (:documentation "A condition of this type is signalled by the JSON parsing
    code, if it cannot process a response according to the expectations. The
    parser usually provides restarts, which allow to skip the offending value,
    or supply a replacement value."))


(defun parser-error (datum expected-type &optional path)
  "Signals a non-continuable error of type `parser-error', using the
   values provided to initialize the condition instance."
  (error 'parser-error
         :datum datum :expected-type expected-type
         :path path))


(defun skip-offending-value ()
  "Invokes the top-most `skip-offending-value' restart. This restart
   is usually provided by parsers, if they can recover from a parse
   error by simply ignoring the offending value."
  (invoke-restart 'skip-offending-value))


(defmacro skipping-offending-values (&body body)
  "Evaluates the forms in `body' in a dynamic context, where a handler
   has been bound for parser-error conditions, which invokes the nearest
   available `skip-offending-value' restart (if there is any). If no
   such restart is currently active, the condition is allowed to propagate
   to the next handler."
  `(handler-bind ((parser-error 
                   #'(lambda (condition)
                       (let ((restarts (compute-restarts condition)))
                         (loop
                            :for restart :in restarts
                            :for name := (restart-name restart)
                            :when (eq name 'skip-offending-value)
                            :do (invoke-restart restart))))))
     ,@body))
                       
