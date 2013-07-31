
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


