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


(defclass resource (annotatable)
  ((uri
     :type uri :initarg uri 
     :reader resource-uri))
  (:documentation "Base class for all Jira resources. A resource is 
    basically anything with a `self' URI. This class is only the base
    class, which provides the self URI. More specific subclasses for
    individual resource types are provided below."))


(defmethod initialize-instance :after ((object resource) &key uri &allow-other-keys)
  (unless (slot-boundp object 'uri)
    (setf (slot-value object 'uri) 
          (if uri
              (parse-uri uri)
              (error "missing resource URI")))))


(defmethod print-object ((object resource) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write-char #\" stream)
    (render-uri (resource-uri object) stream)
    (write-char #\" stream)))


(defclass avatar (annotatable)
  ((width
     :type (unsigned-byte 16) :initarg :width :initform 0
     :reader avatar-width)
   (height
     :type (unsigned-byte 16) :initarg :height :initform 0
     :reader avatar-height)
   (uri
     :type uri :initarg uri
     :reader avatar-uri))
  (:documentation "Each instance of this class describes an avatar 
    image. This is used to represent the avatar images found, for
    example, in the user and project resources."))


(defmethod initialize-instance :after ((object avatar) &key uri &allow-other-keys)
  (unless (slot-boundp object 'uri)
    (setf (slot-value object 'uri) 
          (if uri
              (parse-uri uri)
              (error "missing image URI")))))


(defmethod print-object ((object avatar) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~Dx~D " (avatar-width object) (avatar-height object))
    (write-char #\" stream)
    (render-uri (avatar-uri object) stream)
    (write-char #\" stream)))


(defclass user (resource)
  ((name
     :type string :initarg :name
     :reader user-name)
   (display-name
     :type (or null string) :initarg :display-name
     :reader user-display-name)
   (email-address
     :type (or null string) :initarg :email-address
     :reader user-email-address)
   (avatars
     :type list :initarg :avatars
     :reader user-avatars))
  (:documentation "This kind of resource is used to represent user
    information. Since the fields of this resource are fixed, we
    can simply provide them as regular slots."))


(defmethod print-object ((object user) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (or (user-display-name object) (user-name object)))))


(defclass descriptor (resource)
  ((id
     :type (or null string) :initarg :id
     :reader descriptor-id)
   (name
     :type (or null string) :initarg :name
     :reader descriptor-name)
   (description
     :type (or null string) :initarg :description
     :reader descriptor-description)
   (icon-uri
     :type (or null uri) :initarg uri
     :reader descriptor-icon-uri))
  (:documentation "Base class for simple descriptor resources,
    which are used by Jira to represent things like issue types,
    status codes, priorities, etc. Most of these resource types
    share the very same structure (only issue-type adds a field,
    the others don't have)."))


(defmethod initialize-instance :after ((object descriptor) &key icon-uri &allow-other-keys)
  (unless (slot-boundp object 'icon-uri)
    (setf (slot-value object 'icon-uri) 
          (and icon-uri 
               (parse-uri icon-uri)))))


(defmethod print-object ((object descriptor) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" 
            (descriptor-id object)
            (descriptor-name object))))



(defclass priority (descriptor) ())
(defclass status (descriptor) ())
(defclass resolution (descriptor) ())
(defclass issue-type (descriptor) ())


(defclass issue (resource)
  ((id
     :type string :initarg :id
     :reader issue-id)
   (key
     :type string :initarg :key
     :reader issue-key)
   (fields
     :type attribute-tree :initform (attribute-tree) :initarg :fields
     :reader issue-fields))
  (:documentation "Generic issue. Instances of this class represent issues
    as read from the Jira server. Unlike other resource types, this class
    keeps the actual data in a generic tree structure, instead of its own
    slot values."))


(defun issue-field (issue key &optional default no-slots-p)
  "Answers the value associated with the field identified by `key' in
   issue `issue'. If no matching field is found, answers `default'. As
   a special case, if no value for field `key' can be obtained from 
   the issues field tree, the function returns

   - (issue-id issue) if (string= key :id)
   - (issue-key issue) if (string= key :key)

   unless `no-slots-p' is provided with a value of true, in which case
   those fields are not defaulted from the issue instance's slots.
   This feature allows all fields to be accessed in a generic way
   via this function."
  (multiple-value-bind (value found?) (wbtree-find key (issue-fields issue))
    (cond 
      (found? (values value t))
      (no-slots-p (values default nil))
      ((string= key "ID") (values (issue-id issue) :default))
      ((string= key "KEY") (values (issue-key issue) :default))
      (t (values default nil)))))


(defun map-issue-fields (function issue &optional no-slots-p)
  "Call `function' for each field present in `issue', passing the
   field's key as first, and the associated value as second argument.
   If `no-slots-p' is true, then the special fields `:ID' and `:KEY'
   are omitted, unless they are part of the field tree of `issue'.
   This function guarantees, that the fields are visited in ascending
   order of their names."
  (if no-slots-p
      (wbtree-map (lambda (node) 
                    (funcall function 
                             (wbtree-node-key node) 
                             (wbtree-node-value node)))
                  (issue-fields issue))
      (let ((below-id t) (below-key t))
        (wbtree-map (lambda (node)
                      (let ((key (wbtree-node-key node))
                            (value (wbtree-node-value node)))
                        (when below-key
                          (when below-id
                            (cond
                              ((string< key "ID") nil)
                              ((string= key "ID") (setf below-id nil))
                              (t (funcall function :id (issue-id issue))
                                 (setf below-id nil))))
                          (cond
                            ((string< key "KEY") nil)
                            ((string= key "KEY") (setf below-key nil))
                            (t (funcall function :key (issue-key issue))
                               (setf below-key nil))))
                        (funcall function key value)))
                    (issue-fields issue))
        (when below-id (funcall function :id (issue-id issue)))
        (when below-key (funcall function :key (issue-key issue)))))
    nil)
    




(defmacro define-reader-aliases (reader &body aliases)
  (if (null aliases) `',reader
      `(progn 
         (declaim (inline ,(car aliases)))
         (defun ,(car aliases) (object) (,reader object))
         (define-reader-aliases ,reader ,@(cdr aliases)))))


(define-reader-aliases resource-uri
  user-uri issue-uri priority-uri status-uri resolution-uri issue-type-uri)

(define-reader-aliases descriptor-id 
  priority-id status-id resolution-id issue-type-id)

(define-reader-aliases descriptor-name
  priority-name status-name resolution-name issue-type-name)

(define-reader-aliases descriptor-description
  priority-description status-description resolution-description issue-type-description)

(define-reader-aliases descriptor-icon-uri
  priority-icon-uri status-icon-uri resolution-icon-uri issue-type-icon-uri)
