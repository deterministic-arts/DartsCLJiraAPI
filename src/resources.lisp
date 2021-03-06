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


(defclass subset (annotatable)
  ((offset 
     :type (integer 0) :initform 0 :initarg :offset
     :reader subset-offset)
   (limit
     :type (integer 0) :initform 0 :initarg :limit
     :reader subset-limit)
   (total
     :type (integer 0) :initform 0 :initarg :total
     :reader subset-total)
   (elements
     :type list :initform nil :initarg :elements
     :reader subset-elements)))


(defmethod print-object ((object subset) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~D of ~D (offset ~D limit ~D)"
            (length (subset-elements object))
            (subset-total object)
            (subset-offset object)
            (subset-limit object))))

(defun map-container-elements (function container)
  (map nil function (subset-elements container)))



(defclass project (resource)
  ((id
     :type string :initarg :id 
     :reader project-id)
   (key
     :type string :initarg :key
     :reader project-key)
   (description
     :type (or null string) :initform nil :initarg :description
     :reader project-description)
   (lead
     :type (or null user) :initform nil :initarg :lead
     :reader project-lead)
   (components
     :type list :initform nil :initarg :components
     :reader project-components)
   (issue-types
     :type list :initform nil :initarg :issue-types
     :reader project-issue-types)
   (name 
     :type (or null string) :initform nil :initarg :name
     :reader project-name)
   (avatars
     :type list :initarg :avatars
     :reader project-avatars)))


(defmethod print-object ((object project) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A~@[ ~S~]" (project-id object) (or (project-name object) (project-key object)))))



(defclass comment (resource)
  ((id 
     :type string :initarg :id
     :reader comment-id)
   (body
     :type (or null string) :initform nil :initarg :body
     :reader comment-body)
   (author 
     :type (or null user) :initform nil :initarg :author
     :reader comment-author)
   (editor
     :type (or null user) :initform nil :initarg :editor
     :reader comment-editor)
   (created
     :type (or null timestamp) :initform nil :initarg :created
     :reader comment-created)
   (edited
     :type (or null timestamp) :initform nil :initarg :edited
     :reader comment-edited)))


(defclass worklog (resource)
  ((id
     :type string :initarg :id
     :reader worklog-id)
   (comment
     :type (or null string) :initform nil :initarg :body
     :reader worklog-comment)
   (started
     :type (or null timestamp) :initform nil :initarg :started
     :reader worklog-started)
   (duration
     :type (integer 0) :initform 0 :initarg :duration
     :reader worklog-duration)
   (author 
     :type (or null user) :initform nil :initarg :author
     :reader worklog-author)
   (editor
     :type (or null user) :initform nil :initarg :editor
     :reader worklog-editor)
   (created
     :type (or null timestamp) :initform nil :initarg :created
     :reader worklog-created)
   (edited
     :type (or null timestamp) :initform nil :initarg :edited
     :reader worklog-edited)))


(defclass attachment (resource)
  ((filename
     :type (or null string) :initform nil :initarg :filename
     :reader attachment-filename)
   (author 
     :type (or null user) :initform nil :initarg :author
     :reader attachment-author)
   (created
     :type (or null timestamp) :initform nil :initarg :created
     :reader attachment-created)
   (size
     :type (integer 0) :initform 0 :initarg :size
     :reader attachment-size)
   (mime-type
     :type mime-type :initarg mime-type 
     :reader attachment-mime-type)
   (content-uri
     :type (or null uri) :initarg content-uri :initform nil
     :reader attachment-content-uri)
   (thumbnail-uri
     :type (or null uri) :initarg thumbnail-uri :initform nil
     :reader attachment-thumbnail-uri)))


(defparameter +default-mime-type+ 
  (parse-mime-type "application/octet-stream"))


(defmethod initialize-instance :after ((object attachment) &key content-uri thumbnail-uri mime-type &allow-other-keys)
  (unless (slot-boundp object 'mime-type)
    (setf (slot-value object 'mime-type) 
          (if mime-type
              (parse-mime-type mime-type)
              +default-mime-type+)))
  (when (and (not (attachment-content-uri object)) content-uri)
    (setf (slot-value object 'content-uri) (parse-uri content-uri)))
  (when (and (not (attachment-thumbnail-uri object)) thumbnail-uri)
    (setf (slot-value object 'thumbnail-uri) (parse-uri thumbnail-uri))))
                                               
 


(defclass issue-link-type (resource)
  ((id
     :type string :initarg :id
     :reader issue-link-type-id)
   (name
     :type (or null string) :initform nil :initarg :name
     :reader issue-link-type-name)
   (inwards
     :type (or null string) :initform nil :initarg :inwards
     :reader issue-link-type-inwards)
   (outwards
     :type (or null string) :initform nil :initarg :outwards
     :reader issue-link-type-outwards)))


(defmethod print-object ((object issue-link-type) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A~@[ ~S~]" 
            (issue-link-type-id object) 
            (issue-link-type-name object))))


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
     :type (or null uri) :initarg icon-uri
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
(defclass component (descriptor) ())


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
      (found? (values (car value) (cdr value)))
      (no-slots-p (values default nil))
      ((string= key "ID") (values (issue-id issue) :id))
      ((string= key "KEY") (values (issue-key issue) :key))
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
                    (let ((value (wbtree-node-value node)))
                      (funcall function (wbtree-node-key node) (car value) (cdr value))))
                  (issue-fields issue))
      (let ((below-id t) (below-key t))
        (locally (declare (special +id-pseudo-field+ +key-pseudo-field+)) ; Forward declaration
          (wbtree-map (lambda (node)
                        (let* ((key (wbtree-node-key node))
                               (value (wbtree-node-value node))
                               (field (cdr value))
                               (assoc (car value)))
                          (when below-key
                            (when below-id
                              (cond
                                ((string< key "ID") nil)
                                ((string= key "ID") (setf below-id nil))
                                (t (funcall function :id (issue-id issue) +id-pseudo-field+)
                                   (setf below-id nil))))
                            (cond
                              ((string< key "KEY") nil)
                              ((string= key "KEY") (setf below-key nil))
                              (t (funcall function :key (issue-key issue) +key-pseudo-field+)
                                 (setf below-key nil))))
                          (funcall function key assoc field)))
                    (issue-fields issue))
          (when below-id (funcall function :id (issue-id issue) +id-pseudo-field+))
          (when below-key (funcall function :key (issue-key issue) +key-pseudo-field+)))))
  nil)



(defclass issue-link (resource)
  ((id
     :type string :initarg :id
     :reader issue-link-type-id)
   (type
     :type (or null issue-link-type) :initform nil :initarg :type
     :reader issue-link-type)
   (inward-issue
     :type (or null issue) :initform nil :initarg :inward-issue
     :reader issue-link-inward-issue)
   (outward-issue
     :type (or null issue) :initform nil :initarg :outward-issue
     :reader issue-link-outward-issue)))


(defclass value-type (annotatable) 
  ((identifier
     :type (or null string) :initform nil :initarg :identifier
     :reader value-type-identifier)
   (display-name
     :type (or null string) :initform nil :initarg :display-name
     :reader value-type-display-name)
   (lisp-type 
     :type t :initarg :lisp-type :initform 't
     :reader value-type-lisp-type)
   (documentation 
     :type (or null string) :initarg :documentation :initform nil
     :reader value-type-documentation)))


(defmethod print-object ((object value-type) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (format stream "~S ~A"
            'value-type
            (or (value-type-display-name object)
                (value-type-identifier object)
                (value-type-lisp-type object)))))

    

(defconstant +orderable-flag+ 1)
(defconstant +searchable-flag+ 2)
(defconstant +navigable-flag+ 4)
(defconstant +custom-flag+ 8)

(defclass schema-field ()
  ((id
     :type string :initarg :id
     :reader schema-field-id)
   (name
     :type string :initarg :name
     :reader schema-field-name)
   (flags 
     :type (unsigned-byte 32) :initarg flags
     :reader schema-field-flags)
   (type
     :type string :initarg :type
     :reader schema-field-type)
   (custom-id
     :type (or null (integer 0)) :initform nil :initarg :custom-id
     :reader schema-field-custom-id)
   (plugin
     :type (or null string) :initform nil :initarg :plugin
     :reader schema-field-plugin)))


(defmethod print-object ((object schema-field) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" 
            (schema-field-id object)
            (schema-field-type object))))


(defmethod initialize-instance :after ((object schema-field) &key (orderable nil) (searchable nil) (navigable nil) &allow-other-keys)
  (unless (slot-boundp object 'flags)
    (setf (slot-value object 'flags)
          (logior (if orderable +orderable-flag+ 0)
                  (if searchable +searchable-flag+ 0)
                  (if navigable +navigable-flag+ 0)
                  (if (schema-field-custom-id object) +custom-flag+ 0)))))


(defun schema-field-orderable-p (field)
  (not (zerop (logand (schema-field-flags field) +orderable-flag+))))

(defun schema-field-searchable-p (field)
  (not (zerop (logand (schema-field-flags field) +searchable-flag+))))

(defun schema-field-navigable-p (field)
  (not (zerop (logand (schema-field-flags field) +navigable-flag+))))

(defun schema-field-custom-p (field)
  (not (zerop (logand (schema-field-flags field) +custom-flag+))))


(defclass issue-filter (resource)
  ((id
     :type string :initarg :id
     :reader issue-filter-id)
   (owner 
     :type (or null user) :initform nil :initarg :owner
     :reader issue-filter-owner)
   (name
     :type (or null string) :initform nil :initarg :name
     :reader issue-filter-name)
   (description
     :type (or null string) :initform nil :initarg :description
     :reader issue-filter-description)
   (query
     :type (or null string) :initform nil :initarg :query
     :reader issue-filter-query)
   (favourite
     :type t :initform nil :initarg :favourite
     :reader issue-filter-favourite-p)))

   



(defmacro define-reader-aliases (reader &body aliases)
  (if (null aliases) `',reader
      `(progn 
         (declaim (inline ,(car aliases)))
         (defun ,(car aliases) (object) (,reader object))
         (define-reader-aliases ,reader ,@(cdr aliases)))))


(define-reader-aliases resource-uri
  user-uri issue-uri priority-uri status-uri resolution-uri issue-type-uri
  component-uri project-uri comment-uri issue-link-type-uri issue-link-uri
  attachment-uri worklog-uri issue-filter-uri)

(define-reader-aliases descriptor-id 
  priority-id status-id resolution-id issue-type-id component-id)

(define-reader-aliases descriptor-name
  priority-name status-name resolution-name issue-type-name
  component-name)

(define-reader-aliases descriptor-description
  priority-description status-description resolution-description issue-type-description
  component-description)

(define-reader-aliases descriptor-icon-uri
  priority-icon-uri status-icon-uri resolution-icon-uri issue-type-icon-uri
  component-icon-uri)
