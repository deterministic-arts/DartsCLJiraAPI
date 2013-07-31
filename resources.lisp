
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
  (:documentation ""))


(defmethod print-object ((object user) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (or (user-display-name object) (user-name object)))))


(defclass descriptor (resource)
  ((id
     :type string :initarg :id
     :reader descriptor-id)
   (name
     :type string :initarg :name
     :reader descriptor-name)
   (description
     :type (or null string) :initarg :description
     :reader descriptor-description)
   (icon-uri
     :type (or null uri) :initarg uri
     :reader descriptor-icon-uri)))


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
  (:documentation ""))

