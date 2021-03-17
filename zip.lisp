#|
 This file is a part of depot
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.depot.zip
  (:use #:cl)
  (:local-nicknames
   (#:depot #:org.shirakumo.depot)
   (#:zippy #:org.shirakumo.zippy))
  (:export
   #:zip-archive
   #:zip-entry
   #:zip-directory
   #:zip-file))

(in-package #:org.shirakumo.depot.zip)

(defun starts-with (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defclass zip-archive (depot:depot depot:file zippy:zip-file)
  ((streams :initarg :streams :initform () :reader streams)))

(depot:define-realizer zip
  ((file depot:file)
   (multiple-value-bind (zip-file streams) (zippy:open-zip-file (depot:to-pathname file))
     (let ((file (change-class zip-file 'zip-archive :streams streams
                                                     :pathname (depot:to-pathname file)
                                                     :depot (depot:depot file))))
       (loop for entry across (zippy:entries file)
             do (if (find :directory (zippy:attributes entry))
                    (change-class entry 'zip-directory)
                    (change-class entry 'zip-file)))
       file))))

(defmethod depot:list-entries ((depot zip-archive))
  (coerce (zippy:entries depot) 'list))

(defmethod depot:query-entries ((depot zip-archive) &key name type &allow-other-keys)
  (let ((name (format NIL "~a~@[.~a~]" name type)))
    (loop for entry across (zippy:entries depot)
          when (string= (zippy:file-name entry) name)
          collect entry)))

(defmethod depot:make-entry ((depot zip-archive) &key)
  )

(defclass zip-entry (depot:entry zippy:zip-entry)
  ())

(defmethod depot:attributes ((entry zip-entry))
  (zippy:attributes entry))

(defmethod (setf depot:attributes) (attributes (entry zip-entry))
  (setf (zippy:attributes entry) attributes))

(defmethod depot:delete-entry ((entry zip-entry))
  (setf (zippy:entries (zippy:zip-file entry))
        (delete entry (zippy:entries (zippy:zip-file entry)))))

(defclass zip-directory (zip-entry depot:depot)
  ())

(defmethod depot:list-entries ((depot zip-directory))
  (loop with prefix = (format NIL "~a/" (zippy:file-name depot))
        for entry across (zippy:entries (zippy:zip-file depot))
        when (starts-with prefix (zippy:file-name entry))
        collect entry))

(defmethod depot:make-entry ((depot zip-directory) &key)
  )

(defclass zip-file (zip-entry)
  ())

(defmethod depot:open-entry ((entry zip-entry) direction element-type &key)
  )
