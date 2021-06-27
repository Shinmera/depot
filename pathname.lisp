#|
 This file is a part of depot
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.depot)

(defvar *os-depot*)

(defclass os-depot (depot) ())
(defclass host (depot) ())
(defclass device (depot) ())

(defmethod attributes ((depot host))
  (list :id (id depot)))

(defmethod attributes ((depot device))
  (list :id (id depot)))

(defgeneric to-pathname (entry))

(defun path-component-specific-p (comp)
  (or (eql comp NIL) (eql comp :unspecific)))

(defun normalize-path-component (comp)
  (case comp
    ((NIL) NIL)
    (:unspecific NIL)
    (T comp)))

(defmethod to-pathname ((entry entry))
  (let* ((name (id entry))
         (dotp (position #\. name :from-end T))
         (type (subseq name (1+ dotp)))
         (name (subseq name 0 dotp))
         (depot (depot entry))
         (dirs (loop until (or (typep depot '(or device host (not entry)))
                               (eq depot *os-depot*))
                     collect (id depot)
                     do (setf depot (depot depot))))
         (device (when (typep depot 'device)
                   (prog1 (id depot)
                     (setf depot (depot depot)))))
         (host (if (typep depot 'host)
                   (id depot)
                   (pathname-host *default-pathname-defaults*))))
    (make-pathname :host host :device device :directory (list :absolute dirs)
                   :name name :type type :version (attribute :version entry))))

(defun from-pathname (pathname &key create-directories)
  (let ((depot *os-depot*)
        (pathname (merge-pathnames pathname *default-pathname-defaults*)))
    (setf depot (query-entry depot :host (pathname-host pathname)))
    (when (path-component-specific-p (pathname-device pathname))
      (setf depot (query-entry depot :device (pathname-device pathname))))
    (loop for dir in (rest (pathname-directory pathname))
          do (setf depot (ensure-depot (or (query-entry depot :id dir)
                                           (if create-directories
                                               (make-entry depot :name dir)
                                               (error 'no-such-entry :object depot :id dir))))))
    (if (or (pathname-name pathname)
            (pathname-type pathname))
        (or (query-entry depot :name (pathname-name pathname)
                               :type (pathname-type pathname)
                               :version (pathname-version pathname))
            (make-entry depot :name (pathname-name pathname)
                              :type (pathname-type pathname)))
        depot)))

(defclass directory (depot entry)
  ((depot :initarg :depot :reader depot)
   (pathname :initarg :pathname :reader to-pathname)))

(defmethod print-object ((directory directory) stream)
  (print-unreadable-object (directory stream :type T)
    (format stream "~a" (to-pathname directory))))

(defmethod id ((entry directory))
  (car (last (pathname-directory (to-pathname entry)))))

(defmethod attributes ((entry directory))
  (let ((pathname (to-pathname entry)))
    (list :id (car (last (pathname-directory pathname)))
          :name (car (last (pathname-directory pathname)))
          :pathname pathname
          :author (file-author pathname))))

(defmethod attribute ((name (eql :author)) (entry directory))
  (file-author (to-pathname entry)))

(defmethod list-entries ((depot directory))
  (let* ((pathname (to-pathname depot))
         (directories (cl:directory (merge-pathnames (make-pathname :directory `(:relative :wild) :name NIL :type NIL :version NIL) pathname)))
         (files (cl:directory (make-pathname :name :wild :type :wild :version (or #-(or allegro abcl xcl) :newest) :defaults pathname)))
         (entries ()))
    (dolist (directory directories)
      (push (make-instance 'directory :depot depot :pathname directory) entries))
    (dolist (file files entries)
      (unless (and (path-component-specific-p (pathname-name file))
                   (path-component-specific-p (pathname-type file)))
        (push (make-instance 'file :depot depot :pathname file) entries)))))

(defmethod query-entries ((depot directory) &key name type version id)
  (let ((pathname (to-pathname depot)))
    (cond ((or version type id) ;; Can't do more with ID here since we don't know how the implementation separates name from type in a pathname. Sucks.
           (let ((entries ()))
             (dolist (file (cl:directory (make-pathname :name (or name :wild) :type (or type :wild) :version (or version #-(or allegro abcl xcl) :wild) :defaults pathname)))
               (when (or (null id) (string= (file-namestring file) id))
                 (push (make-instance 'file :depot depot :pathname file) entries)))
             (dolist (file (cl:directory (merge-pathnames (make-pathname :directory `(:relative :wild)) pathname)) entries)
               (when (string= (car (last (pathname-directory file))) id)
                 (push (make-instance 'directory :depot depot :pathname file) entries)))))
          (name
           (let ((entries ()))
             (dolist (directory (cl:directory (merge-pathnames (make-pathname :directory `(:relative ,name)) pathname)))
               (push (make-instance 'directory :depot depot :pathname directory) entries))
             (dolist (file (cl:directory (make-pathname :name name :version (or #-(or allegro abcl xcl) :wild) :defaults pathname)) entries)
               (push (make-instance 'file :depot depot :pathname file) entries))))
          (T (list-entries depot)))))

(defmethod make-entry ((depot directory) &key name type id)
  (cond ((eql type :directory)
         (make-instance 'directory :depot depot :pathname (merge-pathnames (make-pathname :directory `(:relative ,name))
                                                                           (to-pathname depot))))
        (T
         (when id
           (let ((dot (position #\. id :from-end T)))
             (if dot
                 (setf name (subseq id 0 dot)
                       type (subseq id (1+ dot)))
                 (setf name id))))
         (make-instance 'file :depot depot :pathname (make-pathname :name name :type type :defaults (to-pathname depot))))))

(defclass file (entry)
  ((depot :initarg :depot :reader depot)
   (pathname :initarg :pathname :reader to-pathname)))

(defmethod print-object ((file file) stream)
  (print-unreadable-object (file stream :type T)
    (format stream "~a" (to-pathname file))))

(defmethod id ((entry file))
  (file-namestring (to-pathname entry)))

(defmethod attributes ((entry file))
  (let ((pathname (to-pathname entry)))
    (list :id (file-namestring pathname)
          :pathname pathname
          :name (normalize-path-component (pathname-name pathname))
          :type (normalize-path-component (pathname-type pathname))
          :version (normalize-path-component (pathname-version pathname))
          :write-date (file-write-date pathname)
          :author (file-author pathname))))

(defmethod attribute ((name (eql :name)) (entry file))
  (normalize-path-component (pathname-name (to-pathname entry))))

(defmethod attribute ((name (eql :type)) (entry file))
  (normalize-path-component (pathname-type (to-pathname entry))))

(defmethod attribute ((name (eql :write-date)) (entry file))
  (file-write-date (to-pathname entry)))

(defmethod attribute ((name (eql :author)) (entry file))
  (file-author (to-pathname entry)))

(defmethod delete-entry ((entry file))
  (delete-file (to-pathname entry)))

(defclass file-transaction (stream-transaction)
  ((timestamp :initarg :timestamp :reader timestamp)))

(defclass file-write-transaction (file-transaction)
  ())

(defmethod open-entry ((file file) (direction (eql :output)) element-type &key (external-format :default))
  (let* ((pathname (to-pathname file))
         (tmp (make-pathname :name (format NIL "~a-tmp~d~d" (pathname-name pathname) (get-universal-time) (random 100)) :defaults pathname)))
    (make-instance 'file-write-transaction :stream (open tmp :direction direction :element-type element-type :external-format external-format)
                                           :entry file
                                           :timestamp (when (probe-file pathname) (file-write-date pathname)))))

(defmethod commit ((transaction file-write-transaction) &key)
  (call-next-method)
  (let ((target (to-pathname (target transaction)))
        (source (pathname (to-stream transaction))))
    (unwind-protect
         (progn
           (if (null (timestamp transaction))
               (when (probe-file target)
                 (cerror "Ignore and commit anyway." 'entry-already-exists :object (depot (target transaction)) :attributes (attributes (target transaction))))
               (when (< (timestamp transaction) (file-write-date target))
                 (cerror "Ignore and commit anyway." 'write-conflict :object transaction)))
           (rename-file source target))
      (ignore-errors
       (when (probe-file source)
         (delete-file source))))))

(defmethod abort ((transaction file-write-transaction) &key)
  (call-next-method)
  (ignore-errors (delete-file (to-stream transaction))))

(defclass file-read-transaction (file-transaction)
  ())

(defmethod open-entry ((file file) (direction (eql :input)) element-type &key (external-format :default))
  (let ((pathname (to-pathname file)))
    (make-instance 'file-read-transaction :stream (open pathname :direction direction :element-type element-type :external-format external-format)
                                          :entry file
                                          :timestamp (file-write-date pathname))))

(defmethod commit ((transaction file-read-transaction) &key)
  (let ((pathname (to-pathname (target transaction))))
    (cond ((not (probe-file pathname))
           (cerror "Ignore and commit anyway." 'entry-does-not-exist :object (target transaction)))
          ((< (timestamp transaction) (file-write-date pathname))
           (cerror "Ignore and commit anyway." 'read-invalidated :object transaction)))))

(unless (boundp '*os-depot*)
  (setf *os-depot* (make-instance 'os-depot)))

(let ((host (make-instance 'host)))
  (defmethod list-entries ((depot os-depot))
    (list host))

  (defmethod query-entries ((depot os-depot) &key ((:host _)))
    (declare (ignore _))
    (list host))

  (defmethod id ((depot host))
    (pathname-host *default-pathname-defaults*))  

  (defmethod depot ((depot host))
    *os-depot*)

  (defmethod list-entries ((depot host))
    (loop for device in #-windows '(NIL) #+windows '("C")
          collect (make-instance 'device :depot depot :pathname (make-pathname :device device :directory '(:absolute)))))

  (defmethod query-entry ((depot host) &key device)
    (make-instance 'device :depot depot :pathname (make-pathname :device device :directory '(:absolute))))

  (defclass device (directory) ())

  (defmethod id ((depot device))
    (pathname-device (to-pathname depot)))

  (defmethod depot ((depot device))
    host))
