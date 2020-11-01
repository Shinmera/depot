#|
 This file is a part of depot
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.depot)

(defvar *os-depot*)

(defclass os-depot (depot) ())
(defclass device (depot) ())
(defclass host (depot) ())

(defgeneric to-pathname (entry))

(defun path-component-specific-p (comp)
  (or (eql comp NIL) (eql comp :unspecific)))

(defun normalize-path-component (comp)
  (case comp
    ((NIL) NIL)
    (:unspecific NIL)
    (T comp)))

(defmethod to-pathname ((entry entry))
  (let* ((name (attribute :name entry))
         (dotp (position #\. name :from-end T))
         (type (subseq name (1+ dotp)))
         (name (subseq name 0 dotp))
         (depot (depot entry))
         (dirs (loop until (or (typep depot '(or device host))
                               (eq depot *os-depot*))
                     collect (id depot)
                     do (setf depot (depot depot))))
         (device (when (typep depot 'device)
                   (prog1 (id depot)
                     (setf depot (depot depot)))))
         (host (if (typep depot 'host)
                   (id depot)
                   (pathanme-host *default-pathname-defaults*))))
    (make-pathname :host host :device device :directory (list :absolute dirs)
                   :name name :type type :version (attribute :version entry))))

(defun from-pathname (pathname)
  (let ((depot *os-depot*)
        (pathname (merge-pathnames pathname *default-pathname-defaults*)))
    (when (path-component-specific-p (pathname-host pathname))
      (setf depot (query-entry depot :id (pathname-host pathname))))
    (when (path-component-specific-p (pathname-device pathname))
      (setf depot (query-entry depot :id (pathname-device pathname))))
    (loop for dir in (rest (pathname-directory pathname))
          do (setf depot (entry dir depot)))
    (if (or (pathname-name pathname)
            (pathname-type pathname))
        (query-entry depot :name (pathname-name pathname)
                           :type (pathname-type pathname)
                           :version (pathname-version pathname))
        depot)))

(defclass directory (depot entry)
  ((pathname :initarg :pathname :reader to-pathname)))

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
         (directories (directory (merge-pathnames (make-pathname :directory `(:relative ,*wild-component*) :name NIL :type NIL :version NIL) pathname)))
         (files (directory (make-pathname :name "*" :type "*" :version (or #-(or allegro abcl xcl) "*") :defaults pathname)))
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
           (loop for file in (directory (make-pathname :name (or name "*") :type (or type "*") :version (or version #-(or allegro abcl xcl) "*") :defaults pathname))
                 when (or (null id) (string= (file-namestring file) id))
                 collect (make-instance 'file :depot depot :pathname file)))
          (name
           (let ((entries ()))
             (dolist (directory (directory (merge-pathnames (make-pathname :directory `(:relative ,name)) pathname)))
               (push (make-instance 'directory :depot depot :pathname directory) entries))
             (dolist (file (directory (make-pathname :name name :type "*" :version (or #-(or allegro abcl xcl) "*") :defaults pathname)) entries)
               (push (make-instance 'file :depot depot :pathname file) entries))))
          (T (list-entries depot)))))

(defmethod make-entry ((depot directory) &key name type)
  (if (eql type :directory)
      (make-instance 'directory :depot depot :pathname (merge-pathnames (make-pathname :directory `(:relative ,name))
                                                                        (to-pathname depot)))
      (make-instance 'file :depot depot :pathname (make-pathname :name name :type type :defaults (to-pathname depot)))))

(defclass file (entry)
  ((pathname :initarg :pathname :reader to-pathname)))

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

(defmethod attribute ((name (eql :write-date)) (entry file))
  (file-write-date (to-pathname entry)))

(defmethod attribute ((name (eql :author)) (entry file))
  (file-author (to-pathname entry)))

(defmethod delete-entry ((entry file))
  (delete-file (to-pathname entry)))

(defclass stream-transaction (transaction)
  ((stream :initarg :stream :reader stream)))

(defmethod size ((transaction stream-transaction))
  (file-length (stream transaction)))

(defmethod abort :after ((transaction stream-transaction))
  (close (stream transaction)))

(defmethod commit :after ((transaction stream-transaction))
  (close (stream transaction)))

(defclass file-write-transaction (stream-transaction)
  ())

(defmethod open-entry ((file file) (direction :output) element-type &key (external-format :default))
  (let* ((pathname (to-pathname file))
         (tmp (make-pathname :type "tmp" :name (format NIL "~a-tmp~d~d" (pathname-name pathname) (get-universal-time) (random 100)))))
    (make-instance 'file-write-transaction :stream (open tmp :direction direction :element-type element-type :external-format external-format) :entry file)))

(defmethod write-to ((transaction file-transaction) sequence &key start end)
  (write-sequence sequence (stream transaction) :start (or start 0) :end end))

(defmethod commit ((transaction file-write-transaction))
  (rename-file (stream transaction) (to-pathname (entry transaction))))

(defmethod abort ((transaction file-transaction))
  (delete-file (stream file)))

(defclass file-read-transaction (stream-transaction)
  ())

(defmethod open-entry ((file file) (direction :input) element-type &key (external-format :default))
  (make-instance 'file-read-transaction :stream (open (to-pathname file) :direction direction :element-type element-type :external-format external-format) :entry file))

(defmethod read-from ((transaction file-read-transaction) sequence &key start end)
  (read-sequence sequence (stream transaction) :start (or start 0) :end end))
