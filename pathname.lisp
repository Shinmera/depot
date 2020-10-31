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
(defclass directory (depot entry) ())

(defgeneric to-pathname (entry))

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
    (when (pathname-host pathname)
      (setf depot (query-entry depot :host (pathname-host pathname))))
    (when (pathname-device pathname)
      (setf depot (query-entry depot :device (pathname-device pathname))))
    (loop for dir in (rest (pathname-directory pathname))
          do (setf depot (entry dir depot)))
    (if (or (pathname-name pathname)
            (pathname-type pathname))
        (query-entry depot :name (entry-namestring pathname)
                           :version (pathname-version pathname))
        depot)))
