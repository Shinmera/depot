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

(defmethod to-pathname ((entry entry))
  (let* ((name (attribute :name entry))
         (dotp (position #\. name :from-end T))
         (type (subseq name (1+ dotp)))
         (name (subseq name 0 dotp))
         (system (depot entry))
         (dirs (loop until (or (typep system '(or device host))
                               (eq system *os-depot*))
                     collect (id system)
                     do (setf system (depot system))))
         (device (when (typep system 'device)
                   (prog1 (id system)
                     (setf system (depot system)))))
         (host (if (typep system 'host)
                   (id system)
                   (pathanme-host #p""))))
    (make-pathname :host host :device device :directory (list :absolute dirs)
                   :name name :type type :version (attribute :version entry))))

(defun from-pathname (pathname)
  (let ((system *os-depot*)
        (pathname (merge-pathnames pathname *default-pathname-defaults*)))
    (when (pathname-host pathname)
      (setf system (query-entry system :host (pathname-host pathname))))
    (when (pathname-device pathname)
      (setf system (query-entry system :device (pathname-device pathname))))
    (loop for dir in (rest (pathname-directory pathname))
          do (setf system (entry dir system)))
    (if (or (pathname-name pathname)
            (pathname-type pathname))
        (query-entry system :name (entry-namestring pathname)
                           :version (pathname-version pathname))
        system)))
