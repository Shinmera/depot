#|
 This file is a part of depot
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.depot.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:depot #:org.shirakumo.depot)))
(in-package #:org.shirakumo.depot.test)

(defvar *here* #.(or *compile-file-pathname* *load-pathname*))

(defun depot (path)
  (merge-pathnames path (make-pathname :name NIL :type NIL :defaults *here*)))

(define-test depot)

(define-test depot-pathname)

(define-test depot-zip
  :depends-on (depot depot-pathname))

(define-test read-zip
  :parent depot-zip
  (ok (depot:realize-entry (depot "depot.zip") T))
  (is = 2 (length (depot:list-entries (depot:realize-entry (depot "depot.zip") T)))))

