#|
 This file is a part of depot
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.depot.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:depot #:org.shirakumo.depot)
   (#:zip #:org.shirakumo.depot.zip)))
(in-package #:org.shirakumo.depot.test)

(defvar *here* #.(or *compile-file-pathname* *load-pathname*))

(defun depot (path)
  (depot:from-pathname
   (merge-pathnames path (make-pathname :name NIL :type NIL :defaults *here*))))

(define-test depot)

(define-test depot-pathname
  (of-type 'depot:directory (depot ""))
  (of-type 'depot:file (depot:entry "plain" (depot "")))
  (is string= "plain" (depot:read-from (depot:entry "plain" (depot "")) 'character)))

(define-test depot-zip
  :depends-on (depot depot-pathname))

(define-test read-zip
  :parent depot-zip
  (let ((archive (of-type 'zip:zip-archive (depot:realize-entry (depot "depot.zip") T))))
    (is = 2 (length (depot:list-entries archive)))
    (of-type 'zip:zip-entry (depot:entry "inner" archive))
    (is string= "inner
" (depot:read-from (depot:entry "inner" archive) 'character))
    (let ((archive (of-type 'zip:zip-archive (depot:realize-entry (depot:entry "depot.zip" archive) T))))
      (is = 2 (length (depot:list-entries archive)))
      (of-type 'zip:zip-entry (depot:entry "nested" archive))
      (is string= "nested
" (depot:read-from (depot:entry "nested" archive) 'character)))))

