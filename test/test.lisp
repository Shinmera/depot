#|
 This file is a part of depot
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.depot.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:depot #:org.shirakumo.depot)
   (#:zip #:org.shirakumo.depot.zip)
   (#:in-memory #:org.shirakumo.depot.in-memory)))
(in-package #:org.shirakumo.depot.test)

(defvar *here* #.(or *compile-file-pathname* *load-pathname*))

(defun depot (path)
  (depot:from-pathname
   (merge-pathnames path (make-pathname :name NIL :type NIL :defaults *here*))))

(defun test-depot-invariants (depot)
  (group (simple-entry)
    (let ((entry (of-type 'depot:entry (depot:make-entry depot :id "simple-entry"))))
      (finish (depot:write-to entry "testing"))
      (is string= "testing" (depot:read-from entry 'character))
      (finish (depot:delete-entry entry))
      (false (depot:entry-exists-p "simple-entry" depot))
      (fail (depot:entry "simple-entry" depot)
          'depot:no-such-entry)))
  (group (write-conflict)
    (let ((entry (of-type 'depot:entry (depot:make-entry depot :id "write-conflict"))))
      (finish (depot:write-to entry "pre-test"))
      (fail (depot:with-open (transaction entry :output 'character)
              (finish (depot:write-to transaction "testing"))
              (is string= "pre-test" (depot:read-from entry 'character))
              (finish (depot:write-to entry "post-test")))
          'depot:write-conflict)
      (depot:delete-entry entry)))
  (group (read-invalidated)
    (let ((entry (of-type 'depot:entry (depot:make-entry depot :id "read-invalidated"))))
      (finish (depot:write-to entry "pre-test"))
      (fail (depot:with-open (transaction entry :input 'character)
              (finish (depot:write-to entry "testing"))
              (is string= "testing" (depot:read-from entry 'character))
              (is string= "pre-test" (depot:read-from transaction 'character)))
          'depot:read-invalidated)
      (depot:delete-entry entry)))
  (group (attributes)
    (let ((entry (of-type 'depot:entry (depot:make-entry depot :id "attributes"))))
      (finish (depot:write-to entry "test"))
      (let ((original (isnt eql () (depot:attributes entry))))
        (finish (setf (depot:attributes entry) ()))
        (is equal original (depot:attributes entry)))
      (depot:delete-entry entry))))

(define-test depot-pathname
  (let ((depot (of-type 'depot:directory (depot ""))))
    (test-depot-invariants depot)
    (of-type 'depot:file (depot:entry "plain" depot))
    (is string= "plain" (depot:read-from (depot:entry "plain" depot) 'character))
    (let ((entry (of-type 'depot:file (depot:make-entry depot :name "test-temp-file"))))
      (finish (depot:write-to entry "testing"))
      (is string= "testing" (depot:read-from entry 'character))
      (finish (depot:delete-entry entry))
      (false (probe-file (depot:to-pathname entry))))))

(define-test depot-in-memory
  (test-depot-invariants (make-instance 'in-memory:depot)))

(define-test depot-zip
  :depends-on (depot-pathname)
  (test-depot-invariants (make-instance 'zip:zip-archive)))

(define-test read-zip
  :parent depot-zip
  (let ((archive (of-type 'zip:zip-archive (depot:realize-entry (depot "depot.zip") T))))
    (is = 2 (length (depot:list-entries archive)))
    (of-type 'zip:zip-entry (depot:entry "inner" archive))
    (is string= "inner" (depot:read-from (depot:entry "inner" archive) 'character))
    (let ((archive (of-type 'zip:zip-archive (depot:realize-entry (depot:entry "depot.zip" archive) 'zip:zip))))
      (is = 1 (length (depot:list-entries archive)))
      (of-type 'zip:zip-entry (depot:entry "nested" archive))
      (is string= "nested" (depot:read-from (depot:entry "nested" archive) 'character)))))

