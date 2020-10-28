#|
 This file is a part of depot
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.depot)

(defvar *entry-realizers* (make-hash-table :test 'eql))

(define-condition depot-condition (condition) ())
(define-condition no-such-entry (depot-condition error) ())
(define-condition permission-denied (depot-condition error) ())
(define-condition depot-full (depot-condition error) ())
(define-condition unsupported-operation (depot-condition error) ())
(define-condition transaction-aborted (depot-condition error) ())
(define-condition write-conflict (transaction-aborted) ())
(define-condition read-invalidated (transaction-aborted) ())

(defclass realizer () ())
(defclass depot () ())
(defclass entry () ())
(defclass transaction () ())

(defgeneric list-entries (system))
(defgeneric query-entries (system &key))
(defgeneric query-entry (system &key))
(defgeneric entry (id system))
(defgeneric entry-exists-p (id system))
(defgeneric make-entry (system attributes))
(defgeneric delete-entry (entry))
(defgeneric attributes (entry))
(defgeneric (setf attributes) (attributes entry))
(defgeneric attribute (name entry))
(defgeneric (setf attribute) (value name entry))
(defgeneric id (entry))
(defgeneric depot (entry))
(defgeneric realize-entry (entry realizer))
(defgeneric open-entry (entry direction element-type &key))
(defgeneric write-to (transaction sequence &key start end))
(defgeneric read-from (transaction sequence &key start end))
(defgeneric commit (transaction))
(defgeneric abort (transaction))

(flet ((ensure-realizer (realizer)
         (etypecase realizer
           (symbol (make-instance realizer))
           (class (make-instance realizer))
           (realizer realizer))))
  (defun register-realizer (realizer)
    (let ((realizer (ensure-realizer realizer)))
      (check-type realizer realizer)
      (setf (gethash realizer *entry-realizers*) realizer)))

  (defun remove-realizer (realizer)
    (let ((realizer (ensure-realizer realizer)))
      (remhash realizer *entry-realizers*))))

(defmacro define-realizer (name &body dispatchers)
  (let ((nameg (gensym "NAME")))
    `(progn (defclass ',name (realizer) ())

            ,@(loop for ((var class) . body) in dispatchers
                    collect `(defmethod realize-entry ((,var ,class) (,nameg ,name))
                               (declare (ignore ,nameg))
                               ,@body))
            
            (register-realizer ',name))))

(defmethod realize-entry ((entry entry) (all (eql T)))
  (loop for realizer being the hash-keys of *entry-realizers*
        thereis (realize-entry entry realizer)))

(defun entry* (system &rest ids)
  (loop for id in ids
        do (setf system (entry id system)))
  system)

(defmethod query-entry ((system depot) &rest args)
  (first (apply #'query-entries system args)))

(defmethod attribute (name entry)
  (getf (attributes entry) name))

(defmethod (setf attribute) (value name entry)
  (let ((attributes (attributes entry)))
    (setf (getf attributes name) value)
    (setf (attributes entry) attributes)
    value))

(defmethod id ((entry entry))
  (getf (attributes entry) :id))

(defmacro with-open ((transaction entry direction element-type &rest args) &body body)
  `(let ((,transaction (open-entry ,entry ,direction ,element-type ,@args)))
     (restart-case
         (unwind-protect
              (let ((,transaction ,transaction))
                ,@body)
           (when ,transaction (commit ,transaction)))
       (abort (&optional e)
         :report "Abort the entry open."
         (declare (ignore e))
         (when ,transaction (abort ,transaction))))))
