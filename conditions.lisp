#|
 This file is a part of depot
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.depot)

(defun arg! (initarg)
  (error "Argument ~s was required but not passed." initarg))

(defmacro define-condition* (name classes slots report &rest report-args)
  `(define-condition ,name ,classes
     ,slots
     (:report (lambda (c s) (format s ,report ,@(loop for arg in report-args collect `(,arg c)))))))

(define-condition* depot-condition (condition)
  ((object :initarg :object :initform (arg! :object) :reader object))
  "A depot operation failed on~%  ~a" object)

(define-condition* no-such-entry (depot-condition error)
  ((id :initarg :id :initform (arg! :id) :reader id))
  "The depot~%  ~a~%does not have an entry with the ID~%  ~s" object id)

(define-condition* no-such-attribute (depot-condition error)
  ((name :initarg :name :initform (arg! :name) :reader name))
  "The entry~%  ~a~%does not have an attribute named~%  ~s" object name)

(define-condition* permission-denied (depot-condition error)
  ()
  "Access to the object~%  ~a~%was denied." object)

(define-condition* depot-full (depot-condition error)
  ()
  "The depot~%  ~a~%is full and cannot take any new entries." object)

(define-condition* entry-already-exists (depot-condition error)
  ((attributes :initarg :attributes :initform () :reader attributes))
  "The depot~%  ~a~%cannot create an entry with the requested attributes~%  ~a~%as it would conflict with an existing entry." object attributes)

(define-condition* entry-does-not-exist (depot-condition error)
  ()
  "The operation on the entry~%  ~a~%cannot be completed as it does not exist." object)

(define-condition* unsupported-operation (depot-condition error)
  ((operation :initarg :operation :initform (arg! :operation) :reader operation))
  "The operation~%  ~s~%is not supported by the object~%  ~a" operation object)

(define-condition* transaction-aborted (depot-condition error)
  ()
  "The transaction~%  ~a~%has been aborted." object)

(define-condition* write-conflict (transaction-aborted)
  ()
  "The transaction~%  ~a~%could not be committed as another write to the same entry happened first.~%Please try again." object)

(define-condition* read-invalidated (transaction-aborted)
  ()
  "The transaction~%  ~a~%cannot proceed as the entry was modified underneath and the read has been invalidated.~%Please try again." object)

(define-condition* not-a-depot (depot-condition error)
  ()
  "The object~%  ~a~%cannot be turned into a depot." object)
