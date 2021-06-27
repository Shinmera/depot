#|
 This file is a part of depot
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.depot.in-memory
  (:use #:cl)
  (:local-nicknames
   (#:depot #:org.shirakumo.depot)
   (#:atomics #:org.shirakumo.atomics))
  (:export
   #:depot
   #:entry))

(in-package #:org.shirakumo.depot.in-memory)

(defclass depot (depot:depot)
  ((entries :initform (make-hash-table :test 'equal) :accessor entries)))

(defmethod depot:list-entries ((depot depot))
  (loop for entry being the hash-values of (entries depot)
        collect entry))

(defmethod depot:query-entries ((depot depot) &rest attributes &key (id NIL id-p) &allow-other-keys)
  (flet ((test (entry)
           (loop for (key val) on attributes by #'cddr
                 always (depot:entry-matches-p entry key val))))
    (if id-p
        (let ((entry (gethash id (entries depot))))
          (when (and entry (test entry))
            (list entry)))
        (loop for entry being the hash-values of (entries depot)
              when (test entry)
              collect entry))))

(defmethod depot:entry (id (depot depot))
  (or (gethash id (entries depot))
      (error 'depot:no-such-entry :id id :object depot)))

(defmethod depot:entry-exists-p (id (depot depot))
  (gethash id (entries depot)))

(defmethod depot:make-entry ((depot depot) &rest attributes &key &allow-other-keys)
  (let ((id (getf attributes :id)))
    (unless id
      (error "ID argument is required."))
    (when (depot:entry-exists-p id depot)
      (error 'depot:entry-already-exists :object depot :attributes attributes))
    (let ((table (make-hash-table :test 'eql)))
      (loop for (key val) on attributes
            do (setf (gethash key table) val))
      (setf (gethash id (entries depot)) (make-instance 'entry :depot depot :attributes table)))))

(defclass entry (depot:entry)
  ((depot :initform (depot::arg! :depot) :initarg :depot :accessor depot:depot)
   (attributes :initform (make-hash-table :test 'eql) :initarg :attributes :accessor attributes)
   (payload :initform #() :initarg :payload :accessor payload)))

(defmethod depot:delete-entry ((entry entry))
  (remhash (gethash :id (attributes entry)) (entries (depot:depot entry))))

(defmethod entry-matches-p ((entry entry) attribute value)
  (let ((attr (gethash attribute (attributes entry) #1='#:none)))
    (if (eql attr #1#)
        (error 'depot:no-such-attribute :object entry :attribute attribute)
        (equal attr value))))

(defmethod depot:attributes ((entry entry))
  (loop for key being the hash-keys of (attributes entry)
        for val being the hash-values of (attributes entry)
        collect key collect val))

(defmethod (setf depot:attributes) (attributes (entry entry))
  (let ((table (attributes entry)))
    (loop for (key val) on attributes by #'cddr
          do (unless (eql :id key)
               (setf (gethash key table) val)))
    attributes))

(defmethod depot:attribute (name (entry entry))
  (gethash name (attributes entry)))

(defmethod (setf depot:attribute) (value name (entry entry))
  (when (eql name :id)
    (error 'depot:permission-denied :object entry))
  (setf (gethash name (attributes entry)) value))

(defmethod depot:id ((entry entry))
  (gethash :id (attributes entry)))

(defclass write-transaction (depot:transaction)
  ((target :initarg :entry :initform (depot::arg! :entry) :accessor depot:target)
   (payload :initarg :payload :initform (depot::arg! :payload) :accessor payload)
   (new :initarg :new :accessor new)))

(defmethod depot:open-entry ((entry entry) (direction (eql :output)) element-type &key (initial-size 4096))
  (make-instance 'write-transaction :entry entry
                                    :payload (payload entry)
                                    :new (make-array initial-size :element-type element-type :adjustable T :fill-pointer 0)))

(defmethod depot:write-to ((transaction write-transaction) sequence &key start end (block-size 4096))
  (let* ((start (or start 0))
         (end (or end (length sequence)))
         (new (new transaction))
         (index (fill-pointer new))
         (end2 (+ index (- end start))))
    (when (< (array-total-size new) end2)
      (adjust-array new (* block-size (1+ (floor end2 block-size)))))
    (setf (fill-pointer new) end2)
    (replace new sequence :start1 index :end1 end2
                          :start1 start :end2 end)))

(defmethod depot:index ((transaction write-transaction))
  (fill-pointer (new transaction)))

(defmethod depot:size ((transaction write-transaction))
  (length (payload transaction)))

(defmethod depot:commit ((transaction write-transaction) &key)
  (unless (atomics:cas (slot-value (depot:target transaction) 'payload)
                       (payload transaction)
                       (new transaction))
    (error 'depot:write-conflict :object transaction)))

(defmethod depot:abort ((transaction write-transaction) &key))

(defclass read-transaction (depot:transaction)
  ((target :initarg :entry :initform (depot::arg! :entry) :accessor depot:target)
   (payload :initarg :payload :initform (depot::arg! :payload) :accessor payload)
   (index :initarg :index :initform 0 :accessor depot:index)))

(defmethod depot:open-entry ((entry entry) (direction (eql :input)) element-type &key (start 0))
  (make-instance 'read-transaction :entry entry
                                   :payload (payload entry)
                                   :index start))

(defmethod depot:read-from ((transaction read-transaction) (sequence sequence) &key start end)
  (let* ((start (or start 0))
         (end (or end (length sequence)))
         (index (depot:index transaction))
         (payload (payload transaction))
         (length (min (- end start) (- (length payload) index)))
         (end1 (+ start length))
         (end2 (+ index length)))
    (replace sequence payload :start1 start :end1 end1
                              :start2 index :end2 end2)
    (setf (depot:index transaction) end2)
    end1))

(defmethod depot:size ((transaction read-transaction))
  (length (payload transaction)))

(defmethod depot:commit ((transaction read-transaction) &key)
  (unless (eq (payload transaction) (payload (depot:target transaction)))
    (error 'depot:read-invalidated :object transaction)))

(defmethod depot:abort ((transaction read-transaction) &key))
