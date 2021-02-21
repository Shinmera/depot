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
(defclass entry ()
  ((depot :initarg :depot :reader depot)))
(defclass transaction ()
  ((entry :initarg :entry :reader target)))

(defgeneric list-entries (depot))
(defgeneric query-entries (depot &key))
(defgeneric query-entry (depot &key))
(defgeneric entry (id depot))
(defgeneric entry-exists-p (id depot))
(defgeneric make-entry (depot &key))
(defgeneric delete-entry (entry))
(defgeneric entry-matches-p (entry attribute value))
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
(defgeneric size (transaction))
(defgeneric to-stream (transaction))
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

(defun entry* (depot &rest ids)
  (loop for id in ids
        do (setf depot (entry id depot)))
  depot)

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

;;;; Defaulting methods
;;; Thanks to these only the following functions need to be implemented to complete the interfaces:
;;;   DEPOT
;;;   - LIST-ENTRIES
;;;   - MAKE-ENTRY
;;;   ENTRY
;;;   - ATTRIBUTES
;;;   - (SETF ATTRIBUTES)
;;;   - DELETE-ENTRY
;;;   - OPEN-ENTRY
;;;   TRANSACTION
;;;   - SIZE
;;;   - ABORT
;;;   - COMMIT
;;;   - WRITE-TO
;;;   - READ-FROM
;;; All other functions /may/ be implemented in order to provide a more efficient interface.
(defmethod list-entries ((depot depot))
  (error 'unsupported-operation :operation 'list-entries :object depot))

(defmethod make-entry ((depot depot) &key)
  (error 'unsupported-operation :operation 'make-entry :object depot))

(defmethod attributes ((entry entry))
  (error 'unsupported-operation :operation 'attributes :object entry))

(defmethod (setf attributes) (attributes (entry entry))
  (error 'unsupported-operation :operation '(setf attributes) :object entry))

(defmethod delete-entry ((entry entry))
  (error 'unsupported-operation :operation 'delete-entry :object entry))

(defmethod open-entry ((entry entry) direction element-type &key)
  (error 'unsupported-operation :operation 'open-entry :object entry))

(defmethod entry (id (depot depot))
  (or (query-entry depot :id id)
      (error 'no-such-entry :depot depot :id id)))

(defmethod entry-exists-p (id (depot depot))
  (not (null (query-entry depot :id id))))

(defmethod query-entries ((depot depot) &rest args)
  (flet ((matches-query (entry)
           (loop for (attribute val) on args by #'cddr
                 always (entry-matches-p entry attribute val))))
    (loop for entry in (list-entries depot)
          when (matches-query entry)
          collect entry)))

(defmethod query-entry ((depot depot) &rest args &key &allow-other-keys)
  (first (apply #'query-entries depot args)))

(defmethod attribute (name entry)
  (getf (attributes entry) name))

(defmethod (setf attribute) (value name entry)
  (let ((attributes (attributes entry)))
    (setf (getf attributes name) value)
    (setf (attributes entry) attributes)
    value))

(defmethod entry-matches-p ((entry entry) attribute value)
  (equal value (attribute attribute entry)))

(defmethod id ((entry entry))
  (getf (attributes entry) :id))

(defmethod write-to ((entry entry) (vector vector) &key start end)
  (with-open (tx entry :output (array-element-type vector))
    (write-to tx vector :start start :end end)
    entry))

(defmethod read-from ((entry entry) (vector vector) &key start end)
  (with-open (tx entry :input (array-element-type vector))
    (read-from tx vector :start start :end end)
    entry))

(defmethod read-from ((entry entry) (target (eql 'byte)) &key start end)
  (declare (ignore start end))
  (with-open (tx entry :input '(unsigned-byte 8))
    (let ((array (make-array (size tx) :element-type '(unsigned-byte 8))))
      (read-from tx array)
      array)))

(defmethod read-from ((entry entry) (target (eql 'character)) &key start end)
  (declare (ignore start end))
  (with-open (tx entry :input 'character)
    (with-output-to-string (out)
      (loop with buffer = (make-array 4096 :element-type 'character)
            for read = (read-from tx buffer)
            until (= 0 read)
            do (write-sequence buffer out :end read)))))

;;;; TODO: This interface is /slow/ because there is no internal buffering going on at all.
;;;;       A faster version would properly buffer and keep indices.
(defclass transaction-stream (trivial-gray-streams:fundamental-stream)
  ((transaction :initarg :transaction :reader transaction)
   (unread :initform NIL :accessor unread)))

(defmethod to-stream ((transaction transaction))
  (make-instance 'transaction-stream :transaction transaction))

(defmethod cl:close ((stream transaction-stream) &key abort)
  (if abort
      (abort (transaction stream))
      (commit (transaction stream))))

(defmethod trivial-gray-streams:stream-read-char ((stream transaction-stream))
  (let ((char (unread stream)))
    (cond (char
           (setf (unread stream) NIL)
           char)
          (T
           (let ((buf (make-array 1 :element-type 'character)))
             (declare (dynamic-extent buf))
             (read-from (transaction stream) buf)
             (aref buf 0))))))

(defmethod trivial-gray-streams:stream-unread-char ((stream transaction-stream) char)
  (setf (unread stream) char))

(defmethod trivial-gray-streams:stream-read-char-no-hang ((stream transaction-stream))
  (trivial-gray-streams:stream-read-char stream))

(defmethod trivial-gray-streams:stream-peek-char ((stream transaction-stream))
  (let ((char (trivial-gray-streams:stream-read-char stream)))
    (trivial-gray-streams:stream-unread-char stream char)
    char))

(defmethod trivial-gray-streams:stream-listen ((stream transaction-stream)))

(defmethod trivial-gray-streams:stream-read-line ((stream transaction-stream))
  (with-output-to-string (out)
    (let ((buf (make-array 1 :element-type 'character)))
      (declare (dynamic-extent buf))
      (loop
         (read-from (transaction stream) buf)
         (let ((char (aref buf 0)))
           (if (char= char #\Linefeed)
               (return)
               (write-char char out)))))))

(defmethod trivial-gray-streams:stream-clear-input ((stream transaction-stream))
  (abort (transaction stream)))

(defmethod trivial-gray-streams:stream-write-char ((stream transaction-stream) char)
  (let ((buf (make-array 1 :element-type 'character)))
    (declare (dynamic-extent buf))
    (setf (aref buf 0) char)
    (write-to (transaction stream) buf)))

(defmethod trivial-gray-streams:stream-line-column ((stream transaction-stream))
  ;; TODO: implement this, maybe.
  NIL)

(defmethod trivial-gray-streams:stream-start-line-p ((stream transaction-stream))
  (eql 0 (trivial-gray-streams:stream-line-column stream)))

(defmethod trivial-gray-streams:stream-write-string ((stream transaction-stream) string &optional (start 0) (end (length string)))
  (write-to (transaction stream) string :start start :end end))

(defmethod trivial-gray-streams:stream-terpri ((stream transaction-stream))
  (write-to (transaction stream) #.(string #\Linefeed)))

(defmethod trivial-gray-streams:stream-fresh-line ((stream transaction-stream))
  (unless (trivial-gray-streams:stream-start-line-p stream)
    (trivial-gray-streams:stream-terpri stream)))

(defmethod trivial-gray-streams:stream-finish-output ((stream transaction-stream)))
(defmethod trivial-gray-streams:stream-force-output ((stream transaction-stream)))

(defmethod trivial-gray-streams:stream-clear-output ((stream transaction-stream))
  (abort (transaction stream)))

(defmethod trivial-gray-streams:stream-advance-to-column ((stream transaction-stream) col)
  (let ((cur (trivial-gray-streams:stream-line-column)))
    (when (and cur (< cur col))
      (let ((count (- col cur)))
        (cond ((< count 255)
               (let ((buf (make-array count :element-type 'character :initial-element #\Space)))
                 (declare (dynamic-extent buf))
                 (write-to (transaction stream) buf)))
              (T
               (let ((buf (make-array count :element-type 'character :initial-element #\Space)))
                 (write-to (transaction stream) buf))))))))

(defmethod trivial-gray-streams:stream-read-byte ((stream transaction-stream))
  (let ((buf (make-array 1 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buf))
    (read-from (transaction stream) buf)
    (aref buf 0)))

(defmethod trivial-gray-streams:stream-write-byte ((stream transaction-stream) byte)
  (let ((buf (make-array 1 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buf))
    (setf (aref buf 0) byte)
    (write-to (transaction stream) buf)))

(defmethod trivial-gray-streams:stream-read-sequence ((stream transaction-stream) seq start end)
  (when (< start end)
    (when (unread stream)
      (setf (aref seq start) (unread stream))
      (setf (unread stream) NIL)
      (incf start))
    (read-from (transaction stream) seq :start start :end end)))

(defmethod trivial-gray-streams:stream-write-sequence ((stream transaction-stream) seq start end)
  (write-to (transaction stream) seq :start start :end end))

(defmethod trivial-gray-streams:stream-file-position ((stream transaction-stream))
  )

(defmethod (setf trivial-gray-streams:stream-file-position) (pos (stream transaction-stream))
  )
