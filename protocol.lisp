#|
 This file is a part of depot
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.depot)

(defvar *entry-realizers* (make-hash-table :test 'eql))

(defclass realizer () ())
(defclass depot () ())
(defclass entry () ())
(defclass transaction ()
  ((entry :initarg :entry :reader target)
   (element-type :initarg :element-type :reader element-type)))
(defclass input-transaction (transaction) ())
(defclass output-transaction (transaction) ())

(defmethod print-object ((entry entry) stream)
  (print-unreadable-object (entry stream :type T :identity T)
    (format stream "~s" (id entry))))

(defmethod print-object ((transaction transaction) stream)
  (print-unreadable-object (transaction stream :type T :identity T)
    (format stream "~s ~s" (id (target transaction)) (element-type transaction))))

(defgeneric list-entries (depot))
(defgeneric query-entries (depot &key))
(defgeneric query-entry (depot &key))
(defgeneric entry (id depot))
(defgeneric entry-exists-p (id depot))
(defgeneric make-entry (depot &key))
(defgeneric open-p (depot))
(defgeneric delete-entry (entry))
(defgeneric entry-matches-p (entry attribute value))
(defgeneric attributes (entry))
(defgeneric (setf attributes) (attributes entry))
(defgeneric attribute (name entry))
(defgeneric (setf attribute) (value name entry))
(defgeneric id (entry))
(defgeneric depot (entry))
(defgeneric realize-entry (entry realizer))
(defgeneric ensure-depot (thing))
(defgeneric ensure-entry (id depot &rest attributes))
(defgeneric open-entry (entry direction element-type &key))
(defgeneric write-to (transaction sequence &key start end))
(defgeneric read-from (transaction sequence &key start end))
(defgeneric size (transaction))
(defgeneric index (transaction))
(defgeneric element-type (transaction))
(defgeneric (setf index) (index transaction))
(defgeneric to-stream (transaction))
(defgeneric commit (transaction &key))
(defgeneric abort (transaction &key))

(flet ((ensure-realizer (realizer)
         (etypecase realizer
           (symbol (make-instance realizer))
           (class (make-instance realizer))
           (realizer realizer))))
  (defun register-realizer (realizer)
    (let ((realizer (ensure-realizer realizer)))
      (check-type realizer realizer)
      (setf (gethash (type-of realizer) *entry-realizers*) realizer)))

  (defun remove-realizer (realizer)
    (let ((realizer (ensure-realizer realizer)))
      (remhash (type-of realizer) *entry-realizers*))))

(defmacro define-realizer (name &body dispatchers)
  (let ((nameg (gensym "NAME")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (unless (find-class ',name NIL)
         (defclass ,name (realizer) ()))

       ,@(loop for ((var class) . body) in dispatchers
               collect `(defmethod realize-entry ((,var ,class) (,nameg ,name))
                          (declare (ignore ,nameg))
                          ,@body))
       
       (register-realizer ',name))))

(defmethod realize-entry ((entry entry) (all (eql T)))
  (loop for realizer being the hash-values of *entry-realizers*
        thereis (ignore-errors (realize-entry entry realizer))))

(defmethod realize-entry ((entry entry) (realizer symbol))
  (realize-entry entry (or (gethash realizer *entry-realizers*)
                           (error "~s is not a registered realizer." realizer))))

(defun entry* (depot &rest ids)
  (when ids
    (loop for (id . next) on ids
          do (setf depot (entry id depot))
             (when next (setf depot (ensure-depot depot)))))
  depot)

(defmacro with-open ((transaction entry direction element-type &rest args) &body body)
  `(let ((,transaction (open-entry ,entry ,direction ,element-type ,@args)))
     (restart-case
         (unwind-protect
              (multiple-value-prog1
                  (let ((,transaction ,transaction))
                    ,@body)
                (commit ,transaction)
                (setf ,transaction NIL))
           (when ,transaction (abort ,transaction)))
       (abort (&optional e)
         :report "Abort the entry open."
         (declare (ignore e))
         (when ,transaction (abort ,transaction))))))

(defmacro with-depot ((depot init &key commit (close T)) &body body)
  (let ((openp (gensym "OPEN-P")))
    `(let* ((,depot ,init)
            (,openp (when (typep ,depot 'depot) (open-p ,depot)))
            (,depot (ensure-depot ,depot)))
       (unwind-protect
            (let ((,depot ,depot)) ,@body)
         (ecase ,close
           ((NIL :never))
           ((T :always)
            (close ,depot :abort (not ,commit)))
           ((:if-open)
            (when ,openp (close ,depot :abort (not ,commit))))
           ((:if-closed)
            (unless ,openp (close ,depot :abort (not ,commit)))))))))

(defmethod ensure-depot ((depot depot))
  depot)

(defmethod ensure-depot ((entry entry))
  (or (realize-entry entry T)
      (error 'not-a-depot :object entry)))

(defmethod ensure-entry (id (depot depot) &rest attributes)
  (if (entry-exists-p id depot)
      (entry id depot)
      (apply #'make-entry depot :id id attributes)))

;;;; Defaulting methods
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

(defmethod commit ((depot depot) &key))
(defmethod commit ((entry entry) &key))

(defmethod cl:close ((depot depot) &key abort)
  (unless abort
    (commit depot)))

(defmethod cl:close ((entry entry) &key abort)
  (unless abort
    (commit entry)))

(defmethod entry (id (depot depot))
  (or (query-entry depot :id id)
      (error 'no-such-entry :object depot :id id)))

(defmethod entry-exists-p (id (depot depot))
  (not (null (query-entry depot :id id))))

(defmethod query-entries ((depot depot) &rest args &key &allow-other-keys)
  (flet ((matches-query (entry)
           (loop for (attribute val) on args by #'cddr
                 always (entry-matches-p entry attribute val))))
    (loop for entry in (list-entries depot)
          when (matches-query entry)
          collect entry)))

(defmethod query-entry ((depot depot) &rest args &key &allow-other-keys)
  (first (apply #'query-entries depot args)))

(defmethod attribute (name (entry entry))
  (let ((value (getf (attributes entry) name #1='#:none)))
    (if (eq value #1#)
        (error 'no-such-attribute :object entry :name name)
        value)))

(defmethod (setf attribute) (value name (entry entry))
  (let ((attributes (attributes entry)))
    (setf (getf attributes name) value)
    (setf (attributes entry) attributes)
    value))

(defmethod entry-matches-p ((entry entry) attribute value)
  (equal value (attribute attribute entry)))

(defmethod id ((entry entry))
  (attribute :id entry))

(defmethod write-to ((entry entry) (target vector) &rest args &key &allow-other-keys)
  (with-open (tx entry :output (array-element-type target))
    (apply #'write-to tx target args)))

(defmethod read-from ((entry entry) (target vector) &rest args &key &allow-other-keys)
  (with-open (tx entry :input (array-element-type target))
    (apply #'read-from tx target args)))

(defmethod read-from ((entry entry) (target (eql 'byte)) &rest args &key &allow-other-keys)
  (with-open (tx entry :input '(unsigned-byte 8))
    (apply #'read-from tx target args)))

(defmethod read-from ((entry entry) (target (eql 'character)) &rest args &key &allow-other-keys)
  (with-open (tx entry :input 'character)
    (apply #'read-from tx target args)))

(defmethod read-from ((tx transaction) (target (eql 'byte)) &key start end)
  (if start
      (setf (index tx) start)
      (setf start 0))
  (unless end (setf end (size tx)))
  (let ((array (make-array (- end start) :element-type '(unsigned-byte 8))))
    (loop for next = (read-from tx array) then (read-from tx array :start next)
          until (= next end))
    array))

(defmethod read-from ((tx transaction) (target (eql 'character)) &key start end (block-size 4096))
  (if start
      (setf (index tx) start)
      (setf start 0))
  (unless end (setf end (size tx)))
  (with-output-to-string (out)
    (loop with remaining = (- end start)
          with buffer = (make-array block-size :element-type 'character)
          for read = (read-from tx buffer)
          until (or (= 0 read) (<= remaining 0))
          do (write-sequence buffer out :end (min read remaining))
             (decf remaining read))))

(defmethod write-to ((entry entry) (pathname pathname) &rest args &key &allow-other-keys)
  (with-open (tx entry :output '(unsigned-byte 8))
    (apply #'write-to tx pathname args)))

(defmethod write-to ((entry entry) (stream stream) &rest args &key &allow-other-keys)
  (with-open (tx entry :output '(unsigned-byte 8))
    (apply #'write-to tx stream args)))

(defmethod write-to ((tx transaction) (pathname pathname) &key start end)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (when start
      (file-position stream start)
      (setf start NIL))
    (write-to tx stream :start start :end (- (or end most-positive-fixnum) (or start 0)))))

(defmethod write-to ((tx transaction) (stream stream) &key start end)
  (let ((buf (make-array 8192 :element-type '(unsigned-byte 8)))
        (rem (- (or end most-positive-fixnum) (or start 0))))
    (declare (dynamic-extent buf))
    (when start
      (loop while (< 0 start)
            for read = (read-sequence buf stream :end (min start (length buf)))
            do (decf start read)))
    (loop while (< 0 rem)
          for read = (read-sequence buf stream)
          do (write-to tx buf :end read)
             (decf rem read)
          while (< 0 read))))

(defmethod read-from ((entry entry) (pathname pathname) &rest args &key &allow-other-keys)
  (with-open (tx entry :input '(unsigned-byte 8))
    (apply #'read-from tx pathname args)))

(defmethod read-from ((entry entry) (stream stream) &rest args &key &allow-other-keys)
  (with-open (tx entry :input '(unsigned-byte 8))
    (apply #'read-from tx stream args)))

(defmethod read-from ((tx transaction) (pathname pathname) &key start end (if-exists :error))
  (with-open-file (stream pathname :direction :output :element-type '(unsigned-byte 8) :if-exists if-exists)
    (read-from tx stream :start start :end end)))

(defmethod read-from ((tx transaction) (stream stream) &key start end)
  (let ((buf (make-array 8192 :element-type '(unsigned-byte 8)))
        (rem (- (or end (size tx)) (or start 0))))
    (declare (dynamic-extent buf))
    (when start (setf (index tx) start))
    (loop while (< 0 rem)
          for read = (read-from tx buf)
          do (write-sequence buf stream :end read)
             (decf rem read)
          while (< 0 read))))

(defclass stream-transaction (transaction)
  ((stream :initarg :stream :reader to-stream)))

(defmethod size ((transaction stream-transaction))
  (file-length (to-stream transaction)))

(defmethod index ((transaction stream-transaction))
  (file-position (to-stream transaction)))

(defmethod (setf index) (index (transaction stream-transaction))
  (file-position (to-stream transaction) index)
  index)

(defmethod abort ((transaction stream-transaction) &key)
  (close (to-stream transaction)))

(defmethod commit ((transaction stream-transaction) &key)
  (close (to-stream transaction)))

(defmethod write-to ((transaction stream-transaction) (sequence sequence) &key start end)
  (write-sequence sequence (to-stream transaction) :start (or start 0) :end end))

(defmethod read-from ((transaction stream-transaction) (sequence sequence) &key start end)
  (read-sequence sequence (to-stream transaction) :start (or start 0) :end end))

;;;; TODO: This interface is /slow/ because there is no internal buffering going on at all.
;;;;       A faster version would properly buffer and keep indices.
(defclass transaction-stream (trivial-gray-streams:fundamental-stream)
  ((transaction :initarg :transaction :reader transaction)))

(defmethod size ((stream transaction-stream))
  (size (transaction stream)))

(defmethod index ((stream transaction-stream))
  (index (transaction stream)))

(defclass transaction-input-stream (transaction-stream trivial-gray-streams:fundamental-input-stream)
  ((unread :initform NIL :accessor unread)))
(defclass transaction-output-stream (transaction-stream trivial-gray-streams:fundamental-output-stream) ())
(defclass transaction-binary-input-stream (transaction-input-stream trivial-gray-streams:fundamental-binary-input-stream) ())
(defclass transaction-binary-output-stream (transaction-output-stream trivial-gray-streams:fundamental-binary-output-stream) ())
(defclass transaction-character-input-stream (transaction-input-stream trivial-gray-streams:fundamental-character-input-stream) ())
(defclass transaction-character-output-stream (transaction-output-stream trivial-gray-streams:fundamental-character-output-stream) ())

(defmethod to-stream ((transaction output-transaction))
  (cond ((subtypep (element-type transaction) '(unsigned-byte 8))
         (make-instance 'transaction-binary-output-stream :transaction transaction))
        ((subtypep (element-type transaction) 'character)
         (make-instance 'transaction-character-output-stream :transaction transaction))
        (T
         (make-instance 'transaction-output-stream :transaction transaction))))

(defmethod to-stream ((transaction input-transaction))
  (cond ((subtypep (element-type transaction) '(unsigned-byte 8))
         (make-instance 'transaction-binary-input-stream :transaction transaction))
        ((subtypep (element-type transaction) 'character)
         (make-instance 'transaction-character-input-stream :transaction transaction))
        (T
         (make-instance 'transaction-input-stream :transaction transaction))))

(defmethod cl:close ((stream transaction-stream) &key abort)
  (if abort
      (abort (transaction stream))
      (commit (transaction stream))))

(defmethod trivial-gray-streams:stream-read-char ((stream transaction-stream))
  (let ((char (unread stream))
        (tx (transaction stream)))
    (cond (char
           (setf (unread stream) NIL)
           char)
          ((<= (size tx) (index tx))
           :eof)
          (T
           (let ((buf (make-array 1 :element-type 'character)))
             (declare (dynamic-extent buf))
             (read-from tx buf)
             (aref buf 0))))))

(defmethod trivial-gray-streams:stream-unread-char ((stream transaction-stream) char)
  (setf (unread stream) char))

(defmethod trivial-gray-streams:stream-read-char-no-hang ((stream transaction-stream))
  (trivial-gray-streams:stream-read-char stream))

(defmethod trivial-gray-streams:stream-peek-char ((stream transaction-stream))
  (let ((char (trivial-gray-streams:stream-read-char stream)))
    (unless (eql :eof char)
      (trivial-gray-streams:stream-unread-char stream char))))

(defmethod trivial-gray-streams:stream-listen ((stream transaction-stream))
  (let ((tx (transaction stream)))
    (< (index tx) (size tx))))

(defmethod trivial-gray-streams:stream-read-line ((stream transaction-stream))
  (let ((buf (make-array 1 :element-type 'character))
        (tx (transaction stream)))
    (declare (dynamic-extent buf))
    (values
     (with-output-to-string (out)
       (loop while (< (index tx) (size tx))
             do (read-from tx buf)
                (let ((char (aref buf 0)))
                  (if (char= char #\Linefeed)
                      (return)
                      (write-char char out)))))
     (<= (size tx) (index tx)))))

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
  (let ((tx (transaction stream)))
    (if (< (index tx) (size tx))
        (let ((buf (make-array 1 :element-type '(unsigned-byte 8))))
          (declare (dynamic-extent buf))
          (read-from tx buf)
          (aref buf 0))
        :eof)))

(defmethod trivial-gray-streams:stream-write-byte ((stream transaction-stream) byte)
  (let ((buf (make-array 1 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buf))
    (setf (aref buf 0) byte)
    (write-to (transaction stream) buf)))

(defmethod trivial-gray-streams:stream-read-sequence ((stream transaction-stream) seq start end &key)
  (cond ((< start end)
         (when (unread stream)
           (setf (aref seq start) (unread stream))
           (setf (unread stream) NIL)
           (incf start))
         (read-from (transaction stream) seq :start start :end end))
        (T end)))

(defmethod trivial-gray-streams:stream-write-sequence ((stream transaction-stream) seq start end &key)
  (write-to (transaction stream) seq :start start :end end))

(defmethod trivial-gray-streams:stream-file-position ((stream transaction-stream))
  (index (transaction stream)))

(defmethod (setf trivial-gray-streams:stream-file-position) (pos (stream transaction-stream))
  (setf (index (transaction stream)) pos))
