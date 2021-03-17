#|
 This file is a part of depot
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.depot.zip
  (:use #:cl)
  (:local-nicknames
   (#:depot #:org.shirakumo.depot)
   (#:zippy #:org.shirakumo.zippy))
  (:export
   #:zip-archive
   #:zip-entry
   #:zip-directory
   #:zip-file))

(in-package #:org.shirakumo.depot.zip)

(defun starts-with (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defclass zip-archive (depot:depot depot:file zippy:zip-file)
  ((streams :initarg :streams :initform () :reader streams)))

(depot:define-realizer zip
  ((file depot:file)
   (multiple-value-bind (zip-file streams) (zippy:open-zip-file (depot:to-pathname file))
     (let ((file (change-class zip-file 'zip-archive :streams streams
                                                     :pathname (depot:to-pathname file)
                                                     :depot (depot:depot file))))
       (loop for entry across (zippy:entries file)
             do (if (find :directory (zippy:attributes entry))
                    (change-class entry 'zip-directory :depot file)
                    (change-class entry 'zip-file :depot file)))
       file))))

(defmethod depot:list-entries ((depot zip-archive))
  (coerce (zippy:entries depot) 'list))

(defmethod depot:query-entries ((depot zip-archive) &key name type &allow-other-keys)
  (let ((name (format NIL "~a~@[.~a~]" name type)))
    (loop for entry across (zippy:entries depot)
          when (string= (zippy:file-name entry) name)
          collect entry)))

(defmethod depot:make-entry ((depot zip-archive) &key)
  )

(defclass zip-entry (zippy:zip-entry)
  ())

(defmethod close ((entry zip-entry) &rest args)
  (apply #'close (depot:depot entry) args))

(defmethod depot:attributes ((entry zip-entry))
  (destructuring-bind (file-attrs encoding system-attributes) (zippy:attributes entry)
    (list* :encoding encoding
           :system-attributes system-attributes
           :name (zippy:file-name entry)
           :crc-32 (zippy:crc-32 entry)
           :disk (zippy:disk entry)
           :size (zippy:size entry)
           :uncompressed-size (zippy:uncompressed-size entry)
           :encryption-method (zippy:encryption-method entry)
           :compression-method (zippy:compression-method entry)
           :last-modified (zippy:last-modified entry)
           :comment (zippy:comment entry)
           file-attrs)))

(defmethod (setf depot:attributes) (attributes (entry zip-entry))
  )

(defmethod depot:delete-entry ((entry zip-entry))
  (setf (zippy:entries (zippy:zip-file entry))
        (delete entry (zippy:entries (zippy:zip-file entry)))))

(defclass zip-directory (zip-entry depot:depot)
  ())

(defmethod depot:list-entries ((depot zip-directory))
  (loop with prefix = (format NIL "~a/" (zippy:file-name depot))
        for entry across (zippy:entries (zippy:zip-file depot))
        when (starts-with prefix (zippy:file-name entry))
        collect entry))

(defmethod depot:make-entry ((depot zip-directory) &key)
  )

(defclass zip-file (zip-entry depot:entry)
  ())

(defmethod depot:open-entry ((entry zip-entry) (direction (eql :output)) element-type &key (encryption-method (zippy:encryption-method entry))
                                                                                           (compression-method (zippy:compression-method entry))
                                                                                           password)
  )

(defmethod depot:open-entry ((entry zip-entry) (direction (eql :input)) element-type &key password)
  (let* ((disks (zippy:disks (zippy:zip-file entry)))
         (disk (zippy:disk entry))
         (input (or (aref disks disk)
                    (restart-case (error 'zippy:archive-file-required :disk disk)
                      (use-value (new-input)
                        (setf (aref disks disk) new-input))))))
    (zippy:seek input (zippy:offset entry))
    (zippy::lf-to-entry (zippy::parse-structure* input) entry)
    (make-instance 'read-transaction
                   :entry entry
                   :input input
                   :decryption-state (apply #'zippy:make-decryption-state (first (zippy:encryption-method entry)) input password (rest (zippy:encryption-method entry)))
                   :decompression-state (zippy:make-decompression-state (zippy:compression-method entry)))))

(defclass read-transaction (depot:transaction)
  ((input :initarg :input :reader input)
   (index :initform 0 :accessor index :accessor depot:index)
   (decryption-state :initarg :decryption-state :reader decryption-state)
   (decompression-state :initarg :decompression-state :reader decompression-state)
   (buffer :initform #() :initarg :buffer :accessor buffer)
   (buffer-start :initform 0 :accessor buffer-start)
   (buffer-end :initform 0 :accessor buffer-end)))

(defmethod depot:size ((transaction read-transaction))
  (zippy:uncompressed-size (depot:target transaction)))

(defmethod (setf depot:index) :after (index (transaction read-transaction))
  (zippy:seek (input transaction) (+ (zippy:offset (depot:target transaction)) index)))

(defmethod depot:commit ((transaction read-transaction)))
(defmethod depot:abort ((transaction read-transaction)))

(defmethod depot:read-from ((transaction read-transaction) sequence &key start end)
  (let ((decompression-state (decompression-state transaction))
        (decryption-state (decryption-state transaction))
        (target-start (or start 0))
        (target-end (or end (length sequence)))
        (index 0))
    (when (< (depot:index transaction) (zippy:size (depot:target transaction)))
      (block NIL
        (labels ((handle (buffer start end)
                   (let* ((avail (- end start))
                          (to-copy (max 0 (min avail (- target-end target-start)))))
                     (replace sequence buffer :start1 (+ index target-start) :end1 target-end :start2 start :end2 end)
                     (incf index to-copy)
                     (when (<= target-end index)
                       (when (< to-copy avail)
                         (setf (buffer transaction) buffer)
                         (setf (buffer-start transaction) (+ start to-copy))
                         (setf (buffer-end transaction) end))
                       (return))))
                 (decompress (buffer start end)
                   (zippy:call-with-decompressed-buffer #'handle buffer start end decompression-state)))
          ;; First copy from buffer remaining
          (handle (buffer transaction) (buffer-start transaction) (buffer-end transaction))
          ;; Then decode new.        
          (zippy:call-with-decrypted-buffer
           #'decompress
           (input transaction)
           (- (zippy:size (depot:target transaction)) (depot:index transaction) index)
           decryption-state)))
      (incf (slot-value transaction 'index) index))
    (+ target-start index)))
