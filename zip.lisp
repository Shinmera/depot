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
   #:zip
   #:zip-archive
   #:zip-entry
   #:zip-directory
   #:zip-file))

(in-package #:org.shirakumo.depot.zip)

(defun starts-with (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun find-parent (entry)
  (let* ((name (zippy:file-name entry))
         (end (if (char= #\/ (char name (1- (length name))))
                  (1- (length name))
                  (length name)))
         (slashpos (position #\/ (zippy:file-name entry) :from-end T :end end)))
    (if slashpos
        (let ((parent-name (subseq name 0 (1+ slashpos))))
          (or (find parent-name (zippy:entries (zippy:zip-file entry))
                    :key #'zippy:file-name :test #'string=)
              (values NIL parent-name)))
        (zippy:zip-file entry))))

(defun find-id (name)
  (let* ((end (if (char= #\/ (char name (1- (length name))))
                  (1- (length name))
                  (length name))))
    (let ((slashpos (position #\/ name :from-end T :end end)))
      (if slashpos
          (subseq name (1+ slashpos) end)
          (subseq name 0 end)))))

(defun convert-entries (file)
  (loop for entry across (zippy:entries file)
        do (when (getf (first (zippy:attributes entry)) :directory)
             (change-class entry 'zip-directory)))
  (loop for entry across (zippy:entries file)
        do (unless (getf (first (zippy:attributes entry)) :directory)
             (change-class entry 'zip-file)))
  file)

(defclass zip-archive (depot:depot zippy:zip-file)
  ((depot:depot :initarg :depot :reader depot:depot)
   (entries :initform () :accessor entries :reader depot:list-entries)))

(defmethod depot:make-entry ((depot zip-archive) &key name type id encryption-method compression-method (last-modified (get-universal-time)) comment)
  ;; FIXME: check for duplicates and error.
  (let* ((file-name (or id (format NIL "~a~@[.~a~]" name type)))
         (entry (make-instance (if (char= #\/ (char file-name (1- (length file-name))))
                                   'zip-directory
                                   'zip-file)
                               :zip-file depot
                               :encryption-method encryption-method
                               :compression-method compression-method
                               :last-modified last-modified
                               :comment comment
                               :file-name file-name)))
    (vector-push-extend entry (zippy:entries depot))
    entry))

(defclass zip-file-archive (zip-archive depot:file)
  ((streams :initarg :streams :initform () :reader streams)))

(defmethod depot:commit ((depot zip-file-archive) &key password)
  (depot:with-open (tx depot :output '(unsigned-byte 8))
    (zippy:compress-zip depot (depot:to-stream tx) :password password)
    depot))

(defmethod close ((depot zip-file-archive) &key abort)
  (unless abort
    (depot:commit depot))
  (when (zippy:disks depot)
    (loop for disk across (zippy:disks depot)
          do (when (streamp disk)
               (close disk :abort abort)))
    (setf (zippy:disks depot) NIL)))

(depot:define-realizer zip
  ((file depot:file)
   (cond ((probe-file (depot:to-pathname file))
          (multiple-value-bind (zip-file streams) (zippy:open-zip-file (depot:to-pathname file))
            (convert-entries (change-class zip-file 'zip-file-archive
                                           :streams streams
                                           :pathname (depot:to-pathname file)
                                           :depot (depot:depot file)))))
         ((string= "zip" (depot:attribute :type file))
          (make-instance 'zip-file-archive :pathname (depot:to-pathname file)
                                           :entries (make-array 0 :adjustable T :fill-pointer T)
                                           :depot (depot:depot file)))))
  ((entry depot:entry)
   ;; KLUDGE: we can't use the streams interface because zippy requires file-streams (and file-length)
   ;;         which gray streams cannot emulate. :(
   (convert-entries (change-class (zippy:open-zip-file (depot:read-from entry 'byte)) 'zip-archive
                                  :depot (depot:depot entry)))))

(defclass zip-entry (zippy:zip-entry)
  ((id :initarg :id :accessor depot:id)
   (depot :initarg :depot :accessor depot:depot)))

(defmethod shared-initialize :after ((entry zip-entry) slots &key)
  (unless (slot-boundp entry 'id)
    (setf (depot:id entry) (find-id (zippy:file-name entry))))
  (unless (slot-boundp entry 'depot)
    (multiple-value-bind (parent name) (find-parent entry)
      (unless parent
        (setf parent (depot:make-entry (zippy:zip-file entry) :id name)))
      (setf (depot:depot entry) parent)
      (push entry (entries parent)))))

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
           :id (depot:id entry)
           file-attrs)))

(defmethod (setf depot:attributes) (attributes (entry zip-entry))
  )

(defmethod depot:delete-entry ((entry zip-entry))
  (setf (entries (depot:depot entry))
        (delete entry (entries (depot:depot entry))))
  (setf (zippy:entries (zippy:zip-file entry))
        (delete entry (zippy:entries (zippy:zip-file entry)))))

(defclass zip-directory (zip-entry depot:depot)
  ((entries :initform () :accessor entries :reader depot:list-entries)))

(defmethod depot:make-entry ((depot zip-directory) &rest args)
  (let ((name (or (getf args :id) (format NIL "~a~@[.~a~]" (getf args :name) (getf args :type)))))
    (apply #'depot:make-entry (depot:depot depot) :id (format NIL "~a~a" (zippy:file-name depot) name) args)))

(defmethod depot:entry-matches-p ((depot zip-directory) (attribute (eql :id)) id)
  (string= id (depot:id depot)))

(defclass zip-file (zip-entry depot:entry)
  ())

(defmethod depot:open-entry ((entry zip-entry) (direction (eql :output)) element-type &key (encryption-method (zippy:encryption-method entry))
                                                                                           (compression-method (zippy:compression-method entry))
                                                                                           password)
  (unless (subtypep element-type '(unsigned-byte 8))
    (error "Only (unsigned-byte 8) is supported as element-type."))
  (make-instance 'write-transaction
                 :entry entry
                 :element-type element-type
                 :encryption-method encryption-method
                 :compression-method compression-method
                 :password password))

(defclass write-transaction (depot:output-transaction)
  ((index :initform 0 :accessor index)
   (buffers :initform (list (make-array 4096 :element-type '(unsigned-byte 8))) :accessor buffers)
   (encryption-method :initarg :encryption-method :accessor encryption-method)
   (compression-method :initarg :compression-method :accessor compression-method)
   (password :initarg :password :accessor password)))

(defmethod depot:write-to ((transaction write-transaction) (sequence sequence) &key start end)
  (let ((buffer (first (buffers transaction)))
        (index (index transaction))
        (start (or start 0))
        (end (or end (length sequence))))
    (loop
       (let* ((avail (- (length buffer) index))
              (to-copy (max 0 (min avail (- end start)))))
         (replace buffer sequence :start1 index :start2 start :end2 end)
         (incf index to-copy)
         (incf start to-copy)
         (when (<= (length buffer) index)
           (setf buffer (make-array 4096 :element-type '(unsigned-byte 8)))
           (push buffer (buffers transaction))
           (setf index 0))
         (when (<= end start)
           (return))))
    (setf (index transaction) index)))

(defmethod depot:commit ((transaction write-transaction) &key flush password)
  (destructuring-bind (partial . buffers) (buffers transaction)
    (let* ((total-size (+ (index transaction)
                          (loop for buffer in buffers sum (length buffer))))
           (complete (make-array total-size :element-type '(unsigned-byte 8)))
           (index (- total-size (index transaction)))
           (entry (depot:target transaction)))
      (replace complete partial :start1 index)
      (dolist (buffer buffers)
        (decf index (length buffer))
        (replace complete buffer :start1 index))
      (setf (zippy:content entry) complete)
      (setf (zippy:encryption-method entry) (encryption-method transaction))
      (setf (zippy:compression-method entry) (compression-method transaction))
      (when flush
        (depot:commit (depot:depot entry) :password password)))))

(defmethod depot:abort ((transaction write-transaction) &key)
  (setf (buffers transaction) ()))

(defmethod depot:open-entry ((entry zip-entry) (direction (eql :output)) (element-type (eql 'character)) &key password (external-format :utf-8))
  (let ((sub (depot:open-entry entry direction '(unsigned-byte 8) :password password)))
    (change-class sub 'string-write-transaction :external-format external-format)))

(defclass string-write-transaction (write-transaction)
  ((external-format :initarg :external-format :initform :utf-8 :accessor external-format)))

(defmethod depot:element-type ((transaction string-write-transaction)) 'character)

(defmethod depot:write-to ((transaction string-write-transaction) (sequence sequence) &key start end)
  ;; KLUDGE: this sucks. We keep allocating new vectors here.
  (call-next-method transaction (babel:string-to-octets sequence :start (or start 0) :end (or end (length sequence)) :encoding (external-format transaction))))

(defmethod depot:open-entry ((entry zip-entry) (direction (eql :input)) element-type &key password)
  (unless (subtypep element-type '(unsigned-byte 8))
    (error "Only (unsigned-byte 8) is supported as element-type."))
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
                   :element-type element-type
                   :decryption-state (apply #'zippy:make-decryption-state (first (zippy:encryption-method entry)) input password (rest (zippy:encryption-method entry)))
                   :decompression-state (zippy:make-decompression-state (zippy:compression-method entry)))))

(defmethod depot:read-from ((entry zip-entry) (thing (eql 'character)) &rest args)
  (babel:octets-to-string
   (apply #'depot:read-from entry 'byte args)))

(defclass read-transaction (depot:input-transaction)
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

(defmethod depot:commit ((transaction read-transaction) &key))
(defmethod depot:abort ((transaction read-transaction) &key))

(defmethod depot:read-from ((transaction read-transaction) (sequence sequence) &key start end)
  (let ((decompression-state (decompression-state transaction))
        (decryption-state (decryption-state transaction))
        (target-start (or start 0))
        (target-end (or end (length sequence)))
        (input (input transaction))
        (size (zippy:size (depot:target transaction)))
        (index (depot:index transaction)))
    (when (< index size)
      (labels ((decode (buffer bstart bend)
                 (let* ((available (- bend bstart))
                        (copyable (min (- target-end target-start) available)))
                   (replace sequence buffer :start1 target-start :end1 (+ target-start copyable) :start2 bstart)
                   (incf target-start copyable)
                   (+ bstart copyable)))
               (decompress (buffer start end)
                 (let ((consumed (zippy:call-with-decompressed-buffer #'decode buffer start end decompression-state)))
                   (incf index consumed)
                   consumed)))
        (loop until (= 0 (zippy:call-with-decrypted-buffer #'decompress input size decryption-state))))
      (setf (slot-value transaction 'index) index))
    target-start))

(defmethod depot:open-entry ((entry zip-entry) (direction (eql :input)) (element-type (eql 'character)) &key password (external-format :utf-8))
  (let ((sub (depot:open-entry entry direction '(unsigned-byte 8) :password password)))
    (change-class sub 'string-read-transaction :external-format external-format)))

(defclass string-read-transaction (read-transaction)
  ((external-format :initarg :external-format :initform :utf-8 :accessor external-format)))

(defmethod depot:element-type ((transaction string-read-transaction)) 'character)

(defmethod depot:read-from ((transaction string-write-transaction) (sequence sequence) &key start end)
  ;; KLUDGE: oh god this makes me nauseous.
  (let* ((start (or start 0))
         (end (or end (length sequence)))
         (buf (make-array (- end start) :element-type '(unsigned-byte 8))))
    (call-next-method transaction buf)
    (let ((string (babel:octets-to-string sequence :encoding (external-format transaction))))
      (replace sequence string :start1 start :end1 end)
      (min end (+ start (length string))))))
