(in-package #:org.shirakumo.depot)

(defvar *os-depot*)

(defclass os-depot (depot) ())
(defclass host (depot) ())
(defclass device (depot) ())

(defmethod attributes ((depot host))
  (list :id (id depot)))

(defmethod attributes ((depot device))
  (list :id (id depot)))

(defgeneric to-pathname (entry))

(defun path-component-specific-p (comp)
  (not (or (eql comp NIL) (eql comp :unspecific))))

(defun normalize-path-component (comp)
  (case comp
    ((NIL) NIL)
    (:unspecific NIL)
    (T comp)))

(defun directory-p (path)
  (let ((true (truename path)))
    (when (if (stringp true)
              (char= #+windows #\\ #-windows #\/ (char true (1- (length true))))
              (not (or (pathname-name true) (pathname-type true))))
      true)))

(defmethod to-pathname ((pathname pathname))
  pathname)

(defmethod list-entries ((pathname pathname))
  (list-entries (from-pathname pathname)))

(defmethod query-entries ((pathname pathname) &rest args)
  (apply #'query-entries (from-pathname pathname) args))

(defmethod query-entry ((pathname pathname) &rest args)
  (apply #'query-entry (from-pathname pathname) args))

(defmethod entry (id (pathname pathname))
  (entry id (from-pathname pathname)))

(defmethod entry-exists-p (id (pathname pathname))
  (entry-exists-p id (from-pathname pathname)))

(defmethod make-entry ((pathname pathname) &rest args)
  (apply #'make-entry (from-pathname pathname) args))

(defmethod open-p ((pathname pathname))
  (open-p (from-pathname pathname)))

(defmethod delete-entry ((pathname pathname))
  (delete-entry (from-pathname pathname)))

(defmethod entry-matches-p ((pathname pathname) attribute value)
  (entry-matches-p (from-pathname pathname) attribute value))

(defmethod attributes ((pathname pathname))
  (attributes (from-pathname pathname)))

(defmethod (setf attributes) (attributes (pathname pathname))
  (setf (attributes (from-pathname pathname)) attributes))

(defmethod attribute (name (pathname pathname))
  (attribute (from-pathname pathname)))

(defmethod (setf attribute) (value name (pathname pathname))
  (setf (attribute name (from-pathname pathname)) value))

(defmethod id ((pathname pathname))
  pathname)

(defmethod depot ((pathname pathname))
  (depot (from-pathname pathname)))

(defmethod realize-entry ((pathname pathname) realizer)
  (realize-entry (from-pathname pathname) realizer))

(defmethod ensure-depot ((pathname pathname))
  (ensure-depot (from-pathname pathname)))

(defmethod ensure-entry (id (pathname pathname) &rest attributes)
  (apply #'ensure-entry id (from-pathname pathname) attributes))

(defmethod open-entry ((pathname pathname) direction element-type &rest args &key (if-does-not-exist :create))
  (remf args :if-does-not-exist)
  (apply #'open-entry (from-pathname pathname :if-does-not-exist if-does-not-exist) direction element-type args))

(defmethod to-pathname ((entry entry))
  (let* ((name (id entry))
         (dotp (position #\. name :from-end T))
         (type (when dotp (subseq name (1+ dotp))))
         (name (subseq name 0 dotp))
         (depot (depot entry))
         (dirs (loop until (or (typep depot '(or device host (not entry)))
                               (eq depot *os-depot*))
                     collect (id depot)
                     do (setf depot (depot depot))))
         (device (when (typep depot 'device)
                   (prog1 (id depot)
                     (setf depot (depot depot)))))
         (host (if (typep depot 'host)
                   (id depot)
                   (pathname-host *default-pathname-defaults*))))
    (make-pathname :host host :device device :directory (list* :absolute (nreverse dirs))
                   :name name :type type :version (ignore-errors (attribute :version entry)))))

(defun from-pathname (pathname &key (create-directories :pretend) (if-does-not-exist :create))
  (let ((depot *os-depot*)
        (pathname (merge-pathnames pathname *default-pathname-defaults*)))
    (setf depot (query-entry depot :host (pathname-host pathname)))
    (if (path-component-specific-p (pathname-device pathname))
        (setf depot (query-entry depot :device (pathname-device pathname)))
        (setf depot (query-entry depot :device NIL)))
    (if (eql :pretend create-directories)
        (loop for dir in (rest (pathname-directory pathname))
              for path = (to-pathname depot)
              for newpath = (make-pathname :directory (append (pathname-directory path) (list dir)) :defaults path)
              do (setf depot (make-instance 'directory :depot depot :pathname newpath)))
        (loop for dir in (rest (pathname-directory pathname))
              do (setf depot (ensure-depot (or (query-entry depot :id dir)
                                               (if create-directories
                                                   (make-entry depot :name dir :type :directory)
                                                   (error 'no-such-entry :object depot :id dir)))))))
    (if (or (pathname-name pathname)
            (pathname-type pathname))
        (or (query-entry depot :name (pathname-name pathname)
                               :type (pathname-type pathname)
                               :version (pathname-version pathname))
            (ecase if-does-not-exist
              (:create (make-entry depot :name (pathname-name pathname)
                                         :type (pathname-type pathname)))
              (:error (error 'no-such-entry :id (file-namestring pathname) :object depot))
              ((NIL) NIL)))
        depot)))

(defmethod ensure-depot ((pathname pathname))
  (ensure-depot (from-pathname pathname)))

(defmethod realize-entry ((pathname pathname) realizer)
  (realize-entry (from-pathname pathname) realizer))

(defclass directory (depot entry)
  ((depot :initarg :depot :reader depot)
   (pathname :initarg :pathname :reader to-pathname)))

(defmethod print-object ((directory directory) stream)
  (print-unreadable-object (directory stream :type T)
    (format stream "~a" (to-pathname directory))))

(defmethod make-load-form ((directory directory) &optional env)
  (declare (ignore env))
  `(from-pathname ,(to-pathname directory)))

(defmethod id ((entry directory))
  (car (last (pathname-directory (to-pathname entry)))))

(defmethod attributes ((entry directory))
  (let ((pathname (to-pathname entry)))
    (list :id (car (last (pathname-directory pathname)))
          :name (car (last (pathname-directory pathname)))
          :type :directory
          :pathname pathname
          :author (file-author pathname))))

(defmethod attribute ((name (eql :author)) (entry directory))
  (file-author (to-pathname entry)))

(defmethod attribute ((name (eql :type)) (entry directory))
  :directory)

(defmethod entry (id (depot directory))
  (let ((path (merge-pathnames id (to-pathname depot))))
    (or (when (probe-file path)
          (let ((dirpath (directory-p path)))
            (if dirpath
                (make-instance 'directory :depot depot :pathname dirpath)
                (make-instance 'file :depot depot :pathname path))))
        (error 'no-such-entry :object depot :id id))))

(defmethod entry-exists-p (id (depot directory))
  (probe-file (merge-pathnames id (to-pathname depot))))

(defmethod list-entries ((depot directory))
  (let* ((pathname (to-pathname depot))
         (directories (cl:directory (merge-pathnames (make-pathname :directory `(:relative :wild) :name NIL :type NIL :version NIL) pathname)))
         (files (cl:directory (make-pathname :name :wild :type :wild :version (or #-(or allegro abcl xcl) :newest) :defaults pathname)))
         (entries ()))
    (dolist (directory directories)
      (push (make-instance 'directory :depot depot :pathname directory) entries))
    (dolist (file files entries)
      (when (and (path-component-specific-p (pathname-name file))
                 (path-component-specific-p (pathname-type file)))
        (push (make-instance 'file :depot depot :pathname file) entries)))))

(defmethod query-entries ((depot directory) &key name type version id)
  (let ((pathname (to-pathname depot))
        (entries ()))
    ;; FIXME: This does not work correctly when we are on Windows and the file name is mismatched in case.
    (cond ((eql :directory type)
           (dolist (file (cl:directory (merge-pathnames (make-pathname :directory `(:relative :wild)) pathname)) entries)
             (when (cond (id (string= (car (last (pathname-directory file))) id))
                         (name (string= (car (last (pathname-directory file))) name))
                         (T T))
               (push (make-instance 'directory :depot depot :pathname file) entries))))
          ((or version type id) ;; Can't do more with ID here since we don't know how the implementation separates name from type in a pathname. Sucks.
           (dolist (file (cl:directory (make-pathname :name (or name :wild) :type (or type :wild) :version (or version #-(or allegro abcl xcl) :wild) :defaults pathname)))
             (when (or (null id) (string= (file-namestring file) id))
               (push (make-instance 'file :depot depot :pathname file) entries)))
           (dolist (file (cl:directory (merge-pathnames (make-pathname :directory `(:relative :wild)) pathname)) entries)
             (when (string= (car (last (pathname-directory file))) id)
               (push (make-instance 'directory :depot depot :pathname file) entries))))
          (name
           (dolist (directory (cl:directory (merge-pathnames (make-pathname :directory `(:relative ,name)) pathname)))
             (push (make-instance 'directory :depot depot :pathname directory) entries))
           (dolist (file (cl:directory (make-pathname :name name :version (or #-(or allegro abcl xcl) :wild) :defaults pathname)) entries)
             (push (make-instance 'file :depot depot :pathname file) entries)))
          (T (list-entries depot)))))

(defmethod make-entry ((depot directory) &key name type id)
  (cond ((eql type :directory)
         (make-instance 'directory :depot depot :pathname (merge-pathnames (make-pathname :directory `(:relative ,(or name id)))
                                                                           (to-pathname depot))))
        (T
         (when id
           (let ((dot (position #\. id :from-end T)))
             (if dot
                 (setf name (subseq id 0 dot)
                       type (subseq id (1+ dot)))
                 (setf name id))))
         (make-instance 'file :depot depot :pathname (make-pathname :name name :type type :defaults (to-pathname depot))))))

(defmethod open-p ((directory directory))
  T)

(defclass file (entry)
  ((depot :initarg :depot :reader depot)
   (pathname :initarg :pathname :reader to-pathname)))

(defmethod make-load-form ((file file) &optional env)
  (declare (ignore env))
  `(from-pathname ,(to-pathname file)))

(defmethod print-object ((file file) stream)
  (print-unreadable-object (file stream :type T)
    (format stream "~a" (to-pathname file))))

(defmethod id ((entry file))
  (file-namestring (to-pathname entry)))

(defmethod attributes ((entry file))
  (let ((pathname (to-pathname entry)))
    (list :id (file-namestring pathname)
          :pathname pathname
          :name (normalize-path-component (pathname-name pathname))
          :type (normalize-path-component (pathname-type pathname))
          :version (normalize-path-component (pathname-version pathname))
          :write-date (file-write-date pathname)
          :author (file-author pathname))))

(defmethod attribute ((name (eql :name)) (entry file))
  (normalize-path-component (pathname-name (to-pathname entry))))

(defmethod attribute ((name (eql :type)) (entry file))
  (normalize-path-component (pathname-type (to-pathname entry))))

(defmethod attribute ((name (eql :write-date)) (entry file))
  (file-write-date (to-pathname entry)))

(defmethod attribute ((name (eql :author)) (entry file))
  (file-author (to-pathname entry)))

(defmethod delete-entry ((entry file))
  (delete-file (to-pathname entry)))

(defclass file-transaction (stream-transaction)
  ((timestamp :initarg :timestamp :reader timestamp)))

(defclass file-write-transaction (file-transaction output-transaction)
  ((if-exists :initarg :if-exists :reader if-exists)))

(defmethod open-entry ((file file) (direction (eql :output)) element-type &key (external-format :default) (if-exists :supersede))
  (let* ((pathname (to-pathname file))
         (tmp (make-pathname :name (format NIL "~a-tmp~d~d" (pathname-name pathname) (get-universal-time) (random 100)) :defaults pathname)))
    (ecase if-exists
      ((:replace :supersede :overwrite))
      ((NIL)
       (return-from open-entry NIL))
      (:error
       (when (probe-file pathname)
         (error #+sbcl 'sb-ext:file-exists #-sbcl 'file-error :pathname pathname))))
    (ensure-directories-exist tmp)
    (make-instance 'file-write-transaction :stream (open tmp :direction direction :element-type element-type :external-format external-format)
                                           :entry file
                                           :element-type element-type
                                           :timestamp (when (probe-file pathname) (file-write-date pathname))
                                           :if-exists if-exists)))

;; FIXME: testing for changes via timestamp only is error prone.
(defmethod commit ((transaction file-write-transaction) &key (rename-to-target T))
  (call-next-method)
  (let ((target (to-pathname (target transaction)))
        (source (pathname (to-stream transaction))))
    (unwind-protect
         (progn
           (ecase (if-exists transaction)
             (:error
              (if (null (timestamp transaction))
                  (when (probe-file target)
                    (cerror "Ignore and commit anyway." 'entry-already-exists :object (depot (target transaction)) :attributes (attributes (target transaction))))
                  (when (< (timestamp transaction) (file-write-date target))
                    (cerror "Ignore and commit anyway." 'write-conflict :object transaction))))
             ((:overwrite :supersede :replace))
             ((NIL) (return-from commit NIL)))
           #+(or windows nx)
           (when (probe-file target)
             (ignore-errors
              (delete-file target)))
           (if rename-to-target
               (rename-file source target)
               source))
      (ignore-errors
       (when (and rename-to-target (probe-file source))
         (delete-file source))))))

(defmethod abort ((transaction file-write-transaction) &key)
  (call-next-method)
  (ignore-errors (delete-file (to-stream transaction))))

(defclass file-read-transaction (file-transaction input-transaction)
  ())

(defmethod open-entry ((file file) (direction (eql :input)) element-type &key (external-format :default) (if-does-not-exist :error))
  (let* ((pathname (to-pathname file))
         (stream (open pathname :direction direction :element-type element-type :external-format external-format :if-does-not-exist if-does-not-exist)))
    (when stream
      (make-instance 'file-read-transaction :stream stream
                                            :entry file
                                            :element-type element-type
                                            :timestamp (file-write-date pathname)))))

(defmethod commit ((transaction file-read-transaction) &key)
  (call-next-method)
  (let ((pathname (to-pathname (target transaction))))
    (cond ((not (probe-file pathname))
           (cerror "Ignore and commit anyway." 'entry-does-not-exist :object (target transaction)))
          ((< (timestamp transaction) (file-write-date pathname))
           (cerror "Ignore and commit anyway." 'read-invalidated :object transaction)))))

(unless (boundp '*os-depot*)
  (setf *os-depot* (make-instance 'os-depot)))

(let ((host (make-instance 'host)))
  (defmethod list-entries ((depot os-depot))
    (list host))

  (defmethod query-entries ((depot os-depot) &key ((:host _)))
    (declare (ignore _))
    (list host))

  (defmethod id ((depot host))
    (pathname-host *default-pathname-defaults*))  

  (defmethod depot ((depot host))
    *os-depot*)

  (defmethod list-entries ((depot host))
    (loop for device in #-windows '(NIL) #+windows (load-time-value (loop for i from (char-code #\A) to (char-code #\Z) collect (string (code-char i))))
          collect (make-instance 'device :depot depot :pathname (make-pathname :device device :directory '(:absolute)))))

  (defmethod query-entry ((depot host) &key device)
    (make-instance 'device :depot depot :pathname (make-pathname :device device :directory '(:absolute))))

  (defclass device (directory) ())

  (defmethod id ((depot device))
    (pathname-device (to-pathname depot)))

  (defmethod query-entry ((depot device) &key id)
    (if (eql id :home)
        (from-pathname (user-homedir-pathname))
        (call-next-method)))

  (defmethod depot ((depot device))
    host))
