(defpackage #:org.shirakumo.depot.virtual
  (:use #:cl)
  (:local-nicknames
   (#:depot #:org.shirakumo.depot))
  (:export
   #:depot
   #:entry))

(in-package #:org.shirakumo.depot.virtual)

(defclass depot (depot:depot)
  ((map :initform () :reader depot-map :writer set-map)))

;; FIXME: could implement a binary tree kinda thing here to speed up the search.
(defmethod find-depot ((depot depot) (id string))
  (loop for (path . subdepot) in (depot-map depot)
        do (when (and (<= (length path) (length id))
                      (string= path id :end2 (length path)))
             (return (values subdepot (subseq id (length path)))))))

(defun path< (a b)
  (let ((min (min (length a) (length b))))
    (if (string= a b :end1 min :end2 min)
        (< (length b) (length a))
        (string< a b))))

(defun ensure-trailing (path)
  (if (= 0 (length path))
      path
      (let ((trailing (char= #\/ (char path (1- (length path))))))
        (cond ((not trailing) (format NIL "~a/" path))
              ((= 1 (length path)) "")
              (T path)))))

(defmethod (setf depot-map) (map (depot depot))
  (set-map (sort (mapcar #'ensure-trailing map)
                 #'path< :key #'car)
           depot))

(defmethod depot:list-entries ((depot depot))
  (loop for (path . subdepot) in (depot-map depot)
        append (depot:list-entries subdepot)))

(defmethod find-depot ((depot depot) (id string))
  (loop for (path . subdepot) in (depot-map depot)
        do (when (and (<= (length path) (length id))
                      (string= path id :end2 (length path))))))

(defmethod depot:query-entries ((depot depot) &rest attributes &key id)
  (if id
      (multiple-value-bind (subdepot id) (find-depot depot id)
        (apply #'depot:query-entries subdepot :id id attributes))
      (loop for (path . subdepot) in (depot-map depot)
            append (apply #'depot:query-entries subdepot attributes))))

(defmethod depot:query-entry ((depot depot) &rest attributes &key id)
  (if id
      (multiple-value-bind (subdepot id) (find-depot depot id)
        (apply #'depot:query-entry subdepot :id id attributes))
      (loop for (path . subdepot) in (depot-map depot)
            thereis (apply #'depot:query-entry subdepot attributes))))

(defmethod depot:entry ((id string) (depot depot))
  (multiple-value-bind (subdepot id) (find-depot depot id)
    (depot:entry id subdepot)))

(defmethod (setf depot:entry) ((subdepot depot:depot) (base string) (depot depot))
  (setf (depot-map depot) (list* (cons base subdepot) (depot-map depot))))

(defmethod depot:entry-exists-p ((id string) (depot depot))
  (multiple-value-bind (subdepot id) (find-depot depot id)
    (depot:entry-exists-p id subdepot)))

(defmethod depot:make-entry ((depot depot) &rest attributes &key id &allow-other-keys)
  (multiple-value-bind (subdepot id) (find-depot depot id)
    (apply #'depot:make-entry subdepot :id id attributes)))
