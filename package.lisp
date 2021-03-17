#|
 This file is a part of depot
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.depot
  (:use #:cl)
  (:shadow #:abort #:directory)
  ;; conditions.lisp
  (:export
   #:depot-condition
   #:object
   #:no-such-entry
   #:id
   #:permission-denied
   #:depot-full
   #:unsupported-operation
   #:operation
   #:transaction-aborted
   #:write-conflict
   #:read-invalidated
   #:not-a-depot)
  ;; protocol.lisp
  (:export
   #:realizer
   #:depot
   #:entry
   #:depot
   #:transaction
   #:target
   #:entry
   #:list-entries
   #:query-entries
   #:query-entry
   #:entry
   #:entry-exists-p
   #:make-entry
   #:delete-entry
   #:entry-matches-p
   #:attributes
   #:attribute
   #:id
   #:depot
   #:realize-entry
   #:open-entry
   #:write-to
   #:read-from
   #:size
   #:index
   #:to-stream
   #:commit
   #:abort
   #:register-realizer
   #:remove-realizer
   #:define-realizer
   #:entry*
   #:with-open
   #:ensure-depot
   #:transaction-stream)
  ;; pathname.lisp
  (:export
   #:*os-depot*
   #:os-depot
   #:host
   #:device
   #:to-pathname
   #:from-pathname
   #:directory
   #:file
   #:stream-transaction
   #:file-write-transaction
   #:file-read-transaction))
