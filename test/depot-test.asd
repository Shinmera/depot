#|
 This file is a part of depot
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem depot-test
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Testing system for depot."
  :homepage "https://shinmera.github.io/depot"
  :bug-tracker "https://github.com/Shinmera/depot/issues"
  :source-control (:git "https://github.com/Shinmera/depot.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:parachute
               :depot
               :depot-zip)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.depot.test)))
