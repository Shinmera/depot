(asdf:defsystem depot-test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Testing system for depot."
  :homepage "https://shinmera.github.io/depot"
  :bug-tracker "https://github.com/Shinmera/depot/issues"
  :source-control (:git "https://github.com/Shinmera/depot.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:parachute
               :depot
               :depot-zip
               :depot-in-memory)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.depot.test)))
