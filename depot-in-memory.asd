(asdf:defsystem depot-in-memory
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Simple in-memory depots."
  :homepage "https://shinmera.com/docs/depot"
  :bug-tracker "https://shinmera.com/project/depot/issues"
  :source-control (:git "https://shinmera.com/project/depot.git")
  :serial T
  :components ((:file "in-memory"))
  :depends-on (:depot
               :atomics))
