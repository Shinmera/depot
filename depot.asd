(asdf:defsystem depot
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Protocol for transparent collections of files."
  :homepage "https://shinmera.com/docs/depot"
  :bug-tracker "https://shinmera.com/project/depot/issues"
  :source-control (:git "https://shinmera.com/project/depot.git")
  :serial T
  :components ((:file "package")
               (:file "conditions")
               (:file "protocol")
               (:file "pathname")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :trivial-features
               :trivial-gray-streams)
  :in-order-to ((asdf:test-op (asdf:test-op :depot-test))))
