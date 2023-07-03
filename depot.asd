(asdf:defsystem depot
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Protocol for transparent collections of files."
  :homepage "https://shinmera.github.io/depot"
  :bug-tracker "https://github.com/Shinmera/depot/issues"
  :source-control (:git "https://github.com/Shinmera/depot.git")
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
