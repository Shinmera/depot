#|
 This file is a part of depot
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem depot
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
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
