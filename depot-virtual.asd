#|
 This file is a part of depot
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem depot-virtual
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Simple virtual depots."
  :homepage "https://shinmera.github.io/depot"
  :bug-tracker "https://github.com/Shinmera/depot/issues"
  :source-control (:git "https://github.com/Shinmera/depot.git")
  :serial T
  :components ((:file "virtual"))
  :depends-on (:depot))
