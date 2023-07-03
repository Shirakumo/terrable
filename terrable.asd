(asdf:defsystem terrable
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Terragen TER file format reader"
  :homepage "https://github.com/Shirakumo/terrable"
  :serial T
  :components ((:file "package")
               (:file "terrable")
               (:file "documentation"))
  :depends-on (:fast-io
               :ieee-floats
               :static-vectors
               :trivial-garbage
               :documentation-utils))
