#|
 This file is a part of Terrable
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem terrable
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Terragen TER file format reader"
  :homepage "https://github.com/Shirakumo/terrable"
  :serial T
  :components ((:file "package")
               (:file "terrable")
               (:file "documentation"))
  :depends-on (:fast-io
               :ieee-floats
               :static-vectors
               :documentation-utils))
