(asdf:defsystem :piddling-plugins
  :name "Piddling Plugins"
  :description "Lilliputian plugin framework"
  :version "1"
  :author "Andy Hefner <ahefner@gmail.com>"
  :license "MIT-style license"
  :depends-on (:alexandria)
  :serial t
  :components ((:file "plugins")))
