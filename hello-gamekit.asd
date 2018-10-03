(cl:pushnew :bodge-gl2 cl:*features*)
(asdf:defsystem :hello-gamekit
  :description "trivial-gamekit example usage for Getting Started guide"
  :author "Pavel Korolev <dev@borodust.org>"
  :version "1.0.0"
  :depends-on (trivial-gamekit)
  :components ((:file "hello-gamekit")))
