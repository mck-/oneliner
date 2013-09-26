;;;; cl-oneliner.asd

(asdf:defsystem #:cl-oneliner
  :serial t
  :description "Given a piece of text, summarize it with a one-liner"
  :author "mck-"
  :license "wtfpl"
  :version "0.1.0"
  :depends-on (#:lisp-unit #:split-sequence #:cl-ppcre)
  :components ((:file "package")
               (:file "utils")
               (:file "cl-oneliner")
               (:module :test
                        :components
                        ((:file "utils")))))
