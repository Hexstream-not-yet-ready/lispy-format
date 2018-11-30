(asdf:defsystem #:lispy-format

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "To be described."

  :version "0.1"
  :serial cl:t
  :components ((:file "package")
	       (:file "expand")
               (:file "extra")
               (:file "standard")))
