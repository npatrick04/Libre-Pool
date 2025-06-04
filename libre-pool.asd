(asdf:defsystem #:libre-pool
  :description "A pool sensor project to pipe data to a grafana on a libre AML-S905X-CC board"
  :author "Nick Patrick <npatrick04@gmail.com>"
  :license "GPL-v3"
  :depends-on (#:asdf #:rate-monotonic #:usocket #:flexi-streams)
  :serial t
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "libre-pool")))))
