(defsystem :slite
  :serial t
  :depends-on (:str
               :fiveam
               :bordeaux-threads
               :cl-mongo-id
               :trivial-garbage)
  :components ((:file "slite")))

(defsystem :slite/tests
  :serial t
  :depends-on (:slite))
