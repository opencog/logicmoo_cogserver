(use-modules (opencog) 
  (opencog python) (opencog exec) (opencog persist) 
  (opencog cogserver) (opencog persist-cog))
(use-modules (ice-9 threads))

; Logicmoo Public IP
; (cogserver-open "cog://metaverse.1ogicmoo.org/")
(cogserver-open "cog://10.0.0.233:17001/")
; (start-cogserver)


; pull from 1ogicmoo remote server
(load-atomspace)

(Concept "bar" (stv 0.1111 0.87777))
(List (Concept "A") (Concept "B"))
(Set (Concept "A") (Concept "B"))
(Set (Concept "A") (Concept "B") (Concept "C"))
(Evaluation (Predicate "foo")
	(List (Concept "B") (Concept "C") (Concept "oh boy!")))

; Push atomspace out to the remote 1ogicmoo server.
(store-atomspace)


;;(call-with-new-thread (lambda () 
(python-eval "
from opencog.web.api.apimain import RESTAPI
from opencog.atomspace import AtomSpace, types
from opencog.utilities import initialize_opencog
from opencog.type_constructors import *
from opencog.scheme_wrapper import scheme_eval_as

atomspace = scheme_eval_as('(cog-atomspace)')
initialize_opencog(atomspace)

# Endpoint configuration
# To allow public access, set to 0.0.0.0; for local access, set to 127.0.0.1
IP_ADDRESS = '0.0.0.0'
PORT = 5000
api = RESTAPI(atomspace)
api.run(host=IP_ADDRESS, port=PORT)

")
;;))

; NOTE: If you want to get your guile shell then modify the above as follows
; (use-modules (ice-9 threads))
; (use-modules (opencog) (opencog python))
;
; (call-with-new-thread (lambda ()(python-eval "same as above")))


