;#!/usr/local/bin/guile -l 

(use-modules (opencog) 
  (opencog python) (opencog exec) (opencog persist) 
  (opencog cogserver) (opencog persist-cog))
(use-modules (ice-9 threads))
(use-modules (opencog persist-cog-simple))

;(start-cogserver "cogserver-slave.conf")

; Talk to the two machines in two different ways.
; The CogSimpleStorageNode uses the simple, single-threaded
; serialized communications channel; the CogStorageNode uses
; the parallelized, asynchronous channel.

;(define master (CogStorageNode "cog://localhost:17001"))
;(define slave (CogStorageNode "cog://localhost:17101"))

; Open the channel to each; by default, storage nodes are created
; with closed channels. This allows storage nodes to be created,
; when the remote end is not actually available, yet.
; (cog-open master)
;(cog-open slave)

; Logicmoo Public IP
;(cogserver-open "cog://metaverse.1ogicmoo.org/")
;(cogserver-open "cog://10.0.0.233:17001/")
;(cogserver-open "cog://10.0.0.233:17101/")

;(Concept "bar" (stv 0.1111 0.87777))
;(List (Concept "A") (Concept "B"))
;(Set (Concept "A") (Concept "B"))
;(Set (Concept "A") (Concept "B") (Concept "C"))
;(Evaluation (Predicate "foo") (List (Concept "B") (Concept "C") (Concept "oh boy!")))
;(Concept "b" (stv 0.9 0.2))


; Push atomspace out to the remote 1ogicmoo server.
;(store-atomspace slave)
;(store-atomspace master)

; pull from logicmoo remote server
;(load-atomspace master)

(load "exam.scm")

;(call-with-new-thread (lambda () 
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
;))


