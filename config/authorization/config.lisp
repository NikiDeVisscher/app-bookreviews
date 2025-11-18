;;;;;;;;;;;;;;;;;;;
;;; delta messenger
(in-package :delta-messenger)

;; (push (make-instance 'delta-logging-handler) *delta-handlers*) ;; enable if delta messages should be logged on terminal
(add-delta-messenger "http://delta-notifier/")
(setf *log-delta-messenger-message-bus-processing* nil) ;; set to t for extra messages for debugging delta messenger

;;;;;;;;;;;;;;;;;
;;; configuration
(in-package :client)
(setf *log-sparql-query-roundtrip* t) ; change nil to t for logging requests to virtuoso (and the response)
(setf *backend* "http://triplestore:8890/sparql")

(in-package :server)
(setf *log-incoming-requests-p* t) ; change nil to t for logging all incoming requests

;;;;;;;;;;;;;;;;
;;; prefix types
(in-package :type-cache)

(add-type-for-prefix "http://mu.semte.ch/sessions/" "http://mu.semte.ch/vocabularies/session/Session") ; each session URI will be handled for updates as if it had this mussession:Session type

;;;;;;;;;;;;;;;;;
;;; access rights

(in-package :acl)

;; these three reset the configuration, they are likely not necessary
(defparameter *access-specifications* nil)
(defparameter *graphs* nil)
(defparameter *rights* nil)

;; Prefixes used in the constraints below (not in the SPARQL queries)
(define-prefixes
  ;; Core
  :mu "http://mu.semte.ch/vocabularies/core/"
  :session "http://mu.semte.ch/vocabularies/session/"
  :ext "http://mu.semte.ch/vocabularies/ext/"
  ;; Custom prefix URIs here, prefix casing is ignored
  :bf "http://id.loc.gov/ontologies/bibframe/"
  :schema "http://schema.org/"
  :account "http://mu.semte.ch/services/registration-service/accounts/"
  :foaf "http://xmlns.com/foaf/0.1/"
  )


;;;;;;;;;
;; Graphs
;;
;; These are the graph specifications known in the system.  No
;; guarantees are given as to what content is readable from a graph.  If
;; two graphs are nearly identitacl and have the same name, perhaps the
;; specifications can be folded too.  This could help when building
;; indexes.

(define-graph public ("http://mu.semte.ch/graphs/public")
  ("bf:Work" -> _)
  ("schema:Person" -> _)
  ("schema:author" -> _)
  ("schema:Review" -> _)
  ("foaf:Person" -> _)
  ("foaf:OnlineAccount" -> _)
  ("account:Account" -> _)
  ("session:account" -> _)
  ("session:Session" -> _)
  ("schema:roleName" -> _))

(define-graph sessions ("http://mu.semte.ch/graphs/sessions/")
  ("session:Session" -> _))

;; Example:
;; (define-graph company ("http://mu.semte.ch/graphs/companies/")
;;   ("foaf:OnlineAccount"
;;    -> "foaf:accountName"
;;    -> "foaf:accountServiceHomepage")
;;   ("foaf:Group"
;;    -> "foaf:name"
;;    -> "foaf:member"))


;;;;;;;;;;;;;
;; User roles

(supply-allowed-group "public")

(supply-allowed-group "reader"
  :query "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
          PREFIX schema: <http://schema.org/>
          SELECT ?account WHERE {
            <SESSION_ID> session:account ?account .
            ?account schema:roleName \"reader\" .
          } LIMIT 1"
  :parameters ("account"))

(supply-allowed-group "admin"
  :query "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
          PREFIX schema: <http://schema.org/>
          SELECT DISTINCT ?account WHERE {
            <SESSION_ID> session:account ?account .
            ?account schema:roleName \"admin\" .
          }"
  :parameters ("account"))

(grant (read)
       :to-graph public
       :for-allowed-group "public")

(grant (read write)
       :to-graph (public sessions)
       :for-allowed-group "reader")

(grant (read write)
       :to-graph public
       :for-allowed-group "admin")

;; example:

;; (supply-allowed-group "company"
;;   :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
;;           SELECT DISTINCT ?uuid WHERE {
;;             <SESSION_ID ext:belongsToCompany/mu:uuid ?uuid
;;           }"
;;   :parameters ("uuid"))

;; (grant (read write)
;;        :to company
;;        :for "company")
