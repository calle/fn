(ns dojo3.test.core
  (:use [dojo3.core])
  (:use [clojure.test]))

(require '[clj-http.client :as client])

(def products (list 123 234 212 225))

(def baseUrl "http://www.systembolaget.se/Sok-dryck/Dryck/?varuNr=")

(defn buildUrl
      ([articleNr] (str baseUrl articleNr)))

(println (map buildUrl products))

(do (map println *command-line-args*))

(println "hej")

(println (client/get "http://www.systembolaget.se/"))

(deftest dojo3 
  (is false false))
