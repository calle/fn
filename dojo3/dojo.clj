(def baseUrl "http://www.systembolaget.se/Sok-dryck/Dryck/?varuNr=")

(defn buildUrl
      ([articleNr] (str baseUrl articleNr)))

(println (buildUrl 17))
; "http://www.systembolaget.se/Sok-dryck/Dryck/?varuNr=11621"


(do (map println *command-line-args*))