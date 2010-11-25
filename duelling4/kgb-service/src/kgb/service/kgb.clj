(ns kgb.service.kgb
  (:require [kgb.service.controller :as controller])
  (:use compojure.core)
  (:use ring.adapter.jetty)
  (:use ring.middleware.json-params)
  (:require [clj-json.core :as json])
  (:import org.codehaus.jackson.JsonParseException)
  (:import clojure.contrib.condition.Condition)
  (:use clojure.contrib.logging)
  )

(defn json-response [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string data)})
   
(defroutes handler
  (GET "/kgb/tags" []
	(debug "GET tags")
    (json-response (controller/tags)))
  (POST "/kgb/score" {params :params}
	(debug (str "POST data: " params))
  	(json-response (controller/calculate-message-score (params "messages")))))

(def error-codes
  {:invalid 400
   :not-found 404})

(defn wrap-error-handling [handler]
  (fn [req]
    (try
      (or (handler req)
          (json-response {"error" "Not found! Try agaian and disapear!"} 404))
      (catch JsonParseException e
        (json-response {"error" "Malformed json! Try agaian and disapear!"} 400))
      (catch Condition e
        (let [{:keys [type message]} (meta e)]
          (json-response {"error" message} (error-codes type)))))))

(def kgb-app
  (-> handler
    wrap-json-params
	;wrap-error-handling
    ))

(defn start-server [port]
	; Starts the jetty server at port port
	(info (str "Starting server at port:" port))
	(run-jetty #'kgb-app {:port port :host "127.0.0.1"}))

(defn start [& [port]]
	(if port 
		(start-server (Integer/parseInt port))
		(start-server 8080)))
