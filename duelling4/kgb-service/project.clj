(defproject kgb.service "1.0.0-SNAPSHOT"
  :description "The open source KGB service."
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [ring/ring-jetty-adapter "0.2.5"]
    		     [ring-json-params "0.1.0"]
			     [compojure "0.4.0"]
			     [clj-json "0.2.0"]]
  :dev-dependencies
  				  [[lein-run "1.0.0-SNAPSHOT"]]
  :run-aliases {:start-server [kgb.service.kgb start]})
