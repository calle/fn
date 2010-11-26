(ns kgb.service.controller)

(def tags-score 
	{"spion" 113 "ange" 123 "kapitalist" -74 "kaviar" -114
	 "blodpudding" 60 "film" -20 "brain" -78 "fuck" -204
	 "armod" 45 "kreml" 56 "softa" -123 "duelling" 34 "club" 54
	 "menshevik" -132 "bolshevik" 150 "fnnl" 108 "pengar" -201
	 "tjäna" -50 "röd" 45 "moskva" 100  "markus" -88 "1337" -30 
	 "aktie" -32 "depå" -21 "vp-konto" -42 "fack" 12 
	 "fackförening" 45 "arbete" 63 "jobb" 63 "arbetare" 34 
	 "förena" 17 "idg" -82 "kaffe" 91  
	 "er" -38 "alla" 18 "biljett" 3 "talande." 37
	 "de" -36 "dueller" -24 "att" 5 "fyra" -21 "finsk" -27 
     "hur" -5 "ett" 9 "ditt" -30 "mindre" 8 "betala" -26
	 "matematiken" 19 "sex" 18 "askan" -7 "nu" 10 "folk" -36
	 "andra." 4 "i" -23 "december." 36 "ska" -38 "bygga" 43
	 "julfesten" 5 "ju" 26 "tomten" 47 "hos" 20 "med" 2 "den" -48
	 "gemensamt" 3 "ondskefulle" 44 "info" 4 "alla." -5 "bara" 28
	 "oss" 7 "jag" -6 "du" -27 "sig" -47 "implementera" 45 "som" -31
	 "paraply." 37 "kommer" 38 "bra" -50 "fredag" -35 "lag" 44 
	 "se" -21 "varit" -28 "mot" -17 "hittar" 11 "det" -26 "vi" -31 
	 "blir" 18 "vill" -2 "och" -6 "svar" 45 "ganska" 42 "till" 15 
	 "inte" -40 "segrare." 35 "kan" 46 "under" 45 
	 "filmklubben" -1 "klang" -25 "netlight" 145 "har" 4 "inblandade" -36 
	 "man" -4 "betraktar" 34 "tycker" -3 "komma" -12 "pre-weekend" 28 
	 "helst." -6 "ensamt" -23 "om" -18 "jubel" 3 "vem" -48 
	 "imorgon" -38 "bli" 27 "separata" -26 "forza" 60 "allez" 45
	 "bildt" -300 "mona" 50 "moona" -50
	})

; Tiden det tar för ett värde att minska med en faktor 1/e (till ca 37%)
(def decay-time 14400)
(def self-reply-punishment -30)

(defn tags []
	tags-score)

(defn list-words
  "Given a string, return a list of lower-case words with whitespace and
   punctuation removed"
  [s]
  (map #(.toLowerCase %) (filter #(not (.isEmpty %)) (seq (.split #"[^åäöÅÄÖa-zA-Z]+" s)))))

(defn decay-factor [time]
	(Math/pow Math/E (/ (- time) decay-time)))

(defn score-by-content [message]
	(loop [score 0 w (list-words (message "text"))]
	  (if (nil? w)
	    score
		(recur (+ score (tags-score (first w) 0)) (next w)))))

(defn message-score [message]
	(let [decay (decay-factor (message "time"))
		  msg-score (* decay (score-by-content message))]
		(if (= (message "from") (message "to"))
			{(str (message "from")) (* decay self-reply-punishment)}
			{(str (message "from")) msg-score (str (message "to")) (- msg-score)})))

(defn update-score [score message]
	(merge-with + score (message-score message)))
	
(defn calculate-scores [messages]
  (loop [score {} msgs messages]
	(if (nil? msgs)
		score
		(recur (update-score score (first msgs)) (next msgs)))))
		
(defn calculate-message-score [messages]
	(if (empty? messages)
	{}
	(calculate-scores messages)))
