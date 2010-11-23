(ns kgb.service.controller)

(def tags-score 
	{"spion" 73 "ange" 87 "kapitalist" -74 "kaviar" -114
	"blodpudding" 122 "film" -20 "brain" -78 "fuck" -21 
	"armod" 45 "kreml" 56 "softa" -123 "duelling" 34 "club" 54 
	"menshevik" -132 "bolshevik" 150})

; Tiden det tar f�r ett v�rde att minska med en faktor 1/e (till ca 37%)
(def decay-time 1800)

(defn tags []
	tags-score)



(defn list-words
  "Given a string, return a list of lower-case words with whitespace and
   punctuation removed"
  [s]
  (map #(.toLowerCase %) (filter #(not (.isEmpty %)) (seq (.split #"[\s\W]+" s)))))

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
		{(str (message "from")) msg-score (str (message "to")) (- msg-score)}))

(defn update-score [score message]
	(merge-with + score (message-score message)))
	
(defn calculate-scores [messages]
  (loop [score {} msgs messages]
	(if (nil? msgs)
		score
		(recur (update-score score (first msgs)) (next msgs)))))
		
		
(defn calculate-message-score [messages]
	(calculate-scores messages))