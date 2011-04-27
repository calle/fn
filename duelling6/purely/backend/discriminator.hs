import Database.HDBC.PostgreSQL
import Database.HDBC
import List
import Random
import Control.Monad
import System.Random
import System.IO
import System (getArgs)
import Data.Map (Map)
import qualified Data.Map as Map

-- some helper methods
-- get a list of strings and join it, using the second argument as the delimiter
implode :: [String] -> String -> String
implode [] t = ""
implode (x:[]) t = x
implode (x:xl) t = x ++ t ++ (implode xl t)

-- get the max(cnt, length(l)) head elements of a list l
headn :: [a] -> Int -> [a]
headn [] cnt = []
headn xl 0 = []
headn (x:xl) cnt = x : (headn xl (cnt-1)) 

-- shuffle a list (code from http://aoeu.snth.net/?p=119)
shuffle xs g = fst (mix xs (randomRs (True, False) g))
    where mix [ ] r0 = ([ ], r0)
          mix [x] r0 = ([x], r0)
          mix  xs r0 = let (ys, zs, r1) = cut xs r0 [] []
                           (cs,     r2) = mix ys r1
                           (ds,     r3) = mix zs r2
                       in  (cs++ds, r3)

          cut    []     rs  ys zs = (ys, zs, rs)
          cut (x:xs) (r:rs) ys zs = if r then cut xs rs (x:ys) zs
                                         else cut xs rs ys (x:zs)

-- apply a function to all elements of a list, and return the element
-- that gets the highest function result
getBest :: Ord b => (a -> b) -> [a] -> a
getBest f (x:xl) = getBest' f (f x) x xl 
                   where 
                     getBest' f best cand [] = cand
                     getBest' f best cand (x:xl) = 
                       let cv = (f x) in 
                         if cv > best 
                         then (getBest' f cv x xl) 
                         else (getBest' f best cand xl) 

-- the second argument is rotated: each element is removed once, and,
-- together with the list of remaining arguments, presented to the
-- function. The result values are then put into the result list
maprotate :: (a -> [a] -> b) -> [a] -> [b] 
maprotate _ [] = []
maprotate f l = mapr' f l []
                where
                  mapr' _ [] rl = []
                  mapr' f (x:xl) rl = (f x (concat [rl,xl])) : (mapr' f xl (concat [rl, [x]]))

-- Parse the initial arguments
-- some code taken from http://gimbo.org.uk/blog/2007/04/20/splitting-a-string-in-haskell/
pairify :: String -> String -> (String, String)
pairify t s = case find (t `isSuffixOf`) (inits s) of
                      Nothing -> (s, "")
                      Just p -> (take ((length p) - (length t)) p,
                                      drop (length p) s) 

split' :: String -> String -> [String]
split' tok splitme = unfoldr (sp1 tok) splitme
    where sp1 _ "" = Nothing
          sp1 t s = Just (pairify t s)
                                

parseArgs :: String -> [(String, String)]
parseArgs args = map (pairify "=") (split' "," args)

-- database connection string (needs to be a PostreSQL instance)
connectstr = "host=localhost dbname=netlighters user=nl password=nl"
        
-- datatypes used to capture the database contents
data Person = Person Int String String

instance Show (Person) where
     show (Person id name login) = "{\"name\": \"" ++ name ++ "\", \"login\": \"" ++ login ++ "\"}"

data Question = Question Int String String

instance Show (Question) where
     show (Question id name content) = "{\"name\": \"" ++ name ++ "\", \"fulltext\": \"" ++ content ++ "\"}"

type Answermap = Map String (Map String String)

-- Build the SQL that gets the remaining candidates
buildCandidateSQL :: [(String, String)] -> String
buildCandidateSQL l = implode (map ( 
                                  \ (q,a) -> "INNER JOIN Answers a_" ++ q 
                                             ++ " ON a_" ++ q ++ ".netlighter_id = p.id AND a_" ++ q ++ ".content = '" ++ a 
                                             ++ "' INNER JOIN Questions q_" ++ q ++ " ON a_" ++ q ++ ".question_id = q_" ++ q 
                                             ++ ".id AND q_" ++ q ++ ".name = '" ++ q ++ "'") l) " "

buildCandidateQuery :: [(String,String)] -> String
buildCandidateQuery arg = "SELECT p.id, p.name, p.login FROM Netlighters p " ++ buildCandidateSQL(arg)

buildRemainingQuestionsQuery :: [(String, String)] -> String
buildRemainingQuestionsQuery [] = "SELECT id, name, content FROM Questions"
buildRemainingQuestionsQuery l = "SELECT id, name, content FROM Questions WHERE name not in (" 
                                 ++ (implode (map ( \ (q,a) -> "'" ++ q ++ "'") l) ",") ++ ")"                                 

buildCandidatesAnswersMap :: [Person] -> [Question] -> String
buildCandidatesAnswersMap [] quest = "SELECT p.name, q.name, a.content FROM Netlighters p "
                                       ++ "INNER JOIN Answers a ON a.netlighter_id = p.id INNER JOIN Questions q " 
                                       ++ " ON a.question_id = q.id WHERE 1=2" -- what a hack
buildCandidatesAnswersMap cand quest = "SELECT p.name, q.name, a.content FROM Netlighters p "
                                       ++ "INNER JOIN Answers a ON a.netlighter_id = p.id INNER JOIN Questions q " 
                                       ++ " ON a.question_id = q.id WHERE p.id IN (" 
                                       ++ (implode (map ( \ (Person id _ _) -> (show id)) cand) ",")
                                       ++ ") AND q.id IN ("
                                       ++ (implode (map ( \ (Question id _ _) -> (show id)) quest) ",")
                                       ++ ") ORDER BY p.id, q.id"

-- create a "map-of-maps" for the candidate-question-answer query. It
-- contains person names as key, and a map as value; this map has
-- question names as the key, and the answer as the value. For
-- simplification, this is just a single value, but it would be easy
-- to extend it to lists.
createMap :: [(String, String, String)] -> Map String (Map String String)
createMap [] = Map.empty
createMap ((person, question, answer):l) = let recmap = (createMap l) in
                                               case (Map.lookup person recmap) of                                                
                                                 Just x -> Map.insert person (Map.insert question answer x) recmap
                                                 Nothing -> Map.insert person (Map.singleton question answer) recmap

-- "main" method, which uses monads to get the database
-- connection. Uses a consumer method to do the actual (functional)
-- calculation of the results
getCandidates :: String -> (StdGen -> [Person] -> [Question] -> [(String, String)] -> Answermap -> String) -> IO() 
getCandidates arg consumer = 
  do
    -- init random generator (code from http://hackage.haskell.org/trac/ghc/attachment/ticket/3575/random-seed.hs)
    -- f <- openBinaryFile "/dev/urandom" ReadMode
    -- xs <- replicateM 4 $ hGetChar f
    let seed = 5 -- sum $ zipWith (*) (iterate (* 256) 1) (map fromEnum xs)
  
    -- parse the arguments
    let args = (parseArgs arg)
        
    -- connect to database    
    conn <- (connectPostgreSQL connectstr)
    
    -- get the remaining candidates
    candr <- quickQuery' conn (buildCandidateQuery args) []
    let cands = map convPersonRow candr
    -- get the open questions
    remq <- quickQuery' conn (buildRemainingQuestionsQuery args) []
    let quests = map convQuestionRow remq
    -- get the map of person - question - values
    queper <- quickQuery' conn (buildCandidatesAnswersMap cands quests) []
    let answermap = (createMap (map convAnswerRow queper))
    -- call the consumer, which processes the results
    let result = consumer (mkStdGen seed) cands quests args answermap
    -- output the result, and disconnect
    putStr result
    disconnect conn
  where convPersonRow :: [SqlValue] -> Person
        desc x = case fromSql x of
                             Just y -> y
                             Nothing -> "NULL"
                             
        convPersonRow [id, name, login] = (Person (fromSql id) (desc name) (desc login))

        convQuestionRow :: [SqlValue] -> Question
        convQuestionRow [id, name, content] = (Question (fromSql id) (desc name) (desc content))

        convAnswerRow :: [SqlValue] -> (String, String, String)
        convAnswerRow [person, question, content] = ((desc person), (desc question), (desc content))

-- functions for actually finding a question
findQuestionNaive :: [Question] -> [Person] -> Answermap -> Question
findQuestionNaive l cand map = (head l)

--- this function considers all questions, and possible answers (of
--- course, only those answers that have already been given). It will
--- then try to find the question that discriminates the set of
--- candidates best, i.e., that - regardless of the answer given -
--- will (recursively) lead to the smallest group given. Group size is
--- hence the evaluation function in a minimax algorithm.
lookupAnswer :: Answermap -> Person -> String -> Maybe String
lookupAnswer amap (Person id name login) question = case Map.lookup name amap of
                                                           Just x -> Map.lookup question x 
                                                           Nothing -> Nothing

containedIn :: Eq a => [a] -> a -> Bool
containedIn [] e = False
containedIn (e1:l) e2 = if (e1 == e2) then True else (containedIn l e2)

getAnswers :: Answermap -> [Person] -> Question -> [String]
getAnswers amap [] question = []
getAnswers amap (p:l) (Question id name text) = case (lookupAnswer amap p name) of
                                                           Just x -> let res = (getAnswers amap l (Question id name text))
                                                                               in if (containedIn res x) then res else (x:res)
                                                           Nothing -> (getAnswers amap l (Question id name text))  


pruneCandidateSetForQuestion :: [Person] -> Answermap -> Question -> [Person]
pruneCandidateSetForQuestion [] _ _ = []
pruneCandidateSetForQuestion (p:pl) map (Question id qname text) = case (lookupAnswer map p qname) of
			     Just x -> (p : ((pruneCandidateSetForQuestion pl map (Question id qname text))))
			     Nothing -> ((pruneCandidateSetForQuestion pl map (Question id qname text)))

pruneCandidateSetForAnswer :: [Person] -> Answermap -> Question -> String -> [Person]
pruneCandidateSetForAnswer [] _ _ _ = []
pruneCandidateSetForAnswer (p:pl) map (Question id qname text) answer = case (lookupAnswer map p qname) of
			   	      		   	       	      	Just x -> if x == answer then (p : (pruneCandidateSetForAnswer pl map (Question id qname text) answer)) 
									          else (pruneCandidateSetForAnswer pl map (Question id qname text) answer)

executeQuestion :: Int -> [Person] -> Answermap -> Question -> [Question] -> Int
executeQuestion depth cands map q ql = findMinAnswer depth (pruneCandidateSetForQuestion cands map q) ql map q

findMaxQuestion :: Int -> [Person] -> [Question] -> Answermap -> Int
findMaxQuestion depth [] _ _ = -1000
findMaxQuestion depth [p] _ _ = 10000
findMaxQuestion 0 pl _ _  = 100 - (length pl)
findMaxQuestion depth pl [] _ = 200 - (length pl)
findMaxQuestion depth p q map = getBest ( \v -> v ) (maprotate (executeQuestion (depth - 1) p map) q) -- find the best question

executeAnswer :: Int -> String -> [Person] -> [Question] -> Question -> Answermap -> Int
executeAnswer depth answer cands ql q map = findMaxQuestion depth (pruneCandidateSetForAnswer cands map q answer) ql map 

findMinAnswer :: Int -> [Person] -> [Question] -> Answermap -> Question -> Int 
findMinAnswer depth [] _ _ _ = -1000
findMinAnswer depth [p] _ _ _ = 10000
findMinAnswer 0 pl _ _ _ = 100 - (length pl)
findMinAnswer depth pl [] _ _ = 200 - (length pl)
findMinAnswer depth cands ql amap q = getBest ( \v -> 0-v) (map (\a -> executeAnswer (depth - 1) a cands ql q amap)  (getAnswers amap cands q)) -- find the worst answer

findQuestionMinMax :: [Question] -> [Person] -> Answermap -> Question
findQuestionMinMax quest cand map = let (q, v) = (getBest ( \ (q, val) -> val ) (maprotate (\q -> \ql -> (q, executeQuestion 3 cand map q ql)) quest)) in q

-- function acting as a toString method for a person
resultGenerator :: ([Question] -> [Person] -> Answermap -> Question) 
                   -> ([Person] -> [Person]) 
                   -> StdGen -> [Person] -> [Question] 
                   -> [(String, String)] -> Answermap 
                   -> String
resultGenerator qc pc rand [] questions args am = "{\"result\":\"NO PERSON FOUND\"}\n" 
resultGenerator qc pc rand (p:[]) questions args am = "{\"result\":\"PERSON FOUND\", \"person\" : " ++ (show p) ++ "\n"
resultGenerator qc pc rand _ [] args am = "{\"result\":\"NO QUESTION LEFT\"}\n"
resultGenerator qc pc rand cand questions args am = "{ \"result\": \"QUESTION\", \"question\": " ++ (show (qc (shuffle questions rand) cand am))
                                      ++ "," 
                                      ++ "\"candidate_count\": " ++ (show (length cand))
                                      ++ "," 
                                      ++ "\"candidates\":[ " ++ (implode (map ( \ p -> (show p)) (headn (shuffle cand rand) 5)) ",")
                                      ++ "]}\n" 

-- startup code
runDiscriminator args = getCandidates args (resultGenerator findQuestionMinMax (\ pl -> (headn pl 5)))

-- main method assumes one argument
main = do
  args <- getArgs
  let arg = head args
  runDiscriminator arg
  