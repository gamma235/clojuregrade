(ns clojuregrade.landing
  (:require [compojure.core :refer [defroutes GET PUT POST DELETE ANY]]
            [compojure.handler :refer [site]]
            [compojure.route :as route]
            [clojure.java.io :as io]
            [ring.middleware.stacktrace :as trace]
            [ring.middleware.session :as session]
            [ring.middleware.session.cookie :as cookie]
            [ring.adapter.jetty :as jetty]
            [hiccup.core :refer :all]
            [hiccup.form :refer :all]
            [hiccup.element :refer :all]
            [hiccup.page :refer [html5 include-css]]
            [clojure.edn]))


(defn percentify
  "adjust raw score to percentile"
  [raw percentile]
  (* (/ raw 100) percentile))

(defn percentify-vector
  "maps a vector of percentile adjustments to a vector of grades"
  [weights grades]
  (map percentify grades weights))

(defn augment-vectors
  "augments all the elements in all of the vectors in a grades list into their corresponding weighted values"
  [weights grades]
  (mapv (partial percentify-vector weights) grades))

(defn round
  "As we are using Java data-structures, I am reaching for Java-interop here to wrap simple round function in Clojure."
  [number]
  (.setScale (bigdec number) 0 java.math.RoundingMode/HALF_EVEN))

(defn average
  "averages all the numbers in a sequence."
  [grades]
  (/ (reduce + grades) (count grades)))

(defn class-average [grades]
  (float (average grades)))

(defn bellify
  "augemnts a single grade on a curve function. The size of the curve depends on the coefficient and the distance from the class-average."
  [coef grade grades]
  (+ grade (* coef (- grade (class-average grades)))))

(defn bell-curve
  "This function applies the curve to every grade in a sequence."
  [grades coef]
  (->> grades (map (partial #(bellify coef % grades)))))


(defn process-grades
  "Takes user input from home's form-to function and processes it into the final grades list"
  [weights grades]
(->> grades
     (map (partial percentify-vector weights))
     (mapv #(apply + %))
     (map round)
     (map int)))

(defn home [& [weights grades error]]
  (html5
    [:head
    [:title "Home | Clojuregrade"]
     (include-css "/home.css")]
    [:body
    [:h1 [:img {:src "http://tgoossens.files.wordpress.com/2013/02/icon.png" :height "40px"}] "Clojuregrade"]
    [:p error]
    [:hr]
   (form-to [:post "/"]
    [:h3 "WEIGHTS:    (e.g. quizzes 25%, homework 40%, exams 35%)"
    [:br]
     (text-area {:cols 30 :placeholder "[40 10 50] <- adds up to 100%"} "weights" weights)]
    [:h3 "GRADES:    (e.g. Johnny: quizzes 75%, homework 87%, exams 68% ... )"
    [:br]
    (text-area {:rows 15 :cols 30 :placeholder "[[89 78 63] [76 58 98] ...]


                        (Each grade corresponds to one of the weights above, so order is important. You can copy and paste directly from your excel file but don't forget the brackets!)" } "grades" grades)]
     (submit-button "process"))]))


 (defn processed [weights grades]
  (cond
   (empty? weights)
   (home weights grades "You forgot to add the weights!")
   (empty? grades)
   (home weights grades "You forgot to add the grades!")
  :else
  (do
  (html
   [:h2 "These are your final grades."]
   [:hr]
   [:p
    (apply str (interpose " "
          (process-grades (clojure.edn/read-string weights) (clojure.edn/read-string grades))))
    ]))))

(defroutes app
  (GET "/landing" []
       {:status 200
        :headers {"Content-Type" "text/html"}
        :body (home)})
  (POST "/" [weights grades] (processed weights grades))
  (ANY "*" []
       (route/not-found (slurp (io/resource "404.html")))))

(defn wrap-error-page [handler]
  (fn [req]
    (try (handler req)
         (catch Exception e
           {:status 500
            :headers {"Content-Type" "text/html"}
            :body (slurp (io/resource "500.html"))}))))
