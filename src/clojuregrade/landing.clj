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
    [:script {:src "//code.jquery.com/jquery-1.10.2.min.js"}]
    [:script {:src "http://s3.amazonaws.com/codecademy-content/courses/hour-of-code/js/alphabet.js"}]
    [:canvas {:id "myCanvas"}]
    [:script {:src "http://s3.amazonaws.com/codecademy-content/courses/hour-of-code/js/bubbles.js"}]
    [:script {:src "main.js"}]
    [:p error]
    [:div
    [:br]
   (form-to [:post "/"]
    [:h3 "WEIGHTS:    (e.g. quiz 25%, homework 40%, exam 35%)"
    [:br]

     (text-area {:cols 30 :placeholder "[25 40 35] <- adds up to 100%"} "weights" weights)]
    [:br]

    [:h3"GRADES:    (e.g. Johnny: quiz 75%, homework 87%, exam 68% ... )"
    [:br]
    (text-area {:rows 15 :cols 30 :placeholder "[[89 78 63] [76 58 98] ...]

                        (Each grade is out of 100% and corresponds to one of the weights above, so order is important. You can resize this window, copy and paste directly from your excel file but don't forget the brackets!)" } "grades" grades)]
     (submit-button "process"))]
     [:footer [:a {:href "https://github.com/gamma235/clojuregrade"} "source-code"]]]))


(defn processed [weights grades]
  (cond
   (empty? weights)
   (home weights grades "Did you enter data into each field?")
   (empty? grades)
   (home weights grades "Did you enter data into each field?")
   (false? (= 100 (reduce + (clojure.edn/read-string weights))))
   (home weights grades "Your weights don't add up to 100%.")
   :else
   (do
     (html
      [:head
      [:title "Results | Clojuregrade"]]
      [:h2 "These are your final grades."]
      [:hr]
      [:p
       (interpose [:br]
                  (process-grades (clojure.edn/read-string weights) (clojure.edn/read-string grades)))
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
