(ns rest-api.core
  (:require [cheshire.core :refer [generate-string parse-string]]
            [org.httpkit.server :as server]
            [compojure.route :as route]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [compojure.core :refer [defroutes GET POST PUT DELETE]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]])
    (:gen-class))

(def users-collection (atom []))
(def id-counter (atom 0))

(defn adduser [firstname surname city]
  (swap! id-counter inc)
  (let [user {:id @id-counter
              :firstname (str/capitalize firstname)
              :surname (str/capitalize surname)
              :city city}]
    (swap! users-collection conj user)
    user))

; Return List of Users
(defn user-handler [req]
        {:status  200
         :headers {"Content-Type" "text/json"}
         :body    (str (json/write-str @users-collection))})

; Add a new person into the users-collection
(defn adduser-handler [req]
  (let [body (slurp (:body req))
        body-params (parse-string body true)]
        
    (println "body" body)
    (println "body-params" body-params)
        {:status  200
         :headers {"Content-Type" "text/json"}
         :body    (-> (let [firstname (body-params :firstname)
                            surname (body-params :surname)
                            city (body-params :city)]
                        (adduser firstname surname city)
                        (str (json/write-str @users-collection))))}))

(defn add-handler [req]
  (let [body (slurp (:body req))
        body-params (parse-string body true)]
        
    (println "body" body)
    (println "body-params" body-params)
        {:status  200
         :headers {"Content-Type" "text/json"}
         :body    (-> (let [firstname (body-params :x)
                            surname (body-params :y)]
                     
                        ;(adduser firstname surname city)
                        (str (json/write-str {:exp (+ firstname surname)}))))}))

; Updates the city of the given user id
(defn update-city [req]
  (let [id (-> req :params :id)
        city (-> req :params :city)]
    (swap! users-collection
           (fn [users]
             (mapv (fn [user]
                     (if (= (:id user) (Integer. id))
                       (assoc user :city city)
                       user))
                   users)))
    {:status 200
     :headers {"Content-Type" "text/json"}
     :body (json/write-str @users-collection)}))

; Updates the city of the given user id
(defn add-n [req]
  (let [id (-> req :params :x)
        city (-> req :params :y)]
    {:status 200
     :headers {"Content-Type" "text/json"}
     :body (json/write-str {:exp (+ (Integer. id) (Integer. city))})}))

; Deleted the user of the given id
(defn delete-user [req]
  (let [id (-> req :params :id)]
    (swap! users-collection
           (fn [users]
             (->> users
                  (filter #(not= (:id %) (Integer. id)))
                  vec)))
    {:status 200
     :headers {"Content-Type" "text/json"}
     :body (json/write-str @users-collection)}))

(defroutes app-routes
  (GET "/users" [] user-handler)
  (GET "/users/addn" [] add-n)
  (POST "/users/add" [] adduser-handler)
  (POST "/users/addxy" [] add-handler)
  (PUT "/user/update" [] update-city)
  (DELETE "/user/delete" [] delete-user))

(defn -main
  [& args]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "4000"))]
    (server/run-server
     (-> app-routes
         (wrap-defaults (assoc-in site-defaults [:security :anti-forgery] false)))
     {:port port})
    (println (str "Webserver started at http:/127.0.0.1:" port "/"))))
