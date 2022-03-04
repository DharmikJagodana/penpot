;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.http
  (:require
   [app.common.data :as d]
   [app.common.exceptions :as ex]
   [app.common.logging :as l]
   [app.common.spec :as us]
   [app.config :as cf]
   [app.worker :as wrk]
   [app.http.doc :as doc]
   [app.http.errors :as errors]
   [app.http.middleware :as middleware]
   [app.metrics :as mtx]
   [ring.request :as req]
   [ring.response :as resp]
   [reitit.middleware :as rr]
   [reitit.core :as r]
   [clojure.spec.alpha :as s]
   [integrant.core :as ig]
   [yetti.adapter :as yt]))

(declare wrap-router)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTTP SERVER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::handler fn?)
(s/def ::router some?)
(s/def ::port ::us/integer)
(s/def ::host ::us/string)
(s/def ::name ::us/string)
(s/def ::executors (s/map-of keyword? ::wrk/executor))

;; (s/def ::max-threads ::cf/http-server-max-threads)
;; (s/def ::min-threads ::cf/http-server-min-threads)

(defmethod ig/prep-key ::server
  [_ cfg]
  (merge {:name "http"
          :port 6060
          :host "0.0.0.0"}
         (d/without-nils cfg)))

(defmethod ig/pre-init-spec ::server [_]
  (s/keys :req-un [::port ::host ::name ::executors]
          :opt-un [::mtx/metrics ::router ::handler]))

(defmethod ig/init-key ::server
  [_ {:keys [handler router port name metrics host executors] :as cfg}]
  (l/info :hint "starting http server"
          :port port :host host :name name)

  (let [options {:http/port port
                 :http/host host
                 :ring/async true
                 :xnio/dispatch (:default executors)}
        handler (cond
                  (fn? handler)  handler
                  (some? router) (wrap-router cfg router)
                  :else (ex/raise :type :internal
                                  :code :invalid-argument
                                  :hint "Missing `handler` or `router` option."))
        server  (yt/server handler (d/without-nils options))]
    (assoc cfg :server (yt/start! server))))

(defmethod ig/halt-key! ::server
  [_ {:keys [server name port] :as cfg}]
  (l/info :msg "stoping http server" :name name :port port)
  (yt/stop! server))

(defn- not-found-handler
  [_ respond _]
  (respond {::resp/status 404}))

(defn- ring-handler
  [router]
  (fn [request respond raise]
    (if-let [match (r/match-by-path router (req/path request))]
      (let [params  (:path-params match)
            result  (:result match)
            handler (or (:handler result) not-found-handler)
            request (assoc request :path-params params)]
        (handler request respond raise))
      (not-found-handler request respond raise))))

(defn- wrap-router
  [cfg router]
  (let [handler (ring-handler router)]
    (fn [request respond _]
      (handler request respond
               (fn [cause]
                 (l/error :hint "unexpected error processing request"
                          ::l/context (errors/get-error-context request cause)
                          :query-string (:query-string request)
                          :cause cause)
                 (respond {::resp/status 500
                           ::resp/body "internal server error"}))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTTP ROUTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::rpc map?)
(s/def ::oauth map?)
(s/def ::storage map?)
(s/def ::assets map?)
(s/def ::feedback fn?)
(s/def ::ws fn?)
(s/def ::audit-handler fn?)
(s/def ::debug map?)
(s/def ::awsns-handler fn?)
(s/def ::session map?)

(defmethod ig/pre-init-spec ::router [_]
  (s/keys :req-un [::rpc ::mtx/metrics ::ws ::oauth ::storage ::assets
                   ::session ::feedback ::awsns-handler ::debug ::audit-handler]))

(defmethod ig/init-key ::router
  [_ {:keys [ws session rpc oauth metrics assets feedback debug] :as cfg}]
  (rr/router
   [["" {:middleware [[middleware/wrap-server-timing]
                      [middleware/format-response]
                      [middleware/errors errors/handle]
                      [middleware/restrict-methods]
                      [middleware/cookies]]}
     ["/metrics" {:handler (:handler metrics)}]
     ["/assets" {:middleware [(:middleware session)]}
      ["/by-id/:id" {:handler (:objects-handler assets)}]
      ["/by-file-media-id/:id" {:handler (:file-objects-handler assets)}]
      ["/by-file-media-id/:id/thumbnail" {:handler (:file-thumbnails-handler assets)}]]

     ["/dbg" {:middleware [[middleware/parse-request]
                           (:middleware session)]}
      ["" {:handler (:index debug)}]
      ["/error-by-id/:id" {:handler (:retrieve-error debug)}]
      ["/error/:id" {:handler (:retrieve-error debug)}]
      ["/error" {:handler (:retrieve-error-list debug)}]
      ["/file/data" {:handler (:file-data debug)}]
      ["/file/changes" {:handler (:retrieve-file-changes debug)}]]

     ["/webhooks"
      ["/sns" {:handler (:awsns-handler cfg)
               :allowed-methods #{:post}}]]

     ["/ws/notifications" {:middleware [[middleware/parse-request]
                                        (:middleware session)]
                           :handler ws
                           :allowed-methods #{:get}}]

     ["/api" {:middleware [[middleware/cors]
                           [middleware/parse-request]
                           (:middleware session)]}
      ["/health" {:handler (:health-check debug)}]
      ["/_doc" {:handler (doc/handler rpc)
                :allowed-methods #{:get}}]
      ["/feedback" {:handler feedback
                    :allowed-methods #{:post}}]

      ["/auth/oauth/:provider" {:handler (:handler oauth)
                                :allowed-methods #{:post}}]
      ["/auth/oauth/:provider/callback" {:handler (:callback-handler oauth)
                                         :allowed-methods #{:get}}]

      ["/audit/events" {:handler (:audit-handler cfg)
                        :allowed-methods #{:post}}]

      ["/rpc"
       ["/query/:type" {:handler (:query-handler rpc)}]
       ["/mutation/:type" {:handler (:mutation-handler rpc)
                           :allowed-methods #{:post}}]]]]]))
