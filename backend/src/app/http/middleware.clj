;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.http.middleware
  (:require
   [app.common.logging :as l]
   [app.common.transit :as t]
   [app.config :as cf]
   [app.util.json :as json]
   [cuerdas.core :as str]
   [ring.request :as req]
   [ring.response :as resp]
   [yetti.adapter :as yt]
   [yetti.util :as yu])
  (:import java.io.OutputStream))

(defn wrap-server-timing
  [handler]
  (letfn [(get-age [start]
            (float (/ (- (System/nanoTime) start) 1000000000)))

          (update-headers [headers start]
            (assoc headers "Server-Timing" (str "total;dur=" (get-age start))))]

    (fn [request respond raise]
      (let [start (System/nanoTime)]
        (handler request #(respond (update % ::resp/headers update-headers start)) raise)))))

(def server-timing
  {:name ::server-timing
   :compile (constantly wrap-server-timing)})

(defn wrap-parse-request
  [handler]
  (letfn [(parse-transit [request]
            (with-open [is (-> request req/body req/body-stream)]
              (t/read! (t/reader is))))

          (parse-json [request]
            (with-open [is (-> request req/body req/body-stream)]
              (json/read is)))

          (parse-form [request]
            (yu/parse-form-data request))

          ;; TODO: multipart parsing

          (process-request [request]
            (let [query-params (yu/parse-query-data request)
                  request      (assoc request :query-params query-params)]
              (case (req/get-header request "content-type")
                "application/transit+json"
                (let [params (parse-transit request)]
                  (assoc request :body-params params))

                "application/json"
                (let [params (parse-json request)]
                  (assoc request :body-params params))

                "application/x-www-form-urlencoded"
                (let [params (parse-form request)]
                  (assoc request :body-params params))

                request)))

          (merge-params [request]
            (assoc request :params
                   (merge (:path-params request)
                          (:query-params request)
                          (:body-params request))))

          (handle-exception [cause]
            (prn cause)
            (let [data {:type :validation
                        :code :unable-to-parse-request-body
                        :hint "malformed params"}]
              (l/error :hint (ex-message cause) :cause cause)
              {::resp/status 400
               ::resp/headers {"content-type" "application/transit+json"}
               ::resp/body (t/encode-str data {:type :json-verbose})}))]

    (fn [request respond raise]
      (try
        (let [request (-> request process-request merge-params)]
          (handler request respond raise))
        (catch Exception cause
          (respond (handle-exception cause)))))))

(def parse-request
  {:name ::parse-request
   :compile (constantly wrap-parse-request)})

(defn buffered-output-stream
  "Returns a buffered output stream that ignores flush calls. This is
  needed because transit-java calls flush very aggresivelly on each
  object write."
  [^java.io.OutputStream os ^long chunk-size]
  (proxy [java.io.BufferedOutputStream] [os (int chunk-size)]
    ;; Explicitly do not forward flush
    (flush [])
    (close []
      (proxy-super flush)
      (proxy-super close))))

(def ^:const buffer-size (:http/output-buffer-size yt/base-defaults))

(defn wrap-format-response
  [handler]
  (letfn [(transit-streamable-body [data opts]
            (reify resp/StreamableResponseBody
              (-write-body-to-stream [_ _ output-stream]
                (try
                  #_(with-open [bos (buffered-output-stream output-stream buffer-size)]
                    (let [tw (t/writer bos opts)]
                      (t/write! tw data)))
                  (t/write! (t/writer output-stream opts) data)


                  ;; (catch java.io.IOException _cause
                  ;;   ;; Do nothing, EOF means client closes connection abruptly
                  ;;   nil)
                  (catch Throwable cause
                    (l/warn :hint "unexpected error on encoding response"
                            :cause cause))
                  (finally
                    (.close ^OutputStream output-stream))))))

          (format-response [response request]
            (let [body (::resp/body response)]
              (cond
                (coll? body)
                (let [qs   (req/query request)
                      opts {:type (if (str/includes? qs "verbose") :json-verbose :json)}]
                  (-> response
                      (update ::resp/headers assoc "content-type" "application/transit+json")
                      (assoc ::resp/body (transit-streamable-body body opts))))

                (nil? body)
                (assoc response ::resp/status 204 ::resp/body "")

                :else
                response)))

          (process-response [response request]
            (cond-> response
              (map? response) (format-response request)))]

    (fn [request respond raise]
      (handler request
               (fn [response]
                 (let [response (process-response response request)]
                   (respond response)))
               raise))))

(def format-response
  {:name ::format-response
   :compile (constantly wrap-format-response)})

(defn wrap-errors
  [handler on-error]
  (fn [request respond _]
    (handler request respond (fn [cause]
                               (-> cause (on-error request) respond)))))

(def errors
  {:name ::errors
   :compile (constantly wrap-errors)})

(defn wrap-cookies
  [handler]
  (fn [request respond raise]
    (try
      (let [cookies (yu/parse-cookies request)
            request (assoc request :cookies cookies)]
        (handler request
                 (fn [response]
                   (some->> (::resp/cookies response) (yu/set-cookies! @request))
                   (respond response))
                 raise))
      (catch Throwable cause
        (raise cause)))))

(def cookies
  {:name ::cookies
   :compile (constantly wrap-cookies)})

(defn wrap-cors
  [handler]
  (if-not (contains? cf/flags :cors)
    handler
    (letfn [(add-headers [headers request]
              (let [origin (req/get-header request "origin")]
                (-> headers
                    (assoc "access-control-allow-origin" origin)
                    (assoc "access-control-allow-methods" "GET,POST,DELETE,OPTIONS,PUT,HEAD,PATCH")
                    (assoc "access-control-allow-credentials" "true")
                    (assoc "access-control-expose-headers" "x-requested-with, content-type, cookie")
                    (assoc "access-control-allow-headers" "x-frontend-version, content-type, accept, x-requested-width"))))

            (update-response [response request]
              (update response ::resp/headers add-headers request))]

      (fn [request respond raise]
        (if (= (req/method request) :options)
          (-> {::resp/status 200 ::resp/body ""}
              (update-response request)
              (respond))
          (handler request
                   (fn [response]
                     (respond (update-response response request)))
                   raise))))))

(def cors
  {:name ::cors
   :compile (constantly wrap-cors)})

(defn compile-restrict-methods
  [data opts]
  (when-let [allowed (:allowed-methods data)]
    (fn [handler]
      (fn [request respond raise]
        (let [method (req/method request)]
          (if (contains? allowed method)
            (handler request respond raise)
            (respond {::resp/status 405})))))))

(def restrict-methods
  {:name ::restrict-methods
   :compile compile-restrict-methods})
