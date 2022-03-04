;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.workspace.sidebar.options.menus.exports
  (:require
   [app.common.data :as d]
   [app.main.data.messages :as dm]
   [app.main.data.modal :as modal]
   [app.main.data.workspace.changes :as dch]
   [app.main.data.workspace.persistence :as dwp]
   [app.main.data.workspace.state-helpers :as wsh]
   [app.main.refs :as refs]
   [app.main.repo :as rp]
   [app.main.store :as st]
   [app.main.ui.icons :as i]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer  [tr, c]]
   [beicon.core :as rx]
   [rumext.alpha :as mf]))


(def exports-attrs [:exports])

(defn request-export
  [object-id page-id file-id name exports]
  ;; Force a persist before exporting otherwise the exported shape could be outdated
  (st/emit! ::dwp/force-persist)
  (rp/query!
   :export
   {:page-id page-id
    :file-id  file-id
    :object-id object-id
    :name name
    :exports exports}))

(defn use-download-export
  [shapes filename page-id file-id exports]
  (let [loading? (mf/use-state false)

        on-download-callback
        (mf/use-callback
         (mf/deps filename shapes page-id file-id exports)
         (if (= (count shapes) 1)
           (fn [event]
             (dom/prevent-default event)
             (swap! loading? not)
             (->> (request-export (:id (first shapes)) page-id file-id filename exports)
                  (rx/subs
                   (fn [body]
                     (dom/trigger-download filename body))
                   (fn [_error]
                     (swap! loading? not)
                     (st/emit! (dm/error (tr "errors.unexpected-error"))))
                   (fn []
                     (swap! loading? not)))))

           (fn [event]
             (dom/prevent-default event)
             (st/emit!
              (modal/show
               {:type :export-shapes
                :shapes shapes})))))]

    [on-download-callback @loading?]))

(defn use-download-export-by-id
  [ids filename page-id file-id exports]
  (let [objects (deref (refs/objects-by-id ids))]
    (use-download-export objects filename page-id file-id exports)))

(mf/defc export-shapes-dialog
  {::mf/register modal/components
   ::mf/register-as :export-shapes}
  [{:keys [shapes]}]
  (let [selected (wsh/lookup-selected @st/state)
        shapes (if (some? shapes)
                 shapes
                 (if (> (count selected) 0)
                   (deref (refs/objects-by-id selected))
                   (->> (wsh/lookup-page-objects @st/state)
                        vals
                        (filter #(> (count (:exports %)) 0)))))
        exports (mf/use-state (mapv (fn [shape] (assoc shape :exports (mapv #(assoc % :enabled true) (:exports shape)))) shapes))
        checked (->> (map :exports @exports)
                     (flatten)
                     (filter #(get % :enabled)))

        all (->> (map :exports @exports)
                 (flatten))

        all-checked? (->> all
                          (every? #(get % :enabled)))

        all-unchecked? (->> all
                            (every? #(not (get % :enabled))))

        cancel-fn
        (mf/use-callback
         (fn [event]
           (dom/prevent-default event)
           (println "KO")
           (st/emit! (modal/hide))))

        accept-fn
        (mf/use-callback
         (mf/deps @exports)
         (fn [event]
           (dom/prevent-default event)
           (println "OK" (->> (map :exports @exports)
                              (flatten)
                              (filter #(get % :enabled))))
           ;; TODO: filtrar los enabled y llamar donde sea necesario
           ))

        on-change-handler
        (fn [_ shape-index export-index]
          (swap! exports update-in [shape-index :exports export-index :enabled] not))

        change-all
        (fn [_]
          (reset! exports (mapv (fn [shape] (assoc shape :exports (mapv #(assoc % :enabled (not all-checked?)) (:exports shape)))) shapes)))]

    [:div.modal-overlay
     [:div.modal-container.export-shapes-dialog
      [:div.modal-header
       [:div.modal-header-title
        [:h2 (tr "dashboard.export-shapes.title")]]

       [:div.modal-close-button
        {:on-click cancel-fn} i/close]]

      [:*
       [:div.modal-content
        (if (> (count all) 0)
          [:*
           [:div.export-option.table-row.header
            [:div.table-field.check {:on-click change-all}
             (cond
               all-checked? [:span i/checkbox-checked]
               all-unchecked? [:span i/checkbox-unchecked]
               :else [:span i/checkbox-intermediate])]
            [:div.table-field.title (tr "dashboard.export-shapes.selected" (c (count checked)) (c (count all)))]]

           [:div.body
            (for [[shape-index shape] (d/enumerate shapes)]
              (for [[export-index export] (d/enumerate (:exports shape))]
                [:div.export-option.table-row.body
                 [:div.table-field.check {:on-click #(on-change-handler % shape-index export-index)}
                  (if (get-in @exports [shape-index :exports export-index :enabled])
                    [:span i/checkbox-checked]
                    [:span i/checkbox-unchecked])]

                 [:div.table-field.name (str (cond-> (:name shape)
                                               (:suffix export) (str (:suffix export))))]
                 [:div.table-field.scale (str (* (:width shape) (:scale export)) "x" (* (:height shape) (:scale export)) "px ")]
                 [:div.table-field.extension (-> (name (:type export)) clojure.string/upper-case)]]))]]

          [:div "TODO: maquetar esto para cuando no hay selección"])]

       [:div.modal-footer
        [:div.action-buttons
         [:input.cancel-button
          {:type "button"
           :value (tr "labels.cancel")
           :on-click cancel-fn}]

         [:input.accept-button
          {:class "primary"
           :type "button"
           :value (tr "labels.export")
           :on-click accept-fn}]]]]]]))

(mf/defc exports-menu
  {::mf/wrap [#(mf/memo' % (mf/check-props ["ids" "values" "type" "page-id" "file-id"]))]}
  [{:keys [ids type values page-id file-id] :as props}]
  (let [exports  (:exports values [])

        scale-enabled?
        (mf/use-callback
         (fn [export]
           (#{:png :jpeg} (:type export))))

        page (wsh/lookup-page @st/state page-id)
        first-object-name (-> (:objects page)
                              (get (first ids))
                              :name)

        filename (cond
                   ;; one export from one shape
                   (and (= (count ids) 1)
                        (= (count exports) 1)
                        (not (empty (:suffix (first exports)))))
                   (str
                    first-object-name
                    (:suffix (first exports)))

                   ;; multiple exports from one shape
                   (and (= (count ids) 1)
                        (> (count exports) 1))
                   first-object-name

                   :else
                   (:name page))

        [on-download loading?] (use-download-export-by-id ids filename page-id file-id exports)

        add-export
        (mf/use-callback
         (mf/deps ids)
         (fn []
           (let [xspec {:type :png
                        :suffix ""
                        :scale 1}]
             (st/emit! (dch/update-shapes ids
                                          (fn [shape]
                                            (assoc shape :exports (into [xspec] (:exports shape)))))))))

        delete-export
        (mf/use-callback
         (mf/deps ids)
         (fn [position]
           (let [remove-fill-by-index (fn [values index] (->> (d/enumerate values)
                                                              (filterv (fn [[idx _]] (not= idx index)))
                                                              (mapv second)))

                 remove (fn [shape] (update shape :exports remove-fill-by-index position))]
             (st/emit! (dch/update-shapes
                        ids
                        #(remove %))))))

        on-scale-change
        (mf/use-callback
         (mf/deps ids)
         (fn [index event]
           (let [target  (dom/get-target event)
                 value   (dom/get-value target)
                 value   (d/parse-double value)]
             (st/emit! (dch/update-shapes ids
                                          (fn [shape]
                                            (assoc-in shape [:exports index :scale] value)))))))

        on-suffix-change
        (mf/use-callback
         (mf/deps ids)
         (fn [index event]
           (let [target  (dom/get-target event)
                 value   (dom/get-value target)]
             (st/emit! (dch/update-shapes ids
                                          (fn [shape]
                                            (assoc-in shape [:exports index :suffix] value)))))))

        on-type-change
        (mf/use-callback
         (mf/deps ids)
         (fn [index event]
           (let [target  (dom/get-target event)
                 value   (dom/get-value target)
                 value   (keyword value)]
             (st/emit! (dch/update-shapes ids
                                          (fn [shape]
                                            (assoc-in shape [:exports index :type] value)))))))

        on-remove-all
        (mf/use-callback
         (mf/deps ids)
         (fn []
           (st/emit! (dch/update-shapes ids
                                        (fn [shape]
                                          (assoc shape :exports []))))))]

    [:div.element-set.exports-options
     [:div.element-set-title
      [:span (tr (if (> (count ids) 1) "workspace.options.export-multiple" "workspace.options.export"))]
      (when (not (= :multiple exports))
        [:div.add-page {:on-click add-export} i/close])]

     (cond
       (= :multiple exports)
       [:div.element-set-options-group
        [:div.element-set-label (tr "settings.multiple")]
        [:div.element-set-actions
         [:div.element-set-actions-button {:on-click on-remove-all}
          i/minus]]]


       (seq exports)
       [:div.element-set-content
        (for [[index export] (d/enumerate exports)]
          [:div.element-set-options-group
           {:key index}
           (when (scale-enabled? export)
             [:select.input-select {:on-change (partial on-scale-change index)
                                    :value (:scale export)}
              [:option {:value "0.5"}  "0.5x"]
              [:option {:value "0.75"} "0.75x"]
              [:option {:value "1"} "1x"]
              [:option {:value "1.5"} "1.5x"]
              [:option {:value "2"} "2x"]
              [:option {:value "4"} "4x"]
              [:option {:value "6"} "6x"]])
           [:input.input-text {:value (:suffix export)
                               :placeholder (tr "workspace.options.export.suffix")
                               :on-change (partial on-suffix-change index)}]
           [:select.input-select {:value (name (:type export))
                                  :on-change (partial on-type-change index)}
            [:option {:value "png"} "PNG"]
            [:option {:value "jpeg"} "JPEG"]
            [:option {:value "svg"} "SVG"]
            [:option {:value "pdf"} "PDF"]]
           [:div.delete-icon {:on-click (partial delete-export index)}
            i/minus]])

        [:div.btn-icon-dark.download-button
         {:on-click (when-not loading? on-download)
          :class (dom/classnames
                  :btn-disabled loading?)
          :disabled loading?}
         (if loading?
           (tr "workspace.options.exporting-object")
           (tr "workspace.options.export-object" (c (count ids))))]])]))

