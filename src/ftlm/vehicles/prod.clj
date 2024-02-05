(ns ftlm.vehicles.prod
  (:gen-class)
  (:require
   [ftlm.vehicles.server]
   [ftlm.vehicles.system]))

(defn -main [& _]
  (ftlm.vehicles.system/start!))

(comment
  (ftlm.vehicles.system/restart)
  ;; http://localhost:8095/art/g/brownians?page=0
  ;; http://localhost:8095/art/p/brownians/0?controls=true
  ;; http://localhost:8095/art/p/getting-around/0



  )

(defn type [obj]
  (-> obj meta :type))

(defn union [& types] {:type (set types)})

(def alltypes (atom {}))

(defn def-a-type [typename type]
  (swap! alltypes assoc typename type))

(def-a-type :int :int)

(defn lookup-type [typename]
  (get @alltypes typename))


(require '[clojure.string :as str])


;; it's not just the last one, any one below enables all above.Also the first one can't be disabled.

;; any one below enables all above.Also the first one can't be disabled.Here's my code [that I use to validate whatever you send]:

;; func ValidateContentPreferences(input gql.UpdateContentPreferencesInput) error {
;; 	if !HasPreference(input, gql.ContentPreferenceNSFWPOVPictures) && HasPreference(input, gql.ContentPreferenceNSFWCouplePictures) {
;; 		return fmt.Errorf("%s preference cannot be disabled if %s is enabled", gql.ContentPreferenceNSFWPOVPictures, gql.ContentPreferenceNSFWCouplePictures)
;; 	} else if !HasPreference(input, gql.ContentPreferenceNSFWPictures) && HasPreference(input, gql.ContentPreferenceNSFWPOVPictures) {
;; 		return fmt.Errorf("%s preference cannot be disabled if %s is enabled", gql.ContentPreferenceNSFWPictures, gql.ContentPreferenceNSFWPOVPictures)
;; 	} else if !HasPreference(input, gql.ContentPreferenceLewdPictures) && HasPreference(input, gql.ContentPreferenceNSFWPictures) {
;; 		return fmt.Errorf("%s preference cannot be disabled if %s is enabled", gql.ContentPreferenceLewdPictures, gql.ContentPreferenceNSFWPictures)
;; 	} else if !HasPreference(input, gql.ContentPreferenceSFWCouplePictures) && HasPreference(input, gql.ContentPreferenceLewdPictures) {
;; 		return fmt.Errorf("%s preference cannot be disabled if %s is enabled", gql.ContentPreferenceLewdPictures, gql.ContentPreferenceSFWCouplePictures)
;; 	} else if !HasPreference(input, gql.ContentPreferenceSFWPictures) {
;; 		return fmt.Errorf("%s preference cannot be disabled", gql.ContentPreferenceSFWPictures)
;; 	}
;; 	return nil
;; }


(def content-preferences [:sfw :nsfw :nsfw-wild])

(.indexOf content-preferences :nsfw)

(def selected-preferences [])

(defn select-pref [pref]
  ;; select everything up to and including pref
  (let [index (.indexOf content-preferences pref)]
    (into #{pref} (take index content-preferences))))

(select-pref :nsfw)
