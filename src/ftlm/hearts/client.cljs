(ns ftlm.hearts.client
  (:require
   [shadow.graft :as graft]
   [shadow.cljs.modern :refer (js-await)]
   [cljs.reader :as reader]

   [thi.ng.math.core :as m :refer [PI HALF_PI TWO_PI]]
   [thi.ng.color.core :as col]
   [thi.ng.typedarrays.core :as arrays]
   [thi.ng.geom.gl.core :as gl]
   [thi.ng.geom.gl.webgl.constants :as glc]
   [thi.ng.geom.gl.webgl.animator :as anim]
   [thi.ng.geom.gl.buffers :as buf]
   [thi.ng.geom.gl.shaders :as sh]
   [thi.ng.geom.gl.utils :as glu]
   [thi.ng.geom.gl.glmesh :as glm]
   [thi.ng.geom.gl.camera :as cam]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.matrix :as mat :refer [M44]]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.attribs :as attr]
   [thi.ng.glsl.core :as glsl :include-macros true]

   [thi.ng.geom.viz.core :as viz]
   [thi.ng.geom.svg.core :as svg]

   [thi.ng.geom.gl.shaders.basic :as basic]
   [thi.ng.geom.circle :as c]
   [thi.ng.geom.polygon :as poly]

   [thi.ng.geom.gl.shaders.lambert :as lambert]

   )



  ;; (:require-macros
  ;;  [thi.ng.math.macros :as mm])

  )

;; grab csrf token from html head meta, could alternatively be set via a graft
(def csrf-token (when-let [e (js/document.querySelector "meta[name=x-csrf-token]")]
                  (.-content e)))

;; look ma, no libs. these should likely be library functions
;; should obviously do more validation and error checking here, but for our purposes this is enough
(defn req [href opts]
  (js-await [res (js/fetch href (clj->js (assoc-in opts [:headers "x-csrf-token"] csrf-token)))]
    (.text res)))

(defonce state (atom {:curr-clip nil}))

(defn start-clip! [{:clip/keys [timestamps]}]
  (when-let [id (:curr-clip @state)]
    (js/window.clearInterval id))
  (swap! state assoc
         :curr-clip
         (js/setInterval
          (fn
            []
            (js/window.navigator.vibrate (clj->js timestamps)))
          1000)))

(defmethod graft/scion "clip" [opts btn]
  (.addEventListener btn "click" (fn [_] (start-clip! opts))))

(defn init []
  (graft/init reader/read-string))

;; (defn init-stats
;;   []
;;   (let [stats (js/Stats.)
;;         sdom (.call (aget stats "getDomElement") stats)]
;;     (.appendChild (.-body js/document) sdom)
;;     (.setAttribute sdom "class" "stats")
;;     stats))

;; (defn update-stats
;;   [stats]
;;   (.call (aget stats "update") stats))

(defn ^:export demo
  []
  (let [gl        (gl/gl-context "main")
        view-rect (gl/get-viewport-rect gl)
        model     (-> (a/aabb 0.8)
                      (g/center)
                      (g/as-mesh
                       {:mesh    (glm/indexed-gl-mesh 12 #{:col})
                        :attribs {:col (->> [[1 0 0] [0 1 0] [0 0 1] [0 1 1] [1 0 1] [1 1 0]]
                                            (map col/rgba)
                                            (attr/const-face-attribs))}})
                      (gl/as-gl-buffer-spec {})
                      (cam/apply (cam/perspective-camera {:aspect view-rect}))
                      (assoc :shader (sh/make-shader-from-spec gl (basic/make-shader-spec-3d true)))
                      (gl/make-buffers-in-spec gl glc/static-draw))]
    (anim/animate
     (fn [t frame]
       (doto gl
         (gl/set-viewport view-rect)
         (gl/clear-color-and-depth-buffer col/WHITE 1)
         (gl/enable glc/depth-test)
         (gl/draw-with-shader
          (assoc-in model [:uniforms :model] (-> M44 (g/rotate-x t) (g/rotate-y (* t 2))))))
       true))))





(let [center (thi.ng.geom.vector/vec2 0 0)]
  (->> (range 1 10)
       (map #(c/circle center %))
       (reduce (fn [coll c] (conj coll c)) [])))

(demo)




(comment

  (graft/reload!))
