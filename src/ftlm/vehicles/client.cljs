(ns ftlm.vehicles.client
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
   [thi.ng.geom.rect :as rect]

   [thi.ng.geom.gl.shaders.lambert :as lambert]

   )
  ;; (:require-macros
  ;;  [thi.ng.math.macros :as mm])
  )

(defn req [href opts]
  (js-await [res (js/fetch href (clj->js opts))]
    (.text res)))

(defn init []
  (graft/init reader/read-string))

(def triangle (poly/polygon2 [-100 -100]
                             [10 0]
                             [10 10]))

(defn ^:export demo
  []
  (let [gl        (gl/gl-context "main")
        view-rect (gl/get-viewport-rect gl)
        shader (sh/make-shader-from-spec gl (basic/make-shader-spec-2d true))
        model     (-> (rect/rect 0 0 0.48)
                      (gl/as-gl-buffer-spec {:normals false :fixed-color [1 0.3 0 1]})
                      (gl/make-buffers-in-spec gl glc/static-draw)
                      (assoc-in [:uniforms :proj] (gl/ortho view-rect)))]
    (anim/animate
     (fn [t frame]
       (doto gl
         (gl/set-viewport view-rect)
         (gl/clear-color-and-depth-buffer col/WHITE 1)
         (gl/draw-with-shader
          (-> model
              (assoc :shader shader)
              (assoc-in [:uniforms :model]
                        (-> M44 (g/translate (vec3 0.48 0 0)) (g/rotate (- (+ t HALF_PI))))))))
       true))))


;; (defn ^:export demo
;;   []
;;   (enable-console-print!)
;;   (let [gl        (gl/gl-context "main")
;;         view-rect (gl/get-viewport-rect gl)
;;         shader1   (sh/make-shader-from-spec gl (basic/make-shader-spec-2d false))
;;         shader2   (sh/make-shader-from-spec gl (basic/make-shader-spec-2d true))
;;         teeth     20
;;         model     (-> (poly/cog 0.5 teeth [0.9 1 1 0.9])
;;                       (gl/as-gl-buffer-spec {:normals false :fixed-color [1 0 0 1]})
;;                       (gl/make-buffers-in-spec gl glc/static-draw)
;;                       (assoc-in [:uniforms :proj] (gl/ortho view-rect))
;;                       (time))]
;;     (anim/animate
;;      (fn [t frame]
;;        (gl/set-viewport gl view-rect)
;;        (gl/clear-color-and-depth-buffer gl 1 0.98 0.95 1 1)
;;        ;; draw left polygon using color uniform (that's why we need to remove color attrib)
;;        (gl/draw-with-shader
;;         gl (-> model
;;                (assoc :shader  shader1)
;;                (update-in [:attribs] dissoc :color)
;;                (update-in [:uniforms] merge
;;                           {:model (-> M44 (g/translate (vec3 -0.48 0 0)) (g/rotate t))
;;                            :color [0 1 1 1]})))
;;        ;; draw right polygon using color attribs
;;        (gl/draw-with-shader
;;         gl (-> model
;;                (assoc :shader shader2)
;;                (assoc-in [:uniforms :model]
;;                          (-> M44 (g/translate (vec3 0.48 0 0)) (g/rotate (- (+ t (/ HALF_PI teeth))))))))
;;        true))))

(let [center (thi.ng.geom.vector/vec2 0 0)]
  (->> (range 1 10)
       (map #(c/circle center %))
       (reduce (fn [coll c] (conj coll c)) [])))

(demo)
