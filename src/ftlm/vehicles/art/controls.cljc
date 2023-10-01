(ns ftlm.vehicles.art.controls)

(def white [0 0 255])

(def
  cyberpunk-palette-hsb
  [{:h 180 :s 255 :b 255}
   {:h -20 :s 255 :b 255}
   {:h -53 :s 255 :b 255}
   {:h 117 :s 191.25 :b 204}
   {:h 0 :s 255 :b 139}])

(def strawberry-mix
  [{:h 0 :s 217 :b 230}
   {:h 30 :s 255 :b 242}
   {:h 54 :s 248 :b 255}
   {:h 75 :s 204 :b 179}
   {:h 240 :s 191 :b 166}
   {:h 290 :s 179 :b 153}])

(def quite-green {:h 135 :s 100 :v 100})
(def olive-lime-green {:h 72 :s 100 :v 100})

(def default-versions
  {"brownians" {:background 230
                :base-color 7.790258368269614
                :brownian-factor 0.1
                :change-palette? false
                :circle-lifetime [110 10]
                :circle-scale 1.5
                :circle-shine 0
                :circle-wobble 1
                :friction [(/ 1 43) (/ 1 50)]
                :hide-lines? false
                :infected-color 255
                :infected-rate (/ 1 10)
                :infectiousness (/ 1 5)
                :rand-color-count 8
                :spawn-rate {:base 1 :freq 2 :pow 1}
                :speed 2
                :spread 1
                :spread-speed 0}
   "getting-around" {:background-color {:h 0 :s 0 :v 89}
                     :brownian-factor 0.8
                     :cart-scale 1
                     :color-palatte [50 40 60]
                     :distance-when-darting 0
                     :max-temp 1
                     :spawn-amount 30
                     :spawn-spread 0.4
                     :temp-color-high {:a 1 :h 0 :s 68 :v 89}
                     :temp-color-low {:a 0.1 :h 0 :s 68 :v 89}
                     :temp-zone-count 10
                     :time-speed 2
                     :trail-color {:h 0 :s 0 :v 89}
                     :trail-size 20
                     :make-trails? true}})


(def page-layouts
  {"brownians" {:per-page 3}
   "getting-around" {:per-page 1}})

(def versions
  {"brownians" {"0" {}
                "1" {:background 230
                     :base-color 7.790258368269614
                     :brownian-factor 0.1
                     :circle-wobble 1
                     :infected-color (rand-int (inc 360))
                     :infected-rate (/ 1 3)
                     :rand-color-count 8
                     :spread 1
                     :spread-speed 1}
                "10" {:background 0
                      :brownian-factor 0.05
                      :circle-shine 0.2
                      :circle-wobble 0.4
                      :infected-color white
                      :infected-rate (/ 1 3)
                      :rand-color-count 2
                      :spawn-rate {:base 1 :freq 1 :pow 1}
                      :spread 1.5
                      :spread-speed 1}
                "11" {:background 0
                      :brownian-factor 0.05
                      :circle-shine 0.2
                      :circle-wobble 0.4
                      :friction [(/ 4 100) (/ 1 100)]
                      :infected-color white
                      :infected-rate (/ 1 3)
                      :infected-transform {:scale 0.8}
                      :rand-color-count 2
                      :spawn-rate {:base 1 :freq 1 :pow 1}
                      :spread 1.5
                      :spread-speed 1}
                "12" {:background 0
                      :brownian-factor 0.05
                      :circle-shine 0.2
                      :circle-wobble 0.4
                      :friction [(/ 4 100) (/ 1 100)]
                      :infected-color 15
                      :infected-rate (/ 1 3)
                      :infected-transform {:scale 1.8}
                      :rand-color-count 2
                      :spawn-rate {:base 1 :freq 1 :pow 1}
                      :spread 1.5
                      :spread-speed 1}
                "13" {:background 80
                      :brownian-factor 0.1
                      :circle-shine 0.2
                      :circle-wobble 0.4
                      :friction [(/ 4 100) (/ 1 100)]
                      :infected-color 15
                      :infected-rate (/ 1 3)
                      :infected-transform {:scale 1.8}
                      :rand-color-count 2
                      :spawn-rate {:base 1 :freq 1 :pow 1}
                      :spread 1.5
                      :spread-speed 1}
                "14" {:background 0
                      :brownian-factor 0.1
                      :circle-scale 0.1
                      :circle-shine 0.2
                      :circle-wobble 0.4
                      :color-palatte [white]
                      :friction [(/ 4 100) (/ 1 100)]
                      :infected-color white
                      :infected-rate (/ 1 3)
                      :infected-transform {:scale 10}
                      :rand-color-count 2
                      :spawn-rate {:base 2 :freq 1.4 :pow 1}
                      :spread 1.5
                      :spread-speed 1}
                "15" {:background 0
                      :base-color 360
                      :brownian-factor 0.4
                      :circle-scale 0.2
                      :circle-shine 0.2
                      :circle-wobble 0.4
                      :friction [(/ 4 100) (/ 1 100)]
                      :hide-lines? true
                      :infected-color white
                      :infected-rate (/ 1 10)
                      :infected-transform {:scale 3}
                      :rand-color-count 2
                      :spawn-rate {:base 4 :freq 1.4 :pow 1}
                      :spread 1.5
                      :spread-speed 1.4}
                "16" {:background 0
                      :base-color 360
                      :brownian-factor 0.4
                      :circle-scale 0.2
                      :circle-shine 0.2
                      :circle-wobble 0.4
                      :friction [(/ 4 100) (/ 1 100)]
                      :hide-lines? true
                      :infected-color white
                      :infected-rate 0
                      :infected-transform {:scale 3}
                      :rand-color-count 2
                      :spawn-rate {:base 4 :freq 1.4 :pow 1}
                      :spread 1.5
                      :spread-speed 1.4}
                "17" {:background 0
                      :base-color 360
                      :brownian-factor 0.3
                      :circle-scale 0.2
                      :circle-shine 0.2
                      :circle-wobble 0.4
                      :friction [(/ 4 100) (/ 1 100)]
                      :hide-lines? true
                      :infected-color white
                      :infected-rate 0
                      :infected-transform {:scale 3}
                      :rand-color-count 1
                      :spawn-rate {:base 4 :freq 1.4 :pow 1}
                      :spread 1
                      :spread-speed 0.8}
                "18" {:background 0
                      :base-color 360
                      :brownian-factor 0.3
                      :circle-scale 0.2
                      :circle-shine 0.2
                      :circle-wobble 2
                      :friction [(/ 4 100) (/ 1 100)]
                      :hide-lines? true
                      :infected-color white
                      :infected-rate 0
                      :infected-transform {:scale 3}
                      :rand-color-count 8
                      :spawn-rate {:base 4 :freq 1.4 :pow 1}
                      :spread 5
                      :spread-speed 0.3}
                "19" {:background 230
                      :base-color 360
                      :brownian-factor 0.3
                      :circle-scale 0.2
                      :circle-shine 0.2
                      :circle-wobble 2
                      :friction [(/ 4 100) (/ 1 100)]
                      :hide-lines? true
                      :infected-color white
                      :infected-rate 0
                      :infected-transform {:scale 3}
                      :rand-color-count 8
                      :spawn-rate {:base 4 :freq 1.4 :pow 1}
                      :spread 5
                      :spread-speed 0.3}
                "2" {:background 230
                     :base-color 150
                     :brownian-factor 0.1
                     :circle-wobble 1
                     :infected-color white
                     :infected-rate (/ 1 10)
                     :rand-color-count 4
                     :spread 1
                     :spread-speed 1}
                "20" {:background 230
                      :base-color 360
                      :brownian-factor 0.3
                      :circle-scale 0.2
                      :circle-shine 0.2
                      :circle-wobble 2
                      :friction [(/ 4 100) (/ 1 100)]
                      :infected-color (rand (inc 360))
                      :infected-rate (/ 1 6)
                      :infected-transform {:scale 1.1}
                      :rand-color-count 8
                      :spawn-rate {:base 4 :freq 1.4 :pow 1}
                      :spread 5
                      :spread-speed 0.3}
                "21" {:background 230
                      :brownian-factor 0.05
                      :circle-shine 1
                      :circle-wobble 0.4
                      :color-palatte cyberpunk-palette-hsb
                      :infected-color 0
                      :infected-rate 0
                      :rand-color-count 8
                      :spawn-rate {:base 1 :freq 1 :pow 1}
                      :spread 1
                      :spread-speed 0}
                "22" {:background 230
                      :base-color (rand 360)
                      :brownian-factor 0
                      :change-palette? true
                      :circle-lifetime [180 40]
                      :circle-shine 1
                      :circle-wobble 0
                      :infected-rate 0
                      :rand-color-count 8
                      :spawn-rate {:base 4 :freq 0.8 :pow 1}
                      :spread 1
                      :spread-speed 0}
                "23" {:background 230
                      :base-color 7
                      :brownian-factor 0.25
                      :circle-lifetime [180 40]
                      :circle-shine 1
                      :circle-wobble 0
                      :infected-rate 0
                      :rand-color-count 8
                      :spawn-rate {:base 3 :freq 0.8 :pow 1}
                      :spread 1.8
                      :spread-speed 0}
                "24" {:circle-wobble 1 :infected-rate (/ 1 10) :spread 2}
                "25" {:brownian-factor 0.1
                      :circle-shine 1
                      :circle-wobble 2
                      :color-palatte strawberry-mix
                      :friction [0 0]
                      :infected-color 200
                      :infected-rate 0
                      :spawn-rate {:base 20 :freq 10 :pow 3}
                      :spread 1
                      :spread-speed 4}
                "26" {:infected-rate 0
                      :spread 1
                      :brownian-factor 0.1
                      :friction [(/ 1 100) (/ 1 30)]
                      :infected-color 200
                      ;; :spawn-rate {:base 20 :freq 10 :pow 3}
                      :circle-wobble 2
                      :circle-shine 1}
                "27" {:infected-rate (/ 1 10)
                      :spread 1
                      :brownian-factor 0.2
                      :friction [(/ 1 100) (/ 1 30)]
                      :infected-color 160
                      ;; :spawn-rate {:base 20 :freq 10 :pow 3}
                      :circle-wobble 2
                      :circle-shine 1}
                "28" {:background 0
                      :base-color 300
                      :brownian-factor 0.1
                      :change-palette? true
                      :circle-lifetime [110 10]
                      :circle-scale 3
                      :circle-shine 0
                      :circle-wobble 1
                      :friction [0.023255813953488372 0.02]
                      :hide-lines? false
                      :infected-color 255
                      :infected-rate 0.1
                      :infectiousness 0.2
                      :rand-color-count 8
                      :spawn-rate {:base 1 :freq 2 :pow 1}
                      :spread 1.5
                      :spread-speed 1}
                "3" {:background 0
                     :base-color 30
                     :brownian-factor 0.1
                     :circle-wobble 1
                     :infected-color white
                     :infected-rate (/ 1 10)
                     :rand-color-count 8
                     :spread 1
                     :spread-speed 1}
                "4" {:background 130
                     :base-color 30
                     :brownian-factor 0.1
                     :circle-wobble 0
                     :infected-color 0
                     :infected-rate (/ 1 10)
                     :rand-color-count 8
                     :spread 2
                     :spread-speed 1}
                "5" {:background 230
                     :brownian-factor 0.2
                     :circle-wobble 0
                     :friction [(/ 1 100) (/ 1 50)]
                     :infected-color 0
                     :infected-rate 0
                     :rand-color-count 8
                     :spread 0.4
                     :spread-speed 10}
                "6" {:background 230
                     :brownian-factor 0
                     :circle-wobble 0
                     :friction [0 0]
                     :infected-color 0
                     :infected-rate 0
                     :rand-color-count 8
                     :spawn-rate {:base 1 :freq 0.4 :pow 5}
                     :spread 0.4
                     :spread-speed 3}
                "7" {:background 230
                     :brownian-factor 0.05
                     :circle-shine 2
                     :circle-wobble 0.4
                     :infected-color 0
                     :infected-rate 0
                     :rand-color-count 8
                     :spawn-rate {:base 1 :freq 1 :pow 1}
                     :spread 1
                     :spread-speed 0}
                "8" {:background 0
                     :brownian-factor 0.05
                     :circle-shine 2
                     :circle-wobble 0.4
                     :infected-color 0
                     :infected-rate 0
                     :rand-color-count 8
                     :spawn-rate {:base 1 :freq 1 :pow 1}
                     :spread 1
                     :spread-speed 0}
                "9" {:background 0
                     :brownian-factor 0.05
                     :circle-shine 0.2
                     :circle-wobble 0.4
                     :infected-color 0
                     :infected-rate 0
                     :rand-color-count 2
                     :spawn-rate {:base 1 :freq 1 :pow 1}
                     :spread 1.5
                     :spread-speed 1}}
   "getting-around"
     {"0" {}
      "1" {:background-color {:h 100 :s 0 :v 0}
           :brownian-factor 0.8
           :cart-scale 0.4
           :color-palatte [50 40 60]
           :max-temp 1
           :middle-temp-zone-diameter 300
           :middle-temp-zone? true
           :spawn-amount 40
           :spawn-spread 0.4
           :temp-color-high quite-green
           :temp-color-low olive-lime-green
           :temp-zone-count 10
           :trail-color {:h 135 :s 100 :v 100}}
      "10" {:max-temp 10 :spawn-amount 20 :temp-zone-count 20}
      "12" {:brownian-factor 0.8
            :middle-temp-zone-diameter 300
            :middle-temp-zone? true
            :spawn-amount 40
            :spawn-spread 0.2}
      "2" {:brownian-factor 0.8
           :cart-scale 0.4
           :color-palatte [50 40 60]
           :max-temp 1
           :middle-temp-zone-diameter 300
           :middle-temp-zone? true
           :spawn-amount 40
           :temp-color-high {:a 1 :h 146 :s 78 :v 76}
           :temp-color-low {:a 0.1 :h 234 :s 60 :v 82}
           :temp-zone-count 20
           :trail-color {:h 130 :s 52 :v 87}}
      "20"
        {:brownian-factor 0.02 :cart-scale 2 :spawn-amount 3 :temp-zone-count 0}
      "21"
        {:brownian-factor 1 :cart-scale 0.8 :spawn-amount 4 :temp-zone-count 3}
      "4" {:background-color {:h 0 :s 0 :v 0}
           :brownian-factor 0.1
           :cart-scale 0.4
           :color-palatte [50 40 60]
           :dart-distance 0
           :everbody-darts? true
           :make-trails? false
           :max-temp 1
           :spawn-amount 40
           :spawn-spread 0.4
           :temp-zone-count 0}
      "5" {:background-color 0
           :cart-scale 0.8
           :cart-shinyness 200
           :everbody-darts? true
           :make-trails? false
           :max-temp 1
           :spawn-amount 40
           :spawn-spread 0.4
           :temp-color-high quite-green
           :temp-color-low olive-lime-green
           :temp-shinyness 200
           :temp-zone-count 15}
      "6" {:background-color {:h 0 :s 0 :v 0}
           :brownian-factor 2
           :cart-scale 0.4
           :cart-shinyness 100
           :max-temp 5
           :spawn-amount 40
           :spawn-spread 0.2
           :temp-color-high quite-green
           :temp-color-low olive-lime-green
           :temp-shinyness 20
           :temp-zone-count 15
           :trail-color white}
      "7" {:background-color {:h 0 :s 0 :v 0}
           :brownian-factor 0.2
           :cart-scale 0.5
           :color-palatte [0 30 0]
           :max-temp 5
           :spawn-amount 40
           :spawn-spread 0.4
           :temp-zone-count 10}
      "8" {:spawn-amount 1 :temp-zones-always-dart true}
      "9" {:spawn-amount 20 :temp-zones-always-dart true}}})
