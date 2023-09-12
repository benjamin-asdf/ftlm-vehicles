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

(def versions
  {"brownians"
   {"0" {}
    "1"
    {:spread 1
     :background 230
     :base-color 7.790258368269614
     :rand-color-count 8
     :spread-speed 1
     :brownian-factor 0.1
     :infected-rate (/ 1 3)
     :infected-color (rand-int (inc 360))
     :circle-wobble 1}
    "2"
    {:spread 1
     :background 230
     :base-color 150
     :rand-color-count 4
     :spread-speed 1
     :brownian-factor 0.1
     :infected-rate (/ 1 10)
     :infected-color white
     :circle-wobble 1}
    "3"
    {:spread 1
     :background 0
     :base-color 30
     :rand-color-count 8
     :spread-speed 1
     :brownian-factor 0.1
     :infected-rate (/ 1 10)
     :infected-color white
     :circle-wobble 1}
    "4"
    {:spread 2
     :background 130
     :base-color 30
     :rand-color-count 8
     :spread-speed 1
     :brownian-factor 0.1
     :infected-rate (/ 1 10)
     :infected-color 0
     :circle-wobble 0}
    "5"
    {:spread 0.4
     :background 230
     :rand-color-count 8
     :spread-speed 10
     :brownian-factor 0.2
     :infected-rate 0
     :infected-color 0
     :circle-wobble 0
     :friction [(/ 1 100) (/ 1 50)] }
    "6"
    {:spread 0.4
     :background 230
     :rand-color-count 8
     :spread-speed 3
     :brownian-factor 0
     :infected-rate 0
     :infected-color 0
     :circle-wobble 0
     :friction [0 0]
     :spawn-rate {:base 1 :freq 0.4 :pow 5}}
    "7"
    {:spread 1
     :background 230
     :rand-color-count 8
     :spread-speed 0
     :brownian-factor 0.05
     :infected-rate 0
     :infected-color 0
     :circle-wobble 0.4
     :circle-shine 2
     :spawn-rate {:base 1 :freq 1 :pow 1}}
    "8"
    {:spread 1
     :background 0
     :rand-color-count 8
     :speed 2
     :spread-speed 0
     :brownian-factor 0.05
     :infected-rate 0
     :infected-color 0
     :circle-wobble 0.4
     :circle-shine 2
     :spawn-rate {:base 1 :freq 1 :pow 1}}
    "9"
    {:spread 1.5
     :background 0
     :rand-color-count 2
     :spread-speed 1
     :brownian-factor 0.05
     :infected-rate 0
     :infected-color 0
     :circle-wobble 0.4
     :circle-shine 0.2
     :spawn-rate {:base 1 :freq 1 :pow 1}}
    "10"
    {:spread 1.5
     :background 0
     :rand-color-count 2
     :spread-speed 1
     :brownian-factor 0.05
     :infected-rate (/ 1 3)
     :infected-color white
     :circle-wobble 0.4
     :circle-shine 0.2
     :spawn-rate {:base 1 :freq 1 :pow 1}}
    "11"
    {:spread 1.5
     :background 0
     :rand-color-count 2
     :spread-speed 1
     :brownian-factor 0.05
     :infected-rate (/ 1 3)
     :infected-color white
     :circle-wobble 0.4
     :circle-shine 0.2
     :infected-transform {:scale 0.8}
     :friction [(/ 4 100) (/ 1 100)]
     :spawn-rate {:base 1 :freq 1 :pow 1}}
    "12"
    {:spread 1.5
     :background 0
     :rand-color-count 2
     :spread-speed 1
     :brownian-factor 0.05
     :infected-rate (/ 1 3)
     :infected-color 15
     :circle-wobble 0.4
     :circle-shine 0.2
     :infected-transform {:scale 1.8}
     :friction [(/ 4 100) (/ 1 100)]
     :spawn-rate {:base 1 :freq 1 :pow 1}}
    "13"
    {:spread 1.5
     :background 80
     :rand-color-count 2
     :spread-speed 1
     :brownian-factor 0.1
     :infected-rate (/ 1 3)
     :infected-color 15
     :circle-wobble 0.4
     :circle-shine 0.2
     :infected-transform {:scale 1.8}
     :friction [(/ 4 100) (/ 1 100)]
     :spawn-rate {:base 1 :freq 1 :pow 1}}
    "14"
    {:spread 1.5
     :background 0
     :rand-color-count 2
     :spread-speed 1
     :brownian-factor 0.1
     :infected-rate (/ 1 3)
     :color-palatte [white]
     :infected-color white
     :circle-wobble 0.4
     :circle-shine 0.2
     :infected-transform {:scale 10}
     :friction [(/ 4 100) (/ 1 100)]
     :circle-scale 0.1
     :spawn-rate {:base 2 :freq 1.4 :pow 1}}
    "15"
    {:spread 1.5
     :background 0
     :rand-color-count 2
     :base-color 360
     :spread-speed 1.4
     :brownian-factor 0.4
     :infected-rate (/ 1 10)
     :infected-color white
     :circle-wobble 0.4
     :circle-shine 0.2
     :infected-transform {:scale 3}
     :friction [(/ 4 100) (/ 1 100)]
     :circle-scale 0.2
     :spawn-rate {:base 4 :freq 1.4 :pow 1}
     :hide-lines? true}
    "16"
    {:spread 1.5
     :background 0
     :rand-color-count 2
     :base-color 360
     :spread-speed 1.4
     :brownian-factor 0.4
     :infected-rate 0
     :infected-color white
     :circle-wobble 0.4
     :circle-shine 0.2
     :infected-transform {:scale 3}
     :friction [(/ 4 100) (/ 1 100)]
     :circle-scale 0.2
     :spawn-rate {:base 4 :freq 1.4 :pow 1}
     :hide-lines? true}
    "17"
    {:spread 1
     :background 0
     :rand-color-count 1
     :base-color 360
     :spread-speed 0.8
     :brownian-factor 0.3
     :infected-rate 0
     :infected-color white
     :circle-wobble 0.4
     :circle-shine 0.2
     :infected-transform {:scale 3}
     :friction [(/ 4 100) (/ 1 100)]
     :circle-scale 0.2
     :spawn-rate {:base 4 :freq 1.4 :pow 1}
     :hide-lines? true}
    "18"
    {:spread 5
     :background 0
     :rand-color-count 8
     :base-color 360
     :spread-speed 0.3
     :brownian-factor 0.3
     :infected-rate 0
     :infected-color white
     :circle-wobble 2
     :circle-shine 0.2
     :infected-transform {:scale 3}
     :friction [(/ 4 100) (/ 1 100)]
     :circle-scale 0.2
     :spawn-rate {:base 4 :freq 1.4 :pow 1}
     :hide-lines? true}
    "19"
    {:spread 5
     :background 230
     :rand-color-count 8
     :base-color 360
     :spread-speed 0.3
     :brownian-factor 0.3
     :infected-rate 0
     :infected-color white
     :circle-wobble 2
     :circle-shine 0.2
     :infected-transform {:scale 3}
     :friction [(/ 4 100) (/ 1 100)]
     :circle-scale 0.2
     :spawn-rate {:base 4 :freq 1.4 :pow 1}
     :hide-lines? true}
    "20"
    {:spread 5
     :background 230
     :rand-color-count 8
     :base-color 360
     :spread-speed 0.3
     :brownian-factor 0.3
     :infected-rate (/ 1 6)
     :infected-color (rand (inc 360))
     :circle-wobble 2
     :circle-shine 0.2
     :infected-transform {:scale 1.1}
     :friction [(/ 4 100) (/ 1 100)]
     :circle-scale 0.2
     :spawn-rate {:base 4 :freq 1.4 :pow 1}}
    "21"
    {:spread 1
     :background 230
     :rand-color-count 8
     :spread-speed 0
     :color-palatte cyberpunk-palette-hsb
     :brownian-factor 0.05
     :infected-rate 0
     :infected-color 0
     :circle-wobble 0.4
     :circle-shine 1
     :spawn-rate {:base 1 :freq 1 :pow 1}}
    "22"
    {:spread 1
     :background 230
     :base-color (rand 360)
     :rand-color-count 8
     :spread-speed 0
     :brownian-factor 0
     :infected-rate 0
     :circle-wobble 0
     :circle-shine 1
     :change-palette? true
     :circle-lifetime [180 40]
     :spawn-rate {:base 4 :freq 0.8 :pow 1}}
    "23"
    {:spread 1.8
     :background 230
     :base-color 7
     :rand-color-count 8
     :spread-speed 0
     :brownian-factor 0.25
     :infected-rate 0
     :circle-wobble 0
     :circle-shine 1
     :circle-lifetime [180 40]
     :spawn-rate {:base 3 :freq 0.8 :pow 1}}
    "24"
    {:infected-rate (/ 1 10)
     :spread 2
     :circle-wobble 1}
    "25"
    {:color-palatte strawberry-mix
     :infected-rate 0
     :spread 1
     :brownian-factor 0.1
     :spread-speed 4
     :friction [0 0]
     :infected-color 200
     :spawn-rate {:base 20 :freq 10 :pow 3}
     :circle-wobble 2
     :circle-shine 1}
    "26"
    {:infected-rate 0
     :spread 1
     :brownian-factor 0.1
     :friction [(/ 1 100) (/ 1 30)]
     :infected-color 200
     ;; :spawn-rate {:base 20 :freq 10 :pow 3}
     :circle-wobble 2
     :circle-shine 1}
    "27"
    {:infected-rate (/ 1 10)
     :spread 1
     :brownian-factor 0.2
     :friction [(/ 1 100) (/ 1 30)]
     :infected-color 160
     ;; :spawn-rate {:base 20 :freq 10 :pow 3}
     :circle-wobble 2
     :circle-shine 1}}})
