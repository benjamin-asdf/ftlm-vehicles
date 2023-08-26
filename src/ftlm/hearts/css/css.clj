(ns ftlm.hearts.css.css
  (:require
   [shadow.css :refer [css]]))

(def
  button-primary
  (css
    :transition
    :duration-200
    :bg-amber-500
    :text-white
    :w-full
    :py-2.5
    :rounded-lg
    :text-sm
    :shadow-sm
    :font-semibold
    :text-center
    :inline-block:ring
    ["&:focus"
     {:outline "none"
      :box-shadow "0 0 0 3px black"}]))
