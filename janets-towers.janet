# title:   janet's towers
# author:  althea
# desc:    ld53 entry inspired by cohen's towers
# site:    https://ldj.am/$360116
# license: MIT License
# version: 0.1
# script:  janet
# strict:  true

# this pollutes the namespace, but it's
# much more straightforward than importing
(use tic80)

# initial values live here

# constants

# general
(def smol-number 0.1) # a small number that is close enough to 0

# physics
(def friction 0.1)
(def gravity 0.5)
(def max-speed 2)

# buttons
(def up 0)
(def down 1)
(def left 2)
(def right 3)
(def a 4)

# a lil vector type with helpers,
# mostly stolen from
# https://github.com/AlecTroemel/junk-drawer

# pass the operator to both vector
# components and return mutated lhs
(defn pass-operator [op a b]
  # handle vector-vector and vector-scalar operations
  (let [b (cond (number? b) {:x b :y b} b)]
    (set (a :x) (op (a :x) (b :x)))
    (set (a :y) (op (a :y) (b :y)))
    a))

(defn clamp [x a b]
  (if (< x a) a (if (> x b) b x)))

(defn signum [x]
  (if (< x 0) -1 (if (> x 0) 1 0)))

(def vector
  @{:add
    (fn [self other]
      (pass-operator + self other))
    :subtract
    (fn [self other]
      (pass-operator - self other))
    :multiply
    (fn [self other]
      (pass-operator * self other))
    :divide
    (fn [self other]
      (pass-operator / self other))
    :distance-squared
    (fn [self other]
      (let [dx (- (self :x) (other :x))
            dy (- (self :y) (other :y))]
        (+ (* dx dx)
           (* dy dy))))
    :distance
    (fn [self other]
      (math/sqrt (:distance-squared self other)))
    :zero-if-smol
    (fn [self component smol]
      (when (< (math/abs (self component)) smol) (set (self component) 0)))
    :reduce-by
    (fn [self component value]
      (set (self component) (- (self component) (* value (signum (self component))))))
    :clamp
    (fn [self component value]
      (def posval (math/abs value))
      (set (self component) (clamp (self component) (* posval -1) posval)))
    :apply
    (fn [self component value &opt modifier]
      (default modifier 1)
      (+= (self component) (* value modifier)))})

(defn new-vector [&opt x y]
  (default x 0)
  (default y 0)
  (table/setproto @{:x x :y y} vector))

(defn draw-sprite-vector [sprite-id &opt position-vector scale]
  (default position-vector {:x 0 :y 0})
  (default scale 2)
  (spr sprite-id (math/round (position-vector :x)) (math/round (position-vector :y)) 0 scale))

# game handling begins here

(def input @{:left false
             :right false
             :up false
             :down false
             :a false
             :is-left-or-right (fn [self]
                                 (if (or
                                       (self :left)
                                       (self :right)) true false))})

(def entity @{:position (new-vector)
              :velocity (new-vector)
              :acceleration 0
              :max-velocity 0
              :is-dead false
              :is-on-ground false
              :update (fn [self dt]
                        (do
                          (when (not (self :is-on-ground))
                            (:apply (self :velocity) :y gravity dt))

                          (:zero-if-smol (self :velocity) :x smol-number)

                          (when (not (= ((self :velocity) :x) 0))
                            (:reduce-by (self :velocity) :x friction))

                          (:apply (self :velocity) :x (self :acceleration) dt)
                          (:add (self :position) (self :velocity))
                          (:clamp (self :velocity) :x (self :max-velocity))))
              :draw (fn [self]
                      (draw-sprite-vector (self :sprite-id) (self :position)))})

# player
(def player
  (table/setproto @{:sprite-id 256
                    :max-velocity 2
                    :is-controllable false
                    :handle-input (fn [self]
                                    (when (self :is-controllable)
                                      (if (:is-left-or-right input)
                                        (do
                                          (when (input :left)
                                            (set (self :acceleration) -1))
                                          (when (input :right)
                                            (set (self :acceleration) 1)))
                                        (set (self :acceleration) 0))))} entity))
(defn handle-input []
  # TODO: this seems redundant
  (if (btn left) (set (input :left) true) (set (input :left) false))
  (if (btn right) (set (input :right) true) (set (input :right) false))

  (set (player :is-controllable) true)
  (:handle-input player))

(defn update [dt]
  (set (player :is-on-ground) true)

  (:update player dt))

(defn draw []
  (cls)
  (:draw player))

(var dt 0)
(var pt (time))

# main loop goes here

(defn TIC []
  (def now (time))
  (set dt (/ (- now pt) 100.0))
  (set pt now)

  (handle-input)
  (update dt)
  (draw))

# <TILES>
# 001:eccccccccc888888caaaaaaaca888888cacccccccacc0ccccacc0ccccacc0ccc
# 002:ccccceee8888cceeaaaa0cee888a0ceeccca0ccc0cca0c0c0cca0c0c0cca0c0c
# 003:eccccccccc888888caaaaaaaca888888cacccccccacccccccacc0ccccacc0ccc
# 004:ccccceee8888cceeaaaa0cee888a0ceeccca0cccccca0c0c0cca0c0c0cca0c0c
# 017:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
# 018:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
# 019:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
# 020:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
# </TILES>

# <SPRITES>
# 000:002022000202a220020a2c020002cc000002330000c999c00009090000080800
# </SPRITES>

# <WAVES>
# 000:00000000ffffffff00000000ffffffff
# 001:0123456789abcdeffedcba9876543210
# 002:0123456789abcdef0123456789abcdef
# </WAVES>

# <SFX>
# 000:020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200409000000000
# </SFX>

# <PATTERNS>
# 000:600006800006000000000000000000000000800006600006000000000000000000000000000000900006000000000000a00006000000000000000000000000000000000000000000000000400006000000000000000000000000000000000000000000900006000000000000000000000000900006000000000000000000000000900006000000000000000000000000d00006900006000000900006000000100000000000000000000000000000000000000000000000000000000000000000
# </PATTERNS>

# <TRACKS>
# 000:180000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
# </TRACKS>

# <PALETTE>
# 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
# </PALETTE>

