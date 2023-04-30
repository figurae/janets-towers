# title:   janet's towers
# author:  althea
# desc:    ld53 entry inspired by cohen's towers
# site:    https://ldj.am/$360116
# license: MIT License
# version: 0.1
# script:  janet
# strict:  true

(use tic80) # module shmodule :3

# a lil vector type with helpers,
# mostly stolen from
# https://github.com/AlecTroemel/junk-drawer

# pass the operator to both vector
# components and return mutated lhs
(defn pass-operator [op a b]
  # handle vector-vector and vector-scalar operations
  (let [b (cond (number? b) {:x b :y b} b)]
    (put a :x (op (a :x) (b :x)))
    (put a :y (op (a :y) (b :y)))
    a))

(def vec
  @{:add
    (fn [self other]
      (pass-operator + self other))
    :sub
    (fn [self other]
      (pass-operator - self other))
    :mul
    (fn [self other]
      (pass-operator * self other))
    :div
    (fn [self other]
      (pass-operator / self other))
    :dst2
    (fn [self other]
      (let [dx (- (self :x) (other :x))
            dy (- (self :y) (other :y))]
        (+ (* dx dx)
           (* dy dy))))
    :dst
    (fn [self other]
      (math/sqrt (:dist2 self other)))
    :lt
    (fn [self]
      (:sub self {:x 1 :y 0}))
    :rt
    (fn [self]
      (:add self {:x 1 :y 0}))
    :up
    (fn [self]
      (:sub self {:x 0 :y 1}))
    :dn
    (fn [self]
      (:add self {:x 0 :y 1}))
    :toint
    (fn [self]
      (table/setproto @{:x (math/round (self :x))
                        :y (math/round (self :y))} (table/getproto self)))
    :iszero
    (fn [self]
      (and (= (self :x) 0) (= (self :y) 0)))
    :issmol
    (fn [self]
      (or (< (self :x) 1) (< (self :y) 1)))})

(defn newvec [&opt x y]
  (default x 0)
  (default y 0)
  (table/setproto @{:x x :y y} vec))

(defn zerosmol [vec]
  (when (< (math/abs (vec :x)) 1) (set (vec :x) 0))
  (when (< (math/abs (vec :y)) 1) (set (vec :y) 0)))

(defn sprv [id &opt pos-vec scale]
  (default pos-vec {:x 0 :y 0})
  (default scale 2)
  (spr id (math/round (pos-vec :x)) (math/round (pos-vec :y)) 0 scale))

# initial values live here

(def FRCTN 0.1)
(def GRVTY 0.5)

(def up 0)
(def dn 1)
(def lt 2)
(def rt 3)

(def ent @{:pos (newvec)
           :vel (newvec)
           :dead false
           # TODO: handle dt
           :updt (fn [self]
                   (do
                     (if (:issmol (self :vel))
                       (zerosmol (self :vel))
                       # TODO: maybe add/sub instead
                       (:div (self :vel) (+ 1 FRCTN)))
                     (:add (self :pos) (self :vel))))
           :draw (fn [self]
                   (sprv (self :spr) (self :pos)))})

(def plr (table/setproto @{:spr 256} ent))

# main loop goes here

(defn TIC []
  (when (btn up) (:up (plr :vel)))
  (when (btn dn) (:dn (plr :vel)))
  (when (btn lt) (:lt (plr :vel)))
  (when (btn rt) (:rt (plr :vel)))
  (cls 0)
  (:updt plr)
  (:draw plr))

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
