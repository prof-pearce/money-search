breed [traders trader]
breed [personae persona]
undirected-link-breed [partnerships partnership]

traders-own
[
  good-produced        ; what I make
  good-desired         ; what I want next
  good-held            ; what I hold
  total-payoff         ; how much I've consumed
  series-payoff        ; how much I consumed in current series
  strategy             ; current strategy
  partner              ; current partner
  ego                  ; current persona
]

; each trader owns 8 personae, one for each strategy
personae-own
[
  series-payoff        ; how much I consumed in current series
  strategy             ; my permanent strategy
  good-held            ; good I would hold if I was real
  owner                ; the trader I work for
  propensity           ; weighted sum of payoffs
  exp-propensity       ; e^propensity (more or less)
  probability          ; probability owner will make me his ego next series
  regret               ; weighted sum of differences between my payoff and max payoff
  participated?        ; did I already consume before trading?
]

globals
[
   marginal-payoff     ; utility gained from consuming one unit
   num-goods           ; number of types of goods & traders
   num-strategies      ; number of possible strategies
   series              ; series number
   sugar-demand        ; demand for sugar
   spice-demand        ; demand for spice
   money               ; list of # times each good used as money
   offers              ; list of # times a speculative good was offered
   salt                ; good 0
   sugar               ; good 1
   spice               ; good 2
   big-float           ; used to prevent overflows
   total-series-payoff ; how much was consumed by all traders in this series
]

;============================
; Initializers
;============================

to init-model
  clear-all            ; get rid of everything from last run
  random-seed new-seed ; re-seed random number generator
  reset-ticks          ; reset clock (and plots)
  init-globals         ; initialize globals

  create-traders num-salt-producers
  [
    set good-produced salt
    set color green
    init-trader
  ]
  create-traders num-sugar-producers
  [
    set good-produced sugar
    set color red
    init-trader
  ]
  create-traders num-spice-producers
  [
    set good-produced spice
    set color black
    init-trader
  ]

  let circle-radius 10 ; for now
  layout-circle traders circle-radius
  ask patches [init-patch]
end

to init-globals
  set num-goods 3   ; 0 = salt, 1 = sugar, 2 = spice
  set num-strategies 2 ^ num-goods
  set marginal-payoff 1
  set series 0
  set sugar-demand sugar-share * (1 - salt-demand)
  set spice-demand 1 - salt-demand - sugar-demand
  set money n-values num-goods [0]
  set offers n-values num-goods [0]
  set salt 0
  set sugar 1
  set spice 2
  set big-float 1e300 ; = 10^300  (used to prevent overflow)
  set total-series-payoff 0
end

to init-patch
  set pcolor white
end

to init-trader
  set total-payoff 0
  set series-payoff 0
  set good-held good-produced
  set good-desired good-produced     ; this changes each cycle

  set shape "Person"
  set partner nobody
  set label-color black

  set strategy random num-strategies
  set label strategy

  ; create and initialize 8 personae
  let next-strategy 0
  hatch-personae num-strategies
  [
    set owner myself
    hide-turtle                     ; personae are invisible
    init-persona next-strategy
    set next-strategy next-strategy + 1
  ]

  set ego one-of personae with [owner = myself and strategy = [strategy] of myself] ; there must be an easier way!
end

to init-persona [strat]
  set strategy strat
  set series-payoff 0
  set good-held [good-held] of owner
  set regret payoff-sensitivity
  set propensity 0
  set exp-propensity 1
  set probability 1 / num-strategies
  set participated? false
end

;;============================
;; updaters
;;============================

to update-model

  update-globals

  ifelse trade-cycle-probability < random-float 1
  [
    ; begin a new series
    set series series + 1
    ask traders [reset-trader]
    tick
    set total-series-payoff 0
  ]
  [
    ask traders [update-desire]              ; taste shock
    ask traders [update-partner]             ; pair-up
    ask partnerships [update-partnership]    ; trade
    ask traders
    [
      set partner nobody                     ; unpair
    ]
    tick
    clear-links  ; clear-partnerships
  ]
end

; allow user to dynamically alter demands:
to update-globals
  set sugar-demand sugar-share * (1 - salt-demand)
  set spice-demand 1 - salt-demand - sugar-demand
end

;; taste shock: set trader desire based on demand
to update-desire
  let chance random-float 1
  ifelse chance < salt-demand
  [
   set good-desired salt
  ]
  [
    ifelse salt-demand <= chance and chance < salt-demand + sugar-demand
    [
      set good-desired sugar
    ]
    [
      set good-desired spice
    ]
  ]
  ask personae with [owner = myself and good-held = [good-desired] of owner]
  [
    set good-held [good-produced] of owner
    set series-payoff series-payoff + marginal-payoff
    set participated? true
  ]
  update-trader
end

to update-trader
  set good-held [good-held] of ego
  set series-payoff [series-payoff] of ego
end

;; pair self with other and vice-versa
to update-partner
  if partner = nobody
  [
    set partner one-of other traders with [partner = nobody]
    if partner != nobody
    [
      ask partner [set partner myself]
      create-partnership-with partner   ; a bit of graphics for show
    ]
  ]
end

;; this is where trading happends
to update-partnership
  let trader-A end1
  let trader-B end2
  ask trader-A [do-mock-trades]
  ask trader-B [do-mock-trades]
  ask trader-A [update-trader]
  ask trader-B [update-trader]
end

; self's personae mock trade with partner's ego
to do-mock-trades
  if partner != nobody
  [
    let owner-desire  good-desired
    let owner-partner partner
    let partner-good [good-held] of partner
    let partner-ego [ego] of partner
    ask personae with [owner = myself]
    [
      let my-good good-held
      let I-will-trade? offer-trade? good-held partner-good
      ; only paersonas decide if they want to trade
      let partner-will-trade? [offer-trade? good-held my-good] of partner-ego

      ; ego updates moneyness, this should only happen once (awful code, sorry)
      if partner-will-trade? and self = [ego] of owner and partner-good != owner-desire and not participated? and not [participated?] of partner-ego
      [
          set offers replace-item partner-good offers (1 + item partner-good offers)
          if I-will-trade?
          [
            set money replace-item partner-good money (1 + item partner-good money)
          ]
      ]

      ; persona takes partner ego's good
      if I-will-trade? and partner-will-trade?
      [
        ifelse partner-good = owner-desire
        [
          set series-payoff series-payoff + marginal-payoff
          set good-held [good-produced] of owner
        ]
        [
          set good-held partner-good
        ]
      ]
    ]
  ]
end

; persona decides if he will agree to a trade
to-report offer-trade? [held-good offered-good]
  if participated? [report false] ; persona already ate
  if held-good = offered-good [report false] ; why bother?
  let owner-desire [good-desired] of owner
  if held-good = owner-desire [report false] ; this shouldn't happen
  if offered-good = owner-desire [report true] ; I will offer to trade regardless of strategy
  report speculate? offered-good strategy ; use strategy to decide
end


;;////////////////////////////////////////////////
;; reset cycle: update strategy and total payoff
;;///////////////////////////////////////////////

to reset-trader
  update-strategy
  ;set series-payoff [series-payoff] of ego ; needed?
  set total-payoff total-payoff + series-payoff
  set good-desired good-produced
  set good-held good-produced ; ?
  set partner nobody
  ask personae with [owner = myself]
  [
      set series-payoff 0
      set good-held [good-held] of owner
      set participated? false
  ]
end

; trader procedure
to update-strategy
  let my-personae personae with [owner = myself]
  let payoffs [series-payoff] of my-personae
  let max-payoff max payoffs
  set total-series-payoff series-payoff + total-series-payoff
  ask my-personae
  [
    ;let delta abs (max-payoff - series-payoff) ; original formula
    let delta ifelse-value series-payoff < max-payoff [max-payoff - series-payoff] [0] ; modified formula
    set regret weighted-average regret delta
    set propensity weighted-average propensity series-payoff
    set exp-propensity safe-exp (safe-div (propensity * payoff-sensitivity)  regret)
  ]
  let denom reduce safe-add [exp-propensity] of my-personae ; denominator = sum of exp-propensities
  ask my-personae
  [
    set probability exp-propensity / denom
  ]

  ; a kludgy way to compute new strategy
  let probabilities [probability] of my-personae ; list of probabilities
  set strategy 0
  let strategy-selected false
  let cursor 0
  let chance random-float 1
  while [not strategy-selected]
  [
    if num-strategies <= strategy
    [
      error "strategy out of bounds" ; shouldn't get here
    ]
    let new-cursor cursor + item strategy probabilities
    ifelse cursor <= chance and chance < new-cursor
    [
      set strategy-selected true
    ]
    [
      set cursor new-cursor
      set strategy strategy + 1
    ]
  ]
  set label strategy
  ; set new ego
  set ego one-of my-personae with [strategy = [strategy] of myself]
end

;;============================
;; utilities
;;============================

to debug [text val]
  if true
  [
    type who
    type ": "
    type text
    type " = "
    print val
  ]
end

to-report weighted-average [old-avg amt]
  report (1 - weight) * old-avg + weight * amt
end

to-report safe-exp [pow]
  let result 1
  carefully [set result exp pow] [set result big-float]
  report result
end

to-report safe-add [x y]
  let result 0
  carefully [set result x + y] [set result big-float]
  report result
end


to-report safe-div [x y]
  let result 0
  carefully [set result x / y] [set result big-float]
  report result
end

; performs a bit-wise shift
; of num amt places to the right
to-report right-shift [num amt]
  report int (num / (2 ^ amt))
end

; determines bit at position pos in
; binary expansion of num.
to-report bit-at [num pos]
  report (remainder (right-shift num pos) 2)
end

to-report speculate? [good some-strategy]
  report bit-at some-strategy good  = 1
end

to-report avg-utility [trader-type]
  let total 0
  ask traders with [good-produced = trader-type] [set total total + total-payoff]
  let num-traders  count traders with [good-produced = trader-type]
  report ifelse-value num-traders = 0 [0][total / num-traders]
end

to-report moneyness [good]
  let num-offers item good offers
  let num-accepts item good money
  report ifelse-value num-offers = 0 [0] [num-accepts / num-offers]
end
;
;; junkyard
@#$#@#$#@
GRAPHICS-WINDOW
236
10
673
448
-1
-1
13.0
1
10
1
1
1
0
0
0
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
12
18
76
51
Setup
init-model
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
159
20
222
53
Go
update-model
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
26
62
198
95
num-salt-producers
num-salt-producers
0
100
10.0
2
1
NIL
HORIZONTAL

SLIDER
26
102
198
135
num-sugar-producers
num-sugar-producers
0
100
10.0
2
1
NIL
HORIZONTAL

SLIDER
26
142
198
175
num-spice-producers
num-spice-producers
0
100
10.0
2
1
NIL
HORIZONTAL

SLIDER
26
199
198
232
salt-demand
salt-demand
0
1
0.8
.01
1
NIL
HORIZONTAL

SLIDER
26
239
198
272
sugar-share
sugar-share
0
1
0.5
.01
1
NIL
HORIZONTAL

SLIDER
26
296
198
329
payoff-sensitivity
payoff-sensitivity
0
10
2.4
.1
1
NIL
HORIZONTAL

SLIDER
26
336
200
369
trade-cycle-probability
trade-cycle-probability
0
1
0.97
.01
1
NIL
HORIZONTAL

SLIDER
26
378
198
411
weight
weight
0
1
0.45
.01
1
NIL
HORIZONTAL

MONITOR
89
11
146
56
NIL
series
0
1
11

PLOT
695
14
974
164
Moneyness
tick
moneyness
0.0
100.0
0.0
1.0
true
true
"set money [0 0 0]\nset offers [0 0 0]" ""
PENS
"salt" 1.0 0 -13840069 true "" "plot moneyness 0"
"sugar" 1.0 0 -2674135 true "" "plot moneyness 1"
"spice" 1.0 0 -14737633 true "" "plot moneyness 2"

PLOT
696
176
975
326
Strategy Distribution
strategy
# traders
0.0
7.0
0.0
10.0
true
false
"set-plot-x-range 0 8\nset-plot-y-range 0 1 + count traders\nset-histogram-num-bars 8" ""
PENS
"default" 1.0 1 -6459832 true "" "histogram [strategy] of traders"

PLOT
690
342
979
492
series payoff
series
payoff
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "plotxy series total-series-payoff"

@#$#@#$#@
## WHAT IS IT?

Money Search is a virtual laboratory for exploring how a good comes to be used as money in a comodities market. 

## HOW IT WORKS

Monaey Search models a market in which three types of goods are traded: salt, sugar, and spice.

There are three types of traders in the market: salt-producers, sugar-producers, and spice-producers. 


There are two types of update cycles: reset cycles and market cycles.

During a market cycle each trader 

  * Receives a taste shock that determines the type of good he desires
  * Is paired with a partner. 
  * Assume trader A is paired with trader B. Trading follows the rules:
    * If A and B hold the same good, then no trade is made.
    * If A holds the good desired by B, then B offers to trade.
    * If A desires the good held by B, then they trade, they consume their goods, each gets one payoff point, and each replaces the consumed good with their produced good.
    * If A doesn't desire the good held by B, then A uses a decision strategy to decide if he should trade with the hope of maybe trading B's good for a desired good in the future. (In other words, A is accepting B's good as "money".)
  * At the end of a market cycle traders are unpaired.

Occasionally a series of market cycles will be broken by a reset cycle. During a reset cycle a trader uses the Roth-Erev algorithm to select a strategy for making speculative trades (i.e., trading for money) in the next series. 

A strategy is a number n such that 0 <= n < 8. Assume n = 6, the binary representation of n is 110. We interpret this to mean: accept spice and sugar as money, but not salt. 

To select a strategy a trader partitions the unit interval [0, 1] into eight sub-intervals: I-0, I-1, ...,I-7. The length of the I-k is determined by the payoff the trader would have received in the previous series of market cycles had he used strategy k. The trader then picks a random number in [0, 1]. The partition it falls into will be his new strategy. Thus, he is more likely to pick a successful strategy since the sub-intervals of those strategies are longer.

But how does a trader know the payoffs he would have received had he played other strategies? To do this the trader maintains a set of eight personas. A persona is an invisible trader with a fixed strategy. Each persona keeps track of the good would hold if he made trades using his strategy and the payoff he would have received in the current series.

Assume A is partnered with B. A asks each of his personas to do a mock trade with B. B does the same. The persona using the same strategy as the trader-- called the trader's ego-- is performing the actual trade.

(Note that the list of goods held is only updated after A and B have both completed their trades.)


## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

This model makes heavy use of bulk list procedures: map, filter, reduce, for-each, etc. These procedures are interesting because they allow the user to specify a procedure or reporter as an input. In effect, they are higher-order procedures.

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
