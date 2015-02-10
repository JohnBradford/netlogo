extensions [nw]
directed-link-breed [opinions opinion]
opinions-own [
  evaluation
  intensity ;; propagation intensity
  
]

globals [  
  turtles_list
  ]
turtles-own [

  N_Neighbors
  self-regard
  q_list
  
  degree

]


to setup
   clear-all
   reset-ticks
   set turtles_list []
   ;__clear-all-and-reset-ticks
   nw:set-context turtles links
 
 M_presets
 network_presets
 
  create-turtles population [ 
         set size 1 set color blue  ; BLUE COLOR BECAUSE NOT COMPLYING WITH 
         ;setxy random-pxcor random-pycor
         while [any? other turtles-here] [ let empty_patch one-of patches with [any? turtles-here = false] move-to empty_patch ]
        set color random-float 140
        set self-regard 0
   ]
      if Display_Mode = "Matrix" [
  ask turtles [set hidden? true]
   ]
   

   ask turtles [setup-neighbors
     set turtles_list fput self turtles_list    
     ]

end

 
 to M_presets

if Model_Presets = "Equality" [
  set network_type "None"
  set condition "Global"
  set population 40
  set vanity 0.3
  set rho 0.01
  set noise_intensity 0.2
  set sigma 0.35
  set k_neighbors 5]

 if Model_Presets = "Elite" [
    set network_type "None"
   set condition "Global"
  set population 60
  set vanity 0.3
  set rho 0.1
  set noise_intensity 0.2
  set sigma 0.3
  set k_neighbors 5]
   
    if Model_Presets = "Hierarchy" [
       set network_type "None"
      set condition "Global"
  set population 40
  set vanity 0.2
  set rho 0.5
  set noise_intensity 0.2
  set sigma 0.3
  set k_neighbors 10]
    
     if Model_Presets = "Dominance" [
        set network_type "None"
       set condition "Global"
  set population 40
  set vanity 0.4
  set rho 0.8
  set noise_intensity 0.2
  set sigma 0.3
  set k_neighbors 2]
     
   if Model_Presets = "Crisis" [
      set network_type "None"
     set condition "Global"
  set population 40
  set vanity 0.4
  set rho 0.35
  set noise_intensity 0.2
  set sigma 0.5
  set k_neighbors 2]
   
   if Display_Mode = "Matrix" [
  resize-world 0 population 0 population
  set-patch-size 8

   ]
   
 end
 
 to network_presets
   if network_type = "preferential attachment" 
[nw:generate-preferential-attachment turtles opinions population
      ask turtles [
        let town out-opinion-neighbors set N_Neighbors town]]
if network_type = "random" 
[nw:generate-random turtles opinions population rewiring-probability
        ask turtles [
        let town out-opinion-neighbors set N_Neighbors town]]
if network_type = "small world" 
[nw:generate-small-world turtles opinions 10 10 2.0 false
        ask turtles [
        let town out-opinion-neighbors set N_Neighbors town]]
if network_type = "lattice"
[nw:generate-lattice-2d turtles opinions 10 10 false
        ask turtles [
        let town out-opinion-neighbors set N_Neighbors town]]
if network_type = "ring"
[nw:generate-ring turtles opinions population
        ask turtles [
        let town out-opinion-neighbors set N_Neighbors town]]
if network_type = "star"
[nw:generate-star turtles opinions population
        ask turtles [
        let town out-opinion-neighbors set N_Neighbors town]]
if network_type = "wheel"
[nw:generate-wheel turtles opinions population
        ask turtles [
        let town out-opinion-neighbors set N_Neighbors town]]

ask turtles [
  while [any? other turtles-here] 
[let empty_patch one-of patches with [any? turtles-here = false] move-to empty_patch ]]

layout-spring turtles links 10 10 10

end
   
  


to START!
ask opinions [if evaluation < -1 [set evaluation -1] if evaluation > 1 [set evaluation 1]]

ask turtles [
  opinion_propagation
 ; partner_selection
 ; interaction
 ; forgetting
]

graphics


  tick
end

to graphics
  ask opinions [
  if evaluation < 0 [ set color blue] 
  if evaluation > 0 [set color red]]
  
  if Display_Mode = "Matrix" [
    foreach sort opinions [ask ? [let i [who] of end1 let j [who] of end2
      if evaluation < 0 [ask patch i j [set pcolor blue]]
      if evaluation > 0 [ask patch i j [set pcolor red]]]
       ]
  ]
end



to opinion_propagation
  if continuous-rewiring? = true [rewiring]
let partner find-partner
if partner = nobody [set partner one-of other turtles]
let i [who] of self
let j [who] of partner

if opinion i j = nobody [
create-opinion-to partner [set evaluation 0]]

if opinion j i = nobody [ask partner [
  create-opinion-to turtle i [set evaluation 0]]]
     
 if Display_Mode = "Matrix" [
  ask links [set hidden? true]
   ]
   

;[ask opinion i j [set evaluation 1]]
;ask partner [create-opinion-to myself [set evaluation 0]]

let aii [self-regard] of self
let aji [evaluation] of opinion j i
let aij [evaluation] of opinion i j
let ajj [self-regard] of partner



ask opinion i j [
 set intensity  1 / (1 + exp (-1 * ((aij - aii) / sigma)))  
]
let pij [intensity] of opinion i j

  
set self-regard aii + (pij * rho * (aji - aii + noise))
ask opinion i j [set evaluation aij + (pij * rho * (ajj - aij + noise))]

;;STEP 2:  REPUTATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
let no_gossip_list (list self partner) ;; list of agents that cannot be called upon
let t_list []
ask [out-opinion-neighbors] of self [set t_list fput self t_list]
;; creates a list of all neighbors

foreach no_gossip_list [
  set t_list remove ? t_list ]
;; removes self and partner from the list of neighbors of agent i

; not including 'partner', and also another -1 because we're dealing with repeats
;; thus if you have 2 neighbors, the repeat would be 0?  No, to iterate once, it must say 'repeat 1'- repeating 0 causes
;; operation to be skipped.
let lnk count out-link-neighbors - 1 
let k k_neighbors 
let rpt min (list lnk k)
if length t_list > 0 [
repeat rpt [
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
let q one-of t_list
set t_list remove q t_list 
 
 let q_i [who] of q
 let aiq [evaluation] of opinion i q_i
 
 if opinion j q_i = nobody [ask partner [create-opinion-to q]]
 
 let ajq [evaluation] of opinion j q_i
 ask partner [ask opinion j q_i [
 set evaluation evaluation + (pij * rho * (ajq - aiq + noise))

 ]
 ]

]
]
;;STEP 3:  VANITY DYNAMICS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

let w vanity
ask opinion i j [
  set evaluation evaluation + (w * (aji - aii + noise))]
;; in words, agent i's new evaluation of j is the old evaluation plus the difference between j's evaluation of i and i's self-evaluation 
end

to-report noise  ; not used here
let r random 2
ifelse r > 0 [set r 1][set r -1]
let p random-float noise_intensity
let fuzz p * r  ; -1 to +1
report fuzz
end



to setup-neighbors

    if Condition = "Global" AND network_type = "None" [set N_Neighbors Other Turtles]
    
    if Condition = "Local" AND network_type = "None" [
      let town min-n-of Influence_Range other turtles [distance myself]
      set N_Neighbors town]
    
  ;  if Condition = "Small Worlds" [
  ;    let town min-n-of Influence_Range other turtles [distance myself]
  ;    set N_Neighbors town
  ;    small-worlds]
      
end

to rewiring  ;small-worlds re-wiring formula
ifelse N_neighbors = 0 [] [ 
let a self
let N_list [] 
let h turtle-set turtles-on neighbors
let g turtle-set N_Neighbors
; ask N_Neighbors [set color yellow] 
ask N_Neighbors [

      ;; whether to rewire it or not?
      ifelse (random-float 1) < rewiring-probability
      [
      
      
      let b  (turtle-set a h g) ; a = self, original turtle; N_neighbors list here includes this turtle replacing itself with another random turtle
      let c one-of turtles
      while [member? c b = true] [set c one-of turtles]  ; keeps changing the turtle until it isn't itself or a neighbor
       
          ask a [set N_list fput c N_list]
          ;  set N_list replace-item (? - 1) N_list c
          ;show N_list
         ; ask c [set color brown]
            ]
      [ask a [set N_list fput myself N_list]] ;;myself or self?
]
        ;; must be ? - 1 to replace the correct turtle
   
   ask a [set N_Neighbors turtle-set N_list] ; must go back and ask original turtle to do this!

]
end
   
to-report find-partner
  ifelse N_Neighbors = 0 [let partner one-of other turtles report partner] [
let partner one-of N_Neighbors
  report partner
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
196
10
534
369
-1
-1
8.0
1
10
1
1
1
0
1
1
1
0
40
0
40
0
0
1
ticks
30.0

BUTTON
2
10
65
43
NIL
setup
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
65
10
120
43
NIL
START!
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
1
44
141
77
population
population
2
100
40
1
1
NIL
HORIZONTAL

SLIDER
2
124
150
157
Influence_Range
Influence_Range
1
population
4
1
1
NIL
HORIZONTAL

CHOOSER
0
79
138
124
Condition
Condition
"Global" "Local"
0

SLIDER
4
391
179
424
rewiring-probability
rewiring-probability
0
.5
0.1
.01
1
NIL
HORIZONTAL

SLIDER
-2
157
170
190
sigma
sigma
0.1
1
0.35
.05
1
NIL
HORIZONTAL

SLIDER
0
189
171
222
rho
rho
0.01
1
0.01
.01
1
NIL
HORIZONTAL

SLIDER
-1
222
171
255
vanity
vanity
0
1
0.3
.1
1
NIL
HORIZONTAL

SLIDER
-1
255
171
288
k_neighbors
k_neighbors
0
population - 2
5
1
1
NIL
HORIZONTAL

MONITOR
703
20
808
65
Average Opinion
mean [evaluation] of opinions * 100
5
1
11

MONITOR
704
66
830
111
Variance of Opinions
variance [evaluation] of opinions * 100
5
1
11

MONITOR
808
21
865
66
# Links
count links
0
1
11

SWITCH
4
359
181
392
continuous-rewiring?
continuous-rewiring?
1
1
-1000

SLIDER
0
287
172
320
noise_intensity
noise_intensity
0
1
0.2
.01
1
NIL
HORIZONTAL

TEXTBOX
10
342
219
370
Whether preset networks change.
9
0.0
1

CHOOSER
704
124
842
169
Model_Presets
Model_Presets
"None" "Equality" "Elite" "Hierarchy" "Dominance" "Crisis"
1

CHOOSER
706
172
844
217
Display_Mode
Display_Mode
"Matrix" "Links"
0

CHOOSER
707
219
887
264
network_type
network_type
"None" "preferential attachment" "random" "small world" "lattice" "ring" "star" "wheel"
0

@#$#@#$#@
## WHAT IS IT?

Deffuant, Guillaume, Timoteo Carletti, and Sylvie Huet. “The Leviathan Model: Absolute Dominance, Generalised Distrust, Small Worlds and Other Patterns Emerging from Combining Vanity with Opinion Propagation.” Journal of Artificial Societies and Social Simulation 16, no. 1 (2012): 5.   Available here:  http://jasss.soc.surrey.ac.uk/16/1/5.html

Here is a copy of the abstract:  "We propose an opinion dynamics model that combines processes of vanity and opinion propagation. The interactions take place between randomly chosen pairs. During an interaction, the agents propagate their opinions about themselves and about other people they know. Moreover, each individual is subject to vanity: if her interlocutor seems to value her highly, then she increases her opinion about this interlocutor. On the contrary she tends to decrease her opinion about those who seem to undervalue her. The combination of these dynamics with the hypothesis that the opinion propagation is more efficient when coming from highly valued individuals, leads to different patterns when varying the parameters. For instance, for some parameters the positive opinion links between individuals generate a small world network. In one of the patterns, absolute dominance of one agent alternates with a state of generalised distrust, where all agents have a very low opinion of all the others (including themselves)." 

Each agent has a list of opinions about herself and other agents, ranging from -1 to +1. In this version, the parameter "condition" establishes the potential partners of each agent:  global = all agents; local = agents within "influence_range"; and "small worlds" (see below).  

NOTE:  The article expresses the influence of j on i using the subscripts ij.  Here, I use the subscript ij to express the influence of i on j.  

ROUTINE:  
STEP 1:  Opinion Propagation.  
Ask turtles, create an out-link "opinion" from turtle i to turtle j, and vice-versa, Oij and Oji. The "propagation coefficient" is called "intensity" here and is an attribute of opinions.  The logistic equation is used.  Intensity = 1 / (1 + exp (-1 * ((aij - aii) / sigma))), where aij is the evaluation of opinion i of j, and aii is the "self-regard" of agent i for herself.  The basic idea expressed here is that "if i has a high opinion of j, then j is more influential."  "Self-regard" is a turtle attribute.  Sigma is a parameter which can be set in the interface.   

The influence of j on i for i's self-regard is expressed as follows:  
aii = aii + (intensityij * rho * (aji - aii + noise)), where aii is self-regard of agent i, and "rho" is a parameter 'ruling the importance of opinion propagation.'  Now that the self-regard of agent i is updated, the "evaluation" of agent j by agent i (aij) is calculated as follows:  
aij = aij + (intensityij * rho * (ajj - aij + noise)), where aij is the "evaluation" of opinion_ij, and ajj is the self-regard of agent j.   

STEP 2:  REPUTATION
A list is created for agent i of all of its out-opinion neighbors excluding itself and its current partner j.  The "Evaluation" of opinion_iq is recorded, where q is the randomly selected neighbor.  Then, the evaluation of opinion_iq is used to update or influence j's evaluation of q, opinion_jq, using the same equation above.  The parameter "k_neighbors" determines the number of other agents about which agent i gossips- i.e. the # of reputations of other turtles i communicates to j.  

STEP 3:  VANITY DYNAMICS.  
The idea is as follows:  "agents tend to reward the agents that value them more positively than they value themselves and to punish the ones that value them more negatively than they value themselves."  "Evaluation" of opinion ij is set as follows:
Eval_ij = Eval_ij + (w * (aji - aii + noise)), where "w" is the parameter "vanity."  In words, agent i's new evaluation of j is the old evaluation plus the difference between j's evaluation of i and i's self-evaluation, plus some noise.

Recap:
Intensityij = 1 / (1 + exp (-1 * ((aij - aii) / sigma)))
aii = aii + (intensityij * rho * (aji - aii + noise))
aij = aij + (intensityij * rho * (ajj - aij + noise))
aij = aij + (w * (aji - aii + noise))


## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

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
NetLogo 5.1.0
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
