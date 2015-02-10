globals [
  combo
  replacement_rate ; set at 5%
  mutation_rate ;; 2%
  groups  ; just a list of numbers from 1 to N_group
  p_links ;; total possible links given n nodes
  p_networks ;; total possible networks from n nodes
  leaders ;; list of "leaders" in round, to serve as anchors for the layout-spring algorithm
  followers ;; everyone who isn't a leader
  
  avwithingroupvar ;; average within group variance
  betweenvar ;between-group variance

  variance_ratio ;; Fst = var(pj) / [Avvar(pij) + var(pj)]  = population-wide measure of the degree of non-randomness in who interacts with whom; aka *inbreeding coefficient*
  ;; = differences in the probability of being paired with an altruist conditional on being an altruist, and the probability of being paired with an altruist conditional on
  ;; being a non-altruist (defector).  
  ;; one expects cooperation to prevail when Fst > c / b.  
  
  pop_change ;; expected change in fraction of altruists
  
  ]
breed [cooperators cooperator]
breed [defectors defector]

undirected-link-breed [wlinks wlink]   ;within-group links
undirected-link-breed [blinks blink]  ;between-group links


;links-own [memories]
turtles-own [
  earnings ;; accumulated payoffs
  payoff
  N_Neighbors
  mycosts
  mybenefits
  t_threshold
  groupid ;; groups 1 --> N_groups
  group_coop ;; previous number of contributors/cooperators in the previous round, withint he group; 
  ;;should be the same for turtles of the same group
  sorted
  contrite  ;; number = 0 originally, if accidentalyl makes a mistake and defects, then set to 2, which means agent will cooperate next 2 rounds automatically.
  
  wingroupvar ;; within group variance of altruism
  
  p_i ;; probabilistic interaction; likelihood thta other turtles will interact with this turtle.. test
]

to setup
   clear-all
   reset-ticks
   
   set-default-shape turtles "face happy"
   set groups []
   set leaders []
   set followers []
   
   let g 1
   repeat N_groups [
     set groups lput g groups
     set g g + 1]
   
create-turtles (N_groups * size_n) [
   while [any? other turtles-here] [ let empty_patch one-of patches with [any? turtles-here = false] move-to empty_patch ]  
   set sorted false
   set groupid 0
]

  setup-neighbors
   
   ask turtles [
let cnt size_n ;; (i.e. n)
let t random cnt + 1  ;; i.e. between 0 and n, n = # in group.  Interesting to test differences using n and n-1 as 
;; used by Bowles and Gintis.  When using n, turtles with t = n will only cooperate if everybody cooperated in the previous
;; round, including oneself!  A turtle with t = n + 1 is a DEFECTOR, set below.  
set t_threshold t
set contrite 0
set breed cooperators set color yellow set size 1
set group_coop size_n ;; turtles act initially as if everybody in group cooperated last round     
   ]

let pop count turtles
let num_d (Percent_Defectors / 100) * pop
let new_defectors n-of num_d turtles
ask new_defectors [set breed defectors set shape "face sad" set size 1.5 set color red set t_threshold size_n + 1 set group_coop size_n]

;if GAME = "Pairwise Prisoners Dilemma Game" [ask turtles [create-links-with other turtles [set hidden? true]]]
;layout
end


to start  
if count turtles > 0 
[
  if GAME = "Public Goods Game" [Public_goods_game]
  if GAME = "Pairwise Prisoners Dilemma Game" [PD_pairing]

if Replicator_Dynamics? = true AND count cooperators > 0 AND count defectors > 0 [replicator_dynamics]

if count turtles > 0 [
if reassortment? = true [setup-neighbors]
  if starvation? = true [dying-turtles]
  if kill_defectors? = true [kill-d]  

ask turtles [if contrite? = true[ ; cooperate if defected in error from previous 1-2 rounds
  if contrite > 0 [set contrite contrite - 1]]]

update-plots
;layout
tick  
]
]
end


to setup-neighbors
    if GAME = "Public Goods Game" [assign_groups]
    if GAME = "Pairwise Prisoners Dilemma Game" [
      create-pairs
      ;layout
      ]    
end

to create-pairs
  ask turtles[
    if PD_assortment = "Random" [create-pairs-random]
    if PD_assortment = "Fixed" [create-pairs-fixed]]
end

to create-pairs-random
  set n_neighbors other turtles  ;; This will end up being proportional to the population distribution
end

to create-pairs-fixed
      
    ifelse [breed] of self = cooperators [
    let p Probability_of_Altruist_meeting_Altruist
    let r random 100
    ifelse r < p [set n_neighbors other cooperators][set n_neighbors defectors]]
    
    [let p Probability_of_Defector_meeting_Altruist
      let r random 100
      ifelse r < p [set n_neighbors cooperators] [set n_neighbors other defectors]]
end


to-report find-partner
let partner one-of N_Neighbors
if partner = nobody [set partner one-of turtles]
  report partner
end

to assign_groups
   ask turtles [setxy random-pxcor random-pycor
     while [any? other turtles-here] [ let empty_patch one-of patches with [any? turtles-here = false] move-to empty_patch ] 
     set groupid 0 ]
  let unassigned turtles
    ;; start with group 1 and loop to build each group
  let current 1
  while [any? unassigned]
  [
    ;; place a randomly chosen set of group-size turtles into the current
    ;; group. or, if there are less than group-size turtles left, place the
    ;; rest of the turtles in the current group.
    ask n-of (min (list size_n (count unassigned))) unassigned
      [ set groupid current 
        set n_neighbors other turtles with [groupid = current]
        ]
    ;; consider the next group.
    set current current + 1
    ;; remove grouped turtles from the pool of turtles to assign
    set unassigned unassigned with [groupid = 0]
  ]
  
ask turtles
  [
    ;; if i'm in a group, move towards "home" for my group
    if groupid != 0
      [ face get-home
        let p [neighbors] of get-home
        let area (patch-set get-home p)
        let my_patch one-of area
    move-to my_patch       
         ]
    ;; wiggle a little and always move forward, to make sure turtles don't all
    ;; pile up
    lt random 5
    rt random 5
    fd 1
  ]
end


;; Courtesy of Uri Wilensky:
;; figures out the home patch for a group. this looks complicated, but the
;; idea is simple. we just want to lay the groups out in a regular grid,
;; evenly spaced throughout the world. we want the grid to be square, so in
;; some cases not all the positions are filled.
to-report get-home ;; turtle procedure
  ;; calculate the minimum length of each side of our grid
  let side ceiling (sqrt (max [groupid] of turtles + 1))

  report patch
           ;; compute the x coordinate
           (round ((world-width / side) * (groupid mod side)
             + min-pxcor + int (world-width / (side * 2))))
           ;; compute the y coordinate
           (round ((world-height / side) * int (groupid / side)
             + min-pycor + int (world-height / (side * 2))))
end


to PD_pairing ;; Pairwise Prisoner's Dilemma Game
ask turtles [
let partner find-partner
;if partner = nobody [die] ;; dies if isolated!

let utility 0
let total_cost 0
let total_benefit 0
let personal_cost 0
ifelse member? self cooperators [set personal_cost cost] [set personal_cost 0]

   set total_cost total_cost + personal_cost
   ifelse member? partner cooperators [set total_benefit total_benefit + benefit ;; if partner is a cooperator, add benefit to 'totalbenefit' recorder.
     set utility utility + Benefit - personal_cost] ;; if neighbor is a cooperator, then add benefit...
   [set utility utility - personal_cost]  ;;if neighbor is a defector, then no benefit and subtract personal cost, if any...
 
   
   
 set payoff utility
 set earnings earnings + payoff
 set mycosts total_cost
 set mybenefits total_benefit
]
end

To Public_goods_game

   foreach groups [
     let group_share 0
     let thisgroup turtles with [groupid = ?]
    ask thisgroup [
      set mycosts 0
      let t group_coop
      let r random-float 1 ;; ERROR 
      
      ifelse contrite > 0 [ ;; if contrite > 0, then cooperate, unconditionally, otherwise...
       set breed cooperators set shape "face happy" set size 1 set color yellow ;; then cooperate
        set group_share group_share + Benefit
        set mycosts cost]
      ;;ERROR IS BOTH ERROR TOWARD COOPERATING AND ERROR TOWARD DEFECTING. 
      
     [ifelse t >= t_threshold  ;;if enough other group members contributed last round then COOPERATE.
      
      [ifelse r <= error_rate[   ;;  HERE, ERROR MEANS DEFECTING INSTEAD OF COOPERATING
        set breed defectors set shape "face sad" set size 1.5 set color red
        if contrite? = true [if r <= error_rate AND t >= t_threshold [set contrite 2 ]] 
        ]    
       
       [set breed cooperators set shape "face happy" set size 1 set color yellow ;; then cooperate
        set group_share group_share + Benefit
        set mycosts cost]]
     
     [ifelse r <= error_rate[  ;; HERE, ERROR MEANS COOPERATING INSTEAD OF DEFECTING
        set breed cooperators set shape "face happy" set size 1 set color yellow
        set group_share group_share + Benefit
        set mycosts cost]
     [set breed defectors set shape "face sad" set size 1.5 set color red]]]
     
    ]

 ask thisgroup [
     set payoff (group_share / (size_n - 1)) - mycosts  ;; payoff is b/n or b/(n-1) ??  
     set earnings earnings + payoff
     set group_coop count cooperators with [groupid = ?]        
 ]   
  ] 
end


to replicator_dynamics

if Replicator_options = "Relative Payoff" [Relative_Payoff]
if Replicator_options = "Variance Ratio" [Variance_Replicator]
if Replicator_options = "Replicator Equation" [Replicator_equation]
if Replicator_options = "Imitation" [Imitate]

end


;; probability of changing to another strategy is proportional to the difference between the *mean* payoffs for defectors and cooperators.  
;; turtle only can switch if the payoffs are larger for the other strategy.  
to Relative_payoff
 ifelse mean [payoff] of cooperators > mean [payoff] of defectors [
   ;; if cooperators making more payoff, then select the defectors to change
   ask defectors [let pr random-float 1
     if pr <= RD1 [delete_defectors]]]
 
 [  ;; if defectors making more, then ask cooperators to change
      ask cooperators [let pr random-float 1
if pr <= RD1 [delete_cooperators]]] 
end

to dying-turtles ;; turtles die if their earnings (or possibly their payoffs) get below zero.
  let consuming ((benefit - cost) / size_n) / 2
    ask turtles [
    set earnings earnings - consuming
    if earnings < 0 [die]]
;ask turtles [setup-neighbors] ;; must reset potential partners to avoid calling on dead turtles!
end



to kill-d
;;RULE  This just means that half the cost is deducted from earnings each round a turtle has no cooperators to cooperate with
  let consuming ((benefit - cost) / size_n) / 2
ask turtles [
  let g 0 
  ask N_neighbors [if member? self cooperators [set g g + 1]] 
  if g = 0 [set earnings earnings - consuming]
  ]
  
end

to Variance_replicator
  ;; based on variable 'popchange'  
  ;; According to Bowles and Gintis, the ratio of between-group variation (of altruists) to the total variation (which is the weighted-average within-group variation + the between-
  ;; group variation) must be greater than the ratio c/b for evolution to favor altruism.  
  ;; This ratio is also the probability of being paired with an altruist minus the probability of being paired with an altruist conditional on being an altruist or non-altruist,
  ;; respectively, or P(A|A) - P(A|N).  This seems more of a predictive tool than an algorithm to change the population.  

variances
let c count turtles
let new_agents pop_change * c ;;  
let c_r round new_agents
ifelse c_r > 0 [;; add more cooperators, kill defectors
  let c_d count defectors
  let c_min min (list c_r c_d)
  let deleted_defectors min-n-of c_min defectors [payoff]
  ask deleted_defectors [delete_defectors]]
;;add more defectors, kill cooperators
[let p_cr  c_r * -1  ;; convert to a positive number
  let c_c count cooperators
  let c_min min (list p_cr c_c)
    let deleted_cooperators min-n-of c_min cooperators [payoff]
          ask deleted_cooperators [delete_cooperators]
  ] 
  
end

to Replicator_equation    ;; let Pr(i) = the proportion of strategy i
  ;; let $i = the payoff of strategy i, since I can't write the pi symbol here.
  ;; the new proportion of strategy i in the population at time t+1 is given by:
  ;;  Pr(i)t+1 = Pr(i)$(i) / Sum of Weights
  ;; the weight for each strategy is given by the numerator
let expected_coop_change coop_pay - (count cooperators / count turtles)
let expected_defect_change coop_def - (count defectors / count turtles)
let c expected_coop_change * count turtles ;; gives the number of turtles that will be changed
let c_r round c ;rounded

ifelse c_r > 0 [ ;; add more cooperators, kill defectors
  let c_d count defectors
  let c_min min (list c_r c_d)
  let deleted_defectors min-n-of c_min defectors [payoff]
  ask deleted_defectors [delete_defectors]]

;;add more defectors, kill cooperators
[let p_cr  c_r * -1  ;; convert to a positive number
  let c_c count cooperators
  let c_min min (list p_cr c_c)
    let deleted_cooperators min-n-of c_min cooperators [payoff]
          ask deleted_cooperators [delete_cooperators]
  ] 
end

to imitate
  ;; this probably won't work, because its not clear how turtles will decide to imitate..  
  ;; if all agents imitate most successful agent in their group, then it creates immediate within-group homogeneity
  ;setting it initially to 4 closest agents, von Neuman, or Moore neighborhood, can't remember which.
  ask turtles [
    let other_a min-n-of 4 other turtles [distance self]
    let max_a max-one-of other_a [payoff]
    if [payoff] of max_a > [payoff] of self [
      ifelse [breed] of max_a = cooperators [delete_defectors] [delete_cooperators]
      set t_threshold [t_threshold] of max_a  ;; copying the threshold (for public goods games), not just the strategy!
      set group_coop t_threshold
    ]
    
  ]
end



to delete_defectors
 ;; hatch and die
let i [groupid] of self
hatch-cooperators 1 [
  set groupid i
  let mygroup other turtles with [groupid = i]
  ;create-wlinks-with mygroup
let cnt size_n ;; (i.e. n)
let t random cnt ;; t will be automatically between 0 and n and therefore not a defector
set t_threshold t
set color yellow set size .5
set group_coop t_threshold ;; will initially act as if just enough turtles have cooperated in previous round
       ]
  die   
    
end


to delete_cooperators
let i [groupid] of self
hatch-defectors 1 [
 set groupid i
 let mygroup other turtles with [groupid = i]
 ;create-wlinks-with mygroup
set t_threshold size_n + 1 ;; requires more turtles to cooperate than actually exist, therefore a defector
set shape "face sad" set size 1 set color red
set group_coop 0  ;; will initially act as if just enough turtles have cooperated in previous round
       ]
die 
end

to variances
  let jmin min [groupid] of turtles
  let jmax max [groupid] of turtles
  let j jmin
  let avgrouplist []
  let bgrouplist [] 
  
  repeat jmax [
    let grouplist []
    ask turtles with [groupid = j] [
      ifelse [breed] of self = cooperators [set grouplist fput 1 grouplist] [set grouplist fput 0 grouplist] ;; set 1 if altruist, 0 otherwise
    ]
    ask turtles with [groupid = j] [
      set wingroupvar variance grouplist
    ]
    set j j + 1
  ]
  
  let j2 min [groupid] of turtles
  
  repeat jmax [

    let num count turtles with [groupid = j2] 
    let numi count turtles with [groupid = j2 AND breed = cooperators] ;; counts number of cooperators
    let pj numi / num  ;; frequency of altruists in the group
    let f num / count turtles
    let gvar mean [wingroupvar] of turtles with [groupid = j2]  ;; every turtle in the group should have the same within group variance, but just in case, i take the average here.
    
    set avgrouplist fput (f * gvar) avgrouplist  
    set bgrouplist fput pj bgrouplist  
      set j2 j2 + 1
  ]
  
  
  set avwithingroupvar variance avgrouplist ;; reports the weighted-average within-group variance of altruists
  set betweenvar variance bgrouplist
  
  set variance_ratio betweenvar / (avwithingroupvar + betweenvar)

  p_change
  
end

to p_change ;; change in the fraction of altruists population in total population
  let b Benefit
  let c Cost
  let var_pj betweenvar
  let var_pij avwithingroupvar
  
  let p ((b - c) * var_pj) - (c * var_pij)
  set pop_change p 
  
end


to-report coop_pay;; proportion of cooperators*payoff of cooperators divided by sum of weighted payoffs
  let expected_coop (count cooperators / count turtles) * mean [payoff] of cooperators
  let expected_def (count defectors / count turtles) * mean [payoff] of defectors
  let total_payoff_c expected_coop / (expected_def + expected_coop)
  report total_payoff_c
  
end


to-report coop_def
  let expected_coop (count cooperators / count turtles) * mean [payoff] of cooperators
  let expected_def (count defectors / count turtles) * mean [payoff] of defectors
  let total_payoff_d expected_def / (expected_def + expected_coop)
  report total_payoff_d
  
end

to-report RD1 ;; veresion 3.  Qij = B($j - $i)
  ;; probability that agent will switch from less profitable strategy to more profitable strategy
  ;; B has to be sufficiently small so that Qij is always <= 1 !
  
  let B .1  ;; just trying random numbers
  let payoff_c mean [payoff] of cooperators
  let payoff_d mean [payoff] of defectors
  
  ifelse payoff_c > payoff_d [
    ;; probability that defectors will switch to cooperation...
    let Qij B * (payoff_c - payoff_d)
    report Qij]
  [ ;; probability that cooperators will switch to defection...
    let Qij B * (payoff_d - payoff_c)
    report Qij]
  
end

to-report RD2
  ;; Replicator Dynamics Version #2 for Cooperators
  ;;  Pr(i)t+1 = Pr(i) - a * Pr(i)(1-P)B($j - $i)
  
  let B .1  ;; randomly assigned
  
  let p_c (count cooperators / count turtles)  ;; proportion of turtles that are cooperators
  let p_d (count defectors / count turtles)  ;; proportion defectors
  let payoff_c mean [payoff] of cooperators
  let payoff_d mean [payoff] of defectors
  
  let expected_p p_c - ( p_c * (1 - p_d) * B * (payoff_d - payoff_c))
  report expected_p
  
end















@#$#@#$#@
GRAPHICS-WINDOW
177
10
597
451
20
20
10.0
1
10
1
1
1
0
0
0
1
-20
20
-20
20
0
0
1
ticks
60.0

BUTTON
7
10
70
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
7
45
70
78
NIL
start
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
5
186
177
219
Percent_Defectors
Percent_Defectors
round ((1 / size_n) * 100)
100
15
1
1
%
HORIZONTAL

SLIDER
5
216
177
249
Benefit
Benefit
0
size_n
8
1
1
NIL
HORIZONTAL

SLIDER
2
248
174
281
Cost
Cost
0
Benefit + 1
1
1
1
NIL
HORIZONTAL

MONITOR
950
268
1065
313
Average Payoffs
mean [payoff] of turtles
2
1
11

MONITOR
826
184
1069
229
Average Earnings (accumulated payoffs)
mean [earnings] of turtles
2
1
11

MONITOR
825
54
939
99
Cooperator Payoffs
mean [payoff] of cooperators
2
1
11

MONITOR
824
97
940
142
Cooperator Earnings
Mean [earnings] of cooperators
2
1
11

MONITOR
939
53
1053
98
Defector Payoffs
mean [payoff] of defectors
2
1
11

MONITOR
939
98
1054
143
Defector Earnings
mean [earnings] of defectors
2
1
11

MONITOR
825
227
954
272
Average # Links
mean [count N_neighbors] of turtles
2
1
11

SWITCH
-1
453
122
486
kill_defectors?
kill_defectors?
1
1
-1000

SLIDER
7
121
177
154
N_groups
N_groups
2
50
24
1
1
NIL
HORIZONTAL

SLIDER
7
154
177
187
size_n
size_n
2
20
14
1
1
NIL
HORIZONTAL

SLIDER
-3
356
175
389
error_rate
error_rate
0
.1
0.1
.01
1
NIL
HORIZONTAL

MONITOR
968
141
1070
186
Average Threshold
mean [t_threshold] of turtles
2
1
11

MONITOR
950
224
1065
269
Average Cooperation
mean [group_coop] of turtles
2
1
11

MONITOR
825
10
940
55
# Cooperators
count cooperators
0
1
11

MONITOR
938
10
1053
55
# Defectors
count defectors
0
1
11

MONITOR
825
141
897
186
% Defectors
(count defectors / count turtles) * 100
0
1
11

PLOT
596
149
825
290
Contributors vs Defectors
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"%D" 1.0 0 -16777216 true "" "plot (count defectors / (count turtles + 1)) * 100"
"%C" 1.0 0 -2674135 true "" "plot (count cooperators / (count turtles + 1)) * 100"

PLOT
595
10
827
151
Payoffs
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"if ticks > 1[\nset-plot-x-range min [payoff] of turtles max [payoff] of turtles\nset-plot-y-range 0 max [payoff] of turtles\nset-histogram-num-bars N_groups\n]" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [payoff] of turtles"

MONITOR
895
141
970
186
Payoff Range
max [payoff] of turtles - min [payoff] of turtles
5
1
11

MONITOR
1079
160
1197
205
Weight Cooperators
(count cooperators / count turtles) * mean [payoff] of cooperators
5
1
11

MONITOR
1197
160
1309
205
Weight Defectors
(count defectors / count turtles) * mean [payoff] of defectors
5
1
11

MONITOR
1077
26
1309
71
Expected Proportion of Cooperators (t+1)
coop_pay
5
1
11

MONITOR
1077
70
1309
115
Expected Proportion of Defectors (t+1)
coop_def
5
1
11

SWITCH
0
281
175
314
Replicator_Dynamics?
Replicator_Dynamics?
0
1
-1000

MONITOR
879
409
1067
454
Expected Cooperators version2
RD2
4
1
11

MONITOR
879
366
1066
411
Prob. of switching strategies
RD1
4
1
11

MONITOR
825
268
953
313
Threshold Variance
variance [t_threshold] of turtles
5
1
11

BUTTON
73
46
162
79
start-once
start
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
0
389
123
422
contrite?
contrite?
0
1
-1000

MONITOR
1088
349
1170
394
Variance Ratio
variance_ratio
5
1
11

MONITOR
1088
392
1306
437
Weighted-average within-group variance
avwithingroupvar
8
1
11

MONITOR
1171
347
1307
392
Between-Group Variance
betweenvar
5
1
11

BUTTON
1211
314
1305
347
NIL
variances
NIL
1
T
TURTLE
NIL
NIL
NIL
NIL
1

MONITOR
759
324
809
369
c / b
cost / benefit
5
1
11

CHOOSER
-1
311
176
356
Replicator_Options
Replicator_Options
"Replicator Equation" "Relative Payoff" "Variance Ratio" "Imitation"
1

MONITOR
1088
437
1306
482
Expcted Change in Fraction of Cooperators
pop_change
5
1
11

MONITOR
1078
115
1309
160
Expected change in Cooperators 2
(coop_pay - (count cooperators / count turtles))
5
1
11

MONITOR
1079
202
1198
247
# New Cooperators
(coop_pay - (count cooperators / count turtles)) * count turtles
5
1
11

SWITCH
-1
421
122
454
starvation?
starvation?
1
1
-1000

CHOOSER
594
322
761
367
PD_assortment
PD_assortment
"Random" "Fixed"
1

SWITCH
595
290
760
323
reassortment?
reassortment?
0
1
-1000

SLIDER
597
366
844
399
Probability_of_Altruist_meeting_Altruist
Probability_of_Altruist_meeting_Altruist
0
100
90
1
1
%
HORIZONTAL

SLIDER
597
398
844
431
Probability_of_Defector_meeting_Altruist
Probability_of_Defector_meeting_Altruist
0
100
50
1
1
%
HORIZONTAL

TEXTBOX
604
434
845
481
Select Probabilities for \"Fixed\" PD_assortment in Prisoners Dilemma Game. If P(A|A) - P(A|D) > c/b, cooperation will prevail.
11
0.0
1

CHOOSER
5
77
178
122
GAME
GAME
"Public Goods Game" "Pairwise Prisoners Dilemma Game"
0

TEXTBOX
1083
10
1233
28
Replicator Equation Predictions
11
0.0
1

TEXTBOX
882
337
1016
365
Relative Payoff Predictions (2 versions)
11
0.0
1

TEXTBOX
1088
316
1208
347
Variance Ratio Predictions (very slow!)
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

This model is derived from and inspired by Bowles and Gintis, "A Cooperative Species:  Human Reciprocity and its Evolution" (2013:  64-66).

This model contains two games:  an iterated Prisoner's dilemma game, and a Public Goods game consisting of N_groups each of n_size.  Generally speaking, the purpose of the model is to see under what conditions "cooperation" (or "altruism") will prevail given self-interested agents.  It involves the concepts of multi-level (group) selection and  inclusive fitness.  Each turtle faces a choice: CONTRIBUTE or NOT-CONTRIBUTE, or alternatively, COOPERATE or NOT-COOPERATE.  Initially, all turtles behave as if all turtles contributed the previous round, so all agents contribute except defectors.  Then, turtles are randomly placed in different groups each of size n, and play a public goods game with other members of the group.  After the first round, turtles only CONTRIBUTE if a 'sufficient' number of other turtles contributed in the previous round.  

A 'sufficient number' is determined by the Agent's Threshold (the t_threshold variable), which is randomly selected between 0 and size_n (the size of the group).  An agent with a threshold of 5, for example, would cooperate only if 5 other agents cooperated in its group from the previous round.  This simulates the idea of conditional preferences to comply with a social norm.  A percentage of initial defectors can be set.  A defector has a Threshold of size_n + 1, which means it will never cooperate because doing so would require more cooeprators in the group than are total agents in the group.  

In the Prisoner's Dilemma game, each agent interacts with one other agent.  This other agent is either selected randomly from the population (thus making the likelihood of interacting with a cooperator proportional to the percentage of cooperators) or else it is set as fixed, using the PD_assortment chooser.   If the odds are "fixed", then you must select the likelihoods that altruists/cooperators will interact with other altruists, and the likelihoods that other defectors/non-altruists will interact with cooperators.  

Other NOTES
C = Cost of cooperation to agent (self)
B = Benefit of cooperation to neighbor (other)
Agents only derive benefit if others in their group cooperate, and agents always pay cost of cooperating with others.  "Earnings" are the accumulated payoffs of each agent.

Below are some conditions in which cooperation is expected to prevail, although they are not tested explicitly in this model: 
p > c/b, where p is the probability of meeting again
q > c/b, where q is the probability that one's reputation will become known.
k < b/c, where k are the number of cooperating  neighbors 

## HOW IT WORKS
The model has 3 basic steps:
1.  Play the game (either Public Goods or Prisoners Dilemma) and collect payoffs.  	
2.  LEARN (adapt).   This is modeled by the Replicator_Dynamics switch and chooser.
3.  MOVE (i.e. "re-assort" into different groups, if this is switched on)

## REPLICATOR DYNAMICS AND OTHER SETTINGS
The Replicator_Dynamics algorithms change the distribution of cooperators and defectors in the population.  I included these primarily as predictive devices.  They are currently specified at the global level, and are not derived agent interactions.  Basically, they make predictions of expected proportions of cooperators and defectors, and if a greater number of cooperators is predicted, for example, that number of defectors with the smallest payoffs (or earnings- this can be changed) are replaced by cooperators.  

1  The "Replicator Equation" is as follows:
let Pr(i) = the proportion of strategy i, and let $i = the payoff of strategy i.
The 'weights' of each strategy i is given by:  Pr(i)t+1 = Pr(i)$(i), which becomes the numerator in a ratio giving us the new proportion of strategy i in the population at time t+1:   Pr(i)t+1 = Pr(i)$(i) / Sum of Weights for all strategies.
The "strategies" here are just 2:  cooperate or defect.  The idea of a replicator equation (or 'genetic algoritm') in general is that it combines two forces:  a) people blindly imitate the most prevalent or popular strategies, and b) people can also choose optimal strategies.  Here, the strategy is a function of both its proportion and relative payoff.

2  The "Relative Payoff" algorithm is derived from Bowles and Gintis.  The idea is that the probability of changing to another strategy is proportional to the difference between the *mean* payoffs for each strategy (cooperate and defect).  Agents will switch only if the mean payoff is larger for the other strategy.  Currently, I have programmed two versions of this, but am using only the first.  

(i).  Qij = B($j - $i), where Qij is the probability of individual switching from i to j, and $j and $i are the payoffs for strategy j and i, respectively.  In this model, each agent has probability Q of switching.  

(ii).  Pr(i)t+1 = Pr(i) - a * Pr(i)(1-P)B($j - $i), where a is set to unity.  This is a system-level prediction or change, not acting at the level of each agent.  Notice the paramters "a" and "B."  

The problem with both versions is that the parameter "B" has to be set seemingly arbitrarily, and set sufficiently small so that the respective probabilities are less than 1.

3  The "Variance Ratio" algorithm states that:  
Change in Pr(altruists) = (b-c)var(pj) - c * Avar(pij), where b = benefits, c = costs, var(pj) = between-group variance, and Avar(pij) = weighted-average within-group variance.
According to Bowles and Gintis, the ratio of between-group variation (of altruists) to the total variation (which is the weighted-average within-group variation + the between-group variation) must be greater than the ratio c/b for evolution to favor altruism.  This ratio is also equivalent to the probability of being paired with an altruist minus the probability of being paired with an altruist conditional on being an altruist or non-altruist, respectively, or P(A|A) - P(A|N).  The problem with utilizing this is that it slows down the model, almost to a halt.

4 Finally, there is the "Imitation" strategy.   Currently, this is very preliminary.  I adopt a simple approach, in which agents look to their 4 closest neighbors and copy the most successful strategy among them.  This leads to cascades of homogeneity due to the network topology.   

5 Another way you can model population changes more directly is by switching on "Starvation?" and/or "Kill_Defectors?"  The idea behind each is simple:  accumulated resources have to be consumed.   The consumption level is set to (B/size_n) / 2.   These are deducted from Earnings each round.  "Starvation" lets agents die if their total earnings go below zero.   "Kill_defectors" is based on the obvious recognition (usually ignored) that so-called defectors or free riders cannot survive in isolation.  Nobody can survive in isolation!  Therefore a society of completely non-altruistic, non-cooperative individuals is impossible (and maybe even an oxymoron).  The non-cooperators are not self sufficient.  Thus, this parameter says that defectors who cannot find other cooperators to interact with will die.  

6  Finally, "contrite" is taken from Bowles and Gintis.  It says that if an agent defects by mistake, then it will unconditionally cooperate the next two rounds.  

## THINGS TO TRY
The "Variance Ratio" prediction can be tested.   This replicator dynamic CANNOT be used in the Prisoner's Dilemma game since there are no groups.  Instead, set replicator dynamics to either "Replicator Equation" or "Relative Payoff" and switch to PD_assortment = "fixed" and set the probabilities of cooperators and non-cooperators interacting with cooperators (respectively) so that the difference between them is more than the ratio of c/b.  This is equivalent to forming groups.  Then run the Pairwise Prisoner's Dilemma Game.  

For the "Imitation" replicator dynamics Algorithm, the turtles are imitating/watching the 4 closest turtles to them, but interacting randomly with turtles across the whole social space. It may be interesting to see if the results are effected at all by the restricting context of observation to the context of action.

## CREDITS AND REFERENCES

Bowles and Gintis, "A Cooperative Species:  Human Reciprocity and its Evolution" (2013:  64-66).
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
NetLogo 5.0.5
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
