extensions [matrix]
globals [
  average_certainty1 average_certainty1ap 
  average_certainty2 average_certainty2ap 
  average_certainty3 average_certainty3ap
  activity_average
  action_pairs
  system_order
  system_order_log
  system_order_mav10  ;; moving average 10
  system_order_mavM   ;; moving average set to 'memory'
  
  ]
turtles-own [activity_count]
directed-link-breed [actions action]
;undirected-link-breed [undirs undir]
actions-own [activity_matrix activity_history ee_freq ec_cert A_values AP_norm]

to setup
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks 
  set-default-shape turtles "circle"
  
  set action_pairs []
  set system_order_log []

Ifelse interaction = "Preferential Attachment" [SETUP_INTERACTION_PA] 
 [SETUP_INTERACTION]
 
 ask actions [
   let egoid [who] of end1
   let alterid [who] of end2
   if action alterid egoid = nobody [ask end2 [create-action-to turtle egoid]]
      ;;  THIS MAKES CONNECTIONS RECIPROCAL
] 
 
  ask actions [
  set activity_matrix matrix:make-constant possibilities possibilities .05
  set activity_history (list random possibilities)  ;; first messages randomly chosen
  set A_values n-values possibilities [0.01]
  set AP_norm n-values possibilities [0.01]
  ]
end

TO SETUP_INTERACTION
    crt population [
    setxy random-pxcor random-pycor
    set color grey
    while [any? other turtles-here]
  [ fd 1 ]
    ]
  
  If Interaction = "All Actions" [  ;; ALL TURTLES CONNECTED TO ALL OTHER TURTLES
  ask turtles[
   CREATE-actions-TO OTHER TURTLES [SET HIDDEN? true set color blue]
  ]
  ]
  
 If interaction = "All Neighbors" [  ;; EVERY TURTLE HAS AT LEAST ONE NEIGHBOR IN THIS SETUP
  
  ask turtles[
   let N_turtles other turtles in-radius Vision  ;;potential problem:  unreciprocated links, if vision varies.
   CREATE-actions-TO N_turtles [set hidden? false set color blue]
  ]
  
  ask turtles [
   let a self
   let ot other turtles
   let closest_t min-one-of ot [distance myself]
   set heading towards closest_t
   while [count my-out-actions < 1] [
     fd 1
     LET OTR other turtles in-radius Vision
     CREATE-actions-TO OTR [set hidden? false set color blue]
  ]

  ] 
 ]  
END

TO SETUP_INTERACTION_PA
  make-node nobody        ;; first node, unattached
  make-node turtle 0      ;; second node, attached to first node

while [count turtles < population] [
    ask actions [ set color blue ]
    make-node find-partner   
]

let top_dog max-one-of turtles [count my-in-actions]
follow top_dog
END


to make-node [old-node]
  crt 1 
  [set color grey
    if old-node != nobody
      [ create-action-to old-node [ set color blue ]
        ;; position the new node near its partner
        move-to old-node
        fd 8
      ]
  ]
end


;; This code is borrowed from Lottery Example (in the Code Examples
;; section of the Models Library).
;; The idea behind the code is a bit tricky to understand.
;; Basically we take the sum of the degrees (number of connections)
;; of the turtles, and that's how many "tickets" we have in our lottery.
;; Then we pick a random "ticket" (a random number).  Then we step
;; through the turtles to figure out which node holds the winning ticket.
to-report find-partner
  let total random-float sum [count out-action-neighbors] of turtles ;; choose a random number less than the total number of links
  let partner nobody
  ask turtles
  [
    let nc count out-action-neighbors  ;; the number of links this turtle has
    ;; if there's no winner yet...
    if partner = nobody
    [
      ifelse nc > total ;; if the number of links of turtle 1 is greater than the total number of links of all turtles, then...
        [ set partner self ]
        [ set total total - nc ]
    ]
  ]
  report partner
end


to go!
  tick
  ;; ALTERNATIVES:  1. ASK ACTIONS.  2. ASK TURTLES, 2a. ASK MY-OUT-ACTIONS. 2b. ASK ONE-OF MY-OUT-ACTIONS.
    ;;  DYADIC Communication setting:  asking incoming (re)action to reply.
    ;; after ego selects an action, alter will respond simultaneously, and then both will update their matrices.
    ;; in the current setup, there is no clear distinction between sender and receiver.  
    ;; NOTE::  THERE ARE TWO ACTIONS PER TICK.

set action_pairs [] ;; clear paired history every iteration.

   ask actions [
        let egoid [who] of end1
        let alterid [who] of end2
    EE
    ask action alterid egoid  [EE] 
    
    Update_matrix
   
    ask action alterid egoid [
      Update_matrix
      Update_system_order ;; this is placed here to avoid double counting
      ]
  ]

  ;show action_pairs
  Update_Plots
end


to EE
  set ee_freq []
        let egoid [who] of end1
        let alterid [who] of end2
  let m_received item 0 [activity_history] of action alterid egoid  ;; gets the latest message from my-in-action from alter
  let ego_react_list matrix:get-column activity_matrix m_received  ;; this gets the j column; given action j by alter, the total number of responses of ego (i's 1 --> n)
  let i_ee_total sum (ego_react_list)
  set ee_freq map [? / i_ee_total] ego_react_list


EC  
end

to EC
  
  let egoid [who] of end1
  let alterid [who] of end2
  let n possibilities
  let p 0
  let item_list []
  let entropy_i []
  set ec_cert []
  let alter_freq []
    
  repeat possibilities [
  let alter_react_list matrix:get-row activity_matrix p  
  ;; 
  let sum_alter sum alter_react_list
  set alter_freq map [? / sum_alter] alter_react_list
  let react_certainty alter_freq
  set react_certainty map [? * log ? n] alter_freq  ;; This gives entropy value for each item
  set entropy_i 1 + sum (react_certainty)  ;; this gives Certainty for each row(i.e. action i)
  ;; CERTAINTY TELLS US, GIVEN (I.E. FOREACH OF) MY ACTIONS, HOW CERTAIN AM I OF ALTER'S RESPONSE?
  ;; Ego reads its 'alter memory'; i.e. alter's alter_actions to ego.  Check my-in-alter_actions = alter_actions to me.
  set ec_cert lput entropy_i ec_cert
  set p p + 1]
  
  ; show ec_cert
  AV
end

to AV ;; activity values, combine EE and EC
  set A_values n-values possibilities [0.01] ;; clearing the activity_values for new use.
  ;; AV = f(EE, EC) = (1 - a)EE + aEC + (r/N)
  ;; below EE will be list1 (?1) and EC will be list2 (?2)
  let a alpha
  let r .01
  let n possibilities
  set A_values (map [(1 - a) * ?1 + a * ?2 + (r / N)] ee_freq ec_cert)
 ; show a_values
 ; show sum a_values
 ; let av_t map [? ^ 2] a_values
 ; show sum av_t
AP
end

to AP  ;SELECT ACTION!  Based on two selection_mechanisms (deterministic and probabilistic)
 let A_temp A_values
 let A_sum sum A_values
 set AP_norm map [? / A_sum] A_values ;; normalize the A_values and select the highest one.
 ;show sum AP_norm
 
 ;;1.  SELECTION 1
 if Selection = "Maximize" [
   let select max AP_norm ;; choosing the maximum AP value.  
 
;; The problem is that there may be actions with equal probability. "position" takes ONLY the first instance of that number in the list.
;; This might bias the results to actions with lower indexes.  This is esp. problematic considering that in the beginning, all actions
;; have the same probability.  Using "position" by itself would always cause the selection of item 0.  I have two methods for dealing with this:
 ;;METHOD 1  
   let ps position select AP_norm
   let item_listing (list ps)
   let n possibilities - 1 - ps   
   ;; eg. if there are 10 possibilities, 0 - 9, the first choice is item 2, there are 9-2 = 7 options left, or 10 - 1 - 2.
   ;; e.g. if there are 2 possibilities, 0 - 1, and the first option is item 1, there are 2 - 1 - 1 = 0 possibilities left, and won't repeat.
   let item_i ps
   repeat n [
   if item item_i AP_norm = select [
     set item_listing fput item_i item_listing
     set item_i item_i + 1]
   ]
   let select_i one-of item_listing
   set select_i select_i 
  ; show select_i
;;A modification of this is to start from item 0, rather than item ps.
;;For METHOD2 see version 11-26b-11 or earlier.  


;;MEMORY FUNCTION
     ifelse length activity_history > memory [
       let activity_memory fput select_i activity_history
       set activity_history sublist activity_memory 0 memory]
     [set activity_history fput select_i activity_history]
 ]
     
 ;; 2. SELECTION 2  ;; weighted probability, cumulative probability distribution
 if Selection = "Proportional" [
   let select_i -1
   set select_i -1
   let AP_pr map [? * 100] AP_norm
  ;show AP_pr ; show sum AP_pr
   let r random 100
   
   ;print " "
   ;write " random = " show r
   
   let item_list n-values possibilities [?]
   set item_list n-values possibilities [?]
  ;
   let r_value_new 0
   let r_item one-of item_list
   
   ;write "Probabilities = " write AP_pr print " "
   ;write "First selected action = " print r_item

   while [select_i = -1] [
       ifelse item r_item AP_pr >= r OR length item_list <= 1
       [set select_i r_item ;write " select_i = "write select_i print " "
         ]
      
      ; if length item_list <= 1 [set select_i r_item write " select_i = "write select_i print " "] 
    [

       ;;adding the probability of de-selected action and adding it to the next randomly selected action
       let r_value_old item r_item AP_pr
       set AP_pr replace-item r_item AP_pr 0  ;; not necessary, but insurance against selecting actions already selected
      
       ;; Now discarding old action as a possibility, and selecting a new one.
       set item_list remove r_item item_list  ;; must use 'remove' here instead of 'remove-item'
       set r_item one-of item_list ;; selecting next action i from available options, given in the item_list
       
       
       ;; Adding old i probability and adding it to the newly selected action i....
       set r_value_new item r_item AP_pr
       set AP_pr replace-item r_item AP_pr (r_value_old + r_value_new)
       let cum_prob item r_item AP_pr
       
  ;     write "next action = " write r_item write " "
  ;     write "prob. of action = " write cum_prob write " "
  ;     write "new prob. dist. = " write AP_pr print " "
    ]

          
     ]
     ifelse length activity_history > memory [
       let activity_memory fput select_i activity_history
       set activity_history sublist activity_memory 0 memory]
     [set activity_history fput select_i activity_history]

 ; show select_i
   
   ]

end

to Update_matrix
  
  let egoid [who] of end1
  let alterid [who] of end2
  let a [activity_history] of self
  let b [activity_history] of action alterid egoid
  let c (map [list ?1 ?2] a b) 
 ;; type "paired activity history " print c
  ;; this creates paired lists, item 1 of item ? = ego action; item 2 of item ? = alter action
  ;; EG.  [[0 1] [6 2] [5 7]], etc.  EAch corresponds to the action selected by ego and alter.
  ;; item 1 = i row of matrix; item 2 = j column of matrix; 
  ;; the algorithm will count each instance of a pair value, e.g. how many [0 1]'s, how many [6 2]'s?
  ;; Then place that count as the value of the i row, j column of its activity matrix.
  ;; E.g. if there are two [0 1]'s, then "2" will be placed in the activity matrix row 0, col 1.
 
 ifelse memory? = true [
   set activity_matrix matrix:make-constant possibilities possibilities .05
   let n 0
   let lac length activity_history - 1
   let mem memory - 1
   let runs min (list lac mem)
   repeat runs - 1 [
 let i item 0 item n c
 let j item 1 item n c
 let old_ij matrix:get activity_matrix i j
 set activity_matrix matrix:set-and-report activity_matrix i j (old_ij + 1)
 set n n + 1]]
[ let n 0
 let i item 0 item n c
 let j item 1 item n c
 let old_ij matrix:get activity_matrix i j
 set activity_matrix matrix:set-and-report activity_matrix i j (old_ij + 1)
]

; print matrix:pretty-print-text activity_matrix 
end

to Update_system_order
  let egoid [who] of end1
  let alterid [who] of end2
  let a [item 0 activity_history] of self
  let b [item 0 activity_history] of action alterid egoid
  let c (list a b)
  ;show c
  set action_pairs fput c action_pairs
  ;show action_pairs
end


to Update_Plots
update_plots_average_diff_activities  
update_plots_average_certainty
update_plots_system_order
update_network_plot
end

to update_plots_average_diff_activities
  ;;1. Average number of different activities selected during a time interval
  set-current-plot "Average Number of Different Activities"
  let t min (list (possibilities + 1) ticks) ; this is the time interval.  This is in contrast to the suggestion made in section 4.1
  ;let activity_count []
  ;let activity_totals []
  ask turtles [
    let activity_actions [] ;; sets up a list for each turtle, summing all action links
    ask my-out-actions [
    let TL length activity_history
    let tlmin min (list TL t)
    let activity_t sublist activity_history 0 tlmin  ;checks only the most recent activities
    let p 0
    repeat (TLmin - 1) [
    set activity_actions fput item p activity_t activity_actions 
    set p p + 1
    ]
    set activity_actions (sentence activity_actions) 
    ]
    ;show activity_actions
     let activity_combined (sentence activity_actions)
     let activity_list remove-duplicates activity_combined
    ;show activity_list
    ;show length activity_list
     set activity_count length activity_list ;;counts number of different activities
    ; write "activity count = " show activity_count
  ]
  ;show activity_totals
  let ac_av mean [activity_count] of turtles
  set activity_average (ac_av / possibilities) * 100  ;; PERCENTAGE OF TOTAL
 ; write "activity average = " print activity_average
  plot activity_average
end

to update_plots_average_certainty
;;#2 Average Certainty (6 methods)
  set-current-plot "Average Certainty"
  let entropy_list1av []
  let entropy_list1ap []
  let entropy_list2 []
  let entropy_list2ap []
  let entropy_list3 []
  let entropy_list3ap []
  let n possibilities

;;METHOD #1 AV
set-current-plot-pen "1av"
 ask turtles with [count my-links > 0] [
   let my_entropy_list []
   let my_entropy_Average []
 ask my-out-actions [   ;with [length A_values > 1] [
    ;let my_A_list [A_values] of self
    let av_certainty map [? * log ? n] a_values
    let entropy_av 1 + sum (av_certainty)
    ask end1 [set my_entropy_list fput entropy_av entropy_list1av]
 ]
    set my_entropy_average mean my_entropy_list
    set entropy_list1av fput my_entropy_average entropy_list1av
    ]
    set average_certainty1 mean entropy_list1av
    plot average_certainty1

;;METHOD #1 AP
 set-current-plot-pen "1ap"
 ask turtles with [count my-links > 0] [
   let my_entropy_list []
   let my_entropy_Average []
 ask my-out-actions [   
    let av_certainty map [? * log ? n] ap_norm
    let entropy_av 1 + sum (av_certainty)
    ask end1 [set my_entropy_list fput entropy_av entropy_list1ap]
 ]
    set my_entropy_average mean my_entropy_list
    set entropy_list1ap fput my_entropy_average entropy_list1ap
    ]
    set average_certainty1ap mean entropy_list1ap
    plot average_certainty1ap
    
;;METHOD #2 AV
set-current-plot-pen "2av"
 ask actions [   ;with [length A_values > 1] [
    ;let my_A_list [A_values] of self
    let av_certainty map [? * log ? n] a_values
    let entropy_av 1 + sum (av_certainty)
    set entropy_list2 fput entropy_av entropy_list2
    ]
    set average_certainty2 mean entropy_list2
    plot average_certainty2
    
;;METHOD #2 AP
set-current-plot-pen "2ap"
 ask actions [   
    let av_certainty map [? * log ? n] ap_norm
    let entropy_av 1 + sum (av_certainty)
    set entropy_list2ap fput entropy_av entropy_list2ap
    ]
    set average_certainty2ap mean entropy_list2ap
    plot average_certainty2ap


;;METHOD #3
set-current-plot-pen "3av"
    let av_list3 [a_values] of actions
    let av_list3_item item 0 av_list3 ;;  necessary correction, av_list3 gives a list within a list [[1 2 3...]] must get rid of meta-list
    let av_certainty3 []
    set av_certainty3 map [? * log ? n] av_list3_item
    let entropy_av 1 + sum (av_certainty3)
    set entropy_list3 fput entropy_av entropy_list3
    set average_certainty3 mean entropy_list3
    plot average_certainty3

set-current-plot-pen "3ap"
    let ap_list3 [a_values] of actions
    let ap_list3_item item 0 ap_list3 ;;  necessary correction, av_list3 gives a list within a list [[1 2 3...]] must get rid of meta-list
    let A_sum3 sum ap_list3_item
    let AP_norm_list map [? / A_sum3] ap_list3_item
    
    let ap_certainty3 []
    set ap_certainty3 map [? * log ? n] ap_norm_list
    let entropy_ap 1 + sum (ap_certainty3)
    set entropy_list3ap fput entropy_ap entropy_list3ap
    set average_certainty3ap mean entropy_list3ap
    plot average_certainty3ap
end


to update_plots_system_order
;;#3 SYSTEM-LEVEL ORDER
set-current-plot "Systems level order"
set system_order s_order action_pairs
if length system_order_log > 11 [
  let sublist10 sublist system_order_log 0 10  ; last item is excluded, so it includes 0-9
set system_order_mav10 mean sublist10
]
if length system_order_log > memory + 1 [
let sublistM sublist system_order_log 0 memory
set system_order_mavM mean sublistM
]
set-current-plot-pen "SO"
plot system_order

set-current-plot-pen "MAV10"
plot system_order_mav10

set-current-plot-pen "MAVM"
plot system_order_mavM

;;NOTES:  "Note that this measure makes sense only if the number of agents is large compared to the number of messages actively used. 
;; Sociologically we can interpret the value OP as a measure of integration....  In all our simulation experiments a high value of OP 
;;has indicated a closure of the emerging activity system. This means that the better the agents are able to predict the activities of 
;;other agents as reactions to their own activity selections, the more the communication system appears to be 
;;operationally closed, that is certain activities follow certain activities.

end

to-report s_order [a]
  let p possibilities
  let j_list []
  let l length a
  let r random l
  let r2 random 2
  let action_i item r2 item r a
  ;; count all action pairs with this action i (doesn't matter whether its item 0 or item 1)  
  ;; find the other actions (j) in these pairs.  Create frequency distribution of all reactions (j) to i.  
  ;; find the certainty of this action.  
  ;; also possible to do this for all actions, or a sample of them, and then find average certainty.
  foreach a [if member? action_i ? [
      let itemi position action_i ? ;; this is either 1 or 0
      let itemj 1 - itemi  ;; i.e. if i = 1, then j = 1-1=0; if i=0, then j=1-0=1.
      set j_list fput item itemj ? j_list]
  ]
  let s_o sort (sentence j_list)
  ;write "s_o list " print s_o
 ;; How do I figure out frequencies/probabilities for each of these possible responses?
 ;; e.g. 2 possibilities, 0 and 1.  i=0.  j_list = (1 0 0 0 0 0)   
 ;; e.g. n=6, 1=1/6, 0=5/6.  
 ;; Must count number of actions (or turtles) with i; this is also the number of actions (or turtles) who respond.
 ;; What about larger possibilities?  #i = 10, 0-9, j_list = (1 9 9 3 1 0)
 ;; e.g. 1=1/6; 2=0/6; 3=1/6; 4=0/6; 5=0/6 ... 9=2/6.  
 
 let M length s_o
 let j_matrix n-values p [.01]
 ;; need to count each digit, place them into the j_matrix, then get certainty
foreach s_o [set j_matrix replace-item ? j_matrix (item ? j_matrix + 1)]
;write "j_matrix " print j_matrix
  let j_matrix_norm map [? / M] j_matrix
;write "J_Matrix_Norm " print j_matrix_norm
  
  let s_cert map [? * log ? p] j_matrix_norm  ;; This gives entropy value for each item
  let entropy_i 1 + sum (s_cert) 
  set system_order entropy_i
  let slo system_order
  set system_order_log fput slo system_order_log
  
  report slo
end

to update_network_plot
  let max-degree max [count out-action-neighbors] of turtles

  set-current-plot "Degree Distribution"
  plot-pen-reset  ;; erase what we plotted before
  set-plot-x-range 1 (max-degree + 1)  ;; + 1 to make room for the width of the last bar
  histogram [count out-action-neighbors] of turtles

  ;; for this plot, the axes are logarithmic, so we can't
  ;; use "histogram-from"; we have to plot the points
  ;; ourselves one at a time
  set-current-plot "Degree Distribution (log-log)"
  plot-pen-reset  ;; erase what we plotted before
  ;; the way we create the network there is never a zero degree node,
  ;; so start plotting at degree one
  let degree 1
  while [degree <= max-degree]
  [
    let matches turtles with [count out-action-neighbors = degree]
    if any? matches
      [ plotxy log degree 10
               log (count matches) 10 ]
    set degree degree + 1
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
185
13
474
323
16
16
8.455
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
18
23
81
56
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

SLIDER
14
73
186
106
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
14
104
186
137
possibilities
possibilities
2
64
10
1
1
NIL
HORIZONTAL

BUTTON
86
23
149
56
NIL
go!
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
14
136
186
169
alpha
alpha
0
1
0.1
.1
1
NIL
HORIZONTAL

CHOOSER
14
233
152
278
Selection
Selection
"Maximize" "Proportional"
1

SLIDER
14
168
186
201
Memory
Memory
1
100
15
1
1
NIL
HORIZONTAL

PLOT
475
274
796
394
Average Number of Different Activities
NIL
%
0.0
10.0
1.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
475
142
681
271
Average Certainty
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"1av" 1.0 0 -2674135 true "" ""
"2av" 1.0 0 -16777216 true "" ""
"3av" 1.0 0 -10899396 true "" ""
"1ap" 1.0 0 -13345367 true "" ""
"2ap" 1.0 0 -2064490 true "" ""
"3ap" 1.0 0 -11221820 true "" ""

MONITOR
477
395
596
440
Average # of Activities
activity_average
2
1
11

MONITOR
680
142
739
187
#1 (AV)
Average_certainty1
2
1
11

MONITOR
681
184
740
229
#2 (AV)
Average_certainty2
2
1
11

MONITOR
681
228
740
273
#3 (AV)
Average_certainty3
2
1
11

MONITOR
738
142
792
187
#1 (AP)
Average_certainty1ap
2
1
11

MONITOR
739
184
793
229
#2 (AP)
Average_certainty2ap
2
1
11

MONITOR
740
228
793
273
#3 (AP)
Average_certainty3ap
2
1
11

PLOT
475
10
675
142
Systems level order
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"SO" 1.0 0 -2674135 true "" ""
"MAV10" 1.0 0 -16777216 true "" ""
"MAVM" 1.0 0 -13345367 true "" ""

MONITOR
675
10
732
55
SO
system_order
4
1
11

SWITCH
14
200
119
233
memory?
memory?
0
1
-1000

MONITOR
675
51
795
96
Moving Average 10
system_order_mav10
4
1
11

MONITOR
676
95
796
140
Moving Average Mem.
system_order_mavM
4
1
11

CHOOSER
14
278
153
323
Interaction
Interaction
"All Actions" "All Neighbors" "Preferential Attachment"
2

CHOOSER
14
323
152
368
Observation?
Observation?
"Off" "Neighbors" "All"
1

SLIDER
153
335
325
368
Vision
Vision
1
10
5
1
1
NIL
HORIZONTAL

PLOT
823
10
1023
160
Degree Distribution
degree
# of nodes
1.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" ""

PLOT
824
162
1024
312
Degree Distribution (log-log)
log(degree)
log(# of nodes)
0.0
0.3
0.0
0.3
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

MONITOR
824
314
967
359
Average Degree of Nodes
mean ([count my-out-links] of turtles)
5
1
11

MONITOR
823
357
967
402
Variance of Degree Nodes
variance ([count my-out-links] of turtles)
2
1
11

@#$#@#$#@
## WHAT IS IT?

This is a reconstruction of a study by Peter Dittrich, Thomas Kron and Wolfgang Banzhaf (2003) entitled "On the Scalability of Social Order: Modeling the Problem of Double and Multi Contingency Following Luhmann" available here:  
http://jasss.soc.surrey.ac.uk/6/1/3.html

Each agent has two means of reducing uncertainty ('motivations') when interacting with others: expectation-expectation (EE) and expectation-certainty (EC). 

EE refers to the efforts of an agent (�ego�)to make his/her responses to a particular message from alter predictable and hence consistent with ego�s prior responses.   EE refers to ego�s expectation of alter�s expectation of ego.   Agents can generate certainty by conforming to the expectations of others.   Because the expectations of others cannot be observed, however, ego must anticipate that others will expect them to respond as they have in the past.

EC refers to the efforts made by agents to reduce the uncertainty of alter�s responses.  Rather than basing one�s own behavior on the (anticipated) expectations of alter as in expectation-expectation, ego instead selects behaviors (or messages) that will elicit the most predictable responses.

Expectation-expectation orients behavior according to the query �How do I usually respond in these situations?�, whereas expectation-certainty orients behavior according to the query �How do you normally respond to my actions?  The two behavioral algorithms are sometimes conflicting.

The purpose of the model is to see under what conditions can expect 'social order' to emerge.  The primary variables of interest are 1. population size:  will social order appear when 'double contingency' is 'scaled up' to multiple contingencies?  2. The 'strategies' that the agents adopt:  will EE or EC be more successful at generating order?  There are three different measures of social order which appear in the graphs.

## HOW IT WORKS

Each agent selects an activity (from among a number of 'possibilities') in the following way:  
1.  For each activity, the agent determines how much it is expected by Agent B (EE)  
2.  The agent then determines how well the reaction of alter can be predicted by this activity.    
3.  The agent combines these two scores (calculated using Shannon's entropy formula) to arrive at an overall activity value.  The 'alpha' parameter sets the relative weight given to EE or EC.   A value of 0 means the agent wants to maximize EE, and 1 means the agent maximizes EC.  

The agent then repeats steps 1-3 for each possible activity (set by the 'possibilities' parameter) and selects an activity.   Alter does the same.   In this model, their interaction is simultaneous.  

As of now, all agents are tied to all other agents by directed links called 'actions.'  Each action has an 'activity matrix'- a square matrix with dimensions set to the number of possibilities.  So, if 'possibilities' is set to 2, this means that there are only two possible actions that an agent can choose.   Each agent is connected to every other by links called 'actions' and each action has an activity matrix.  The rows of the matrix represent ego's activities, and the columns represent alter's activities.  After ego and alter have both selected activities (i for ego, j for alter), then 1 is added to that cell in ego's matrix, and respectively in alter's own activity matrix.  In an interaction then, two directed links are activated, one from ego and one from alter, and the activity matrix of each is updated.  The activity matrix is essentially a counting tool.  It records and counts how many times a particular pairing of i and j have occurred.  

The activity matrices are updated in one of two ways.  If 'Memory?' is set to false, then this means that the agents have infinite memory!  Then, each action pair that occurs causes the corresponding cell in each of the activity matrices to be updated (for ego and alter's 'action' links to the other).  If 'Memory?' is turned on, then the extent of each agent's memory is set by the slider.  Action links also possess another variable called 'action_history'- a list which records all of the action pairs.  The length of this list is set to the memory (or infinite if the memory function is turned off).  When memory is turned on, the action matrices are essentially erased every turn and then the rebuilt from scratch using the data from the action history list, plugging into each cell the number of times each particular action pair has occurred.  

Each action link also possesses another matrix variable called 'A_values' which refers to each possible action's 'value'.  Two different entropy values are calculated for each row (from ego's point of view, representing each possible action), one for EE and one for EC.  For EE, ego is presented with the message received by alter (in this case the last action alter selected from their previous encounter), representing column j in ego's activity matrix for that action link to alter.  A sum of this column is taken, and then for each cell in this column, a frequency distribution is calculated.  This represents the percentage of times ego has responded with action i, to alter's action j.  For EC, something similar happens, but the frequency distribution is counted for each row, then each cell is turned into a corresponding entropy value according to the formula:  p*log p, to the base of n, where n is the number of possibilities.  These are stored in a list, and then summed, to get the entropy value for each row (i.e. action alternative):  entropy_i = 1 + the sum of the list.  The procedure for EC is a little more complicated than EE, because in EE the frequency distribution will suffice because we are only dealing with one column, the given action of alter, whereas with EC we have to deal with each column (action by alter), comparing each row by  converting each into cell into a frequency and then calculating the overall entropy for each row (i.e. action).  For example, if there are only two possible actions, then there would be a 2x2 matrix.  Given row i (e.g. action 1), ego looks at the distribution of j's responses.  So, if, for row 1, cell 1,1 has a count of 25, and cell 1,2 has a count of 1, then ego knows that the action pair 1,1 is far more frequent than 1,2.  Given action i, ego is pretty certain of alter's likely response.  Ego must then, however, compare this certainty with that of action 2.  If cell 1,1 consists of 10, but cell 1,2 has a count of 50 (assuming a very long memory!), then ego will be even more certain of action 2 and according to EC would select action 2 over that of action 1.  This comparison is accomplished by using entropy values.  

Finally, the selected action of the agent is determined by the equation:    
(1 - a)EE + aEC  
where 'a' is alpha.   Each action is given an activity value based on this equation.  The final action will be selected according to two criteria, given below.   

## HOW TO USE IT

The "selection" tab lets you choose either "Maximize" or "Proportional."  Maximize means that the agents will select the activity with the largest 'activity value', whereas 'proportional' means that each activity is given a probability, a normalized measure of the activity value above, and the likelihood that the agent will choose this activity is equal to the activity's respective probability.  

3 measures of order are given.  The most interesting, sociologically, is the 'system level order' described as follows:

"Sociologically we can interpret the value OP as a measure of integration....  In all our simulation experiments a high value of OP has indicated a closure of the emerging activity system. This means that the better the agents are able to predict the activities of other agents as reactions to their own activity selections, the more the communication system appears to be operationally closed, that is certain activities follow certain activities."

## EXTENDING THE MODEL

1. As of now, all agents are connected to all others.  The obsever asks each *action* (not each agent, but every action-link every turtle has to every other) to select an action.  This means that for each turn, every agent performs n number of actions, where n is the number of other agents and also the number of action-links it possesses.  It also means that for each tick, each turtle has performed n * 2 actions, because of double counting, each action sends and receives.   This is cumbersome and the model slows down as the population size increases and as the number of possible communications increase.  It would be interesting and relatively easy to constrain each agent's number of contacts, so that it can communicate with only its neighbors.  

2. Another possibility is that each agent can observe the interactions among other turtles. Essentially, the idea is to allow 'information, knowledge, or belief' of the social whole to influence behavior.   

3. Yet another possibility is to simulate 'language' as a cultural repository, since in this model, there is no distinction between utterance and information:  they collapse into each other.  This models communication at the 'analogical' or behavioral level, rather than the symbolic level.  The only referent is the body itself, making present through demonstration that which it represents.  There are no symbols.  Furthermore, unlike honeybees who can communicate symbolically, human communication is primarily indirect:  we repeat what we heard rather than what we saw personally.  A honeybee cannot transmit second-hand messages.  This would be an interesting addition to the model.  

4.  VARIABLE MEMORies.

5.  Complexity = possibilities exceed memory [i.e. 'information society' or 'triply helix'].   Test what affect varying the memory and possibilities has on system order. 

## NOTES

6-8-12:     
It should be possible to simulation referential communication:  agents communicate *with* one another *about* some external referent, which could of course include themselves or other agents.  


## CREDITS AND REFERENCES

Preferential attachment algorithm found in the "Preferential Attachment" model included with NetLogo. Copyright 2005 Uri Wilensky. 
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
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

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
<experiments>
  <experiment name="Interaction Test" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go!</go>
    <timeLimit steps="100"/>
    <metric>system_order_mav10</metric>
    <metric>Average_certainty1</metric>
    <metric>activity_average</metric>
    <enumeratedValueSet variable="alpha">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Observation?">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="possibilities">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Interaction">
      <value value="&quot;Preferential Attachment&quot;"/>
      <value value="&quot;All Actions&quot;"/>
      <value value="&quot;All Neighbors&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Selection">
      <value value="&quot;Proportional&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
