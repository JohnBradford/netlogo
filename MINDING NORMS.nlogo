extensions [matrix]
directed-link-breed [messages message]
messages-own [what how]
breed [NDs ND] ; ND = norm detectors
breed [SCs SC];  SC = Social Conformers
globals [
  actions_l  ; actions list
   actions_m  ; actions matrix
   forget
   t  ;; total actions
   ]
turtles-own [
  agenda ; sequence for each social setting; in this case, each setting visited no more than once, thus it is a PATH (not a trail or a random walk)
  time_allocation ; percentage time distributed across each setting, summing to number_of_ticks (100%)
  time_points ; list of ticks at which setting changes for agent, the running sum of time_allocation
  ; max_partners = # of potential interaction partners (may be constant or vary)
  setting ; attached to each agent, indicates which social setting the agent occupies at a given time
  setting_history
  counter ;; records the current item in time_points
  ;NOTE:  "SETTING" = THE CURRENT SITUATION OF THE TURTLE AT TIME T.  "SETTINGS" IS THE GLOBAL PARAMETER SPECIFYING HOW MANY TOTAL SETTINGS EXIST.
  action_history ; records actions of the agent - agents will observe the most recent action of agents in its particular setting
  norm_board     
  working_memory ;; = working memory; observed behaviors or messages of others are stored here until time "memory"
  threshold ;; SALIENCE; "frequency of the corresponding normative behaviors observed; i.e. the percentage of the compliant population" (p. 100) -
  ACTION
  ]

to setup
  
 clear-all
 reset-ticks
  
 set-default-shape turtles "person"
  
 let pop population
 let pop_nd round ((.01 * Percentage_ND) * population)
 create-NDs pop_nd
 let pop_sc population - (count NDs)
 create-SCs pop_sc
 
 set forget 1 / memory
 set t actions_per_setting + universal_actions
 
 ask NDs [set color blue]
 ask SCs [set color red]
  
 
  ask turtles [ 

  set size 1.5
 let close min-one-of other turtles [distance myself]
 while [distance close < 1]
[let r random 360
  set heading r
  fd 1
  set close min-one-of other turtles [distance myself]
  ]
  
  ]
setup_actions
  
setup_attributes
  
setup_WM  ; working memories
  
end

to setup_WM
  ;; row 1 = c_a (observed compliant actions)
  ;; row 2 = n (observed agents) in this model, always observes only 1, so updates across all columns + 1 per tick
  ;; row 3 = m (message strength); accumulates over time;  need to weight by time..
  ask nds [
    set working_memory matrix:make-constant 3 t 0
    set norm_board []
  ]
  
  
end

to setup_actions
  ;; creating ACTIONS
;; actions 0, 1, 2, etc for common/universal actions
;; actions 11, 12, 13, etc. for scenario 1;  21, 22, 23... for scenario 2, and so on.
;;  first, create a global list of possible actions.. then, have each agent choose one randomly and record it, depending on their situation.
let s settings
let hlist [] let b 11  
repeat s [let nlist n-values t [? + b] set hlist lput nlist hlist  set b b + 10] 
set actions_m matrix:from-row-list hlist

set actions_l []
let i 1
let i2 actions_per_setting 
repeat universal_actions [
let ulist n-values s [i]
matrix:set-column actions_m i2 ulist
set i i + 1
set i2 i2 + 1
]

let i3 1   ;; because procedure a_list below substracts 1
repeat s [
  let alist a_list i3
set actions_l lput  alist actions_l
set i3 i3 + 1
]

set actions_l reduce [(sentence ?1 ?2)] actions_l   
set actions_l remove-duplicates actions_l
set actions_l sort actions_l
;show actions_l
end

to setup_attributes
  ask turtles [set threshold random-float .7]  ;  thresholds are between 0 and 70%.  
  
  set_agenda
  set_time
  
end

to set_agenda
  
ask turtles [ 
  let s settings
  let s_list []
  let i 1
  while [length s_list < s] [set s_list lput i s_list set i i + 1] ;; creates a list 1 --> n, # settings  
set agenda []
while [length s_list > 0] [ 
let n one-of s_list
set s_list remove n s_list
set agenda fput n agenda ]
    ]

ask turtles [
  set setting item 0 agenda]
end

to set_time
 ;; must distribute available ticks to each social setting 
; here I need to distribute the ticks over s settings, creating a list "time_allocation"
; To do this, I go over each position in the list, deciding with 50-50 probability whether to add 1 or 0, until all of the ticks are gone. 
let s settings

ask turtles[
  set action_history []
  let n number_of_ticks 
  set time_allocation []  
  repeat s [set time_allocation fput 0 time_allocation]
  let i 0 ; item # in list   
  while [n > 0] [
      let iv item i time_allocation + 1
      let p random 2 ;  creates 0 or 1
      if p > 0 [set time_allocation replace-item i time_allocation iv        
        set n n - 1
             ]
      ifelse i >= (s - 1) [set i 0] [set i i + 1]     
  ]
  set time_points []
  set setting_history []
  set counter 0 ; item 0 in time_points
  set setting item counter agenda
  
    ;;setting random action corresponding to initial setting
  let row setting - 1 ;corresponds to row in actions matrix
  let action_p matrix:get-row actions_m row
  set action one-of action_p
  set action_history lput action action_history
]


ask turtles [
  let i 0
  repeat s - 1 [
  let new_list sublist time_allocation 0 (i + 1)
  let new_total sum new_list
  set time_points lput new_total time_points
  set i i + 1

  ]
  set time_points lput number_of_ticks time_points

]
end





to start
  ifelse ticks >= number_of_ticks [stop]
  
  [
    

move_to_group ; [code taken from "Grouping Turtles Example"]
interact    
set_setting    
 
   tick 
  ]
update_plots
end





to interact
  ask-concurrent turtles [
    ifelse breed = nds [nds_action] [scs_action]
  ]
    
end

to nds_action
  
 let s [setting] of self 
 let sd s - 1
 let n_update n-values t [1] ;; this creates a list [1 1 1] which I use to add to the second row (demoninator)
 
nds_action_update_denominator
nds_action_update_numerator
nds_action_update_messages
nds_action_setup_norm_board
nds_action_forgetting
nds_action_select
end

to-report alters [scs?]   ;; if scs? = 1, then possible partners = all turtles; if = 0, then only nds.
   let s [setting] of self 
   let partners nobody
   ifelse scs? = 1 [set partners other turtles with [setting = s]]
   [set partners other nds with [setting = s]]

  ifelse partners = nobody [report self] [report one-of partners]
  
end

to-report a_list [s] ;; reports the available actions in a setting ('situation')

  let sit s - 1
  report matrix:get-row actions_m sit
  
end

to-report m_strength  ;; may want to tweak
report random-in-range forget 1
  
end

to nds_action_update_denominator
  ;;updating denominator, row 1 (i.e. the second row)
  let update_d matrix:get-row working_memory 1  ;; get the values of the 'n' row making a list..
  let new_d map [? + 1] update_d
  matrix:set-row working_memory 1 new_d 
end
  
to nds_action_update_numerator  ;; can observe actions of ALL TURTLES (not just NDS)
  ;;updating numerator (row 0, i.e. first row) ;; observed action
  ;; must record the position of this action, from the actions_m, so we update the WM in the right column
 let s [setting] of self
; let partner alters
 let alist a_list s  ;reporter
 let c_a [action] of alters 1
 let p position c_a alist ;; gets the column position of action c_a from the actions_m matrix, and then uses
  ;; that same position to update the working_memory column, row 0. 
 
  ifelse member? c_a alist [  
  let old_value matrix:get working_memory 0 p
  let new_value old_value + 1
  matrix:set working_memory 0 p new_value
  ]
  [ ]  ;; if values aren't legal, then skip...
  
end
  
  
to nds_action_update_messages
  ;;updating messages, row 2; observed communications
  ;;agents with norms communicate messages!  w
  ;; right now, randomly assign arbitrary value to random column of row 2
  ;; UPDATE, RECEIVING MESSAGE REGARDING ACTION OBSERVED
 let s [setting] of self 
 let sd s - 1
 let partner alters 1  ;; 
  create-message-to partner [  ;; for ND's, alter is set to only other ND's to send a message to; SCs do not process messages.
   set what [action] of end1  ;;  setting the "WHAT" attribute as the action of the sender
   set how m_strength  ;; m_strength is a random variable 
 ];; in this case, turtle is SENDING MESSAGE about its own current action
 
  let r1 random t ;; 0 to t-1  ; random action column
  let r2 m_strength ;; THESE VALUES WILL BE RANDOM ONLY IF TURTLE HAS NO IN-MESSAGES
 
  if count my-in-messages > 0 [  ;; SELECTING an incoming message regarding action and updating working memory
    let my_m one-of my-in-messages
    let my_what [what] of my_m  ;action
    let my_how [how] of my_m ; m, strength of message
    if member? my_what a_list s[  ;; if the message is an about an action in the current setting...
      set r1 position my_what a_list s   ;; DOUBLE CHECK, reporter
      set r2 [how] of my_m
    ]
  
  let old_m matrix:get working_memory 2 r1
  ifelse old_m < 1 [
    let new_m old_m ^ 2 + r2
    matrix:set working_memory 2 r1 new_m]
  [ ]  ;; otherwise do nothing, leave as is if above 1.  
    ;matrix:set working_memory 2 r1 1]  ;; alternative:  setting values to 1 if not below 1
  ]
end

to nds_action_setup_norm_board
 let s [setting] of self 
 let sd s - 1

 ;; now, must calculate a new vector (row) that is (row 0) / (row 1), or v=c_a/n.  To do this, a new vector from each row must be created first.
 ;; Procedure, IF v > threshold AND m > 1, THEN store action as norm in "NORM_BOARD"
 
 let row0 matrix:get-row working_memory 0  ;; frequency
 let row1 matrix:get-row working_memory 1  ;; denominator (total cases)
 let row2 matrix:get-row working_memory 2  ;; message strength
 
 let freq (map / row0 row1)   ;; a new list, each item is c_a/n, for each action-  actions are recorded by their position in the list.
 foreach freq [if ? > threshold [
     let th_a position ? freq  ;; position of the action crossing the threshold value
     let p_a item th_a row2  ;; check the strength of this action
     if p_a > 1 [
       let new_norm matrix:get actions_m sd th_a 
       ;; records the action listed in the action_m matrix, in setting s (in row (s-1),) column th_a    
     ifelse member? new_norm norm_board [] [set norm_board fput new_norm norm_board  ;; if its new, record it as new norm
       set norm_board remove-duplicates norm_board  ;; clearning up
       set norm_board sort norm_board ;; cleaning up
       
       ]  
 ]]]
  

end

to nds_action_forgetting  ;; this is to reduce the strength of m over time by a constant factor
  ;;forgetting...
 
  let m_row matrix:get-row working_memory 2
  set m_row map [? - forget] m_row
  foreach m_row [if ? < 0 [let b position ? m_row set m_row replace-item b m_row 0]]
  
  matrix:set-row working_memory 2 m_row
  
end  
   

to nds_action_select
  let s [setting] of self
   let a s - 1 ;; = row for setting in actions_m matrix
  ;; prefers to select norm in given situation; if norm_board empty, nds act like scs; another possiblity is that they choose randomly
  ifelse empty? norm_board [scs_action]
  [
    let alist a_list s ;reporter
    let afilter filter [member? ? norm_board] alist  ;; this filters out all actions in the norm_board not appropriate for that setting
    
    if empty? afilter [set afilter alist]
  ;; choose afilter item with highest m score in working memory;  
  ;; step 1, find positions of each in actions_m (row s - 1)
  ;; step 2, record values for identical positions in row2 of working memory\
  ;; step 3, highest value is selected...  find position for this value again
  ;; step 4, record value (i.e. action) for same position in actions_m (row s -1)
  ;; choose norm with highest m in working memory if more than one relevant norm
  
  
   let wm matrix:get-row working_memory 2 
   let am matrix:get-row actions_m a
 
 ifelse length afilter > 1 [
    ;; e.g. actions 21 and 23 in setting 2 are in norm_board, how to choose between them?
    ;; procedure:  find highest m in row 2 of working_memory; record position and find corresponding action in actions_m (row s-1, col ?)
    ;; IF action(i) is member? of norm_board, then select action(i).  
    ;; IF NOT, then repeat... 

   let norm_positions []
  foreach afilter [let p position ? am set norm_positions fput p norm_positions]
  let wm_values []
  foreach sort norm_positions [let v item ? wm set wm_values fput v wm_values]
  let max_v max wm_values
  let max_p position max_v wm  ;;  be careful, if same values exist for multiple actions, then could run into problems
  let new_action item max_p am
  if member? new_action afilter [set action new_action]
   

  ]
  [
    set action one-of afilter
    ]
  ;set action 
  set action_history fput action action_history
    
  ]
  
  forgetting
end

to scs_action

  let s [setting] of self
  let my_action [action] of self
  let partners turtles with [setting = s]  ;including self
  let action_list []
  
 ; let a s - 1 ; corresponds to the row # with possible actions for that setting in the actions_matrix
  let alist a_list s
  let new_list [action] of partners
  let cfilter filter [member? ? new_list] alist  ;;VERY IMPORTANT!  This basically excludes all actions of partners that aren't allowed in that setting..  
  if empty? cfilter [set cfilter alist]
 ; let new_action modes [action] of partners
  let n_action one-of cfilter ;; chooses just one mode if a tie
  set action n_action
  set action_history fput action action_history
  
  forgetting

  
end

to forgetting
    
    if length action_history > memory [let i memory - 1 set action_history remove-item i action_history]    
  
end

to set_setting   ;; moving turtle around asynchronously from situation to situation
    ; must find item # in the time_points list correspondin to ticks
    ; if ticks > item 0, then go to item 1; if ticks > item 1, then go to item 2, and so on..  
    ; until we rearch the highest value in the list which is less than ticks
    ; then we record item #, and set setting = item i of agenda
    
    ; Example, turtle 0: agenda = [0 3 1 2]; time_allocation (out of 10) = [2 3 2 3]; time_points = [2 5 7]
    ; Suppose ticks = 8, then setting of turtle 0 will be 2.  Why?  Because ticks > item 2 on time_points, 
    ; which means that we set the agenda to item #3 on agenda.  Item 3 = 2.  Therefore, setting for turtle 0 = 2.

  ask turtles [
let ti item counter time_points
if ticks > ti [
  set counter counter + 1
  set setting item counter agenda
  
  ;; NEED TO RESET WORKING MEMORIES! 
  ;; NEED TO RESET MY-IN-MESSAGES:  in this model, communications are only allowed about actions available in the setting

  set working_memory matrix:make-constant 3 t 0
  ask my-in-messages [die]
]
  set setting_history lput setting setting_history
  
  ]
  
  
end

to move_to_group
  ask-concurrent turtles [move-to get-home
      ;; wiggle a little and always move forward, to make sure turtles don't all
    ;; pile up
    lt random 5
    rt random 5
    fd 1
  ]
end
  
;; figures out the home patch for a group. this looks complicated, but the
;; idea is simple. we just want to lay the groups out in a regular grid,
;; evenly spaced throughout the world. we want the grid to be square, so in
;; some cases not all the positions are filled.
to-report get-home ;; turtle procedure
  ;; calculate the minimum length of each side of our grid
  let side ceiling (sqrt (max [setting] of turtles + 1))

  report patch
           ;; compute the x coordinate
           (round ((world-width / side) * (setting mod side)
             + min-pxcor + int (world-width / (side * 2))))
           ;; compute the y coordinate
           (round ((world-height / side) * int (setting / side)
             + min-pycor + int (world-height / (side * 2))))
end

to-report random-in-range [low high]
  report low + random-float (high - low)
end  

to-report SC-freq  ;;report how many choose most popular action
  let c count SCs
  let newlist [] 
  foreach sort actions_l 
  [let v count SCs with [action = ?] 
    set newlist lput v newlist] 
   let SC_max max newlist ;; this is how many SCs choose the most popular action among them
   let SC_p position SC_max newlist ;; this identifies the position on the list of the most popular action among SCs
   let SC_action position SC_p actions_l
   report SC_max
  ; report SC_action
  
end

to-report SC_pop_action ;; most popular action among SCs
    let c count SCs
  let newlist [] 
  foreach sort actions_l 
  [let v count SCs with [action = ?] 
    set newlist lput v newlist] 
   let SC_max max newlist ;; this is how many SCs choose the most popular action among them
   let SC_p position SC_max newlist ;; this identifies the position on the list of the most popular action among SCs
   let SC_action item SC_p actions_l
   report SC_action
end

to-report ND-freq  ;;report how many choose most popular action
  let c count NDs
  let newlist [] 
  foreach sort actions_l 
  [let v count NDs with [action = ?] 
    set newlist lput v newlist] 
   let ND_max max newlist ;; this is how many SCs choose the most popular action among them
   let ND_p position ND_max newlist ;; this identifies the position on the list of the most popular action among SCs
   let ND_action position ND_p actions_l
   report ND_max
  ; report SC_action
  
end

to-report ND_pop_action ;; most popular action among SCs
    let c count NDs
  let newlist [] 
  foreach sort actions_l 
  [let v count NDs with [action = ?] 
    set newlist lput v newlist] 
   let ND_max max newlist ;; this is how many SCs choose the most popular action among them
   let ND_p position ND_max newlist ;; this identifies the position on the list of the most popular action among SCs
   let ND_action item ND_p actions_l
   report ND_action
end

to update_plots
  set-current-plot "Convergence Rate"
  set-current-plot-pen "social conformers"
  let c1 count SCs
  if c1 = 0 [set c1 1]
  let f SC-freq
  let prcnt_sc (f / c1) * 100
  plot prcnt_sc
  
  set-current-plot-pen "norm detectors"
  let c2 count NDs
  if c2 = 0 [set c2 1]
  let f2 ND-freq
  let prcnt_nd (f2 / c2) * 100
 plot prcnt_nd

end
  
@#$#@#$#@
GRAPHICS-WINDOW
210
10
649
470
16
16
13.0
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
26
71
89
104
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
93
70
156
103
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
26
113
198
146
population
population
2 * settings
500
100
1
1
NIL
HORIZONTAL

SLIDER
26
149
198
182
settings
settings
2
10
4
1
1
NIL
HORIZONTAL

SLIDER
26
182
198
215
Percentage_ND
Percentage_ND
0
100
50
1
1
NIL
HORIZONTAL

SLIDER
26
218
198
251
actions_per_setting
actions_per_setting
2
10
2
1
1
NIL
HORIZONTAL

SLIDER
26
254
198
287
universal_actions
universal_actions
0
5
1
1
1
NIL
HORIZONTAL

SWITCH
1071
60
1233
93
visit_each_setting?
visit_each_setting?
0
1
-1000

INPUTBOX
25
326
180
386
number_of_ticks
100
1
0
Number

SWITCH
1071
94
1288
127
variable_partners?
variable_partners?
1
1
-1000

BUTTON
92
22
169
55
go-once
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

SLIDER
26
288
198
321
memory
memory
1
25
10
1
1
NIL
HORIZONTAL

BUTTON
665
273
875
306
# turtles performing each action
foreach actions_l [output-type \"action \" output-type ?  output-type \" # turtles = \" output-print count turtles with [action = ?]]  
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
881
274
944
307
clear
clear-output
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
1072
128
1210
173
salience?
salience?
"absolute/local" "absolute/global" "relative/local" "relative/global"
0

TEXTBOX
1075
17
1225
51
Variables not yet implemented.
11
0.0
1

CHOOSER
1072
173
1210
218
observing?
observing?
"one at a time" "all at once"
1

PLOT
662
10
1014
160
Convergence Rate
Time
%
0.0
100.0
0.0
100.0
true
true
"" ""
PENS
"norm detectors" 1.0 0 -16777216 true "" ""
"social conformers" 1.0 0 -2674135 true "" ""

OUTPUT
664
312
954
468
12

MONITOR
664
165
775
210
# SCs converging
SC-freq
0
1
11

MONITOR
793
165
998
210
Most popular action chosen by SCs
SC_pop_action
0
1
11

MONITOR
794
209
1000
254
Most popular action chosen by NDs
ND_pop_action
0
1
11

MONITOR
664
212
776
257
# NDs converging
ND-freq
0
1
11

@#$#@#$#@
## WHAT IS IT?

This simulation compares how different types of agents (social conformers and norm detectors) converge on a particular action while interacting across multiple settings.  Social conformers adopt the most popular action in a given situation.  Norm detectors both observe the actions of other agents and also send and receive messages about those actions.  Norm detectors recognize an action as a norm if and only if:  (a) the observed compliance of an action (i.e. the % adopting the act in a social setting) exceeds their personal threshold, and (b) accumulated force of messages (i.e. the 'message strength') concerning that action exceeds 1.  Once an action is regarded as a norm for a given social setting, a Norm Detector will adopt it regardless of what other agents are doing, although it is possible for Norm Detectors to have multiple norms for a given setting.

This is a replication of the model from chapter 7 entitled "Hunting for Norms in Unpredictable Societies" in Minding Norms:  Mechanisms and Dynamics of Social Order in Agent Societies, Eds.  Rosaria Conte, Giulia Andrighetto, Marco Campenni
2014, Oxford University Press.

## HOW IT WORKS

In this model there are two types of agents:  SOCIAL CONFORMERS (SCs) and NORM DETECTORS (NDs).  Agents interact in different situations or scenarios, determined by the parameter "settings."  In the original model, there are 4 situations, each of which has 3 possible actions:  2 actions available (unique to) only that setting, and 1 action available in all settings.  In this model, the number of unique actions and universal actions are set by the parameters "actions_per_setting" and "universal_actions," respectively.    

Social conformers have no memory and adopt the most frequently chosen action by agents in their particular setting.  Norm detectors have memory and select their action based on a salient norm (See below).  All agents have the following attributes:

1. agenda = personal agenda (sequence of settings randomly chosen) 
2. time_allocation = time of performance in each scenario 
3. vision =  window of observation (capacity for observing and interacting with fixed number of agents)  
4. setting = which social setting the agent occupies at a given time; determines who the agent can interact with.
5.  Threshold = between 0 and 1 (salience); "frequency of the corresponding normative behaviors observed; i.e. the percentage of the compliant population" (p. 100) 

ND's receive input from TWO sources:  BEHAVIORS and MESSAGES.  Messages are directed links sent to and from other NDs with two attributes:  (1) the content (WHAT is said)- i.e. the action of the sender (who communicates to other agents via message links *about* that action), and (2) the means of conveying this content (HOW it is communicated).   The 'HOW' attribute refers to the strength of the message, labelled "m."   Varying message strengths are supposed to simulate different forms of persuasion. The original text discusses ASSERTIONS; REQUESTS; DEONTICS (evaluations of as good/acceptable and bad/unaccaptable); VALUATIONS- assertions about what is right or wrong; and finally, DEONTICS: "Every time a message containing a deontic (D) is rceived ... or a normative valuation (V) ... it will directly access the second layer of the architecture, giving rise to a candidate normative belief" (p. 99).  In this model, different kinds of normative messages are simulated by the varying strengths (HOWs) of those messages.  We improve upon the original model by using continuous values rather than discrete values.  For example, "assertions" are simulated by messages with low m values, whereas normative valuations are simulated by messages about actions with high m values.


ND ROUTINE:
I.  Update Messages

1a.  Send random out-message to another agent in the situation.  Strength (HOW) is set between "forget" and 1.  Forget = 1 / memory.  

1b.  Pick a random in-message (if any available), record the action (WHAT) and strength (HOW).  

1c.  Reset the 'Working Memory' matrix row2 (m, or message strength about each action).  The update is right now produced by the following equation:  "let new_m old_m ^ 2 + r2" ; which means the new 'strength' (i.e. salience or accumulation) of a message is equal to the previous strength of that action (0 to 1) squared plus the strength of the new message.   For example, agent i receives a message from agent j while they are in situation 2 about action 23 (third action in situation 2).  The strength (HOW) of this message is .3.  If the previous strength for action 23 was .5, the new strength will be:  .5^2+.3=.55.  The new strengths are recorded in row2 (i.e. third row) of the working_memory matrix.  

II. Setup Norm_Board:  IF v > threshold AND m > 1, THEN store action as norm in "NORM_BOARD".  Va = OBSERVED COMPLIANCE, i.e. the percentage of observed agents in situation s performing action a (row0/row1 of working_memory matrix). [See note in the 'details' about how OBSERVED COMPLIANCE is actually calcalated- there are many possibilities for this.]  NEXT, if the strength (m, row2 of working_memorymatrix) of the messages for this action are greater than 1, then it automatically becomes a new norm.  Thus, observing other agents perform an action is by itself insufficient to become a norm.   Agents must also receive messages about these actions.  Currently the threshold value for m (strength) is 1, but this should be varied.  Right now, m is the accumulated history of 'HOW's pertaining to a particular action, which updates the working_memory matrix as explained in the step above.  

III.  FORGETTING (NDs).  For NDs, in between setting up the norm_board and selecting an action is a procedure called "nds_action_forgetting." This reduces the strength of m (messages- row2 of working_memory matrix) over time by a constant factor.  In the future, an exponential function may be implemented.  Right now, m for every action is weakened by a factor of f, where f = 1/memory.  So, if memory is set to 5, m for each action is weakened by .2 each tick.   To compensate for this, reporter "m_strength" for new out-messages is set between f and 1.  So, if memory is only 2, m is weakened by .5 each turn, but every new message received is also some number between .5 and 1.  


IV. Choose action:  If agent has a norm_board, agent chooses the salient action (for that situation) from its norm_board with the highest "m" (strength) value. 

(5.)  Forgetting.  NDs have an "action_history" list which records their previous actions up to length "memory."  If the length exceeds their memory, then the most distant action is removed.  CURRENTLY, ACTION HISTORY IS JUST A RECORDING DEVICE AND HAS NO FUNCTIONALITY.  




## DETAILS

I.  HOW IS OBSERVED COMPLIANCE MEASURED?. In some ways, "salience" is presupposed, because only in certain settings as some actions possible.  Thus, we presume that only some actions are "salient" given the social setting!  Here 'salience' is given by the  "frequency of the corresponding normative behaviors observed; i.e. the percentage of the compliant population" (p. 100) This is ambiguous.  I can think of at least 4 ways that *observed compliance* might be modeled. First, we can use either absolute or relative frequencies of compliance- i.e. the threshold for agents may correspond to absolute numbers or to percentages.  Second, we can use frequencies of compliance for agents in that given setting or for all agents across all settings- which changes things radically! Cross-tabulating yields 4 possibilities.  

Here we opt for a 5th measure:  the relative compliance observed in a situation over the entirety of the duration. In other words, we calculate TOTAL COMPLIANCE as follows: let c_a = agents compliant to action a, and n = total agents *in a given setting*.  TC = [c_a (t0) + c_a (t1) + c_a (t2) ...] /  n (t0) + n (t1) + n (t2) ...  

Moreover, this can be done in two ways:  (1) each agent observes only 1 at a time, and so the relative frequencies are updated by 1 each tick; or (2) each agent observes all at once all of the compliant agents for each action in a given setting, and then the relative frequencies are updated en mass, using the same procedure as before.  We here choose 'one at a time' to keep the numbers low.

II. The statistic "most popular action" is a little misleading because it compares universal actions, which all turtles can perform, versus actions embedded in specific situations, which only a fraction of turtles can perform.  For example, if you check to see what actions turtles adopt initially, more of them will adopt one of the "universal actions" (e.g. action 1) only because that option is available to all turtles, whereas the other actions are only available as possible actions for the number of turtles in that specific setting.  


III.  The actions are labeled in the "actions_m" matrix and "actions_l" list as follows:  universal actions are labelled from 0 --> 9;  actions unique to setting 1 are labelled from 11 to 19; actions unique to setting 2 are labelled 21 to 29; actions in setting 3, 31 to 39, etc.  {Using matrices might have been an unnecessary complication, but it worked out nicely.  Each ND has a matrix called "working_memory."  The WORKING_MEMORY MATRIX has  3 rows and j columns.  Each column represents a possible action (in that given setting).  We keep track of which column (0 to j) and match it up with the position on the actions_m matrix and actions_l global lists.  Row 0 of the "working_memory" matrix is the number of agents observed performing action j.  Row 1 of the matrix is the total number of agents in that setting.  Row 0 is the numerator, and Row 1 is the denominator.  Row 0 / Row 1 = prnctg frequency of agents in a setting performing j action.  The WM is always reset when changing social settings! Row 2 (i.e. 3rd row) of the WM matrix is the message strength row.   

IV.  Finally, the interpretation given to this model can be challenged.  In the original model, NDs converge on the one 'universal action' available across all social situations.  Authors cite “standing in line” as an example, or “answering when called upon."   They are arguing that ND’s learn norms faster than SC’s, but the problem is obviously that they are just defining ‘norms’ as any action common to the multiple settings.  In real life, *universal actions and situation-specific actions are not mutually exclusive, but rather presuppose each other.*  In their model, if agents choose 'standing in line' the agents do so *instead* of doing whatever else they were going to do- i.e. agents stand in line instead of doing what they are waiting in line for, i.e. the action specified by the situation and which makes the situation unique in the first place.   Put another way, if all agents choose the universal action in every setting, then there are no longer multiple settings!  



## THINGS TO NOTICE
The main finding of this model is that agents capable of internalizing and memorizing salient social norms (norm "immergence" similar to second-order emergence or awareness) are better at converging on behaviors than are simple social conformers.  

This works for the conditions established in the original study, in which there are 4 situations with 2 possible unique actions each and 1 universal action.  But does this also hold true when there are many universal actions, zero universal actions, or many possible situation-specific actions? The results seem to critically depend upon the existence of 'universal actions' not specific to any particular situation.

## EXTENDING THE MODEL

PARTNER SELECTION:
Right now "messages" are randomly received- AGENTS COMMUNICATE TO OTHER randomly targeted agents their own actions at randomly varying strength (m).  Agents do not select the partners with whom they communicate.  Nor do agents communicate indirect, third-person information about others- rather, they only communicate their own actions.  It would be fascinating to see what happens when action and communication are differentiated.  


## NETLOGO FEATURES
Matrix.

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES
"Hunting for Norms in Unpredictable Societies" in Minding Norms:  Mechanisms and Dynamics of Social Order in Agent Societies, Eds.  Rosaria Conte, Giulia Andrighetto, Marco Campenni, 2014, Oxford University Press.
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
