; This model is specifically for the case of 7 agents
extensions [matrix]
globals [
  total-cases
  global-rule
  equilibria-upper-bound
  equilibria-lower-bound
  initial-equilibria
  lowest-point
  highest-point
  freq-of-rule-diffs
  majority-converged
  clusters
  token-pool
]
turtles-own [
  rule
  accept
  reject
  ignore
  turtle-tolerance
  current-rule
  test-case
  turtle-cases
  turtle-disposition
  class-result
  precedent-case
  checking-precedent?
  case-index
  prec-test-case
  temp-breed ;; keeps track of how the agent is functioning when precedent-deliberating
  how-many-rule-changes
]

breed [randos rando]
breed [peelers peeler]
breed [shelvers shelver]
breed [tol-changers tol-changer]
breed [rule-changers rule-changer]

to setup
  if outputs? = true [random-seed 232022]
  clear-all
  ;random-seed 1
  set global-rule []
  if determination != "none" and intuition-assignment != "no-nec-cond" [user-message "cannot have both a determination and an intuition-assignment. choose one or the other." stop]
  if determination = "none" [;; if intuition assignments needn't be handled on a turtle-by-turtle basis, then they are done in advance of turtle creation.
    set total-cases (ifelse-value
      intuition-assignment = "fixed-all-intuitive" [fixed-cases-I (possible-cases rule-length)]
      intuition-assignment = "fixed-some-intuitive" [fixed-cases-S (possible-cases rule-length)]
      intuition-assignment = "nec-cond" [primed-intuitions (possible-cases rule-length)]
      intuition-assignment = "no-nec-cond" [assign-intuitive-random (possible-cases rule-length)]
      [0])
    ifelse global-shuffle [set total-cases shuffle total-cases] [set total-cases total-cases]
  ]
  repeat rule-length [set global-rule fput random 2 global-rule] ;; creates the center case for all agents
  crt 7 [
    setxy random-xcor random-ycor
    if agent-type = "randos" [set breed randos]
    if agent-type = "peelers" [set breed peelers]
    if agent-type = "shelvers" [set breed shelvers]
    if agent-type = "tol-changers" [set breed tol-changers]
    if agent-type = "rule-changers" [set breed rule-changers]
    set test-case [] ;; this will be the case that an agent is considering
    set turtle-cases []
    set rule []
    ;;;;;;;;;;;;;;;;;;;;; assigning center cases;;;;;;;;;;;;;;;;;;;
    ifelse intuition-assignment = "fixed-all-intuitive" or intuition-assignment = "fixed-some-intuitive" [ ;; when intuitions are fixed for the whole population, center cases are handled on a turtle-by-turtle basis
      if rule-agreement = "all-same" [set rule [1 1 0 1 1]]
      if rule-agreement = "lucky" [
        set rule []
        let c 0
        while [c < rule-length] [
          set rule fput 1 rule
          set c c + 1
        ]
        let x random length rule
        set rule replace-item x rule 0
      ]
      if rule-agreement = "unlucky" [
        set rule []
        let c 0
        while [c < rule-length] [
          set rule fput 0 rule
          set c c + 1
        ]
        let x random rule-length
        let y random rule-length
        while [x = y] [set y random rule-length]
        set rule replace-item x rule 1
        set rule replace-item y rule 1
      ]
    ][ ;; this covers when intuition-assignment is "nec-condition" or "no-nec-condition"
      ifelse rule-shuffle [repeat rule-length [set rule fput random 2 rule]] [set rule global-rule]
    ]
;; setting the
    set current-rule [] ;; distinguishing between the agent's rule and its current rule. This helps avoid accepting changes that should be rejected (and vice versa).
    set accept []
    set reject []
    set ignore []
    set turtle-tolerance tolerance
    set class-result "XX"
    if determination = "none" [ ifelse agent-shuffle [set turtle-cases shuffle total-cases] [set turtle-cases total-cases]] ;; where turtle-cases is set
    set turtle-disposition disposition
    set checking-precedent? false
    set case-index []
    set prec-test-case [] ;; this is the hearer's copy of the case that the speaker has set as a precedent
    set temp-breed "xx"
    set how-many-rule-changes 0 ;; might be interesting to know how many times an agent changes its rule
  ]
  ;;;;;;;;;;;;; end of turtle-creation

  if intuition-assignment = "no-nec-cond" and determination = "bare" [ ;;setting intuition assignments on a turtle-by-turtle basis
    set token-pool bare-tokens
    do-determinations
    ask turtles [set turtle-cases shuffle sentence turtle-cases fill-out-cases turtle-cases]
  ]

  if intuition-assignment = "no-nec-cond" and determination = "max" [
    set token-pool max-tokens
    do-determinations
    ask turtles [set turtle-cases shuffle sentence turtle-cases fill-out-cases turtle-cases]
  ]

  layout-circle (sort turtles) 10
  make-network
  layout
  set equilibria-upper-bound upper-bound
  set initial-equilibria equilibria-upper-bound
  set clusters 0
  reset-ticks
end

to do-determinations
  while [length token-pool != 0][
    let a random length token-pool
    let b item a token-pool
    let turt one-of turtles
    let tc [turtle-cases] of turt
    if not member? b tc [ask turt [set turtle-cases fput b turtle-cases] ]
    if member? b tc [do-determinations]
    if not empty? token-pool [set token-pool remove-item a token-pool] ;;a little hacky but it works
    ]
end

to-report bare-tokens
  set token-pool []
  set token-pool (list ["IA" 1 1 1 1 1] ["IA" 1 1 1 1 0] ["IR" 1 1 1 0 0])
  let distribution-pool []
  repeat token-saturation [
    set distribution-pool fput token-pool distribution-pool]
  report reduce sentence distribution-pool
end

to-report max-tokens
  set token-pool []
  set token-pool fixed-cases-I (possible-cases rule-length)
  let distribution-pool []
  repeat token-saturation [
    set distribution-pool fput token-pool distribution-pool]
  report reduce sentence distribution-pool
end

to-report fill-out-cases [turtle-case-list]
  let tl remove-duplicates turtle-case-list
  if not empty? tl [ set tl map but-first tl]
  let remaining-cases filter [ s -> not member? s tl] possible-cases rule-length
  let nl []
  repeat length remaining-cases [set nl fput "NI" nl]
  let final (map fput nl remaining-cases)
  report final
end

;;;;;;; creating the network
to make-network
  if network-type = "empty" []
  if network-type = "complete" [
    ask turtles [
      create-links-with other turtles
    ]
  ]
  if network-type = "ring" [
    let n 0
    while [ n < count turtles ] [
      make-edge turtle n turtle ((n + 1) mod count turtles) "default"
      set n n + 1
    ]
  ]
  if network-type = "ring2" [
    let n 0
    while [ n < count turtles ] [
      ; make edges with the next two neighbors
      ; this makes a lattice with average degree of 4
      make-edge turtle n turtle ((n + 1) mod count turtles) "default"
      ; Make the neighbor's neighbor links curved
      make-edge turtle n turtle ((n + 2) mod count turtles) "default"
      set n n + 1
    ]
  ]
  if network-type = "random-1" [
    ask turtles [
      create-links-with n-of 1 other turtles
    ]
  ]
  if network-type = "random-2" [
    ask turtles [
      create-links-with n-of 2 other turtles
    ]
  ]
  if network-type = "random-3" [
    ask turtles [
      create-links-with n-of 3 other turtles
    ]
  ]
end

; Connects two nodes
to make-edge [ node-A node-B the-shape ]
  ask node-A [
    create-link-with node-B  [
      set shape the-shape
   ;   set rewired? false
    ]
  ]
end

;;;;;;;;;; universe of possible cases. generates a list of lists of 1's and 0's for a given list size
to-report possible-cases [howbig]
  let universe [[]]
  repeat howbig [
    let old-u universe
    let new-u []
    while [not empty? old-u] [
      let new-case1 item 0 old-u
      let new-case2 item 0 old-u
      set new-case1 fput 0 new-case1
      set new-case2 fput 1 new-case2
      set new-u fput new-case1 new-u
      set new-u fput new-case2 new-u
      set old-u remove-item 0 old-u
    ]
    set universe new-u
  ]
  report universe
end



to go
  if outputs? = true [print ""
    type "CURRENT TIMESTEP: " print ticks]
  ask turtles [
    set-rule ;; there's rule and current-rule. this sets current-rule to rule. every turtle has to do this first
    set prec-test-case [] ;; maybe redundant but at the start of each round turtles shouldn't have any cases they're considering for precedent
  ]
  ask turtles [
    clean-lists
    if outputs? = true [
      print ""
      show "I am going now"
      type "pre-test rule for turtle " type who type ": " print rule]
    ifelse not empty? turtle-cases [get-case-uni] [get-case-ignore]
    do-test
    if outputs? = true [
      type "case for turtle " type who type ": " print test-case
      type "post-test rule for turtle " type who type ": " print rule]
    if precedence? = true [
      ask link-neighbors [
        if rule != current-rule [set current-rule rule]
        if outputs? = true [
          type "now turtle " type who type" checks the precedent from " print [who] of myself
          type "rule for turtle " type who type ": " print rule]
        check-precedent ]
    ]
  ]
  check-high-and-low
  if length reduce sentence [turtle-cases] of turtles = 0 and length reduce sentence [ignore] of turtles = 0 [
    ask turtles [clean-lists]
    final-measures
 ;   ask turtles [show rule show accept]
    stop]
 ; ask turtles [show rule show accept]
  tick
end

to get-case-uni
    set test-case item 0 turtle-cases
    set turtle-cases remove-item 0 turtle-cases
end

to get-case-ignore
  ifelse not empty? ignore [
    set test-case item 0 ignore
    set test-case replace-item 0 test-case "NI"
    set ignore remove-item 0 ignore
  ][
    setxy xcor + .01 ycor + .01
  ] ;; some agents sort their lists before others, and this line gives them something to do in case they're waiting for the others
end

to-report initial-measure
  let temp-upper []
  ask turtles [
    let case-thr-pair [ ]
    set case-thr-pair fput turtle-tolerance case-thr-pair
    set case-thr-pair fput rule case-thr-pair
    set temp-upper fput case-thr-pair temp-upper
  ]
  set equilibria-upper-bound length remove-duplicates temp-upper
  report temp-upper
end

to final-measures
  set equilibria-lower-bound length remove-duplicates [rule] of turtles
  let temp-upper []
  ask turtles [
    let case-thr-pair [ ]
    set case-thr-pair fput turtle-tolerance case-thr-pair
    set case-thr-pair fput rule case-thr-pair
    set temp-upper fput case-thr-pair temp-upper
  ]
  set equilibria-upper-bound length remove-duplicates temp-upper
  set freq-of-rule-diffs [ ] ;here we're looking at how much "overlap" there is in rules by looking at hamming distances
  let n count turtles
  let i 0
  while [i < (n - 1)] [
    ask turtles with [who = i]  [
      ask turtles with [who > i] [
        let r [rule] of self
        let c [rule] of myself
        let h hamm-diffs-abs r c
      set freq-of-rule-diffs fput h freq-of-rule-diffs
      ]
    ]
    set i (i + 1)
  ]
  set majority-converged FALSE
  if length filter [j -> j < 1] freq-of-rule-diffs > 6 [set majority-converged TRUE] ;only works for the case of 7 agents
  if length filter [j -> j < 1] freq-of-rule-diffs = 6 and equilibria-lower-bound = 4 [set majority-converged TRUE] ; in 4 groups, it's possible for one group to have the majority in a total of 7 agents
;set clusters get-clusters
end

to-report to-decimal
  let final-l []
  foreach [rule] of turtles [l ->
    let u 0
    let p (length l - 1)
    while [p >= 0] [ ;; converting rules to decimal because they're easier to compare
      let n item ((length l - 1) - p) l
      set u (u + (n * (2 ^ p )))
      set p (p - 1)
    ]
    set final-l lput u final-l
  ]
  report final-l
end

to-report get-clusters [list-of-list] ;; outputs in order of frequency with item 0 indicating how many agents have the rule
  let f remove-duplicates list-of-list
  let final []
  let final-output []
  foreach list-of-list [ x ->
    ;set final lput x final
    set final lput (frequency x list-of-list) final]
  let output (map fput final list-of-list)
  set output remove-duplicates sort-by [ [a b ] -> first a > first b ] output
  foreach output [ o ->
    set final-output lput item 0 o final-output
    set final-output lput but-first o final-output]
  report output
end

to-report any-truth?
  let temp-list get-clusters [rule] of turtles
  let output 0
  foreach temp-list [l ->
    if (remove-duplicates but-first l) = [1] [
    set output first l]
  ]
  report output
end

to-report total-manymax-maxsize-matches
  let winner []
  let fixed-rule-list get-clusters [rule] of turtles
  let max-cluster-size max reduce sentence get-clusters [rule] of turtles
  let how-many-max-clusters 0 ;; hacky fix but gets the job done
  ifelse max-cluster-size = 1 [
    set how-many-max-clusters count turtles] [
    set how-many-max-clusters frequency max-cluster-size reduce sentence get-clusters [rule] of turtles]
  foreach fixed-rule-list [f ->
    if item 0 f = max-cluster-size [set winner lput but-first f winner]
  ]
  let rank-list []
  let rank 0
  foreach winner [ l ->
    set rank ((matchy-matchy  l [1 1 1 1 1]) * rule-length)
    set l fput rank l
    set rank-list lput l rank-list
    ;set rank-list lput l rank-list
  ] ;print rank-list
  let final-winner item 0 rank-list
  foreach rank-list [rl ->
    if first rl > first final-winner [set final-winner rl] ;print final-winner
  ]
  let number-of-matches first final-winner
  let total-clusters length get-clusters [rule] of turtles
  report (list total-clusters how-many-max-clusters max-cluster-size number-of-matches)

end

to-report upper-bound ;; convenience reporter for interface plot
  let temp-upper []
  ask turtles [
    let case-thr-pair [ ]
    set case-thr-pair fput turtle-tolerance case-thr-pair
    set case-thr-pair fput rule case-thr-pair
    set temp-upper fput case-thr-pair temp-upper
  ]
  report length remove-duplicates temp-upper
end

;;NI -- not intuitive, IA -- intuitive accept, IR -- intuitive reject
to-report assign-intuitive-random [all-cases]
  let temp []
  let NI-cases []
  let IA-cases []
  let IR-cases []
  foreach all-cases [ ac ->
    let value random 100
    ifelse value >= I-NI-ratio [
      set ac fput "NI" ac
      set NI-cases fput ac NI-cases  ][
      set ac fput one-of ["IA" "IR"] ac
      ifelse item 0 ac = "IA" [set IA-cases fput ac IA-cases][set IR-cases fput ac IR-cases]
    ]
  ]
  report reduce sentence (list IA-cases NI-cases IR-cases)
end

to-report primed-intuitions [all-cases] ;; strategy is to generate a list of intuition labels and then assign them to strings meeting particular conditions
  let temp []
  let tempIA []
  let tempIR []
  let tempNI []
  repeat round ((I-NI-ratio / 100) * (2 ^ rule-length)) / 2 [set tempIA fput "IA" tempIA] ;; IA's to assign
  repeat round ((I-NI-ratio / 100) * (2 ^ rule-length)) / 2 [set tempIR fput "IR" tempIR] ;; IR's to assign
  let tl (length tempIA + length tempIR)
  repeat (2 ^ rule-length) - tl [set tempNI fput "NI" tempNI] ;; NI's to assign
  let subIA [] ;; to pick out strings to be tagged as IA
  repeat cond [set subIA lput 1 subIA]
  let subIR [] ;; to pick out strings to be tagged as IR
  repeat cond [set subIR lput 0 subIR]
  foreach all-cases [ac ->
    if (subIA = (sublist ac (length ac - cond) (length ac))) and not empty? tempIA [set ac fput "IA" ac set tempIA but-first tempIA]
    if (subIR = (sublist ac (length ac - cond) (length ac))) and not empty? tempIR [set ac fput "IR" ac set tempIR but-first tempIR]
    if item 0 ac != "IR" and item 0 ac != "IA" [set ac fput "NI" ac ]
    set temp fput ac temp]
  report temp
end

to-report fixed-cases-I [all-cases]
  let temp []
  foreach all-cases [ac ->
    ifelse sum ac >= 4 [set ac fput "IA" ac] [set ac fput "IR" ac]
    set temp fput ac temp
  ]
  report temp
end

to-report fixed-cases-S [all-cases]
  let temp []
  foreach all-cases [ac ->
    if sum ac >= 4 [set ac fput "IA" ac]
    if sum ac <= 1 [set ac fput "IR" ac]
    if sum ac = 2 or sum ac = 3 [set ac fput "NI" ac]
    set temp fput ac temp
  ]
  report temp
end

to set-rule
  set current-rule rule
end

to do-test
  ifelse ((item 0 test-case) = "NI") [check-non-intuitive-case ] [check-intuitive-case]
end

to-report matchy-matchy [r c]
  let z (map [[i j] -> i = j] r c) ;; get matches between rule and case
  report (length (filter [i -> i = true] z) / length r) ;; how many matches out of total number of possible matches
end

to-report hamm-diffs-abs [r c]
  let z (map [[i j] -> i = j] r c) ;; get matches between rule and case (or two cases, whatever)
  report (length (filter [i -> i = false] z)) ;; how many differences
end

to check-non-intuitive-case
  ifelse matchy-matchy current-rule but-first test-case >= (1 - turtle-tolerance) [ ;;accept or reject relative to tolerance
    set accept fput test-case accept
    set class-result "EA" ] [
    set reject fput test-case reject
    set class-result "ER" ]
end

to check-intuitive-case
  if checking-precedent? = false [ ;; this checks the case against the rule for classifying not empty? test-case and
    ifelse item 0 test-case = "IA"  [
      ifelse matchy-matchy current-rule but-first test-case >= (1 - turtle-tolerance) [
        ;show rule
        ;show current-rule
        ;show test-case
        ;show matchy-matchy current-rule but-first test-case >= (1 - turtle-tolerance)
        set accept fput test-case accept
        set class-result "EA"
      ][
        deliberate
      ]
    ][
      ifelse matchy-matchy current-rule but-first test-case < (1 - turtle-tolerance)  [
        set reject fput test-case reject
        set class-result "ER"
      ][
        deliberate
      ]
    ]
  ]
end

to deliberate ;; peelers, shelvers, randos have equal chances of picking tol-changers or rule-changers for their precedent-del disposition
  if breed = peelers [
    ifelse random 100 < turtle-disposition  [ peel ] [pick-one ]
  ]
  if breed = shelvers [
    ifelse random 100 < turtle-disposition [ do-ignore ] [ pick-one ]
  ]
  if breed = tol-changers [
    ifelse random 100 < turtle-disposition [ change-tolerance ] [ pick-one ]
  ]
  if breed = rule-changers [
    ifelse random 100 < turtle-disposition [change-rule ] [pick-one ]
  ]
  if breed = randos [ pick-one ]
end

to peel
  ifelse checking-precedent? = false [
    set test-case but-first test-case
    set test-case fput "NI" test-case
    check-non-intuitive-case
  ][
     precedent-deliberate
  ]
end

to do-ignore
  let temp-case []
  ifelse checking-precedent? = false [set temp-case test-case] [set temp-case prec-test-case]
  set ignore fput temp-case ignore
end

to change-tolerance ;; strategy: check how much turtle-tolerance needs to change in order to accept/reject test-case and then see if accept/reject lists change with new turtle-tolerance
  let temp-case []
  ifelse checking-precedent? = false [set temp-case test-case] [set temp-case prec-test-case]
  ifelse item 0 temp-case = "IA" [
    let needed-tol precision (1 - (matchy-matchy current-rule but-first temp-case)) 4 ;; this is the turtle-tolerance needed to accept the test case.
    let temp-reject item 1 (check-cases reject current-rule needed-tol) ;; these are the cases from the reject bin that are still rejected with the new tolerance level
    ifelse length temp-reject = length reject [ ;; if these lists are the same length
      set turtle-tolerance needed-tol ;; change the tolerance to the needed tolerance
      if checking-precedent? = false [
        set accept fput test-case accept
        set class-result "EA"
      ]
    ][
      if checking-precedent? = false [
       set ignore fput test-case ignore ]]
  ][
    let needed-tol precision ((1 - (matchy-matchy current-rule but-first temp-case)) - 0.01) 4
    let temp-accept item 0 (check-cases accept current-rule needed-tol)
    ifelse length temp-accept = length accept [
      set turtle-tolerance needed-tol
      if checking-precedent? = false [
        set reject fput test-case reject
        set class-result "ER"
      ]
    ][
      if checking-precedent? = false [
        set ignore fput test-case ignore
      ]
    ]
  ]

  if turtle-tolerance < 0 [set turtle-tolerance 0]
  if turtle-tolerance > 1 [set turtle-tolerance 1]
end

to change-rule;; strategy is to find how many sites need changing and then choosing sites to be changed
  let temp-case []
  ifelse checking-precedent? = false [set temp-case test-case] [set temp-case prec-test-case] ;; sets temp-case to the actual testing case or to the case to the case to check for precedent

  let gap precision ((1 - turtle-tolerance) - (matchy-matchy but-first temp-case rule)) 2 ;; find the gap between my threshold and the rule/case match.
  let needs-changing 0
  ifelse gap > 0 [ set needs-changing ceiling (length current-rule * gap)] [set needs-changing abs (floor (length current-rule * gap) - 1)] ;; identifies number of sites needing changing

  if item 0 temp-case = "IA" [
    let a current-rule
    let b but-first temp-case
    let zz (map [[i j] -> i = j] a b ) ;; find where there are matches and misses
    let locations list-locations false zz  ;; get the indices of the misses
    let spot []
    if needs-changing > length locations [set needs-changing length locations]
    ifelse coordinated-change [set spot sublist locations 0 needs-changing] [set spot up-to-n-of needs-changing locations] ;; set places to change the rule
    foreach spot [s ->
      ifelse item s current-rule = 0 [
        set current-rule remove-item s current-rule
        set current-rule insert-item s current-rule 1 ] [
        set current-rule remove-item s current-rule
        set current-rule insert-item s current-rule 0]
    ]
    let temp-accept item 0 (check-cases accept current-rule turtle-tolerance) ;; these are the cases from the accept bin that are still accepted with the new rule
    ifelse length temp-accept = length accept ;; if these lists are the same length
    [
      set rule current-rule ;; adopt the proposed rule change
      set how-many-rule-changes how-many-rule-changes + 1
      if checking-precedent? = false
      [
        set accept fput test-case accept ;; classify the result if not checking precedent
        set class-result "EA" ;; and set the result so other know how the agent classified it
      ]
    ][
      set current-rule rule
      if checking-precedent? = false [
        set ignore fput test-case ignore
      ]
    ]
  ]
  if item 0 temp-case = "IR"  [
    let a current-rule
    let b but-first temp-case
    let zz (map [[i j] -> i = j] a b ) ;; find where there's no match
    let locations list-locations true zz  ;; get the indices of the matches
    let spot []
    if needs-changing > length locations [set needs-changing length locations]
    ifelse coordinated-change [set spot sublist locations 0 needs-changing] [set spot up-to-n-of needs-changing locations]
    foreach spot [s ->
      ifelse item s current-rule = 0 [
        set current-rule remove-item s current-rule
        set current-rule insert-item s current-rule 1 ] [
        set current-rule remove-item s current-rule
        set current-rule insert-item s current-rule 0]
    ]
    let temp-reject item 1 (check-cases reject current-rule turtle-tolerance)
    ifelse length temp-reject = length reject [
      set rule current-rule
      set how-many-rule-changes how-many-rule-changes + 1
        if checking-precedent? = false [
        set reject fput test-case reject
        set class-result "ER"
      ]
    ][
      if checking-precedent? = false [
      set ignore fput test-case ignore
        set class-result "IG"
      ]
    ]
  ]
end

to pick-one
  let p random 4
  if p = 0 [ifelse breed != peelers [ peel ] [pick-one]]
  if p = 1 [ifelse breed != shelvers [ do-ignore] [pick-one]]
  if p = 2 [ifelse breed != tol-changers [ change-tolerance ] [pick-one]]
  if p = 3 [ifelse breed != rule-changers [ change-rule] [pick-one]]
end

to modify-lists [case target-list] ;; turtle procedure. adds a case to the accept or reject list
  set target-list fput case target-list
end

to check-precedent
  ;;step 1: get class-result
  set prec-test-case [] ;; to make sure that an old test case isn't being carried through
  let cr [class-result] of myself ; calling turtle sets the classification result, i.e. establishes the precedent
  ;;step 2: find the case and keep track (a) which list it came from (unclass or ignore) and (b) its index in that list
  set case-index []
  set precedent-case but-first [test-case] of myself ;; asked turtles assigned precedent case from asking turtle. precedent-case is used to find prec-test-case in turtle-cases or ignore lists.
  ;;can't just copy precedent case into prec-test-case because called turtles are not to reconsider cases they've already classified. they are still theoretically conservative.
  set precedent-case fput cr precedent-case ;; case assigned EA, ER
  set case-index fput find-case turtle-cases case-index ;; find the index of the case to be considered and keep track of the list it came from
  set case-index fput "TC" case-index;; default is that the case to be considered as precedent will be found in turtle-cases
  if outputs? = true [type "this is the precedent case " print precedent-case]

  if member? 999999 case-index [ ;; if the case isn't found in turtle-case list...
    set case-index []
    set case-index fput find-case ignore case-index ;; go look for it in the ignore list
    set case-index fput "I" case-index
  ]
  if (item 0 case-index = "TC") [ ;; copy the case from turtle-cases
    if not member? 999999 case-index [
      set prec-test-case item (item 1 case-index) turtle-cases
    ]
  ]
  if (item 0 case-index = "I") [ ;; copy the case from ignore list
    if not member? 999999 case-index [
      set prec-test-case item (item 1 case-index) ignore
      set prec-test-case replace-item 0 prec-test-case "NI"
    ]
  ]

  if outputs? = true [type "this is where the precedent is located " print case-index type "this is the case " print prec-test-case]
  ;;step 3: consider combinations of intuitions, precedents, and precedent-styles
  if not member? 999999 case-index [
    if outputs? = true [ type "my pre-deliberate rule is: " print rule]
    ;;step 3.1: consider EA cases
    if item 0 precedent-case = "EA" [ ;; if the precedent says to accept...
      ;;step 3.1.1: consider EA-IA cases
      if item 0 prec-test-case = "IA"  [;;and my intuition on the case is to accept...
        set checking-precedent? true
        if precedent-style = "Full Salience" [
          ifelse matchy-matchy current-rule but-first precedent-case >= (1 - turtle-tolerance) [
            let n item 1 case-index
            let caserino []
            ifelse item 0 case-index = "TC" [set caserino item n turtle-cases][set caserino item n ignore]
            set accept lput caserino accept
            ifelse item 0 case-index = "TC" [set turtle-cases remove-item n turtle-cases ][ set ignore remove-item n ignore ]
          ][
            check-intuitive-case-EA-IA
          ]
        ]
        if precedent-style != "Full Salience" [check-intuitive-case-EA-IA]
        set checking-precedent? false
      ]
      ;;step 3.1.2: consider EA-NI cases
      if item 0 prec-test-case = "NI" [;;and i don't have any intuitions...
        set checking-precedent? true
        check-intuitive-case-EA-NI
        set checking-precedent? false
      ]
      ;;step 3.1.3: consider EA-IR cases
      if item 0 prec-test-case = "IR" [ ;; non-fs: remove IR label if in extension, otherwise do nothing
        if precedent-style != "Full Salience" [
          if matchy-matchy current-rule but-first precedent-case >= (1 - turtle-tolerance) [
            precedent-deliberate
          ]
        ]
        if precedent-style = "Full Salience" [
          ifelse matchy-matchy current-rule but-first precedent-case >= (1 - turtle-tolerance) [
            let n item 1 case-index
            let caserino []
            ifelse item 0 case-index = "TC" [set caserino item n turtle-cases][set caserino item n ignore]
            set reject lput caserino reject
            ifelse item 0 case-index = "TC" [set turtle-cases remove-item n turtle-cases ][ set ignore remove-item n ignore ]][
            precedent-deliberate]]

        set checking-precedent? false
      ]
    ] ;; if my intuition says "reject" then change the label on the case to "NI" provided the rule is able to reject it
     ;;(i.e. the rule can be changed to classify it as "reject" without changing the contents of the reject list). Otherwise, do nothing.
    ;;step 3.2: check ER cases
    if item 0 precedent-case = "ER" [ ;; the rationale for these cases mirrors the decisions in lines 447-460. commentary is suppressed for these
      ;;step 3.2.1: check ER-IR
      if item 0 prec-test-case = "IR" [ ;;ER & IR
        set checking-precedent? true
        if precedent-style = "Full Salience" [
          ifelse matchy-matchy current-rule but-first precedent-case >= (1 - turtle-tolerance) [
            precedent-deliberate
          ][
            let n item 1 case-index
            let caserino []
            ifelse item 0 case-index = "TC" [set caserino item n turtle-cases][set caserino item n ignore]
            set reject lput caserino reject
            ifelse item 0 case-index = "TC" [set turtle-cases remove-item n turtle-cases ][ set ignore remove-item n ignore ]
          ]
        ]
        if precedent-style != "Full Salience" [
          check-intuitive-case-ER-IR
        ]
      set checking-precedent? false
      ]
      ;;step 3.2.2: check ER-NI
      if item 0 prec-test-case = "NI" [ ;;ER & NI
        set checking-precedent? true
        check-intuitive-case ;; all deliberation styles need to get thrown into check-intuitive for ER & NI
        set checking-precedent? false
      ]
      ;;step 3.2.3: check ER-IA
      if item 0 prec-test-case = "IA" [ ;; ER & IA
        set checking-precedent? true
        if precedent-style = "Full Salience" [
          ifelse matchy-matchy current-rule but-first precedent-case >= (1 - turtle-tolerance) [
            let n item 1 case-index
            let caserino []
            ifelse item 0 case-index = "TC" [set caserino item n turtle-cases][set caserino item n ignore]
            set accept lput caserino accept
            ifelse item 0 case-index = "TC" [set turtle-cases remove-item n turtle-cases ][ set ignore remove-item n ignore ]][
            check-intuitive-case]
        ]
        if precedent-style != "Full Salience" [
          precedent-deliberate
        ]
        set checking-precedent? false
      ]
    ]
  ]

  if outputs? = true [type "my post-deliberate rule is: " print rule if not empty? prec-test-case [print item 0 prec-test-case print item 0 precedent-case print accept]]
  ;;some defensive coding;;;;;;;;;;;;;;;;;;;;;;;;;;
  if not empty? prec-test-case [
    let x but-first prec-test-case
    let y but-first precedent-case
    if member? false (map [[i j] -> i = j] x y ) [user-message "something has happened with test cases and precedent cases. their digits don't match. look at check-precedent procedure" ]]
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
end

to check-intuitive-case-EA-IA
   if not empty? prec-test-case and checking-precedent? = true [ ;; this checks the precedent
    if item 0 prec-test-case = "IA" and item 0 precedent-case = "EA" [
      if precedent-style = "Full Salience" [
        ifelse matchy-matchy current-rule but-first prec-test-case >= (1 - turtle-tolerance) [
          let n item 1 case-index
          let caserino []
          ifelse item 0 case-index = "TC" [set caserino item n turtle-cases][set caserino item n ignore]
          set accept lput caserino accept
          ifelse item 0 case-index = "TC" [set turtle-cases remove-item n turtle-cases ][ set ignore remove-item n ignore ]
        ][
          deliberate
        ]
      ]
     if precedent-style != "Full Salience" [
        ;;if NOT in the extension, try to change the rule
        if matchy-matchy current-rule but-first prec-test-case < (1 - turtle-tolerance) [
          deliberate
        ]
      ]
    ]
  ]
end

to check-intuitive-case-EA-NI
  if item 0 prec-test-case = "NI" [
      if item 0 precedent-case = "EA" [
        if precedent-style = "Intuition Revision" [
          ifelse matchy-matchy current-rule but-first prec-test-case < (1 - turtle-tolerance) [ deliberate ][
            let n item 1 case-index
            let caserino []
            ifelse item 0 case-index = "TC" [set caserino item n turtle-cases][set caserino item n ignore]
            set caserino replace-item 0 caserino "IA"
            ifelse item 0 case-index = "TC" [set turtle-cases replace-item n turtle-cases caserino][ set ignore replace-item n ignore caserino ]
          ]
        ]
        if precedent-style = "No Defeaters" [
          ifelse matchy-matchy current-rule but-first prec-test-case < (1 - turtle-tolerance) [ deliberate ][
            let n item 1 case-index
            let caserino []
            ifelse item 0 case-index = "TC" [set caserino item n turtle-cases][set caserino item n ignore]
            set accept lput caserino accept
            ifelse item 0 case-index = "TC" [set turtle-cases replace-item n turtle-cases caserino][ set ignore replace-item n ignore caserino ]
          ]
        ]
      if precedent-style = "Soft Precedence" or precedent-style = "Full Salience" [
        let gap precision ((1 - turtle-tolerance) - (matchy-matchy but-first prec-test-case rule)) 2
        let needs-changing 0
        ifelse gap > 0 [ set needs-changing ceiling (length current-rule * gap)] [set needs-changing abs (floor (length current-rule * gap) - 1)]
        if matchy-matchy current-rule but-first prec-test-case < (1 - turtle-tolerance) [
          let a current-rule
          let b but-first prec-test-case
          let zz (map [[i j] -> i = j] a b ) ;; find where there are matches and misses
          let locations list-locations false zz  ;; get the indices of the misses
          let spot []
          if needs-changing > length locations [set needs-changing length locations]
          ifelse coordinated-change [set spot sublist locations 0 needs-changing] [set spot up-to-n-of needs-changing locations] ;; set places to change the rule
          foreach spot [s ->
            ifelse item s current-rule = 0 [
              set current-rule remove-item s current-rule
              set current-rule insert-item s current-rule 1 ] [
              set current-rule remove-item s current-rule
              set current-rule insert-item s current-rule 0]
          ]
          let temp-accept item 0 (check-cases accept current-rule turtle-tolerance)
          ifelse length temp-accept = length accept [
            set rule current-rule ;; adopt the proposed rule change
            set how-many-rule-changes how-many-rule-changes + 1
          ][
            set current-rule rule
          ]
        ]
      ]
    ]
  ]

end

to check-intuitive-case-ER-IR
  if not empty? prec-test-case and checking-precedent? = true [ ;; this checks the precedent
    if item 0 prec-test-case = "IR" and item 0 precedent-case = "ER" [
      if precedent-style = "Full Salience" [
        ifelse matchy-matchy current-rule but-first prec-test-case < (1 - turtle-tolerance) [ ;; if case is NOT in extension, reject
          let n item 1 case-index
          let caserino []
          ifelse item 0 case-index = "TC" [set caserino item n turtle-cases][set caserino item n ignore]
          set reject lput caserino reject
          ifelse item 0 case-index = "TC" [set turtle-cases remove-item n turtle-cases ][ set ignore remove-item n ignore ]
        ][
          deliberate
        ]
      ]
     if precedent-style != "Full Salience" [
        if matchy-matchy current-rule but-first prec-test-case >= (1 - turtle-tolerance) [
          deliberate
        ]
      ]
    ]
  ]
end

to check-intuitive-case-ER-NI
   if item 0 prec-test-case = "NI" [
      if item 0 precedent-case = "ER" [
        if precedent-style = "Intuition Revision" [
          ifelse matchy-matchy current-rule but-first prec-test-case >= (1 - turtle-tolerance) [ deliberate ][
            let n item 1 case-index
            let caserino []
            ifelse item 0 case-index = "TC" [set caserino item n turtle-cases][set caserino item n ignore]
            set caserino replace-item 0 caserino "IR"
            ifelse item 0 case-index = "TC" [set turtle-cases replace-item n turtle-cases caserino][ set ignore replace-item n ignore caserino ]
          ]
        ]
        if precedent-style = "No Defeaters" [
          ifelse matchy-matchy current-rule but-first prec-test-case >= (1 - turtle-tolerance) [ deliberate ][
            let n item 1 case-index
            let caserino []
            ifelse item 0 case-index = "TC" [set caserino item n turtle-cases][set caserino item n ignore]
            set reject lput caserino reject
            ifelse item 0 case-index = "TC" [set turtle-cases replace-item n turtle-cases caserino][ set ignore replace-item n ignore caserino ]
          ]
        ]
      if precedent-style = "Soft Precedence" or precedent-style = "Full Salience" [
        if matchy-matchy current-rule but-first prec-test-case >= (1 - turtle-tolerance) [
          let gap precision ((1 - turtle-tolerance) - (matchy-matchy but-first prec-test-case rule)) 2
          let needs-changing 0
          ifelse gap > 0 [ set needs-changing ceiling (length current-rule * gap)] [set needs-changing abs (floor (length current-rule * gap) - 1)]
          if matchy-matchy current-rule but-first prec-test-case < (1 - turtle-tolerance) [
            let a current-rule
            let b but-first prec-test-case
            let zz (map [[i j] -> i = j] a b ) ;; find where there are matches and misses
            let locations list-locations false zz  ;; get the indices of the misses
            let spot []
            if needs-changing > length locations [set needs-changing length locations]
            ifelse coordinated-change [set spot sublist locations 0 needs-changing] [set spot up-to-n-of needs-changing locations]
            foreach spot [s ->
              ifelse item s current-rule = 0 [
                set current-rule remove-item s current-rule
                set current-rule insert-item s current-rule 1 ] [
                set current-rule remove-item s current-rule
                set current-rule insert-item s current-rule 0]
            ]
            let temp-reject item 0 (check-cases reject current-rule turtle-tolerance)
            ifelse length temp-reject = length reject [
              set rule current-rule ;; adopt the proposed rule change
              set how-many-rule-changes how-many-rule-changes + 1
            ][
              set current-rule rule
            ]
          ]
        ]
      ]
    ]
  ]
end

to precedent-deliberate ;; this is long b/c it covers mismatches for both EA and ER
  if breed = peelers or breed = randos or breed = shelvers [set temp-breed one-of ["tc" "rc" ]]
  if breed = rule-changers [ ifelse random 100 < turtle-disposition [set temp-breed "rc"] [set temp-breed "tc"]]
  if breed = tol-changers [ ifelse random 100 < turtle-disposition [set temp-breed "tc"] [set temp-breed "rc"]]

  if outputs? = true [ type "my pre-pd rule is " print rule]

  if (item 0 precedent-case = "EA") [;; if the asking turtle has accepted the case, and it now acts as a precedent for other turtles... TRIGGERED ON STEP 4 WITH SEED 232022 AT TOLERANCE OF .8
    ;; for EA-IR cases, agents check if the case is in the rule extension. if it is, then all precedent-styles except full-salience replace IR with NI
    ;; if the case is not in the rule extension, then agents do nothing. this is why the control flow is "if" and not "ifelse"
    ;; full-salience EA-IR case is addressed in check-precedent
    if temp-breed = "tc" [
      let needed-tol precision (1 - (matchy-matchy current-rule but-first prec-test-case)) 4
      let temp-reject item 1 (check-cases reject current-rule needed-tol)
      if length temp-reject = length reject [
        let n item 1 case-index
        let caserino []
        ifelse item 0 case-index = "TC" [set caserino item n turtle-cases][set caserino item n ignore]
        set caserino replace-item 0 caserino "NI"
        ifelse item 0 case-index = "TC" [set turtle-cases replace-item n turtle-cases caserino][ set ignore replace-item n ignore caserino ]
        ;set turtle-tolerance needed-tol
      ]
    ]
    if temp-breed = "rc" [
      let gap precision ((1 - turtle-tolerance) - (matchy-matchy but-first prec-test-case rule)) 2 ;; find the gap between my threshold and the rule/case match.
      let needs-changing 0
      ifelse gap > 0 [ set needs-changing ceiling (length current-rule * gap)] [set needs-changing abs (floor (length current-rule * gap) - 1)]
      let a current-rule
      let b but-first prec-test-case
      let zz (map [[i j] -> i = j] a b ) ;; find where there are matches and misses
      let locations list-locations false zz  ;; get the indices of the misses
      let spot []
      if needs-changing > length locations [set needs-changing length locations]
      ifelse coordinated-change [set spot sublist locations 0 needs-changing] [set spot up-to-n-of needs-changing locations] ;; set places to change the rule
      foreach spot [s ->
        ifelse item s current-rule = 0 [
          set current-rule remove-item s current-rule
          set current-rule insert-item s current-rule 1 ] [
          set current-rule remove-item s current-rule
          set current-rule insert-item s current-rule 0]
      ]
      let temp-accept item 0 (check-cases accept current-rule turtle-tolerance) ;; these are the cases from the accept bin that are still accepted with the new rule
      if length temp-accept = length accept ;; if these lists are the same length

      [ let n item 1 case-index
        let caserino []
        ifelse item 0 case-index = "TC" [set caserino item n turtle-cases][set caserino item n ignore]
        set caserino replace-item 0 caserino "NI"
        ifelse item 0 case-index = "TC" [set turtle-cases replace-item n turtle-cases caserino][ set ignore replace-item n ignore caserino ]
        ;set rule current-rule
        ;set current-rule rule
      ]
    ]
  ]
  if (item 0 precedent-case = "ER") [;; if the asking turtle has accepted the case, and it now acts as a precedent for other turtles;; if the asking turtle has accepted the case, and it now acts as a precedent for other turtles... TRIGGERED ON STEP 4 WITH SEED 232022 AT TOLERANCE OF .8
    ;; for ER-IA cases, agents check if the case is in the rule extension. if it is, then all precedent-styles except full-salience replace IR with NI
    ;; if the case is not in the rule extension, then agents do nothing. this is why the control flow is "if" and not "ifelse"
    ;; full-salience ER-IA case is addressed in check-precedent
    if temp-breed = "tc" [
      let needed-tol precision ((1 - (matchy-matchy current-rule but-first prec-test-case)) - 0.01) 4
      let temp-accept item 0 (check-cases accept current-rule needed-tol)
      if (length temp-accept = length accept) [
        let n item 1 case-index
        let caserino []
        ifelse item 0 case-index = "TC" [set caserino item n turtle-cases][set caserino item n ignore]
        set caserino replace-item 0 caserino "NI"
        ifelse item 0 case-index = "TC" [set turtle-cases replace-item n turtle-cases caserino][ set ignore replace-item n ignore caserino ]
        ;set turtle-tolerance needed-tol
      ]
    ]
    if temp-breed = "rc" [
      let gap precision ((1 - turtle-tolerance) - (matchy-matchy but-first prec-test-case current-rule)) 2 ;; find the gap between my threshold and the rule/case match.
      let needs-changing 0
      ifelse gap > 0 [ set needs-changing ceiling (length current-rule * gap)] [set needs-changing abs (floor (length current-rule * gap) - 1)]
      let a current-rule
      let b but-first prec-test-case
      let zz (map [[i j] -> i = j] a b ) ;; find where there's no match
      let locations list-locations true zz  ;; get the indices of the matches
      let spot []
      if needs-changing > length locations [set needs-changing length locations]
      ifelse coordinated-change [set spot sublist locations 0 needs-changing] [set spot up-to-n-of needs-changing locations]
      foreach spot [s ->
        ifelse item s current-rule = 0 [
          set current-rule remove-item s current-rule
          set current-rule insert-item s current-rule 1 ] [
          set current-rule remove-item s current-rule
          set current-rule insert-item s current-rule 0]
      ]
      let temp-reject item 1 (check-cases reject current-rule turtle-tolerance)
       if length temp-reject = length reject [
        let n item 1 case-index
        let caserino []
        ifelse item 0 case-index = "TC" [set caserino item n turtle-cases][set caserino item n ignore]
        set caserino replace-item 0 caserino "NI"
        ifelse item 0 case-index = "TC" [set turtle-cases replace-item n turtle-cases caserino][ set ignore replace-item n ignore caserino]
        ;set rule current-rule
        ;set current-rule rule
      ]
    ]
  ]
  if outputs? = true  [type "my post-pd rule is " print rule]
end

to-report find-case [case-list]
  let t 999999
  foreach (range length case-list) [i ->
  let t-case item i case-list
  let label-c item 0 t-case
  let label-p item 0 precedent-case; print "precedent-case: " print precedent-case
  let z (map [[j k] -> j = k] but-first precedent-case but-first t-case)
    if not member? false z [set t i]]
  report t
end

to-report hammy-matrix [h-list] ;; given a list of list of 0/1's, converts to
 ;; matrix with each cell specifying the hammy-distance, where 0 is least similar, and 1 is most similar
  let l length h-list
  let m matrix:make-constant l l 9
  let i 0
  repeat l [
    let j l - i
    let r (range (l - j) l)
    foreach r [ k ->
      let case-i item i h-list
      let case-k item k h-list
      if item 0 case-i = "NI" or item 0 case-i = "IA" or item 0 case-i = "IR" [set case-i butfirst case-i] ;; this strips the "I" or "NI" marker at the beginning of the case
      if item 0 case-k = "NI" or item 0 case-k = "IA" or item 0 case-k = "IR" [set case-k butfirst case-k]
      let cell-value matchy-matchy case-i case-k
      matrix:set m i k cell-value
    ]
    set i i + 1
  ]
  report m
end

to-report width-of-case-list [c-list] ;;finds the min value in a hammy-matrix, which represents the least similarity between two cases
  let m "empty"
  if not empty? c-list [
    set m min reduce sentence matrix:to-row-list hammy-matrix c-list
  ]
  report m
end

to-report max-min-tol
  let a max [turtle-tolerance] of turtles
  let b min [turtle-tolerance] of turtles
  report (a - b)
end

to-report similarity-to-rule [r-rule c-list]
  let d [];;this will be the list of similarity distances between the rule and the cases, where 1 is perfect similarity and 0 is no similarity
  let l length c-list
  let i 0
  repeat l [
    let case-i item i c-list
    set case-i butfirst case-i
    let v matchy-matchy r-rule case-i
    set d lput v d
    set i i + 1
  ]
  report d
end

to-report rule-case-similarity
  let l []
  foreach (range 0 count turtles) [r ->
    let temp []
    set temp similarity-to-rule [rule] of turtle r [accept] of turtle r
    set l lput temp l]
  report l
end

to-report accept-width-turtles
  let awl []
  set awl map width-of-case-list [accept] of turtles
  report awl
end


to-report check-cases [list-of-list turt-rule turt-tol]
  let temp-accept []
  let temp-reject []
    foreach list-of-list [ a ->
      ifelse matchy-matchy turt-rule (but-first a) >= (1 - turt-tol) [
        set temp-accept lput a temp-accept  ] [
        set temp-reject lput a temp-reject
      ]
    ]
  report (list temp-accept temp-reject)
end

to-report list-locations [an-item a-list]
  let temp []
   foreach (range 0 length a-list) [i ->
    if item i a-list = an-item [
      set temp lput i temp ]]
  report temp
end

to-report frequency [an-item a-list]
    report length (filter [ i -> i = an-item] a-list)
end

to clean-lists
  set ignore remove-duplicates ignore
  set accept remove-duplicates accept
  set reject remove-duplicates reject
end

to check-high-and-low
  ifelse ticks <= 0 [
  let temp-upper []

  set lowest-point initial-equilibria
  set highest-point initial-equilibria
  ][
  let temp []
  ask turtles [
    let case-thr-pair [ ]
    set case-thr-pair fput turtle-tolerance case-thr-pair
    set case-thr-pair fput rule case-thr-pair
      set temp fput case-thr-pair temp]
    if length remove-duplicates temp < lowest-point [set lowest-point length remove-duplicates temp]
    if length remove-duplicates temp > highest-point [set highest-point length remove-duplicates temp]
  ]
end

to-report changing-equil
  report (list "Initial:" initial-equilibria "Lowest:" lowest-point "Highest:" highest-point "Final:" equilibria-upper-bound)
end

to layout
  ;; the number 3 here is arbitrary; more repetitions slows down the
  ;; model, but too few gives poor layouts
  repeat 1 [
    ;; the more turtles we have to fit into the same amount of space,
    ;; the smaller the inputs to layout-spring we'll need to use
    let factor sqrt count turtles
    ;; numbers here are arbitrarily chosen for pleasing appearance
    layout-spring turtles links (1 / factor) (7 / factor) (1 / factor)
    display  ;; for smooth animation
  ]
  ;; don't bump the edges of the world
  let x-offset max [xcor] of turtles + min [xcor] of turtles
  let y-offset max [ycor] of turtles + min [ycor] of turtles
  ;; big jumps look funny, so only adjust a little each time
  set x-offset limit-magnitude x-offset 0.1
  set y-offset limit-magnitude y-offset 0.1
  ask turtles [ setxy (xcor - x-offset / 2) (ycor - y-offset / 2) ]
end


to-report limit-magnitude [number limit]
  if number > limit [ report limit ]
  if number < (- limit) [ report (- limit) ]
  report number
end

;;;;;;;;;;;;;;;;;;;;;
;;this came out of check-intuitive-case but i don't want to delete it just yet, even though I've revised the code extensively (5/12/2022)
;
;  if not empty? prec-test-case and checking-precedent? = true [ ;; this checks the precedent
;    if item 0 prec-test-case = "IA" and item 0 precedent-case = "EA" [
;      if precedent-style = "Full Salience" [
;        ifelse matchy-matchy current-rule but-first prec-test-case >= (1 - turtle-tolerance) [
;          let n item 1 case-index
;          let caserino []
;          ifelse item 0 case-index = "TC" [set caserino item n turtle-cases][set caserino item n ignore]
;          set accept lput caserino accept
;          ifelse item 0 case-index = "TC" [set turtle-cases remove-item n turtle-cases ][ set ignore remove-item n ignore ]
;        ][
;          deliberate
;        ]
;      ]
;     if precedent-style != "Full Salience" [
;        if matchy-matchy current-rule but-first prec-test-case < (1 - turtle-tolerance) [
;          deliberate
;        ]
;      ]
;    ]
;    if item 0 prec-test-case = "IR" [
;      if matchy-matchy current-rule but-first prec-test-case >= (1 - turtle-tolerance) [ deliberate ]] ;; if there are too many matches to reject the precedent, then go into the deliberate procedure
;
;      if item 0 prec-test-case = "NI" [
;      if item 0 precedent-case = "EA" [
;        if precedent-style = "Intuition Revision" [
;          ifelse matchy-matchy current-rule but-first prec-test-case < (1 - turtle-tolerance) [ deliberate ][
;            let n item 1 case-index
;            let caserino []
;            ifelse item 0 case-index = "TC" [set caserino item n turtle-cases][set caserino item n ignore]
;            set caserino replace-item 0 caserino "IA"
;            ifelse item 0 case-index = "TC" [set turtle-cases replace-item n turtle-cases caserino][ set ignore replace-item n ignore caserino ]
;          ]
;        ]
;        if precedent-style = "No Defeaters" [
;          ifelse matchy-matchy current-rule but-first prec-test-case < (1 - turtle-tolerance) [ deliberate ][
;            let n item 1 case-index
;            let caserino []
;            ifelse item 0 case-index = "TC" [set caserino item n turtle-cases][set caserino item n ignore]
;            set accept lput caserino accept
;            ifelse item 0 case-index = "TC" [set turtle-cases replace-item n turtle-cases caserino][ set ignore replace-item n ignore caserino ]
;          ]
;        ]
;      ]
;    ]
;    if item 0 prec-test-case = "NI" [
;      if item 0 precedent-case = "ER" [
;        if precedent-style = "Intuition Revision" [
;          ifelse matchy-matchy current-rule but-first prec-test-case >= (1 - turtle-tolerance) [ deliberate ][
;            let n item 1 case-index
;            let caserino []
;            ifelse item 0 case-index = "TC" [set caserino item n turtle-cases][set caserino item n ignore]
;            set caserino replace-item 0 caserino "IR"
;            ifelse item 0 case-index = "TC" [set turtle-cases replace-item n turtle-cases caserino][ set ignore replace-item n ignore caserino ]
;          ]
;        ]
;        if precedent-style = "No Defeaters" [
;          ifelse matchy-matchy current-rule but-first prec-test-case >= (1 - turtle-tolerance) [ deliberate ][
;            let n item 1 case-index
;            let caserino []
;            ifelse item 0 case-index = "TC" [set caserino item n turtle-cases][set caserino item n ignore]
;            set reject lput caserino reject
;            ifelse item 0 case-index = "TC" [set turtle-cases replace-item n turtle-cases caserino][ set ignore replace-item n ignore caserino ]
;          ]
;        ]
;      ]
;    ]
;  ]
@#$#@#$#@
GRAPHICS-WINDOW
458
13
654
210
-1
-1
5.7
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
0
0
1
ticks
30.0

BUTTON
14
20
77
53
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
67
65
130
98
NIL
go
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
12
111
184
144
rule-length
rule-length
0
50
5.0
1
1
NIL
HORIZONTAL

SLIDER
14
163
186
196
tolerance
tolerance
0
1
0.2
.05
1
NIL
HORIZONTAL

BUTTON
121
16
198
49
go-once
go
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
15
213
187
246
disposition
disposition
0
100
100.0
1
1
NIL
HORIZONTAL

SWITCH
227
151
371
184
global-shuffle
global-shuffle
0
1
-1000

SWITCH
227
200
368
233
agent-shuffle
agent-shuffle
0
1
-1000

SWITCH
226
243
410
276
coordinated-change
coordinated-change
1
1
-1000

CHOOSER
20
260
158
305
agent-type
agent-type
"randos" "rule-changers" "peelers" "shelvers" "tol-changers"
1

SWITCH
227
111
346
144
rule-shuffle
rule-shuffle
0
1
-1000

SLIDER
17
319
189
352
I-NI-ratio
I-NI-ratio
0
100
10.0
1
1
NIL
HORIZONTAL

INPUTBOX
225
280
319
340
cond
1.0
1
0
Number

PLOT
677
12
941
207
Number of equilibria
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"plot equilibria-upper-bound" "plot upper-bound"
PENS
"default" 1.0 0 -16777216 true "" "plot upper-bound"

CHOOSER
472
304
610
349
network-type
network-type
"empty" "complete" "ring" "ring2" "random-1" "random-2" "random-3"
1

MONITOR
976
15
1078
60
NIL
[rule] of turtle 0
17
1
11

MONITOR
977
69
1079
114
NIL
[rule] of turtle 1
17
1
11

MONITOR
976
128
1078
173
NIL
[rule] of turtle 2
17
1
11

MONITOR
1103
16
1255
61
NIL
[turtle-tolerance] of turtle 0
17
1
11

MONITOR
1103
67
1254
112
NIL
[turtle-tolerance] of turtle 1
17
1
11

MONITOR
1103
127
1254
172
NIL
[turtle-tolerance] of turtle 2
17
1
11

SWITCH
232
25
336
58
outputs?
outputs?
1
1
-1000

SWITCH
230
71
356
104
precedence?
precedence?
0
1
-1000

CHOOSER
468
252
612
297
precedent-style
precedent-style
"Soft Precedent" "Intuition Revision" "No Defeaters" "Full Salience"
0

CHOOSER
20
363
178
408
intuition-assignment
intuition-assignment
"fixed-all-intuitive" "fixed-some-intuitive" "nec-cond" "no-nec-cond"
3

CHOOSER
218
362
356
407
rule-agreement
rule-agreement
"all-same" "lucky" "unlucky"
0

MONITOR
633
234
1215
279
NIL
get-clusters [rule] of turtles
17
1
11

SLIDER
473
354
610
387
token-saturation
token-saturation
1
7
7.0
1
1
NIL
HORIZONTAL

CHOOSER
475
393
613
438
determination
determination
"none" "bare" "max"
1

@#$#@#$#@
## GENERAL CODING NOTES

## UPDATES
5/13/2022: Added a new reporter "changing-equil". It gives the initial number of equilibria, the lowest number, the highest number, and the final number.
5/31/2022: Added a new variable "how-many-rule-changes" because turtles are ending up with items in their accept list that shouldn't be there
5/31/2022: I had a fucking "<" where I should have had ">=". 

## DOCUMENTING THE CODE

There are 5 breeds of turtles: rule-changers, tolerance-changers, peelers, shelvers, randos. Each has a default way of achieving intrapersonal equilibrium. Randos choose one at random.

The main procedures are check-intuitive-case, check-precedent, and precedent-deliberate. Main subprocedures under these are change-rule, change-tolerance, peel, and do-ignore. These are all called by the procedure **deliberate**. It is a stepping stone to these main subprocedures.

## IMPORTANT VARIABLES, PARAMETERS
**IA** intuition label, for "intuitive accept"
**IR** intuition label, for "intuitive reject"
**NI** intuition label, for "no intuition"
**EA** precedent classification label, for "external accept"
**ER** precedent classification label, for "external reject"
**IG** precedent classification label, for "ignored". There are no instructions for how to handle ignored precedents, so neighboring turtles effectively ignore this precedent classification. 
**rule**: the turtle's center case, consisting of a binary string of length N
**current-rule**: rule is copied into current-rule at the start of every turtle's turn. this is to make sure that proposed changes to the rule aren't accidentally adopted. if some proposed change is to be accepted, current-rule is copied to rule before the end of the procedure *change-rule*
**ACCEPT** cases that a turtle accepts, given its rule
**REJECT** cases a turtle rejects, given its rule
**IGNORE** a list of cases that the turtle is temporarily ignoring until other cases have been considered. it goes on to the list with its intial intuition assignment. when it is taken off the list, its intuition assignment is replaced with *NI*
**turtle-cases** cases to be considered by the turtle. this is in the same other as every other turtle if *agent-shuffle?* is false
**turtle-tolerance** initially set by the global variable *tolerance*. it varies as turtles try to accept or reject cases, especially if their breed is *tolerance-changer*
**class-result** how the judging turtle classifies a case. this can be either *EA* in cases of accepting or *ER* in cases of rejecting
**checking-precedent?** a Boolean that lets turtles reuse the *deliberate* procedure to change their rules (i.e. center case [*rule*] and tolerance [*turtle-tolerance*]) but without classifying the case
**precedent-case** the case that the judging turtle has classified and for which the precedent has been set. It will have the prefix *EA*, *ER*, or *IG* (for "ignored").
**prec-test-case** the copy of the precedent case from the turtle's *turtle-cases* list or *ignore* list
**case-index** tracks where the case in prec-test-case is. "TC" means it came from *turtle-cases*. "I" means it came from *IGNORE*. The number afterwards gives the index of the case in the respective list. So, e.g., [TC 16] would indicate the precedent case to be considered comes from *turtle-cases* with an index of 16. "999999" is used to indicate that the case isn't found on either *turtle-cases* or *IGNORE* because it already appears on *ACCEPT* or *REJECT*

## MAIN PROCEDURES
**I. Check-intuitive-case:**
Turtles check to see if there are enough matches to accept IA (intuitive accept) cases or enough misses to reject IR (intuitive reject) cases. Every turtle classifies a case (or ignores it to deal with later) and immediately afterwards informs its neighbors of its judgment. When the deciding turtle accepts or rejects a case, it sets the classification result, i.e. establishes the precedent: EA (external accept) or ER (external reject). If there are insufficient matches for IA cases or too many matches for IR cases, then turtles move to the **deliberate** procedure.

**II. Check-precedent:**
This is executed by the turtles that the deciding turtle is linked to. This procedure is called in the main "go" procedure. It has several parts. The first is for the neighbor to get the classification result of the initial turtle. The second it for the neighbor to find the case in its unclassified or ignore list. If the case is in neither, then the case has already been decided for that neighbor and does nothing else. Third, the neighbor begins comparing precedent with its intuition and rules. There are many possibilities here and they're easiest to present Tractatus-style

1. EA & IA 
1.1 if the neighbor's precedent style is Full Salience
*1.1.1* accept the case if it is in the rule's extension. otherwise, attempt to change the rule extension (i.e. go into the **deliberate** procedure). this is done in the procedure check-intuitive-case-EA-IA
1.2 if the neighbor's precedent style is anything other than Full Salience
*1.2.1* do nothing if the case is in the rule's extension. attempt to change the rule or tolerance if it is not in the extension (i.e. go into the **deliberate** procedure but DO NOT actually classify the case; this is handled by means of a Boolean, "checking-precedent?")

2. EA & NI
2.1 if the neighbor's precedent style is either Full Salience or Soft Intuition
*2.1.1* do nothing if the case is in the rule's extension. otherwise go into the **deliberate** procedure.
2.2 if the neighbor's precedent style is Intuition Revision
*2.2.1* revise the intuition label from NI to IA if the case is in the rule's extension. otherwise, go into the **deliberate** procedure.
2.3 if the neighbor's precedent style is No Defeaters
*2.3.1* accept the case if it is in the rule's extension. otherwise, go into the **deliberate** procedure.

3. EA & IR
3.1 if the neighbor's precedent style is anything other than Full Salience
*3.1.1* check if the case is in the rule extension. if it is, go into **precedent-deliberate**. Otherwise, do nothing
3.2 if the neighbor's predent style is Full Salience
*3.2.1* check if the case is in the rule extension. if it is, reject it. otherwise, go into **precedent-deliberate**

4. ER & IR
4.1 if the neighbor's precedent style is Full Salience
*4.1.1* reject the case if it is not in the rule's extension. otherwise, attempt to change the rule extension (i.e. go into the **deliberate** procedure). this is done in the procedure check-intuitive-case-EA-IA
4.2 if the neighbor's precedent style is anything other than Full Salience
*4.2.1* do nothing if the case is not in the rule's extension. attempt to change the rule or tolerance if it is  in the extension (i.e. go into the **deliberate** procedure but DO NOT actually classify the case; this is handled by means of a Boolean, "checking-precedent?")

5. ER & NI
5.1 if the neighbor's precedent style is either Full Salience or Soft Intuition
*5.1.1* do nothing if the case is not in the rule's extension. otherwise go into the **deliberate** procedure.
5.2 if the neighbor's precedent style is Intuition Revision
*5.2.1* revise the intuition label from NI to IR if the case is not in the rule's extension. otherwise, go into the **deliberate** procedure.
5.3 if the neighbor's precedent style is No Defeaters
*5.3.1* reject the case if it is not in the rule's extension. otherwise, go into the **deliberate** procedure.

6. ER & IA
3.1 if the neighbor's precedent style is anything other than Full Salience
*3.1.1* check if the case is in the rule extension. if it is not, go into **precedent-deliberate**. Otherwise, do nothing
3.2 if the neighbor's predent style is Full Salience
*3.2.1* check if the case is not in the rule extension. if it is not, accept it. otherwise, go into **precedent-deliberate**

1 & 4, 2 & 5, and 3 & 6 are symmetrical pairs (e.g. EA & IA and ER & IR are handled similarly).

**III. Precedent-deliberate**
Just as a turtle needn't always follow its primary disposition in deliberating, so too can a turtle deviate from its primary disposition in deliberating over precedent. However, the only dispositions relevant here are rule-changing and tolerance-changing. The others (peeling and shelving) don't make sense in this context.

First, a turtle settles its precedent-deliberating disposition. It can be either "rc" (for rule-change) or "tc" (for tolerance-change). Then it checks to see if the precedent case has been tagged as EA or ER by the judging turtle. Again there are a few combinations to consider:

1. EA & tc
If the case can be accepted without having to change anything in the ACCEPT or REJECT lists, then replace the "IR" intuition label with "NI" 

2. EA & rc
If the case can be accepted without having to change anything in the ACCEPT or REJECT lists, then replace the "IR" intuition label with "NI" 

3. ER & tc
If the case can be rejected without having to change anything in the ACCEPT or REJECT lists, then replace the "IA" intuition label with "NI" 

4. ER & rc
If the case can be rejected without having to change anything in the ACCEPT or REJECT lists, then replace the "IR" intuition label with "NI"

## MAIN SUBPROCEDURES

These are the main subprocedures executed by turtles when they **deliberate**. Turtles go into the **deliberate** procedure when it (i) has a case labelled "IA" but it isn't in the rule's extension or (ii) has a case labelled "IR" and it is in the rule's extension.

**I. Change-rule**
The turtle identifies the minimum number bits in the center case that would have to change if they were to accept or reject the case. (This follows from a theoretical committment to conservatism: turtles change their rules only as much as they have to and no more.) If the agent can change their rule without having to reclassify cases in their ACCEPT or REJECT lists, then the agent adopts the rule change and classifies the case. Otherwise, the turtle puts the case on the IGNORE list.

**II. Change-tolerance**
The turtle identifies the how much the tolerance threshhold that would have to change if they were to accept or reject the case. If the agent can change their tolerance level without having to reclassify cases in their ACCEPT or REJECT lists, then the agent adopts the rule change and classifies the case. Otherwise, the turtle puts the case on the IGNORE list.

**III. Do-ignore**
The turtle puts the case on the IGNORE list with its intuition label. When the case is pulled from this list, the initial label is replaced with "NI"

**IV. Peel**
The turtle replaces the intuition label with "NI" and then treats the case as a nonintuitive one.

## OTHER IMPORTANT PROCEDURES
**1. Check-non-intuitive-case**
This is just a sorting procedure. If the case is in the rule's extension, then it is put on the ACCEPT list. Otherwise, it goes on the REJECT list.

**2. Check-intuitive-case**
This procedure handles cases with labels "IA" and "IR". If an IA case is in the rule's extension, it's put on the ACCEPT list. Otherwise, the turtle goes into **deliberate**. If an IR case is not in the rule's extension, it's put on the REJECT list. Otherwise, the turtle goes into **deliberate**. 

**DESCRIPTION OF MODEL FLOW**
1. turtle gets a case from turtle-cases or IGNORE
2. if it's tagged "NI", it goes into **check-non-intuitive-case** and then sets the precedent classification as "EA" or "ER"
3. if it's tagged "IR or "IA", the turtle goes into **check-intuitive-case** and sets the precedent classification.
4. The initial turtle shares its result with its neighbors (i.e. the ones with which it has links)
5. Each neighbor goes into **check-precedent**
6. Repeat 1-5 until all cases have been classified.
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
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <metric>width-of-case-list [rule] of turtles</metric>
    <metric>equilibria-upper-bound</metric>
    <metric>equilibria-lower-bound</metric>
    <metric>rule-case-similarity</metric>
    <metric>accept-width-turtles</metric>
    <enumeratedValueSet variable="tolerance">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-type">
      <value value="&quot;changers&quot;"/>
      <value value="&quot;peelers&quot;"/>
      <value value="&quot;ignorers&quot;"/>
      <value value="&quot;wideners&quot;"/>
      <value value="&quot;randos&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="disposition" first="80" step="1" last="100"/>
    <enumeratedValueSet variable="coordinated-change">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-shuffle">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-shuffle">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-shuffle">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="diagnostic_experiment1" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>width-of-case-list [rule] of turtles</metric>
    <metric>equilibria-upper-bound</metric>
    <metric>equilibria-lower-bound</metric>
    <metric>accept-width-turtles</metric>
    <enumeratedValueSet variable="tolerance">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-length">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="intuition-input">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-type">
      <value value="&quot;wideners&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disposition">
      <value value="80"/>
      <value value="85"/>
      <value value="95"/>
      <value value="98"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="coordinated-change">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-shuffle">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-shuffle">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NI-I-ratio">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-shuffle">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="priming condition" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>width-of-case-list [rule] of turtles</metric>
    <metric>equilibria-upper-bound</metric>
    <metric>equilibria-lower-bound</metric>
    <metric>accept-width-turtles</metric>
    <metric>[turtle-tolerance] of turtles</metric>
    <enumeratedValueSet variable="tolerance">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="intuition-input">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="disposition" first="90" step="2" last="100"/>
    <enumeratedValueSet variable="agent-type">
      <value value="&quot;rule-changers&quot;"/>
      <value value="&quot;tol-changers&quot;"/>
      <value value="&quot;randos&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="coordinated-change">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-shuffle">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-shuffle">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="I-NI-ratio">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-shuffle">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="primed?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cond">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>equilibria-upper-bound</metric>
    <enumeratedValueSet variable="primed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="intuition-input">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disposition">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-type">
      <value value="&quot;rule-changers&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nec-cond">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="coordinated-change">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-shuffle">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-shuffle">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="I-NI-ratio">
      <value value="2"/>
      <value value="6"/>
      <value value="10"/>
      <value value="14"/>
      <value value="18"/>
      <value value="22"/>
      <value value="24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-shuffle">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exploration 1" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>initial-equilibria</metric>
    <metric>equilibria-upper-bound</metric>
    <enumeratedValueSet variable="primed?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-length">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="disposition" first="80" step="5" last="100"/>
    <enumeratedValueSet variable="agent-type">
      <value value="&quot;rule-changers&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-shuffle">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;complete&quot;"/>
      <value value="&quot;ring2&quot;"/>
      <value value="&quot;ring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precedence?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cond">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="coordinated-change">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-shuffle">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="outputs?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="I-NI-ratio">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-shuffle">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="majority-convergence with 7 agents" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>width-of-case-list [rule] of turtles</metric>
    <metric>equilibria-upper-bound</metric>
    <metric>equilibria-lower-bound</metric>
    <metric>accept-width-turtles</metric>
    <metric>[turtle-tolerance] of turtles</metric>
    <metric>freq-of-rule-diffs</metric>
    <metric>majority-converged</metric>
    <enumeratedValueSet variable="tolerance">
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-type">
      <value value="&quot;rule-changers&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disposition">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-shuffle">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;complete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precedence?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nec-cond?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cond">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precedent-style">
      <value value="&quot;No Defeaters&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="coordinated-change">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-shuffle">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="outputs?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="I-NI-ratio">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-shuffle">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PrecedentAndLuckiness" repetitions="1000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>total-manymax-maxsize-matches</metric>
    <metric>any-truth?</metric>
    <enumeratedValueSet variable="intuition-assignment">
      <value value="&quot;fixed-some-intuitive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-type">
      <value value="&quot;rule-changers&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disposition">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-shuffle">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;empty&quot;"/>
      <value value="&quot;ring&quot;"/>
      <value value="&quot;ring2&quot;"/>
      <value value="&quot;complete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precedence?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cond">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="coordinated-change">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precedent-style">
      <value value="&quot;Soft Precedent&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-shuffle">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="outputs?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="I-NI-ratio">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-shuffle">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-agreement">
      <value value="&quot;all-same&quot;"/>
      <value value="&quot;lucky&quot;"/>
      <value value="&quot;unlucky&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="distributing tokens" repetitions="10000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total-manymax-maxsize-matches</metric>
    <metric>any-truth?</metric>
    <enumeratedValueSet variable="intuition-assignment">
      <value value="&quot;no-nec-cond&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerance">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="determination">
      <value value="&quot;bare&quot;"/>
      <value value="&quot;max&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disposition">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-type">
      <value value="&quot;rule-changers&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-shuffle">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;complete&quot;"/>
      <value value="&quot;ring&quot;"/>
      <value value="&quot;ring2&quot;"/>
      <value value="&quot;empty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precedence?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="token-saturation" first="1" step="1" last="7"/>
    <enumeratedValueSet variable="cond">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precedent-style">
      <value value="&quot;Soft Precedent&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="coordinated-change">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-shuffle">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="outputs?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="I-NI-ratio">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-shuffle">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-agreement">
      <value value="&quot;all-same&quot;"/>
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
