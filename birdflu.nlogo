;; author: Amanda Beaudoin and Alan G. Isaac
;;         (authors are in alphabetical order)
;; email: beau0209@umn.edu and aisaac@american.edu
;;
;; Code for an SEIR+network model of birdflu transmission in Thailand.
;; Version: 1.0.0
;; NOTE: run model from a folder that has an `out` subfolder, which
;; in turn must have subfolder: `locations`, `log`.
;; REQUIRES: Netlogo 5.2+
;;
;; See accompanying paper for model details.
;;
;; Note: for ease of reading:
;;
;; - local variables defined with `let` begin with `_`
;; - local variables defined as procedure formal arguments begin with `#`
;; - variables ending in `-pct` should be an integer percentage
;;
;; TODO:
;; - fix timing chkchkchk
;; - add sampling regime for detection/destruction
;; - document colors and shapes
;; - add non-poultry-related households?
;; - move plot code inside plots

__includes ["utilities.nls"]
extensions [table network]

;type declarations
; NOTE: the GUI z-order is determined by declaration order
;;declare road-related types
undirected-link-breed [road-links road-link]           ;; link intersections with roads
breed [intersections intersection]                     ;; road intersections
;;; other inanimate-types chkchkchk
undirected-link-breed [egg-links egg-link]             ;; stable eggtrader links
undirected-link-breed [livebird-links livebird-link]   ;; volatile livetrader links
undirected-link-breed [fgd-links fgd-link]             ;; tie fgds to owners
;;; poultry-types
breed [chickens chicken]         ;; on a large farm, or free ranging in a backyard
breed [fcs fc]                   ;; fighting cocks
breed [ducks duck]               ;; on a large farm, or free grazing, or penned in a backyard
;;; human-types (may move w/out poultry to other households, market, arena, etc)
breed [flock-owners flock-owner] ;; poultry flock owners
breed [eggtraders eggtrader]     ;; egg traders
breed [livetraders livetrader]   ;; live poultry traders


;;;;;;;;;;;;;;;;;;;;;;;;
;;      Globals       ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;; globals declared in choosers: ;;;
  ;baseline ;;(string) : "mode" (default) | "pert"
  ;infection-source ;(sring) : "fgduck" | "farm" | "byduck" | "bychicken"
  ;scenario ;;(string) : the scenario name
  ;n-cooperatives  ;; number of central egg cooperatives (affects egg delivery behavior)
;;; globals declared in sliders: ;;;
  ;prop-fgd-barntofield
  ;shedding-period-duck   ;; this is the length of the shedding period in days for ducks
  ;shedding-period-chicken;; this is the length of the shedding period in days for chickens
;;; declared globals: ;;;
globals [ 
  ;;; CONSTANTS (calibrated to data) ;;;
  N-LIVETRADERS     ;(int) : fixed number of live-bird traders (vs. endog # eggtraders)
  N-FGDS            ;(int) : small number of large free grazing duck flocks
  N-BYCHICKENS      ;(int) : large number of small backyard chicken flocks
  N-FCS             ;(int) : large number of fighting cocks
  N-BYDUCKS         ;(int) : large number of small backyard duck flocks
  N-DUCK-FARMS      ;(int) : small number of large duck farms
  N-CHICKEN-FARMS   ;(int) : small number of large layer-chicken farms
  N-BARNS           ;(int) : large number of patches that are barns (i.e., poultry-owning households or farms)
  N-PRACTICE-ARENAS ;(int) : number of patches that are practice arenas
  N-MATCH-ARENAS    ;(int) : number of patches that are practice arenas
  N-MARKETS         ;(int) : number of patches that are practice arenas
  RAI-PER-DUCK      ;(number) : per duck daily consumption rate (as "area" consumed)
  LIVEBIRD-REPLENISH ;(int) : days to replenish livebird supply after sale
  meat-eggs-both-byd ;(list) : the number of byduck flocks producing only meat, only eggs, or both
  n-flocks-per-pickup-ltrdr ;(int) : number of flocks the livetrader buys from each day s/he trades
  
  ;;; patch sets: assignment to these must be finalized in setup
  rice-fields             ;; field patches can be in 3 states; components of larger rice fields
  access-fields           ;; rice-field patches that provide field access
  barns                   ;; the patches that are barns
  match-arenas            ;; arenas for fighting cock matches
  practice-arenas         ;; arenas for fighting cock practice
  markets
  cooperatives
  out-of-subdistrict-patches ;; patches representing areas outside of our modeled subdistrict

  ;;; turtle agentsets: assignment to these must be finalized during setup
  ;; owner subtypes ;;;  convenient to avoid repeated recreation
  ;; may move w/out poultry to other households, market, arena, etc;
  fgd-owners             ;; FGD owners (can move w/out ducks to households, egg cooperative, market, etc)
  byd-owners             ;; backyard duck owners
  fmd-owners             ;; duck farm owners
  fmc-owners             ;; chicken farm owners
  byc-owners             ;; backyard chicken owners (move w/out poultry to other households, market, arena, etc)l
  fc-owners              ;; fighting cock owners (some byc-owners are also fc owners)
  farm-owners            ;; owners of *large* poultry farms (currently unused) chk

  ;; flock subtypes
  fgducks                ;; free grazing ducks (ducks with [biotype = "fg"])
  byducks                ;; penned backyard ducks (ducks with [biotype = "pen"])
  fmducks                ;; farm ducks (ducks with [biotype = "fm"])
  bychickens             ;; backyard chickens (chickens with [biotype = "by"])
  fmchickens             ;; farm chickens (chickens with [biotype = "fm"])
  farms                  ;; fmducks + fmchickens (farms used in plots and reporters)

  ;;; breed lists ;;;
  human-types
  trader-types
  flock-types
  poultry-types          ;; flock-types + fighting cocks
  
  ;;; key duration parameters, in days
  latent-period          ;(int) : duration of the viral latent period (common to all flocks)
  road-contam-period     ;(int) : duration after contamination that the road will pose risks
  human-contam-period    ;(int) : duration after contamination (by infected birds) that traders pose risks
  field-contam-period    ;(int) : duration after contamination (by infected fgd) that fields pose risks
  
  ;;contaimination/infection probabilities (as pct) following a risky event
  flock-field-contam-pct      ;; flock contaminates field
  flock-road-contam-pct       ;; flock contaminates road
  flock-eggtrader-contam-pct  ;; eggtrader contaminated when getting eggs from contaminated premises
  eggtrader-flock-infect-pct  ;; contaminated eggtrader infects a susceptible flock when visiting
  flock-livetrader-contam-pct ;; live trader is contaminated while acquiring birds from an infected premises
  livetrader-flock-infect-pct ;; contaminated live trader causes infection of a susceptible flock while visiting
  flock-visitor-contam-pct    ;; visitor is contaminated while visiting an infected premises
  from-visit-infect-pct       ;; visitor that goes to an infected flock will infect own susceptible flock
  to-visit-infect-pct         ;; an infectious flock owner visits and infects a neighbor's flock
  arena-fc-infect-pct         ;; FC becomes infected while at the arena
  fc-byc-infect-pct           ;; infected FC (infected at arena) infects his home flock
  ;;roads can become contaminiated when infected FGDs travel on them:
  road-byc-infect-pct         ;; road infects bychickens
  road-pen-infect-pct         ;; road infects penned duck or farmed layer flock

  ;visiting parameters
  n-nbrs                 ;; natural number, the number of other humans a human can visit (size of the social circle)
  visit-radius           ;; positive real number, radius from which we select neighbors to visit (chkchk units)
  n-visits-task          ;; task; sets number of visits per day by humans to other humans' home patches (scenario-based task!)
  etlinks-task           ;; task; sets the number of links (suppliers) an eggtrader can service

  ;odds and ends
  fgd-deliver-pct        ;; number, the percentage of FGD flocks that deliver to market (if no cooperative)
  n-onsite-eggtraders-fgd-task
  n-onsite-eggtraders-byd-task
  n-onsite-eggtraders-fm-task
  n-egg-delivery-days-fgd-task
  n-egg-delivery-days-byd-task
  n-egg-delivery-days-fm-task
  n-egg-pickup-days-fgd-task
  n-egg-pickup-days-byd-task
  n-egg-pickup-days-fm-task
  n-bird-pickup-days-ltrdr-task


  ;;; calendar variables  ;;;
  ;;constants:
  LIVETRADER-PICKUP-SCHEDULE  ;; list, specifies for each day of the week who will do live-bird pickup
  DAY-NAMES                   ;; list of strings (days of the week)
  EGGTRADER-PICKUP-SCHEDULE   ;; list, specifies for each day of the week which flocks want egg pickup
  ;;variables updated by `update-calendar`
  day-idx                     ;; int in range 0 to 6, current day index
  day-name                    ;; string, one item of DAY-NAMES (i.e., a weekday name), the current day
  week                        ;; integer, incremented every 7 days, the current week
  livetrader-pickup-days      ;; one sub-list of LIVETRADER-PICKUP-SCHEDULE, determining who picks up today
  eggtrader-pickup-days       ;; one sub-list of EGGTRADER-PICKUP-SCHEDULE, determining who picks up today
  visit-days                  ;; one sub-list of EGGTRADER-PICKUP-SCHEDULE, determining who visits today chkchk


  ;chkchkchk `experiment-name` is no longer needed
  experiment-name             ;; string, name of running experiment (*if* assigned by current experiment)
  base-filename               ;(string) : base filename for current replicate
  locations-file              ;; string, filename for outputting location data
  log-file                    ;(string) : filename for logging info about this run

  runsim?                     ;(boolean) : for simulation control
  stop-sim-reason             ;(string) : diagnostic msg
  debug                       ;(int) : global debug level

  ;;; convenience tables ;;;
  days-on-field          ;(table) : map FGD who numbers to lists
  transmissions          ;(table) : map transmission types to lists of daily numbers
  contaminations         ;(table) : map contamination types to lists of daily numbers
  transition-dates       ;(table) : map date (ticks) to list (agents due for state transition)
  ;;TODO: delete expired transitions chkchk

]


road-links-own [
  roadtype     ;;string, one of: "highway" "major_road" "loose_road" "small_road" "field_road"
  road-patches ;;patch set, patches along road-link
]


patches-own [
  ;; rai used as rough measure of productive capacity for rice fields:
  rai                       ;; number (area in rai; 1 rai =1,600 m^2)
  field-patches             ;; an agentset of patches; ``nobody`` for all but access fields
  field-num                 ;; nonnegative integer; unique field identifier
  field-state               ;; "", "harvested", "growing", or "fallow"
                            ;; - harvested: the fields that duck flocks move to 
                            ;; - growing: ducks do not graze in these fields 
                            ;; - fallow: After ducks exhaust a field, it is left until the next harvest
  occupant                  ;; field is the target of occupant (who may not always be physically present)

  ;; patches will be contaminated if next to a road with infectious ducks passing by; rice field can be contaminated
  pcontam-until             ;; day contamination ends
  pcontam-period            ;; set to field-contam-period or road-contam-period (chk cd use patch type...)
  patch-type                ;;ricefield, barn, market, practice-arena, match-arena, etc
  road-access               ;;closest `intersection`
]



;;attributes common to all types
turtles-own [
  contam-until           ;; day contamination ends
  contam-period          ;; set to human-contam-period for humans)
                         ;; (can contaminate if contam-period>0)
  home-patch             ;; barn for each FGD flock; house/yard for backyard poultry;
                         ;; house/yard for fighting cocks; house/slaughterhouse for local traders
]

;;fighting-cock attributes
fcs-own [
  owner                    ;; turtle, the owner of the fgd flock
  practice-fight-week      ;; 0 or 1 for even or odd weeks
  target-match-arena
  target-practice-arena
  flu-state                ;(string) : "s" | "e" | "i" | "d"
  infection-time           ;; "days" since infection; increment each iteration (i.e., day)
  shedding-period
]

ducks-own [                ;; attributes of each duck flock
  biotype                  ;; "fg" | "pen" | "farm"  (remember, byducks are penned)
  owner                    ;; turtle, the owner of the flock
  flu-state                ;(string) : "s" | "e" | "i" | "r" | "d"
  infection-time           ;(int) : "days" since infection; Increases by one day with each `go`.
  shedding-period

  n-birds                   ;; int, number of birds in flock (to determine grazing consumption)
  has-eggs?                 ;; bool, eggs are available for pickup
  n-egg-pickup-days
  n-onsite-eggtraders       ;; number of eggtraders that come to flock to pick up eggs
  target-egg-market
  n-egg-delivery-days       ;; number of days per week that flock delivers eggs 
  meat-delivery-days        ;; number of days per week that flock delivers meat (not currently used)

  target-field              ;; target is a harvested rice field (i.e., a dayfield)
  target-road-access
  field-only?
]

chickens-own [
  biotype                  ;; "fg", "by", "farm"  (remember, bychickens are *not* penned)
  owner                    ;; turtle, the owner of the flock
  flu-state                ;(string) : "s" | "e" | "i" | "r" | "d"
  infection-time           ;; "days" since infection; Increases by one day with each `go`.
  shedding-period

  n-birds                  ;; number of birds in the flock (not currently used)
  has-eggs?                ;; eggs are available for pickup
  n-egg-pickup-days
  n-onsite-eggtraders      ;; number of eggtraders that come to flock to pick up eggs
  target-egg-market
  n-egg-delivery-days      ;; number of days per week that flock delivers eggs 
  meat-delivery-days       ;; number of days per week that flock delivers meat (not currently used)
]

flock-owners-own [
  n-visits                  ;; number of visits per day
  visit-list                ;; agentset of neighbors that receive visits from this owner

  n-visit-days-per-week
  flock
  fighting-cock 

  livebird-selldate
]

eggtraders-own [
  n-visits
  visit-list

  n-eggtrader-links
  egg-type           ;unused? chkchk
  target-egg-market  ;; one of the three markets
]

livetraders-own [
  n-visits
  visit-list

  livetrader-links
  n-bird-pickup-days
  n-flocks-per-pickup
]




;;;;;;;;;;;;;;;;;;;;;;;;
;;  SETUP PROCEDURES  ;;
;;;;;;;;;;;;;;;;;;;;;;;;

to startup
  ;; `startup` runs when the model first loads in GUI (not BS)
  ;; we can only set breed shapes here (not our other turtle sets)
  set-default-shape eggtraders "car"
  set-default-shape livetraders "truck"
  set-default-shape flock-owners "person"
  set-default-shape ducks "duck"
  set-default-shape chickens "backyard"  ;;chkchk
end

to setup        ;; setup the display with globals, patches, agents
  print (word "begin setup: run number " behaviorspace-run-number)
  ;; do not use clear-all! (it's a problem for our BS;
  ;; it resets **all** non-interface globals, including those set by BS,
  ;; where we are setting some of them!)
  clear-ticks   ;; version 5+
  clear-turtles
  clear-patches
  clear-drawing
  clear-all-plots
  clear-output

  setup-globals
  setup-roads       ;;must set up road **before** patches!
  setup-patches     ;;calls place-along-roads!
  setup-agents
  ask fgducks [table:put days-on-field ([who] of self) (list )]  ;;initialization of table

  initialize-locations-file  ;;must come before initialize-infection

  ;;reset ticks at *end* of setup (version 5+)
  ;;  http://ccl.northwestern.edu/netlogo/docs/dictionary.html#reset-ticks
  reset-ticks
  initialize-infection   ;; calls `infect-poultry` which executes ``ticks``, so comes *after* ``reset-ticks``
  do-plots   ;chkchk: plot initial state

  setup-visuals
  print (word "setup complete: run number " behaviorspace-run-number)
  test-setup
end


;; Setup the enumerations
to setup-globals
  ;;sliders and switches set the following:
  ;;  baseline
  ;;  scenario
  ;;  infection-source
  ;;  prop-fgd-barntofield
  ;;  shedding-period-duck
  ;;  shedding-period-chicken
  ;;  n-cooperatives

  ;;preliminaries
  ;; set `debug` to 0 during BahaviorSpace experiments (to speed runs)
  ifelse (behaviorspace-run-number > 0) [ set debug 0 ] [ set debug 5 ]
  ;;set the random seed for experiments (but allow it to vary in GUI)
  if (behaviorspace-run-number > 0) [
    random-seed (314159 + behaviorspace-run-number)
  ]
  set base-filename (make-filename
    behaviorspace-experiment-name scenario behaviorspace-run-number)
  ;;
  set runsim? true          ;; set runsim? false for programmatic stop of simulation
  set stop-sim-reason ""

  ;;; FILES
  ;; file for event logging
  set log-file (word "out/log/" base-filename ".log")
  carefully [file-delete log-file] []  ;;chkchk better location?
  log2file (word date-and-time "\n")
  ;; file for location data
  set locations-file (word "out/locations/" _fname ".txt")
  set locations-file (word "out/locations/" base-filename ".txt")

  ;;define agent-type lists (i.e., lists of breeds)
  ;; (must NOT be agentsets; here, those may be empty)
  ;; (comment: fc-owners are not a separate breed ...)
  set human-types (list eggtraders livetraders flock-owners)  ;;MUST *list* all human breeds
  set poultry-types (list fcs ducks chickens) ;;MUST *list* all poultry breeds
  set flock-types (list ducks chickens)
  set trader-types (list eggtraders livetraders)


  set access-fields nobody     ;; initialization is just a place-holder
  set days-on-field table:make
  set transmissions table:make
  foreach ["road-flock" "after-visit" "visitor-flock" "eggtrader-flock" "livetrader-flock" "arena-fc" "fc-flock"] [
    table:put transmissions ? [0]           ;; chkchkchkchk initial 0 needed??? chkchk is "visitor-flock" being used?
  ]
  set contaminations table:make
  foreach ["flock-livetrader" "flock-eggtrader" "flock-visitor" "flock-road" "flock-field"] [
    table:put contaminations ? [0]  ;;chkchkchkchk initial 0? I don't think so ...
  ]
  ;setup a flu-state transition table, to map date (ticks) to list (transitioning agents)
  set transition-dates table:make
  
  ;; calendar variables
  set DAY-NAMES ["Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"]
  
  setup-baseline
  setup-scenario
  record-params ;; record the final parameter values for this run
end ;;setup-globals

to setup-baseline
  ;; chkchk EGGTRADER-LINKS- Not here because need to set manually during runs chkchkchk
  ;;NOTE: this will be executed *after* BehaviorSpace sets values!
  ;;NOTE: this will be executed *before* a scenario sets values!
  ;;      (so do not set slider or switch variables here)
  if not (member? baseline ["pert" "mode"]) [error "unrecognized baseline"]
  ;;(chkchkchk counts in baseline.ini diverge slightly from table in paper!)
  ;; set values of constants
  let _ini read-params "baseline.ini"
  let _constantParams [ ;;constant throughout experiment
    "N-BYCHICKENS" "N-FCS" "N-CHICKEN-FARMS" ;;; chicken types
    "N-FGDS" "N-BYDUCKS" "N-DUCK-FARMS"      ;;; duck types
    "meat-eggs-both-byd"
    "N-BARNS" "N-LIVETRADERS"
    "N-MATCH-ARENAS" "N-PRACTICE-ARENAS"
    "N-MARKETS" "RAI-PER-DUCK"
    "flock-road-contam-pct" "flock-field-contam-pct" "flock-visitor-contam-pct"
    "latent-period" "road-contam-period" "field-contam-period" "human-contam-period"
    "n-nbrs" "visit-radius"
    "fgd-deliver-pct"
    "LIVETRADER-PICKUP-SCHEDULE" "EGGTRADER-PICKUP-SCHEDULE"
    "LIVEBIRD-REPLENISH"
    "n-flocks-per-pickup-ltrdr"
  ]
  foreach _constantParams [zerosToNumericDefaults ? _ini]  ;;only changes zeros

  let _replicateParams [ ;;constant throughout one replicate
    "road-pen-infect-pct" "road-byc-infect-pct"
    "from-visit-infect-pct" "to-visit-infect-pct"
    "flock-eggtrader-contam-pct" "eggtrader-flock-infect-pct"
    "flock-livetrader-contam-pct" "livetrader-flock-infect-pct"
    "fc-byc-infect-pct" "arena-fc-infect-pct"
  ]
  ;;for the replicateParams, we will use a different _ini table if baseline=mode
  ;;(otherwise we'll keep the previous assignment to _ini)
  let _ini2 _ini
  ifelse (baseline = "mode") [
    set _ini2 table:from-list (list
      (list "road-pen-infect-pct" 25)
      (list "road-byc-infect-pct" 55)
      (list "from-visit-infect-pct" 15 )
      (list "to-visit-infect-pct" 20 )
      (list "flock-eggtrader-contam-pct" 70)
      (list "eggtrader-flock-infect-pct" 70)
      (list "flock-livetrader-contam-pct" 83)
      (list "livetrader-flock-infect-pct" 82)
      (list "fc-byc-infect-pct" 83)
      (list "arena-fc-infect-pct" 48)
      (list "n-visits-task" task [1])  ;;chkchkchk
      (list "etlinks-task" task [1])  ;;chkchkchk
    )
  ] [
    if (baseline != "pert") [error "unrecognized baseline"]
  ]
  ;; change any zeros to the (pert or mode) param values
  foreach _replicateParams [zerosToNumericDefaults ? _ini2]

  ;;finally set the tasks
  foreach [
    "n-visits-task" "etlinks-task"
    "n-onsite-eggtraders-fgd-task" "n-onsite-eggtraders-byd-task" "n-onsite-eggtraders-fm-task"
    "n-egg-delivery-days-fgd-task" "n-egg-delivery-days-byd-task" "n-egg-delivery-days-fm-task"
    "n-egg-pickup-days-fgd-task" "n-egg-pickup-days-byd-task" "n-egg-pickup-days-fm-task"
    "n-bird-pickup-days-ltrdr-task"
  ] [
    let _taskname ?
    if (runresult _taskname = 0) [ ;;again, only change zeros
      zerosToTaskDefaults _taskname _ini
    ]
  ]
end

to setup-scenario  ;;observer proc
  ;;Called by `setup` *after* `setup-baseline`.
  ;;Side effects: set specified global parameters.
  ;;
  ;;First we choose a baseline (`pert` or `mode`);
  ;;then we choose a scenario to deviate from the baseline
  ;;(As it comes last in setup, this may **re**set some values!)
  ;;Note: use the `scenario` choser to choose a scenario;
  ;;this proc will then set the implied parameters.
  ;;CAUTION: a scenario comes last in setup and
  ;;  thus overwrites current values.
  ;;  (For example, those values set by an experiment!)
  ;;CAUTON: scenario names are incorporated in file names
  ;;  without testing; make sure they will be valid.
  ;;  (Alphanumeric is safe.)
  let _set-params []
  if (scenario = "none") [] ;; change no values
  ;;;;;; ROAD-FLOCK SCENARIOS ;;;;;;
  if (scenario = "etlinks(low)") [
    set _set-params
    (list
      (list "etlinks-task" (task [1]))
    )
  ]
  if (scenario = "etlinks(high)") [
    set _set-params
    (list
      (list "etlinks-task" (task [10]))
    )
  ]
  if (scenario = "road-flock(high)") [
    set _set-params
    [
      ["road-pen-infect-pct" 40]
      ["road-byc-infect-pct" 71]
    ]
  ]
  if (scenario = "road-flock(low)") [
    set _set-params
    [
      ["road-pen-infect-pct" 0]
      ["road-byc-infect-pct" 29]
    ]
  ]
  if (scenario = "road-flock(zero)") [
    set _set-params
    [
      ["road-pen-infect-pct" 0]
      ["road-byc-infect-pct" 0]
    ]
  ]
  ;;;;;; VISIT SCENARIOS ;;;;;;
  if (scenario = "visit(veryhigh)") [
    set _set-params
    [
      ["from-visit-infect-pct" 98]
      ["to-visit-infect-pct" 100]
    ]
  ]
  if (scenario = "visit(high)") [
    set _set-params
    [
      ["from-visit-infect-pct" 30]
      ["to-visit-infect-pct" 40]
    ]
  ]
  if (scenario = "visit(low)") [
    set _set-params
    [
      ["from-visit-infect-pct" 5]
      ["to-visit-infect-pct" 10]
    ]
  ]
  if (scenario = "visit(zero)") [
    set _set-params
    [
      ["from-visit-infect-pct" 0]
      ["to-visit-infect-pct" 0]
    ]
  ]
  ;;;;;; EGGTRADER SCENARIOS ;;;;;;
  ;; (used by eggtrade experiment)
  if (scenario = "eggtrade(high)") [
    set _set-params
    [
      ["flock-eggtrader-contam-pct" 88]
      ["eggtrader-flock-infect-pct" 85]
    ]
  ]
  if (scenario = "eggtrade(low)") [
    set _set-params
    [
      ["flock-eggtrader-contam-pct" 53]
      ["eggtrader-flock-infect-pct" 55]
    ]
  ]
  ;;set the parameter values for the chosen scenario
  foreach _set-params [run (word "set " item 0 ? " " item 1 ?)]
end

to record-params
  ;;record the parameter values used for the run
  ;; (should call this *after* `set-scenario`)
  let _filename (word "out/params/" base-filename ".txt")
  carefully [file-delete _filename] []
  file-open _filename
  let _params [
    "scenario"
    "road-byc-infect-pct"
    "road-pen-infect-pct"
    "from-visit-infect-pct"
    "to-visit-infect-pct"
    "flock-eggtrader-contam-pct"
    "eggtrader-flock-infect-pct"
  ]
  foreach _params [file-print (word ? " : " run-result ?)]
  file-close
end

to initialize-locations-file
  ;;;initialize the file of the transmission locations with initial location
  ;; delete possible old file for writing the transmission locations
  carefully [file-delete locations-file][]
  ;; write the initial locations
  file-open locations-file
  file-print "positions after setup:"
  ask turtle-set poultry-types [
    file-show patch-here
    if (member? flu-state "ei") [error "nobody should be infected yet"]
  ]
  file-close
  log2file locations-file  ;; log the filename of the locations file
end

;;;;;;;;;
;Patches;
;;;;;;;;;

;; Even though our nodes (intersections) are turtles rather than patches,
;; we must call setup-roads *before* `setup-patches` (which has an error check),
;; because the latter calls `setup-road-patches` (which needs our roads).
to setup-roads
  ;;first we create the road intersections (later we link them)
  let nodes table:make  ;;we will map ids to turtles (our nodes/intersections)
  file-open "intersections.txt"  ;;SSV file; separator is a *single* space
  while [not file-at-end?] [
    let data file-read-line
    if not (first data = "#") [  ;;ignore comment lines
      set data split data " "  ;;split on *single* (!!) space
      ;; cast the co-ordinates from strings to numbers
      let x read-from-string item 0 data
      let y read-from-string item 1 data
      ;; get the intersection id and intersection type
      let _id item 2 data
      ;let _itype last data   ;;chk: intersection type not currently used
      ask patch x y [
        sprout-intersections 1 [
          set home-patch patch-here
          setxy x y  ;;in case x and y are not integers
          table:put nodes _id self  ;;map id to intersection (node)
          ;;set label (word "(" _id ")")
        ]
      ]
    ]
  ]
  file-close

  log2file (word "nodes: " nodes)

  ;;next we represent roads by linking intersections
  file-open "roads.txt"  ;;SSV file; separator is a *single* space
  while [not file-at-end?] [
    let data file-read-line
    if not (first data = "#") [  ;; prevent reading of the comment line
      set data split data " "   ;; split location indicated by one (!) space
      let node1 table:get nodes item 0 data
      let node2 table:get nodes item 1 data
      ask node1 [
        create-road-link-with node2 [
          set roadtype last data
        ]
      ]
    ]
  ]
  file-close

  log2file (word "roads & types:\n" [list self roadtype] of road-links)
end ;;END:setup-roads


to setup-patches ;;observer proc
  if not (count road-links > 0) [error "must setup-roads *before* setup-patches"]
  ;; First, initialize all patches as growing rice fields (afterwards, we'll repurpose some of them)
  ask patches [
    set patch-type "ricefield"   ;;this inital value changes whenever we change the patch type
    set field-state "growing"    ;;we change this for non-field patch types
    set field-patches nobody
    set occupant nobody
    set road-access min-one-of intersections [distance myself]
    ;; set pcontam-until 0  ; default
  ]
  
  setup-road-patches
  place-objects-along-roads ;;(arenas, markets, coops, barns, & two "out of DKY" patches)
  setup-ricefields
end ;;END:setup-patches



to setup-road-patches
  ;;let every road keep a set of neaby patches
  ask road-links [
    set road-patches patches with [patch-near-link? self myself]
    ask road-patches [
      set pcontam-period road-contam-period
      set field-state ""
      ;;some road patches may later become barns, etc., but initially:
      set patch-type "road"
    ]
  ]
end

to place-objects-along-roads
  ;;;; PLACE OBJECTS ALONG ROADS (arenas, markets, coops, barns, two "out of DKY" patches ;chk )
  ;; patches along roads are currently road patches (but that will now change)
  if (count patches with [patch-type = "road"] = 0) [error "must first setup road patches"]
  let _major-roads road-links with [roadtype = "major_road"]
  let _loose-roads road-links with [roadtype = "loose_road"]
  let _small-roads road-links with [roadtype = "small_road"]

  ;; place out-of-subdistrict-patches  (location for flocks that "leave" the district)
  ;; (do this first so it won't overwrite an object "placement")
  ;; WARNING: using hard-coded patch coordinates (to match roads) chkchkchk
  ;; (can probably put these on the edge chkchk)
  set out-of-subdistrict-patches (patch-set patch min-pxcor 30 patch max-pxcor 30)
  ask out-of-subdistrict-patches [set patch-type "out" set field-state ""]

  ;; place match arenas
  place-along-roads N-MATCH-ARENAS "match-arena" _loose-roads
  set match-arenas patches with [ patch-type = "match-arena" ]

  ;; place practice arenas                                                     
  let _n3 int (N-PRACTICE-ARENAS / 3)
  place-along-roads-north _n3 "practice-arena" _loose-roads  ;;chkchk
  place-along-roads-south _n3 "practice-arena" _loose-roads
  place-along-roads (N-PRACTICE-ARENAS - (2 * _n3)) "practice-arena" _loose-roads
  set practice-arenas patches with [ patch-type = "practice-arena" ]

  ;; place markets
  place-along-roads N-MARKETS "market" _major-roads
  set markets patches with [ patch-type = "market" ]

  ;; place cooperatives (no cooperatives in baseline!)
  if (n-cooperatives < 0 or n-cooperatives != round n-cooperatives) [
    error "n-cooperatives must be nonnegative integer"
  ]
  place-along-roads-north n-cooperatives "cooperative" _major-roads
  set cooperatives patches with [ patch-type = "cooperative" ]

  ;; place barns (60%, 30% and 10% around major, loose and small roads)
  place-along-roads round (0.6 * N-BARNS) "barn" _major-roads
  place-along-roads round (0.3 * N-BARNS) "barn" _loose-roads
  place-along-roads round (0.1 * N-BARNS) "barn" _small-roads
  set barns patches with [ patch-type = "barn" ]
end ;;END:place-objects-along-roads


to place-along-roads [#num #type #roads]  ;;observer proc
  let _patches ((patch-set [road-patches] of #roads) with [(not any? turtles-here) and (patch-type = "road")])
  ask n-of #num _patches [ 
    set patch-type #type
    set field-state ""
  ]
end

to place-along-roads-north [#num #type #roads]  ;;observer proc
  let _patches ((patch-set [road-patches] of #roads) with [(not any? turtles-here) and (patch-type = "road") and ( pycor > 0)])
  ask n-of #num _patches [ 
    set patch-type #type
    set field-state ""
  ]
end

to place-along-roads-south [#num #type #roads]  ;;observer proc
  let _patches ((patch-set [road-patches] of #roads) with [(not any? turtles-here) and (patch-type = "road") and ( pycor < 0)])
  ask n-of #num _patches [ 
    set patch-type #type
    set field-state ""
  ]
end


to setup-ricefields ;;observer proc
  set rice-fields (patches with [ field-state = "growing" ])  ;;finalize global variable
  ask rice-fields [set pcontam-period field-contam-period]   ;; are these the only agents for this?

  create-fields
  ask access-fields [
    harvest-field                               ;;changes field-state (and color), makes rai available
  ]

  log2file (word "total rai available at sim start is " sum [rai] of access-fields)
  log2file (word "this shd match " sum [rai] of patches)
end

to create-fields  ;; observer proc
  if (debug > 5) [log2file "enter proc: create-fields"]
  let total-ricefield-rai 6552 ;;shd be a global chkchkchk
  let num-field-patches count rice-fields
  if (num-field-patches = 0) [error "first establish rice fields"]
  let av-rai-per-patch (total-ricefield-rai / num-field-patches)
  let most-likely (150 / av-rai-per-patch)  ;; PERT most likely value (shd be global chkchk)
  let spacing ceiling sqrt most-likely
  let xvals n-values floor (world-width / spacing) [spacing / 2 + ? * spacing]
  let yvals n-values floor (world-height / spacing) [spacing / 2 + ? * spacing]
  let _targetradius 9
  ;; create the field-access patches
  let _y min-pycor + (_targetradius / 2)
  while [_y < max-pycor] [
    let _x min-pxcor + (_targetradius / 2)
    while [_x < max-pxcor] [
      let _new-access-field patch _x _y 
      if ([field-state] of _new-access-field = "growing") [
        set access-fields (patch-set access-fields _new-access-field)
      ]
      set _x (_x + _targetradius)
    ]
    set _y (_y + _targetradius)
  ]
  log2file (word (count access-fields) " access fields created")
  ;; number the access fields and set their size
  let _n-access-fields 0
  ask access-fields [
    set _n-access-fields (_n-access-fields + 1)
    set field-num _n-access-fields
    set rai random-pert 20 150 610 ;;chk is this the right way to set field size??
    log2file (word "provides access to " rai " rai")
  ]
  ;; make minimal fields around the access patch
  ask access-fields [
    let _field patch-set [neighbors] of neighbors
    ask _field with [field-state = "growing" and field-num = 0] [ ;; does *not* include the access-field patch
      set field-num [field-num] of myself
    ]
  ]
  ;; fill in the fields
  ask access-fields [
    let _n-patches (rai / 2)
    let _radius ceiling sqrt (_n-patches / 3)
    let _field patches in-radius _radius with [field-state = "growing" and field-num = 0]  ;; does *not* include the access-field patch
    ask _field [
      set field-num [field-num] of myself
    ]
  ]
  ;; attach remaining rice-field patches to fields
  ask patches with [field-state = "growing" and field-num = 0] [
    let _field-patches patches with [field-state = "growing" and field-num != 0]
    set field-num [field-num] of min-one-of _field-patches [distance myself]
  ]
  ask access-fields [
   set field-patches (patches with [field-num = [field-num] of myself])    ;; *include* the access patch
  ]
  if (debug > 5) [log2file "exit proc: create-fields"]
end


; to old-create-fields [#num]  ;; observer proc
;   if (debug > 5) [log2file "enter proc: create-fields"]
;   ;; randomly create growing fields
;   let _new-access-fields n-of #num rice-fields with [field-state = "growing" and field-num = 0]
;   set access-fields (patch-set access-fields _new-access-fields)
;   ;; number the field-access patches
;   ask _new-access-fields [ 
;     set n-access-fields (n-access-fields + 1)
;     set field-num n-access-fields               ;; unique field identifier
;   ]
;   ;; create the fields
;   ask _new-access-fields [
;     let _field patch-set [neighbors] of neighbors
;     set _field _field with [field-state = "growing" and field-num = 0]  ;; does *not* include the access-field patch
;     ask _field [
;       set field-num [field-num] of myself
;     ]
;     set field-patches (patch-set self _field)    ;; *include* the access patch
;   ]
;   if (debug > 5) [log2file "exit proc: create-fields"]
; end

to harvest-field ;;patch (access field) proc
  if (debug > 5) [log2file "enter proc: harvest-field"]
  if (field-state != "growing") [ error (word "field-state '" field-state "' should be 'growing'")]
  if (field-patches = nobody) [ error "apply harvest-field proc only to access fields"]
  ;;set rai random-pert  20 150 610 ;;chkchkchk do we still want this?? already used when create access fields!
  ask field-patches [ set field-state "harvested"]
  if (debug > 5) [log2file "exit proc: harvest-field"]
end

;to fallow-field ;;access-field (patch) proc
;  if (debug > 5) [log2file "enter proc: fallow-field"]
;  if (field-state != "harvested") [
;    let _msg (word "patch " self " has field-num " field-num " and field-patches " field-patches)
;    log2file _msg
;    error (word _msg "field-state '" field-state "' should be 'harvested'")
;  ]
;  if (field-patches = nobody) [ error "apply fallow-field proc only to access fields"]
;  set rai 0                ;; "wasted rai" chkchk! shd separate area from food-days
;  ask field-patches [set field-state "fallow"]
;  if (debug > 5) [log2file "exit proc: fallow-field"]
;end

to fallow [#field] ;;access-field (patch) proc
  if (debug > 5) [log2file "enter proc: fallow"]
  let _field-patches [field-patches] of #field
  let _field-state [field-state] of #field
  ;first, do some error checks
  if (_field-patches = nobody) [ error "apply `fallow` only to access fields"]
  if (_field-state != "harvested") [
    let _msg
      (word
      "patch " #field
      "has field-num " [field-num] of #field
      " and field-patches " _field-patches
      )
    log2file _msg
    error (word _msg "field-state '" field-state "' should be 'harvested'")
  ]
  ;finally, do the actual fallowing
  ask #field [set rai 0]                ;; "wasted rai" chkchk! shd separate area from food-days
  ask _field-patches [set field-state "fallow"]
  if (debug > 5) [log2file "exit proc: fallow"]
end




;;;;;;;;;;;;
;; Agents ;;
;;;;;;;;;;;;

to setup-agents ;;observer proc; sets up flocks and owners, and traders
  if (debug > 5) [log2file "enter proc: setup-agents"]
  
  setup-fgds-and-owners
  setup-bychickens-and-owners
  setup-fcs-and-owners
  setup-byducks-and-owners
  setup-duckfarms-and-owners     ;;sets `fmducks` and `fmd-owners` globals
  setup-chickenfarms-and-owners  ;;sets `fmchickens` and `fmc-owners` globals

  ;; for completeness, create the `farm` & `farm-owners` agentset, but currently unused
  set farms (turtle-set fmducks fmchickens)  ;;chkchk unneeded??
  set farm-owners (turtle-set fmd-owners fmc-owners)

  setup-egg&meat-pd  ;;pickup and deliver days; must come before setting up traders!
  ;;must setup eggtraders *after* poulty creation (and *before* any work with all "humans")
  setup-eggtraders  ;;creates eggtraders and the egg pickup network
  setup-livetraders

  ;;;; Establish a fixed visit-list for each human
  ;;  comment: this does not ensure the neighbor will be home when visited, but we are more concerned about the feces on the ground
  let _humans turtle-set human-types
  ask _humans [
    set contam-period human-contam-period ;;;ALL humans have same contam period
    ;set the number of visits per day for each human
    set n-visits (runresult n-visits-task)
    let _home home-patch
    ;; visit-list is an agentset of humans
    ;; (note: all neighbors within visit-radius are equally likely to be in the visit-list)
    set visit-list min-n-of n-nbrs other _humans [int (distance _home / visit-radius)]
    if (debug >= 5) [
      let _msg (word "turtle " who " with type " breed " has visit list: " [self] of visit-list) 
      ask visit-list [
        set _msg (word _msg "\n\t" self " is distance: " (distance myself))
      ]
      log2file _msg
    ]
  ]
 
  if (debug > 5) [log2file "exit proc: setup-agents"]
end ;;setup-agents


to setup-livetraders  ;;observer proc (called by setup-agents)
  ;; distribute livetraders roughly evenly spaced on a circule around the center of the subdistrict
  ;; (livetraders trade live birds)
  let _cx ((min-pxcor + max-pxcor) / 2)
  let _cy ((min-pycor + max-pycor) / 2)
  let _center patch _cx _cy
  let _radius int min (list world-width world-height) / 3
  let _locations (list )
  let _rotation (360 / N-LIVETRADERS)
  let _offset (_rotation / 2)
  foreach n-values N-LIVETRADERS [?] [
    ask _center [set _locations lput patch-at-heading-and-distance (_offset + ? * _rotation) _radius _locations]
  ]
  foreach _locations [
    let _location ?
    let _okbarns barns with [not any? turtles-here]
    ask min-one-of _okbarns [distance _location] [
      sprout-livetraders 1 [ init-livetrader ] ;; sets homepatch
    ]
  ]
end

to add-flockowner  ;; flock proc: associate flock with owner
  let _flock self
  let _owner nobody   ;; to be set in following block
  hatch-flock-owners 1 [
    set flock _flock
    set _owner self
    set home-patch patch-here
  ]
  set owner _owner
end

to setup-byducks-and-owners
  ;;BYD-OWNERS: sprout backyard ducks and owners from barn patches          
  ask n-of N-BYDUCKS barns with [ not any? turtles-here ] [
    sprout-ducks 1 [ init-byduck ] 
  ]
  set byducks ducks with [biotype = "pen"]
  if (N-BYDUCKS != count byducks) [error "wrong number of byducks"]
  ask byducks [add-flockowner]
  set byd-owners turtle-set [owner] of byducks
  ask byd-owners [init-byd-owner]
end

to setup-fgds-and-owners
  ;;FGD: sprout n-fgds duck flocks from barn and field patches (home patch)
  let _n-barn-fgds (ceiling ((prop-fgd-barntofield) * N-FGDS ))  ;;chkchk round?
  let _n-field-fgds (N-FGDS - _n-barn-fgds)
  ;; some fgds have barns
  ask n-of _n-barn-fgds barns with [ not any? turtles-here ] [
    sprout-ducks 1 [ init-fgduck set field-only? false ]
  ]
  ;; other fgds stay on their fields
  ask n-of _n-field-fgds access-fields with [not any? turtles-here] [
    sprout-ducks 1 [ init-fgduck set field-only? true ]
  ]
  ;; create an agent set of fgds, which will not change
  set fgducks ducks with [biotype = "fg"]
  ask fgducks [
    add-flockowner
    if (field-only?) [
      ask owner [create-fgd-link-with myself [tie hide-link show (word self " tied")]]
    ]
  ]
  set fgd-owners turtle-set [owner] of fgducks
  ask fgd-owners [init-fgdowner]
end

to setup-bychickens-and-owners
  ;;sprout backyard poultry from barn patches, 
  ;;  elsewhere we assign a certain number of byc-owners to have fighting cocks
  ;; error check (make sure we first ran `place-objects-along-roads`):
  if (N-PRACTICE-ARENAS != count practice-arenas) [error "must first setup practice arenas"]
  ask practice-arenas [
    if (any? turtles-here) [error "practice-arenas should not yet be populated!" ]
    sprout-chickens 1 [ init-bychicken ] ;;sets biotype
  ]
  ask n-of (N-BYCHICKENS - N-PRACTICE-ARENAS) barns with [ not any? turtles-here ] [
    sprout-chickens 1 [ init-bychicken ] ;;sets biotype
  ]
  set bychickens chickens with [biotype = "by"]
  ask bychickens [ add-flockowner ]
  set byc-owners (turtle-set [owner] of bychickens)
  ask byc-owners [init-byc-owner]
end

to setup-fcs-and-owners
  ;; the global `fc-owners` agentset is set only once (here)
  ;; 
  if (N-FCS < N-PRACTICE-ARENAS) [error "too few fighting cocks"]
  if (N-PRACTICE-ARENAS != count patches with [patch-type = "practice-arena"]) [error "first setup arenas"]
  set fc-owners (turtle-set
                (byc-owners with [patch-type = "practice-arena"])
                (n-of (N-FCS - N-PRACTICE-ARENAS) (byc-owners with [patch-type = "barn"]))
                )
  ask fc-owners [
    let _owner self
    let _fc nobody
    hatch-fcs 1 [
      set _fc self
      set owner _owner
      init-fc
    ]
    set fighting-cock _fc
  ]
end


to setup-duckfarms-and-owners
  ;;sprout large duck farms from barn patches                       
  ask n-of N-DUCK-FARMS barns with [ not any? turtles-here ] [
    sprout-ducks 1 [ init-fmduck ] ;;sets biotype to "farm"
  ]
  ;;set global variables to agentsets (*not* breeds)
  set fmducks ducks with [biotype = "farm"]
  ask fmducks [ add-flockowner ]
  set fmd-owners turtle-set [owner] of (ducks with [biotype = "farm"])
  ask fmd-owners [ init-farm-owner ] ;;chkchk
end  

to setup-chickenfarms-and-owners
  ;;sprout a few large chicken farms from barn patches                       
  ask n-of N-CHICKEN-FARMS barns with [ not any? turtles-here ] [
    sprout-chickens 1 [ init-fmchicken ] 
  ]
  ;;set global variables to agentsets (*not* breeds)
  set fmchickens chickens with [biotype = "farm"]
  ask fmchickens [ add-flockowner ]
  set fmc-owners turtle-set [owner] of fmchickens
  ask fmc-owners [ init-farm-owner ] ;;chkchk
end  

to initialize-infection
  ;;randomly choose a flock for the initial infection
  if (infection-source = "fgduck") [
    ask one-of fgducks [infect-poultry]
    stop
  ]
  if (infection-source = "byduck") [
    ask one-of byducks [infect-poultry]
    stop
  ]
  if (infection-source = "bychicken") [
    ask one-of bychickens [infect-poultry]
    stop
  ]
  if (infection-source = "farm") [
    ask one-of farms [infect-poultry]
    stop
  ]
  error "unknown infection source"
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agent Initializations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

to init-flock-owner
  set fighting-cock nobody
  ;;hard-code number of visiting days (add explanation chkchk )
  set n-visit-days-per-week random-binomial 7 0.42857
  set livebird-selldate (- LIVEBIRD-REPLENISH) ;;initially all have birds to sell chkchk
end

to init-byd-owner
  init-flock-owner
end

to init-byc-owner
  init-flock-owner
end

to init-farm-owner
  init-flock-owner
end

to init-fgdowner
  init-flock-owner
  set home-patch patch-here  ;;chkchkchk
  ;; The number of traders per FGD flock will vary, as determined by a PD
end

to init-flock
  set home-patch patch-here ;;chkchk
  set flu-state "s"
end

to init-duck
  init-flock
  set has-eggs? true
  set shedding-period shedding-period-duck
end

to init-chicken
  init-flock
  set shedding-period shedding-period-chicken
end

to init-fmduck
  init-duck
  set biotype "farm"
end

to init-fmchicken
  init-chicken
  set has-eggs? true
  set biotype "farm"
end

to init-byduck
  init-duck
  set biotype "pen"
  ;n-onsite-eggtraders is set below
end

to init-bychicken
  init-chicken
  set biotype "by"
  set n-onsite-eggtraders 0             ;; note: bycs don't sell eggs
end

to init-fgduck  ;;initializations for an fgduck
  init-duck
  set biotype "fg"
;  let rw-values n-values 23 [(? + 1) * 1e-3] chkchkchk
;  let rw-weights [ 0.0037 0.0383 0.0728 0.0846 0.0844 0.0796 0.0814 0.0794 0.0715 0.0645 0.0605 0.0534 0.0479 0.0393 0.0303 0.027 0.0222 0.0178 0.015 0.01 0.0069 0.0051 0.0044 ]
  ;; (Road contamination can also be made a continuous variable based on flock size and shedding amount per duck.)
  set n-birds 2000 ;; flock size of 2000 ducks may be changed in the future chk
  set target-field nobody
  set has-eggs? true   ;;chkchkchk use this attribute?
end


to init-fc ;; fc proc chkchk
  init-chicken  ;; no biotype?? chkchkchk

  ;;choose a practice arena and a match-arena for each fighting cock
  set target-match-arena min-one-of match-arenas [ distance myself ] ;;only one in baseline model
  set target-practice-arena one-of practice-arenas  ;;distance irrelevant? chkchkchk
  ;;half of fcs are eligible to practice each week
  set practice-fight-week one-of [0 1]
end

to init-eggtrader
  set home-patch patch-here
  set n-eggtrader-links runresult etlinks-task ;; varied in our Se analysis: tested 1 and 10 chkchk
  log2file (word "eggtrader " who " can service " n-eggtrader-links " farms")               
end

to init-livetrader  ;;livetrader proc
  set home-patch patch-here
  set n-flocks-per-pickup n-flocks-per-pickup-ltrdr
  set n-bird-pickup-days runresult n-bird-pickup-days-ltrdr-task
  log2file (word "livetrader " who " has " n-bird-pickup-days " bird-pickup-days.")
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      Go        ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go  ;; observer proc (the schedule; one iteration)
  file-open locations-file file-print (word "day " (ticks + 1)) file-close
  ifelse runsim? [
    update-calendar
    ask (turtle-set fgducks fmducks byducks fmchickens) [set has-eggs? true]  ;;chkchkchk why is this in here? chkchk

    ask fgducks [go-to-field]  ;note: field-only fgducks may need a new field!
    trade-eggs
    trade-birds
    fight-cocks
    ask fgducks with [not field-only?] [
      travel-roads-to home-patch 
    ]

    ask fgd-links [untie print (word end1 "untied for visiting")]  ;;chkchk find more elegant way to do all this
    ask flock-owners [visit]
    ask fgd-links [tie print (word end1 "tied after visiting")]

    roads-infect-flocks
    ask fcs [infect-backyard]
    update-flu-state  ;does all needed flu-state transitions
    ;ask turtles with [(member? breed poultry-types) and (member? flu-state "ei")] [old-update-flu-state ]

    update-visuals ;; calls do-plots, which must come before decontamination of traders, locations chkchkchk
    
    tick  ;;note: we tick at the end of the iteration
    test-for-endsim ;;note: don't call stop here (bc then BS won't record final outcomes)
  ] [
    let _msg (word "Simulation stopped: " stop-sim-reason)
    file-open log-file file-print _msg file-close
    print _msg
    stop
  ]
  test-go
end

to update-calendar ;; observer proc
  ;;The calendar is updated at the beginning of each iteration.
  log2file (word "ticks is " ticks ". ")
  set day-idx (ticks mod 7)
  set day-name item day-idx DAY-NAMES  ;;just for reading convenience
  set week int (ticks / 7)
  set livetrader-pickup-days item day-idx LIVETRADER-PICKUP-SCHEDULE
  set eggtrader-pickup-days item day-idx EGGTRADER-PICKUP-SCHEDULE
  set visit-days item day-idx EGGTRADER-PICKUP-SCHEDULE
  print (word "run " behaviorspace-run-number ": " day-name " of week " week " is starting!!")
  foreach table:keys transmissions [
    table:put transmissions ? (lput 0 table:get transmissions ?)
  ]
  foreach table:keys contaminations [
    table:put contaminations ? (lput 0 table:get contaminations ?)
  ]
end


;;;;;;;;;;;;;;;;;
;;Grazing Procs;;
;;;;;;;;;;;;;;;;;

to go-to-field  ;; fgduck proc
  if (debug > 5) [log2file "enter proc: go-to-field"]
  set-target-field    ;;note: sets occupant on target fields and homepatch of field-only fgd-owners
  travel-roads-to target-field
  graze-here ;; chk make infection possible during grazing?
  if (debug > 5) [log2file "exit proc: go-to-field"]
end

to set-target-field ;; fgduck proc
  ;;change target if needed (otherwise do nothing)
  if (debug > 5) [log2file "enter proc: set-target-field"]
  if ((target-field != nobody) and (member? target-field out-of-subdistrict-patches)) [stop] ;; chk out of sim forever ...
  let _rai-needed (n-birds * RAI-PER-DUCK)
  if (debug > 3) [log2file (word "rai-needed: " _rai-needed)]
  let _old-target target-field
  if ((_old-target != nobody) and ([rai] of _old-target < _rai-needed)) [
    if (debug > 3) [ log2file (word "rai available (" ([rai] of _old-target) ") is inadequate; change target.") ]
    ask _old-target [
      set occupant nobody
    ]
    fallow _old-target
    set _old-target nobody
  ]
  ifelse (_old-target = nobody) [ ; set a new target
    let _candidates access-fields with [(rai >= _rai-needed) and (occupant = nobody)]
    if (not any? _candidates) [
      show "WARNING: no unoccupied fields left with adequate rai; move flocks out of district"
      set _candidates out-of-subdistrict-patches
      show " moving out of district"
    ]
    let _new-target min-one-of _candidates [ distance myself ]
    set target-field _new-target
    ask target-field [set occupant self]
    show (word " chose target field " target-field ". ")
    if (field-only?) [
      set home-patch _new-target
      ask owner [set home-patch _new-target move-to home-patch]
    ]
    ;; update days on field
    let _who ([who] of self)
    table:put days-on-field _who (lput 0 table:get days-on-field _who)
  ][
    let _who ([who] of self)
    let _oldlist table:get days-on-field _who
    table:put days-on-field _who (lput (last _oldlist + 1) butlast _oldlist)
  ]
  if (debug > 5) [log2file "exit proc: set-target-field"]
end

to graze-here  ;; fgduck proc
  if ((debug > 3) and (member? patch-here out-of-subdistrict-patches)) [
    show "grazing out of subdistrict"
  ]
  let consume (n-birds * RAI-PER-DUCK)
  set rai (rai - consume)  ;; rai is a patch attribute
  if (flu-state = "i") [risk-ground-contamination "flock-field"]
 ; expose
end

;;;;;;;;;;;;;;;;;;
;;Movement Procs;;
;;;;;;;;;;;;;;;;;;


to travel-roads-to [#target]  ;; fgduck proc, #target is a patch
  ;; infected flocks may contaminate road patches, which can
  ;; include barns, etc.
  if (breed != ducks or biotype != "fg") [error "travel-roads-to is an fgduck proc"]
  if (debug > 5) [log2file (word "enter proc: travel-roads-to with target " #target)]
  if (debug > 3) [log2file (word "traveling to " #target)]
  if ((debug > 3) and (member? #target out-of-subdistrict-patches)) [
    log2file "traveling out of subdistrict"
  ]
  if (#target = nobody) [ log2file "not given a target!" error "no target" ]
  if (#target = patch-here) [log2file "already at target" stop]
  let _intA road-access
  let _intB [road-access] of #target
  let _path-nodes 0  ;; arbitrary initialization; value set below
  let _path-links 0
  ask _intA [
    set _path-nodes network:link-path-turtles _intB road-links
    set _path-links network:link-path _intB road-links
  ] 
  log2file (word "short path from " _intA " to " _intB " is " _path-nodes)
  log2file (word "short path from " _intA " to " _intB " is " _path-links)
  ;;confirm that all nodes are connected via roads ...
  ifelse (length _path-nodes = 0) [ 
    log2file (word "WARNING: no roads from " _intA " to " _intB)
    move-to _intB
    set _path-nodes (list _intA)
  ][
    if (debug > 3) [log2file "traveling roads"]
  ]
  ;;first, move to the road access of the current patch
  move-to _intA
  ;;second, move from node to node along the path
  foreach but-first _path-nodes [
    let next-node ?
    if (debug > 3) [print (word "traveling to " next-node)]
    face next-node
    while [distance next-node > 0.5] [forward 0.5]
    move-to next-node
  ]
  ;;finally, move from the access to the target patch
  move-to #target
  if (debug > 3) [log2file (word "reached target" #target)]
  if (flu-state = "i") [
    let _risks patch-set [road-patches] of link-set _path-links
    ask _risks [ risk-ground-contamination "flock-road" ]  ;;note: road patches can include barns!
  ]
  if (debug > 5) [log2file (word "exit proc: travel-roads-to; current location " patch-here)]
end

to risk-transmission [#type]  ;; poulty proc
  if (member? flu-state "eir") [stop] ;;exit proc  chkchk (allow duck reinfection?)
  let _p 0
  if (#type = "eggtrader-flock") [
    set _p eggtrader-flock-infect-pct
  ]
  if (#type = "livetrader-flock") [
    set _p livetrader-flock-infect-pct
  ]
  if (#type = "visitor-flock") [
    set _p to-visit-infect-pct
  ]
  if (#type = "after-visit") [
    set _p from-visit-infect-pct
  ]
  if (#type = "road-flock") [
    if member? self bychickens [
      set _p road-byc-infect-pct
    ]
    let _penned (turtle-set byducks fmducks fmchickens) ;;chkchk
    if member? self _penned [
      if member? self bychickens [error "dual membership not allowed"]
      set _p road-pen-infect-pct
    ] 
  ]
  if (#type = "arena-fc") [
    set _p arena-fc-infect-pct
  ]
  if (#type = "fc-flock") [
    set _p fc-byc-infect-pct
  ]
  if (random-float 100 < _p) [ ;;infection spreads!
    infect-poultry  ;; state transition
    ;increment this period's count for this transmission type
    ;;first, get the list of transmission
    let _oldct table:get transmissions #type
    ;;chkchkchk this v is ugly ...
    if (length _oldct != ticks + 2) [error (word "Compare " length _oldct " and " ticks)]
    ;;next, increment the last item of the list
    let _newct lput (last _oldct + 1) butlast _oldct
    table:put transmissions #type _newct
  ]
  if (_p < 1 and _p != 0) [ ;; a test for value error
        let _msg (word "WARN: problem probability? " #type _p)
        set _msg (word _msg " (state probabilies as pct)")
        print _msg
        file-open "out/birdflu-xxx.log"
        file-print _msg
        file-close
  ]
end


to risk-human-contamination [#type]  ;; human proc
  ;; allows for recontamination
  let _p 0
  let _old-until contam-until
  let _new-until 0
  if (#type = "flock-eggtrader") [
    set _p flock-eggtrader-contam-pct
  ]
  if (#type = "flock-livetrader") [
    set _p flock-livetrader-contam-pct
  ]
  if (#type = "flock-visitor") [
    set _p flock-visitor-contam-pct
  ]
  if (random-float 100 < _p) [
    set _new-until (ticks + contam-period)
  ]
  if (_new-until > _old-until) [
    set contam-until _new-until
    file-open log-file file-show "is contaminated" file-close
    let _oldct table:get contaminations #type
    let _newct lput (last _oldct + 1) butlast _oldct
    table:put contaminations #type _newct
  ]
end


to risk-ground-contamination [#type]  ;; patch proc
  ;; allows for recontamination
  let _old-until pcontam-until
  let _new-until 0
  let _p 0  ;;chkchk
  if (#type = "flock-road") [
    set _p flock-road-contam-pct
  ]
  if (#type = "flock-field") [
    set _p flock-field-contam-pct
  ]
  if (random-float 100 < _p) [
    set _new-until (ticks + pcontam-period)
  ]
  if (_new-until > _old-until) [
    set pcontam-until _new-until
    file-open log-file file-show "is contaminated" file-close
    let _oldct table:get contaminations #type
    let _newct lput (last _oldct + 1) butlast _oldct
    table:put contaminations #type _newct
  ]
end
    
to infect-poultry ;; poultry proc; state transition
  if not member? breed poultry-types [error (word breed " is not a poultry type")]
  if (flu-state != "s") [error "flu-state should be 's'"]
  set flu-state "e"     ;;chk color code this?
  ;; latent-period is one day for all flocks
  add-transition (ticks + latent-period) self
  set infection-time 0
  record-infection-location
  log2file "is infected"
end


to add-transition [#date #agent]
  ;; initialize flu-state transitions for this date if necessary
  if not table:has-key? transition-dates #date [
    table:put transition-dates #date (list )
  ]
  ;; add a transition
  table:put transition-dates #date (lput #agent table:get transition-dates #date)
end

to record-infection-location
  file-open locations-file
  file-show patch-here
  file-close
end


;;;;;;;;;;;;;;;;;
;; Egg Network ;;
;;;;;;;;;;;;;;;;;


to setup-egg&meat-pd  ;;chkchkchkchk
  ;; Set the **number** of pickup and delivery days for each supplier.
  ;; (Really only for eggs, because meat delivery is a no-op (i.e., no risks).)
  ;; Also set the number of onsite eggtraders.
  ;; NOTE: bychickens produce livebirds but NOT eggs for sale.
  ;; WARNING: there are a few hard coded numbers in here! (But they scale appropriately.)

  ;;FGDs  (eggs only!)
  ;; ONLY fgds deliver to coops (conditional on coop existence).
  ;;First break FGDs into those that deliver and those that have pickup.
  ;;(If there is a coop, all FGDs deliver to a coop.
  ;; Otherwise, half have pickup, and half deliver to markets.)
  ;; Form two groups, those that get pickup, and those that deliver.
  let _deliver-pct fgd-deliver-pct
  if (n-cooperatives > 0) [set _deliver-pct 100] ;;all deliver to coops when possible
  ;; divide into pickup and delivery (baseline: about half deliver to market)
  setup-egg-pickup-deliver-both fgducks (list (100 - _deliver-pct) _deliver-pct 0)
    n-egg-pickup-days-fgd-task n-egg-delivery-days-fgd-task n-onsite-eggtraders-fgd-task

  ;;BYDUCKS (penned ducks; BOTH eggs and meat)
  let _meat-eggs-both weighted-integer-partition N-BYDUCKS meat-eggs-both-byd
  let _groups disjoint-subsets byducks _meat-eggs-both
  let _byd-meat (turtle-set item 0 _groups item 2 _groups)
  let _byd-eggs (turtle-set item 1 _groups item 2 _groups)
  user-message (word "byds: " count _byd-eggs " " count _byd-meat)
  ask _byd-meat [
    set meat-delivery-days 0  ;;NOTE: meat delivery currently ignored (i.e., it creates no risks)
  ]
  ;;hard-coded (but scalable) numbers
  setup-egg-pickup-deliver-both _byd-eggs [1 1 0] ;;half deliver their eggs (no onsite traders), and half have them picked up
    n-egg-pickup-days-byd-task n-egg-delivery-days-byd-task n-onsite-eggtraders-byd-task

  ;;; FARMS (farms produce eggs only; not meat)
  ;; of the large egg farms: 1/3 have pickup, 1/3 delivery, and 1/3 both
  ;; (e.g., of 6 large chicken egg farms: 2 have pickup, 2 delivery and 2 both,
  ;; of 3 large duck egg farms, 1 has pickup, 1 delivery and 1 both)
  ;;hard-coded (but scalable) numbers
  setup-egg-pickup-deliver-both fmchickens [2 2 2]
    n-egg-pickup-days-fm-task n-egg-delivery-days-fm-task n-onsite-eggtraders-fm-task
  setup-egg-pickup-deliver-both fmducks [1 1 1]
    n-egg-pickup-days-fm-task n-egg-delivery-days-fm-task n-onsite-eggtraders-fm-task

  ;; set target markets for any deliveries
  ask (turtle-set ducks chickens) with [n-egg-delivery-days != 0] [
    set target-egg-market min-one-of markets [distance myself]
  ]
  ;;In (nondefault) scenarios that include coops,
  ;;ALL and ONLY FGDs deliver to coops (see birdflu_supp.txt) chk
  if (n-cooperatives > 0) [
    ask fgducks with [n-egg-delivery-days > 0] [
      set target-egg-market min-one-of cooperatives [distance myself]
    ]
  ]
end ;;setup-egg&meat-pd

to setup-egg-pickup-deliver-both [#tset #wts #pickup-days-task #deliver-days-task #n-traders-task]
  ;; #tset : turtleset
  ;;   the flocks to be initialized
  ;; #wts : list
  ;;   the weighted division bt pickup, deliver, or both
  ;;   (see procedure `weighted-integer-partition`)
  ;;note: assumes agents have attributes:
  ;;  n-egg-pickup-days, n-egg-delivery-days, n-onsite-eggtraders
  let _pdbpartition weighted-integer-partition count #tset #wts
  if (count #tset != sum _pdbpartition) [error "partition error"]
  let _groups disjoint-subsets #tset _pdbpartition
  ;;set pickup days and onsite traders for those with pickup
  ask (turtle-set item 0 _groups item 2 _groups) [
    set n-egg-pickup-days runresult #pickup-days-task
    set n-onsite-eggtraders runresult #n-traders-task
    log2file (word who " has " n-onsite-eggtraders " onsite traders")
  ]
  ;;set delivery days for those that deliver
  ask (turtle-set item 1 _groups item 2 _groups) [
    set n-egg-delivery-days runresult #deliver-days-task
  ]
end ;;setup-egg-pickup-deliver-both



;;The egg pickup network is stable; we only make it once, during setup.
;;Goals for setting up egg-pickup network``:
;;1. get the number of eggtraders "needed" by each flock type (i.e., n-onsite-eggtraders)
;;2. limit the number of sites each eggtrader can visit to its ``n-eggtrader-links``)
;;3. ultimately create just as many eggtraders as are needed by creating too many and then removing those not needed
;;4. eggtraders choose closest available suppliers
;;5. associate each eggtrader with a barn (so each barn is associated with 0 or 1 eggtraders)
to setup-eggtraders
  let _suppliers (turtle-set ducks chickens) with [n-egg-pickup-days != 0]
  if (count _suppliers = 0) [error "modeler probably forgot to set egg pickup days"]
  if any? (_suppliers with [n-onsite-eggtraders = 0]) [
    error (word "mismatch: n-onsite-eggtraders=0 but n-egg-pickup-days>0")
  ]
  let _n-links sum [n-onsite-eggtraders] of _suppliers
  if (_n-links > count barns) [error "model should create more barns than egglinks"]
  ;; create more than enough potential eggtraders:
  ask n-of _n-links barns [
    sprout-eggtraders 1 [ init-eggtrader ] ;;sets n-eggtrader-links for each trader
  ]
  ;;make links bt eggtraders and suppliers
  ask eggtraders [
    ;; how many eggtrader links are still needed by suppliers?
    let _n-needed (_n-links - count egg-links)
    ifelse (_n-needed > 0) [
      ;;check whether any suppliers are still available
      set _suppliers _suppliers with [ count my-egg-links < n-onsite-eggtraders ]
      let _n-suppliers count _suppliers
      ifelse (_n-suppliers > n-eggtrader-links) [
        create-egg-links-with (min-n-of n-eggtrader-links _suppliers [distance myself])
      ] [
        create-egg-links-with _suppliers
      ]
    ] [
      die ;;no more eggtraders needed
    ]
  ]
  ask egg-links [hide-link]  ;; we do not need these to show in the GUI
  ask eggtraders [set target-egg-market one-of markets]
end ;;END:setup-eggtraders

;;;;;;;;;;;;;;;;;;;;;
;; go: Egg Network ;;
;;;;;;;;;;;;;;;;;;;;;


to trade-eggs ;;observer proc
  ;; having egg trading first facilitates the transmission process, since
  ;; contam of the egg trader depends on there being infectious turtles, not infected patch.   
  ask eggtraders [ pickup-eggs ]
  ask (turtle-set ducks chickens) with [n-egg-delivery-days != 0] [ deliver-eggs ]
end

to pickup-eggs     ;; eggtrader proc
  ;; Each day, egg traders determine whether it is a pickup day for any of their contact flocks.
  ;; If it is, they collect from flocks who have that pickup day.
  let _suppliers in-link-neighbors with [has-eggs? and member? n-egg-pickup-days eggtrader-pickup-days] 
  if (not any? _suppliers) [show (word "no pickups on " day-name)]
  foreach [self] of _suppliers [
    move-to ?
    show (word "getting eggs from " ?)
    contaminate-eggtrader
  ]
  ;;after done collecting, take eggs to usual market
  move-to target-egg-market                              ;;currently, we do not consider market infection from eggtraders
  move-to home-patch
end


to contaminate-eggtrader  ;; eggtrader proc
  if not (member? self eggtraders) [error (word self " is not an eggtrader")]
  let _at-risk turtles-here with [(member? breed poultry-types) and (flu-state = "s")]
  let _risks turtles-here with [(member? breed poultry-types) and (flu-state = "i")]
  ;; eggtrader may contaminate poultry here
  if (contam-until > ticks) [
    ask _at-risk [risk-transmission "eggtrader-flock"]
    show (word "at risk: " count _at-risk)
  ]
  ;; contaminate egg trader at inf farm
  if (any? _risks) [
    risk-human-contamination "flock-eggtrader"
  ]
end

to deliver-eggs  ;;flock proc
  ;; deliver eggs to market
  ;; delivering eggs does not create contamination risk but *affects* contamination risk,
  ;; since the eggs are delivered rather than picked up (which has contamination risk).
  if (debug > 5) [show "enter proc: deliver-eggs"]
  if (n-egg-delivery-days = 0) [error "only apply this proc to egg deliverers"]
  if (biotype = "fg") [
    ask my-fgd-links [untie log2file (word "fgd-links untied for egg delivery")]
  ]
  ask owner [
    move-to [home-patch] of myself          ;; no movement, if share home-patch
    move-to [target-egg-market] of myself
    move-to home-patch
  ]
  if (biotype = "fg") [
    if (home-patch != [home-patch] of owner) [error "home patches shd match for fgducks"]
    ask my-fgd-links [tie log2file (word "fgd-links retied after egg delivery")]
  ]
  if (debug > 5) [show "exit proc: deliver-eggs"]
end  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Live Poultry Trading Network ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Each day of the week, a livetrader is either collecting or not,
;; based on the number of days per week they trade (1, 2 or 3)
;; There is no poultry trading network made at setup, because
;; unlike the egg trading network,
;; it will change daily based on who they have recently collectd birds from.
;; Each call to ``go`` , each trader that trades on the present day
;; will form a network of the 5 nearest houses that have not yet sold
;; birds during the 2-month iteration

to trade-birds ;;observer proc
  ;; note: only backyard duck and chicken farmers supply live birds
  ;; Each day each livetrader determines if it is a pickup day for any contacts;
  ;; if it is, the livetrader collects live birds.
  ask livetraders with [member? n-bird-pickup-days livetrader-pickup-days] [
    make-livetrader-pickup-network  ;;volatile network (reflecting reality)
    pickup-livebirds ;; includes transmission and contamination risks
  ]
  ;; clear links after the trading, because network renewed each next day
  ask links with [ color = blue ] [ die ]
  ;; comment: in our subdistrict, livetraders slaughter birds at home
  ;;   (i.e., live birds don't go to mkt)
end

to make-livetrader-pickup-network ;; livetrader proc
  ;; This is called by *each* trader on each day s/he trades,
  ;; so it may be called 1, 2 or 3 times each week, depending on the trader's pickup schedule.
  if (debug > 5) [show "enter proc: make-livetrader-pickup-network"]
  ;; find *all* suppliers chk
  let _cutoff (ticks - LIVEBIRD-REPLENISH)
  let _livebird-suppliers (turtle-set byd-owners byc-owners) with [
    _cutoff >= livebird-selldate
  ]
  let _ct min (list n-flocks-per-pickup count _livebird-suppliers)
  if (_ct < n-flocks-per-pickup) [log2file (word "WARNING: not enough live birds available, run number " behaviorspace-run-number)]
  create-livebird-links-with min-n-of _ct _livebird-suppliers [distance myself]
  ask livebird-links [ set color blue ]
  if (debug > 5) [show "exit proc: make-livetrader-pickup-network"]
end

to pickup-livebirds   ;; livetrader proc
  let _cutoff (ticks - LIVEBIRD-REPLENISH)
  let _candidates sort-on [distance myself]
    out-link-neighbors with [livebird-selldate <= _cutoff]
  while [length _candidates > 0] [
    let _livebird-supplier first _candidates
    set _candidates butfirst _candidates
    move-to _livebird-supplier
    ask _livebird-supplier [set livebird-selldate ticks] ;;this is the sale
    ;;NOTE: does NOT include fcs, because that would double count risks
    let _flocks (turtles-here with [member? breed flock-types])
    ;;trading involves transmission and contamination risks:
    let _risks _flocks with [flu-state = "i"]
    let _at-risk _flocks with [flu-state = "s"]
    ;; live trader may infect susceptible farm
    if (contam-until > ticks) [
      ask _at-risk [risk-transmission "livetrader-flock"]
    ]
    ;; infected farm may contaminate live trader
    if (any? _risks) [
      risk-human-contamination "flock-livetrader"
    ]
  ]
  move-to home-patch                ;; they go home, where they will slaughter the birds 
end





;;;;;;;;;;;;;;;;;;;;;;;
;;Fighting Cock Procs;;
;;;;;;;;;;;;;;;;;;;;;;;

to fight-cocks  ;;observer proc (called in ``go``)
  ;;for simplicity, only the fcs travel to the matches (not the owners)
  ;; (does not matter since we do not introduce owner contamination at the matches)
  if (debug > 5) [show "enter proc: fight-cocks"]
  ;file-open log-file
  if ((day-name != "Wednesday") and (day-name != "Sunday")) [
    ;file-print (word day-name ": No fighting cock activity today. ")
  ]
  ;;as a simplification, all of our practice matches take place on Wed
  if (day-name = "Wednesday") [ 
    ;file-print (word day-name ": practice matches")
    ;let my-owners fc-owners with [practice-fight-week = (int (ticks / 7)) mod 2] ;;half fight on even weeks, half on odd
    ask practice-arenas [
      let _fcs n-of 20 (fcs with [target-practice-arena = myself])
      fight-at self _fcs
      ;file-show (word count _fcs " FCs practicing here:")
    ]
  ];endif
  if (day-name = "Sunday") [
    ;; Only fcs who compete go to the matches. (Simplification.)
    ;; Each fc-owner brings a FC to the match arena about twice yearly;
    ;; thus we average N-FCS/26 cocks fighting per week.
    ;; (Initially we had 50/26~=2; we raised it to 200/26~=8.) chkchk
    ;; That does not count cocks from other subdistricts.
    ;; Realistically, there are about 10 fights (20 cocks) on a day.
    ;; Do we have enough coming from our subdistrict?
    ;;ai: chkchkchk NOTE switching to fixed *number* of fc owners at matches!!  OK??
    ;; NOTE this still makes it so that only 2 owners from this subdistrict fight each week!
    ;; chk Add risk from by birds from other subdistricts???
    print (word day-name ": fighting-cock matches:")
  ;; chkchkchk we need to determine how to make this a chance of infection at the arena
  ;; becuase when this is TRUE, the arena infects a FC every time.
  ;;**************20120410 AB Altered the transmission procs of FC a bit below
    ask match-arenas [ ;only one in baseline model
      let _fcs n-of round (N-FCS / (26 * N-MATCH-ARENAS)) fcs
      fight-at self _fcs
    ]
  ];endif
  ;file-close
  if (debug > 5) [show "exit proc: fight-cocks"]
end


to fight-at [#arena #fcs]
  ask #fcs [move-to #arena]
  if (any? #fcs with [flu-state = "i"]) [      ;; must come after all move
    let _at-risk (#fcs with [flu-state = "s"])
    ask _at-risk [risk-transmission "arena-fc"]
  ]
  ask #fcs [
    move-to home-patch
  ]
end

;;;;;;;;;;;;;;;;;;
;;Backyard Procs;;
;;;;;;;;;;;;;;;;;;

to visit ;;  poultry owner procedure
  ;; all poultry owners can move to a few neighbors daily for a visit
  ;; (number of visits per day determined by a binomial at initialization) chkchk
  if not member? breed human-types [error "should be a human"]
  if not (member? n-visit-days-per-week visit-days) [stop]
  ;; contaminate owner at their house/barn (with 100% probability chkchk)
  if (any? turtles-here with [member? breed poultry-types and (flu-state = "i")]) [ set contam-until (ticks + contam-period)]
  let _to-visit [home-patch] of n-of n-visits visit-list
  foreach _to-visit [
    move-to ?
    ifelse (contam-until > ticks) [
      let _at-risk turtles-here with [(member? breed poultry-types) and (flu-state = "s")]         ;; expose flock at neighbor's house
      ask _at-risk [ risk-transmission "visitor-flock" ]
    ] [
      let _risks (turtles-here with [(member? breed poultry-types) and (flu-state = "i")])
      if (any? _risks) [risk-human-contamination "flock-visitor"]
    ]
  ]
 
  ;; after visit, backyard owner may infect own flock
  move-to home-patch
  if (contam-until > ticks) [
    let _at-risk turtles-here with [(member? breed poultry-types) and (flu-state = "s")]            ;; 120913AB: I changed the "i" to "s" chkchkchk
    ask _at-risk [ risk-transmission "after-visit" ]
    ;;decontaminate (cleaned up) after one day chkchkchk (see contam-until)
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Contamination and Infection;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to roads-infect-flocks ;; observer proc
  ;; check for infection from road
  ask bychickens [ road-infect-backyard ]
  let _penned (turtle-set byducks fmducks fmchickens)
  ask _penned [road-infect-penned]
end

to road-infect-backyard ;;bychicken proc
  ;; this is NOT applied to byducks, which are penned!
  ;; byducks don't roam like bychickens; less likely reach road/driveway
  if (pcontam-until > ticks) and (flu-state = "s") [
    risk-transmission "road-flock"
  ]
end

;; note: looks identical to road-infect-backyard, but
;; the prob of transmission to road-penned birds (ducks&farmed chickens) is lower than bychickens
to road-infect-penned ;;byduck and farm proc
  if (pcontam-until > ticks) and (flu-state = "s") [
   risk-transmission "road-flock"
  ]
end

;                                           ;; New AB 20120405
; to infect-backyard-old                   ;; in fighting procedures, ask fc-owners to infect-backyard poultry
;   ;; assumption is that if flock is not infected by the FC, then the infection dies and is not transported to neighbors chk
;   if ((own-infected-fc? = TRUE) and (not any? turtles-here with [member? breed poultry-types] with [member? flu-state "eir"]))
;   [ ask turtles-here with [member? breed poultry-types] [risk-transmission "fc-flock"]] 
;   set own-infected-fc? FALSE                                                    ;; 20120214 New.
; end

to infect-backyard   ;;fc proc called in fight-cocks;  in fight-cocks proc we ask fc-owners to infect-backyard poultry ;;chkchk better as fc proc?
  ;; assumption is that if flock is not infected by the FC, then the infection dies and is not transported to neighbors chk
  if (not member? self fcs) [error "This proc shd only be called on fcs"]
  let _flock ([flock] of owner)
  if ((flu-state = "i") and ([flu-state] of _flock = "s")) [
    ask _flock [risk-transmission "fc-flock"]
  ]
end


;;;;;;;;;;;;;;;;;;;;;;
;;Transmission Procs;;
;;;;;;;;;;;;;;;;;;;;;;

to update-flu-state ;;time-based flu-state transitions (NOT initial infection)
  let _transitions (list ) ;default for this period's transitions
  if table:has-key? transition-dates ticks [
    set _transitions table:get transition-dates ticks
  ]
  foreach _transitions [
    let _flock ?       
    let _breed ([breed] of _flock)
    let _is-chicken? member? _breed (list chickens fcs)
    let _is-duck? (_breed = ducks)
    if not (_is-chicken? or _is-duck?) [error "unidentified flock type"]
    let _state [flu-state] of _flock    ;;current state
    if not (member? _state ["e" "i"]) [
      error (word "flu-state " _state " is not valid for transition")
    ]
    if (_state = "e") [
      ask _flock [ set flu-state "i"]
      add-transition (ticks + [shedding-period] of _flock) _flock
    ]
    if (_state = "i") [
      if _is-chicken? [ ;; chickens had a 4 day shedding period. 
        ask _flock [set flu-state "d"]
        ;;chkchk why are we not just killing them off? just to count them
      ]
      if _is-duck? [ ;; ducks had a 7 day shedding period. 
        ask _flock [set flu-state "r"]
      ]
    ]
  ]
end



to-report basic-info
  report (word
  "fc-owners: " (count fc-owners)
  "\neggtraders: " (count eggtraders)
  "\nrice fields: " (count patches with [patch-type = "ricefield"])
  )
end

to test-for-endsim
  if ((count turtles with [member? breed poultry-types and (member? flu-state "ei") ])
      + (count turtles with [member? breed human-types and contam-until > ticks]) 
      + (count barns with [ pcontam-until > ticks ]) 
      + (count practice-arenas with [ pcontam-until > ticks ])  ;;chkchk ai: why not match arena?
      = 0) or ticks > 150 [   ;;chkchkchk
    set stop-sim-reason (word "Outbreak is over after " ticks " ticks.")
    set runsim? false   ;;don't call stop bc then BS won't record final outcomes
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to test-setup
  show "enter proc: test-setup"
  test-setup-patches
  test-setup-agents
  show "exit proc: test-setup"
end

to test-setup-patches
  ;;;;;;  check that setup-patches meets expectations, ow raise error


  ;; ensure that only ricefields shd have a non-empty field-state 
  ask patches [
    if (patch-type = "ricefield") and (not member? field-state ["growing" "harvested" "fallow"]) [
      error "ricefields should have field-state"
    ]
    if (patch-type != "ricefield" and field-state != "") [
      error "non-ricefields should not have field-state"
    ]
  ]

  ask access-fields [
    ask field-patches [
      if (field-num != [field-num] of myself)
      or (field-state != [field-state] of myself) [
        error "problem in field setup"
      ]
    ]
  ]
  if (N-PRACTICE-ARENAS != count practice-arenas) [error "bad setup of practice arenas"]
  if (N-MARKETS != count markets) [error "bad setup of markets"]
  if (n-cooperatives != count cooperatives) [error "bad setup of markets"]
end

to test-setup-agents
  ask (turtle-set flock-types) [
    if (n-onsite-eggtraders != count my-egg-links) [
      ;;every supplier should get their full contingent of eggtraders
      ;;(see setup-eggtraders)
      error "mismatch error"
    ]
  ]
  ask (turtle-set flock-types) [
    if (n-onsite-eggtraders > 0 and n-egg-pickup-days = 0)
      or (n-onsite-eggtraders = 0 and n-egg-pickup-days != 0) [
      error "egg pickup iff onsite eggtraders"
    ]
  ]
  if (N-FGDS != count fgducks) [error (word N-FGDS " should equal " count fgducks)]
  ask fgducks [
    if (not is-turtle? owner) [error "problem setting owner of fgducks"]
    if (breed != ducks) [error "breed not set correctly"]
    if (biotype != "fg") [error "biotype not set correctly"]
  ]
  if (fgd-deliver-pct < 0 or fgd-deliver-pct > 100) [error "bad percentage"]

  ask byducks [
    if (not is-turtle? owner) [error "problem setting owner of byducks"]
    if (breed != ducks) [error "breed not set correctly"]
    if (biotype != "pen") [error "biotype not set correctly"]
    if not (member? n-onsite-eggtraders [0 1]) [error "byducks have at most 1 eggtrader"]
  ]
  ;;WARNING: hard coded (nongeneric) error chk
  if (52 != count byducks with [n-egg-pickup-days != 0 or n-egg-delivery-days != 0]) [
    ask byducks [show (word n-egg-pickup-days " " n-egg-delivery-days)]
    error (word "setup problem: 52 != " (count byducks with [n-egg-pickup-days != 0 or n-egg-delivery-days != 0]))
  ]
  if not (N-BYDUCKS = sum meat-eggs-both-byd) [error "mismatch error"]

  if (count livetraders != N-LIVETRADERS) [error "setup error: wrong number of livetraders"]
  if (count bychickens != N-BYCHICKENS) [error "setup error: wrong number of bychickens"]
  if (N-CHICKEN-FARMS != count fmchickens) [error "didn't run setup-chickenfarms-and-owners?"]
  if (N-DUCK-FARMS != count fmducks) [error "didn't run setup-duckfarms-and-owners?"]

  ;;log some info
  file-open log-file
  ask practice-arenas [
    file-show (word " is practice arena for " count fcs with [target-practice-arena = myself])
  ]
  ask fcs [
    file-show (word "chose match arena " target-match-arena)
    file-show (word "chose practice arena " target-practice-arena)
  ]
  file-close
end

to test-setup-farms
  ;;WARNING: nongeneric tests, for our model only 
  if (count fmducks != 3) [error "wrong number of duck farms"]
  if (count fmchickens != 6) [error "wrong number of chicken farms"]
  ask (turtle-set fmducks fmchickens) with [n-onsite-eggtraders > 0] [
    if (n-egg-pickup-days = 0) [error "mismatch"]
  ]
end

to test-go
  ask fgducks with [field-only?] [
    let _patch patch-here
    let _owner-patch nobody
    ask owner [set _owner-patch patch-here]
    if not (_owner-patch = _patch) [
      show (word "I'm on " _patch " but my owner is on " _owner-patch)
      error "fgducks should be tied to owner"
    ]
  ]
  if (N-FGDS != s:fgducks + e:fgducks + i:fgducks + r:fgducks) [
    error "bad state counts"
  ]
  if (N-BYDUCKS != s:byducks + e:byducks + i:byducks + r:byducks) [
    error "bad state counts"
  ]
  if (N-BYCHICKENS != s:bychickens + e:bychickens + i:bychickens + d:bychickens) [
    error "bad state counts"
  ]
  if (N-DUCK-FARMS + N-CHICKEN-FARMS != s:farms + e:farms + i:farms + dr:farms) [
    error "bad state counts"
  ]
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VISUAL (GUI) INSPECTION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-visuals
  ;;after all agents created, we setup the visuals
  setup-patch-visuals
  ask intersections [
    ;set color black set shape "target" set size 0.5
    hide-turtle
  ]
  ask turtles with [not hidden?] [set size 3.5]
  ask road-links [
    if roadtype = "highway" [set color black set thickness 0.8]
    if roadtype = "major_road" [set color black set thickness 0.6]
    if roadtype = "loose_road" [set color black set thickness 0.4]
    if roadtype = "small_road" [set color black set thickness 0.2]
    if roadtype = "field_road" [set color black set thickness 0.1]
  ]
  ;;old visuals chkchkchk
  ;ask road-links with [roadtype = "major_road"] [set color red]
  ;ask road-links with [roadtype = "loose_road"] [set color violet]
  ;ask road-links with [roadtype = "small_road"] [set color pink]

  ;;; poulty visuals
  ;;fc visuals:
  ;ask fcs [set color orange] ;chk (too cluttered to show?)
  ask fcs [hide-turtle]
  ;;fmchickens visuals
  if not any? fmchickens [error "must create fmchickens before setting visuals"]
  ask fmchickens [set color orange set shape "bird side"]
  ;;bychicken visuals
  ask bychickens [set color orange]
  ;;byduck visuals
  ask byducks [set color brown set shape "duck"]
  ;;fmduck visuals
  ask fmducks [set color brown set shape "bird side"]
  ;;fgduck visuals
  ask fgducks [set heading 270 set color white]

  ;;flock owner visuals
  if (not any? flock-owners) [error "problem setting up owners"]
  ask flock-owners [set color [color] of flock]
  ask flock-owners [hide-turtle]  ;;show only while moving

  ;;eggtrader visuals:
  ask eggtraders [set color gray set size 4]
  ;;livetrader visuals:
  ask livetraders [set color gray set size 4]
end

to setup-patch-visuals
  update-field-visuals
  ask patches [
    if (member? self match-arenas) [ set pcolor red set plabel "M-Arena"]
    if (member? self practice-arenas) [ set pcolor red set plabel "P-Arena" ]
    if (member? self barns) [ set pcolor (blue + 1) ]
    if (member? self markets) [ set pcolor violet set plabel "Market"]
    if (member? self cooperatives) [ set pcolor violet set plabel "Coop"]
    if (member? self out-of-subdistrict-patches) [set pcolor brown set plabel "OUT"]
  ]
    
  ask road-links [ask road-patches [set pcolor (brown + 2)]]
  ;;old visuals: color code the roads (by coloring their nearby patches) chkchk
  ;ask road-links with [roadtype = "field_road"] [ask road-patches [set pcolor (brown + 3)]]
  ;ask road-links with [roadtype = "small_road"] [ask road-patches [set pcolor (brown + 2)]]
  ;ask road-links with [roadtype = "loose_road"] [ask road-patches [set pcolor (brown + 1)]]
  ;ask road-links with [roadtype = "major_road"] [ask road-patches [set pcolor brown]]
  ;ask road-links with [roadtype = "highway"] [ask road-patches [set pcolor (brown - 1)]]
end ;;END:setup-patch-visuals

to update-visuals
  update-field-visuals
  update-flock-visuals
  do-plots
end

to update-field-visuals
  let COLOR-FALLOW 48
  let COLOR-HARVESTED yellow
  let COLOR-GROWING 57 ;; light green
  ask patches [
    if (field-state = "fallow") [ set pcolor COLOR-FALLOW ]
    if (field-state = "harvested") [ set pcolor  COLOR-HARVESTED]
    if (field-state = "growing") [ set pcolor COLOR-GROWING ]
  ]
end

to update-flock-visuals
  let _flocks (turtle-set chickens fcs ducks)
  ask _flocks [
    if (flu-state = "i") [
      set color pink 
    ]
    if (flu-state = "d" or flu-state = "r") [
      set color gray 
    ]
  ]
end

to do-plots                                                                
  ;; see GUI for plot: "Contamination"
  ;; Look at the Contaminations table for number of fields contam
  
  ;; see GUI for plot: "FGD Infection State"
  
  set-current-plot "Backyard Chickens Infection State"
  set-current-plot-pen "Latent Flocks"
  plot count bychickens with [ flu-state = "e" ]
  set-current-plot-pen "Infectious Flocks"
  plot count bychickens with [ flu-state = "i" ]
  set-current-plot-pen "Flock Death"
  plot count bychickens with [ flu-state = "d" ]
  
  set-current-plot "Sm Duck Flocks Infection State"
  set-current-plot-pen "Latent Flocks"
  plot count byducks with [ flu-state = "e" ]
  set-current-plot-pen "Infectious Flocks"
  plot count byducks with [ flu-state = "i" ]
  set-current-plot-pen "Recovered Flocks"
  plot count byducks with [ flu-state = "r" ]
  
  set-current-plot "Farm Infection State"
  set-current-plot-pen "Latent Flocks"
  plot count farms with [ flu-state = "e" ]
  set-current-plot-pen "Infectious Flocks"
  plot count farms with [ flu-state = "i" ]
  set-current-plot-pen "Flock Death/ Recovery"
  plot count farms with [ flu-state = "d" or flu-state = "r"]

  set-current-plot "Flocks Affected (Post-shedding)"
  set-current-plot-pen "Backyard Flocks"
  plot ((count bychickens with [flu-state = "d"]) / N-BYCHICKENS)
  set-current-plot-pen "Small Duck Flocks"
  plot ((count byducks with [flu-state = "r"]) / N-BYDUCKS)
  set-current-plot-pen "FGD Flocks"
  plot ((count fgducks with [flu-state = "r"]) / N-FGDS)
  set-current-plot-pen "Farms"
  plot ((count farms with [flu-state = "r" or flu-state = "d"]) / (count farms))

  set-current-plot "rai available"
  clear-plot
  set-current-plot-pen "rai01"
  set-plot-pen-mode 0
  ;;make cumulative sum
  let _rai reduce [lput (?2 + last ?1) ?1] fput [0] (sort [rai] of access-fields)
  let _xvals n-values (count access-fields + 1) [?]
  ;;if (last _rai < 0.5 * plot-y-max) [set-plot-y-range 0 last _rai]
  (foreach _xvals _rai [plotxy ?1 ?2])
  set-plot-y-range 0 7500
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reporters purely for convenient BS output headers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to-report ntrans:road-flock
  report last (table:get transmissions "road-flock")
end

to-report ntrans:after-visit
  report last (table:get transmissions "after-visit")
end

to-report ntrans:visitor-flock
  report last (table:get transmissions "visitor-flock")
end

to-report ntrans:eggtrader-flock
  report last (table:get transmissions "eggtrader-flock")
end

to-report ntrans:livetrader-flock
  report last (table:get transmissions "livetrader-flock")
end

to-report ntrans:arena-fc
  report last (table:get transmissions "arena-fc")
end

to-report ntrans:fc-flock
  report last (table:get transmissions "fc-flock")
end

to-report ncont:flock-livetrader
  report last (table:get contaminations "flock-livetrader")
end

to-report ncont:flock-eggtrader
  report last (table:get contaminations "flock-eggtrader")
end

to-report ncont:flock-visitor
  report last (table:get contaminations "flock-visitor")
end

to-report ncont:flock-road
  report last (table:get contaminations "flock-road")
end

to-report ncont:flock-field
  report last (table:get contaminations "flock-field")
end

;; counts of exposed poultry ;;
to-report e:bychickens
  report count bychickens with [flu-state = "e"]
end

to-report e:byducks
  report count byducks with [flu-state = "e"]
end

to-report e:fgducks
  report count fgducks with [flu-state = "e"]
end

to-report e:farms
  report count farms with [flu-state = "e"]
end

to-report e:flocks
  report count turtles with [(member? breed flock-types) and (flu-state = "e")]
end

to-report e:poultry
  report count turtles with [(member? breed poultry-types) and (flu-state = "e")]
end

;; report counts of infectious poultry ;;
to-report i:fgducks
  report count fgducks with [flu-state = "i"]
end

to-report i:bychickens
  report count bychickens with [flu-state = "i"]
end

to-report i:byducks
  report count byducks with [flu-state = "i"]
end

to-report i:farms
  report count farms with [flu-state = "i"]
end

to-report i:flocks
  report count turtles with [(member? breed flock-types) and (flu-state = "i")]
end

to-report i:poultry
  report count turtles with [(member? breed poultry-types) and (flu-state = "i")]
end

;; counts of recovered or removed (dead) poultry ;;
to-report r:fgducks
  report count fgducks with [flu-state = "r"]
end

to-report r:byducks
  report count byducks with [flu-state = "r"]
end

to-report d:bychickens
  report count bychickens with [flu-state = "d"]
end

to-report dr:farms
  report count farms with [
    (flu-state = "r") or (flu-state = "d")
  ]
end

to-report dr:flocks
  report count (turtle-set flock-types) with [
    flu-state = "r" or flu-state = "d"
  ]
end

to-report dr:poultry
  report count (turtle-set poultry-types) with [
    flu-state = "r" or flu-state = "d"
  ]
end


;; counts of poultry not yet exposese state ;;
to-report s:fgducks
  let s count fgducks with [not (member? flu-state "eir")]
  if not (s = count fgducks with [flu-state = "s"]) [
    error "counting error"
  ]
  report s
end

to-report s:byducks
  let s count byducks with [not (member? flu-state "eir")]
  if not (s = count byducks with [flu-state = "s"]) [
    error "counting error"
  ]
  report s
end

to-report s:bychickens
  let s count bychickens with [not (member? flu-state "eid")]
  if not (s = count bychickens with [flu-state = "s"]) [
    error "counting error"
  ]
  report s
end

to-report s:farms
  let s count farms with [not (member? flu-state "eird")]
  if not (s = count farms with [flu-state = "s"]) [
    error "counting error"
  ]
  report s
end

to-report s:flocks
  let f (turtle-set flock-types) ;;chkchk
  let s count f with [not (member? flu-state "eird")]
  if not (s = count f with [flu-state = "s"]) [
    error "counting error"
  ]
  report s
end

to-report s:poultry
  let p (turtle-set poultry-types)  ;;chkchk
  let s count p with [not (member? flu-state "eird")]
  if not (s = count p with [flu-state = "s"]) [
    error "counting error"
  ]
  report s
end

@#$#@#$#@
GRAPHICS-WINDOW
255
10
945
721
60
60
5.63
1
10
1
1
1
0
0
0
1
-60
60
-60
60
1
1
1
days
30.0

BUTTON
5
5
65
38
NIL
Setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
70
5
130
38
NIL
Go
T
1
T
OBSERVER
NIL
G
NIL
NIL
0

BUTTON
135
5
195
38
Go Once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

CHOOSER
5
40
190
88
infection-source
infection-source
"fgduck" "farm" "byduck" "bychicken"
0

SLIDER
5
90
190
123
shedding-period-duck
shedding-period-duck
0
10
7
1
1
NIL
HORIZONTAL

SLIDER
5
125
190
158
shedding-period-chicken
shedding-period-chicken
0
10
4
1
1
NIL
HORIZONTAL

SLIDER
5
160
190
193
prop-fgd-barntofield
prop-fgd-barntofield
0
1
0.6
0.1
1
NIL
HORIZONTAL

CHOOSER
5
195
190
238
n-cooperatives
n-cooperatives
0 1 2
0

CHOOSER
1165
580
1303
625
scenario
scenario
"none" "eggtrade(high)" "eggtrade(low)" "road-flock(high)" "road-flock(low)" "road-flock(zero)" "visit(veryhigh)" "visit(high)" "visit(low)" "visit(zero)"
6

PLOT
5
250
247
400
Contamination
time
totals
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Barns Near Road" 1.0 0 -13345367 true "" "plot count barns with [pcontam-until > ticks]"
"Rice Fields" 1.0 0 -13840069 true "" "plot count rice-fields with [ (field-state = \"harvested\") and (pcontam-until > ticks)]"
"Egg Traders" 1.0 0 -955883 true "" "plot count eggtraders with [contam-until > ticks]"
"Live Traders" 1.0 0 -5825686 true "" "plot count livetraders with [contam-until > ticks]"

PLOT
5
405
246
555
FGD Infection State
time
totals
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Latent Flocks" 1.0 0 -10899396 true "" "plot count fgducks with [flu-state = \"e\"]"
"Infectious Flocks" 1.0 0 -2674135 true "" "plot count fgducks with [flu-state = \"i\"]"
"Recovered Flocks" 1.0 0 -5987164 true "" "plot count fgducks with [flu-state = \"r\"]"

PLOT
5
560
245
710
Backyard Chickens Infection State
time
totals
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Latent Flocks" 1.0 0 -10899396 true "" ""
"Infectious Flocks" 1.0 0 -2674135 true "" ""
"Flock Death" 1.0 0 -5987164 true "" ""

MONITOR
960
10
1060
55
rice fields
count rice-fields
0
1
11

MONITOR
1065
10
1165
55
non-rice patches
count patches with [field-state = \"\" ]
0
1
11

MONITOR
1170
10
1320
55
Current Date
(word day-name \" of week: \" week)
0
1
11

MONITOR
960
55
1060
100
chk egg traders
count eggtraders with [ all? in-link-neighbors [egg-type = \"chicken\"]]
0
1
11

MONITOR
1065
55
1165
100
duck egg traders
count eggtraders with [ all? in-link-neighbors [egg-type = \"duck\"]]
0
1
11

MONITOR
1170
55
1320
100
chk & duck egg traders
count eggtraders with [ any? (in-link-neighbors with [egg-type = \"duck\"]) and any? (in-link-neighbors with [egg-type = \"chicken\"])]
0
1
11

PLOT
955
110
1285
265
Sm Duck Flocks Infection State
time
totals
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Latent Flocks" 1.0 0 -10899396 true "" ""
"Infectious Flocks" 1.0 0 -2674135 true "" ""
"Recovered Flocks" 1.0 0 -7500403 true "" ""

PLOT
955
265
1285
420
Farm Infection State
time
totals
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Latent Flocks" 1.0 0 -10899396 true "" ""
"Infectious Flocks" 1.0 0 -2674135 true "" ""
"Flock Death/ Recovery" 1.0 0 -7500403 true "" ""

PLOT
955
420
1285
575
Flocks Affected (Post-shedding)
time
proportion
0.0
10.0
0.0
0.5
true
true
"" ""
PENS
"Backyard Flocks" 1.0 0 -11221820 true "" ""
"FGD Flocks" 1.0 0 -13840069 true "" ""
"Small Duck Flocks" 1.0 0 -5825686 true "" ""
"Farms" 1.0 0 -6459832 true "" ""

PLOT
955
575
1155
725
rai available
num fields
total rai
0.0
32.0
0.0
6000.0
true
false
"" ""
PENS
"rai01" 1.0 0 -16777216 true "" ""

CHOOSER
1165
580
1303
625
scenario
scenario
"none" "eggtrade(high)" "eggtrade(low)" "road-flock(high)" "road-flock(low)" "road-flock(zero)" "visit(veryhigh)" "visit(high)" "visit(low)" "visit(zero)"
6

CHOOSER
1165
650
1303
695
baseline
baseline
"pert" "mode"
1

@#$#@#$#@
## WHAT IS IT?

This is an agent-based model of the subdistrict poultry sector in Central Thailand.

For this project, we conducted interviews with free-grazing duck owners, backyard poultry owners, large farm owners, poultry traders, slaughterhouse workers and egg traders in three subdistricts of Suphanburi Province, Thailand. The interviews focused on the frequency and character of poultry-related contacts, including the transport, sale and purchase of both birds and eggs. All of these aspects are reflected in the model. The agents interact on a spatially representative backdrop of a Thai subdistrict. 

The objective for modeling this system is to observe the contact frequencies and types that occur among members of the subdistrict poultry sector and hypothesize what contact types pose the greatest risk for transmission of highly pathogenic avian influenza (HPAI) H5N1. Generation of different scenarios also allow the assessment of interventions that could result in a decrease of between-flock avian influenza virus transmission risk. 

## HOW IT WORKS

BROWN-Roads  
GREEN- Fields with growing rice  
YELLOW- Fields with harvested rice  
LIGHT YELLOW- Fallow fields  
BLUE- Barns  
RED- Fighting cock arenas  
VIOLET- Markets or egg cooperative (marked with "C")  

GOLD PERSON- Backyard chicken owner, subset owns fighting cocks  
Backyard characteristics: Freely roam around home yard and close to/ into road. Do not travel from home. No eggs collected/ delivered. Infrequent pickup by live poultry traders.  
Backyard contacts made: FGD passing by yard on foot (direct); Infected fighting cocks returning to flock (direct); FGD by contaminated roads (indirect); Other flocks by live poultry traders (indirect); Other flocks by owner visiting (indirect)

Fighting cock characteristics: Owners go weekly to practice arena for fights, but do not bring cocks every time. Owners bring fighting cocks at rate of twice yearly to match arena.  
Fighting cock contacts made: Fighting against infectious cock at practice or match (direct); Infectious birds at practice arena, owner as fomite (indirect)

WHITE DUCK- FGD flock; WHITE PERSON- FGD owner  
Characteristics: Move from home to field daily. Have the potential to become infected at the rice field via theoretical interaction with wild birds. When traveling on the roads from home to field may contact, directly or indirectly, backyard poultry. Have eggs collected/ delivered.  
Contacts made: Backyard poultry en route to fields (direct); Backyard poultry by contaminated roads (indirect); Other flocks by egg collectors (indirect); Other flocks by owner visiting (indirect)

GREEN PERSON- Chicken or duck farm owner  
Characteristics: Large flocks of chickens or ducks kept in barns, usually with open sides and located over body of water. Used primarily for egg production.  
Contacts made: Other flocks by egg collectors (indirect); Other flocks by owner visiting (indirect)

BROWN PERSON- Backyard duck owner  
Characteristics: Penned ducks in small flocks of approx. 50-500 head. Do not travel from home. Eggs picked up/ delivered. Infrequent pickup by live poultry traders.  
Contacts made: FGD by contaminated roads?(indirect); Other flocks by egg collectors (indirect); Other flocks by live poultry traders (indirect); Other flocks by owner visiting

CAR- Egg trader  
Characteristics: Create a network of farms to go to on a certain number of trading days each week. Go to the same farms on each trading day. Bring eggs to a specific market or egg cooperative each trading day.  
Contacts made: Farms where pick up eggs (contamination source); Flocks by acting as fomites, via shoes and vehicles (location to contaminate)

TRUCK- Live poultry trader  
Characteristics: On a certain number of trading days each week, create a network of farms to go to. Go to the different farms on each trading day. Bring birds back to home for slaughter.  
COntacts made: Farms where pick up birds (contamination source); Flocks by acting as fomites, via shoes, clothing and vehicles (location to contaminate)

## HOW TO USE IT

This section could explain how to use the model, including a description of each of the items in the interface tab.

## THINGS TO NOTICE

This section could give some ideas of things for the user to notice while running the model.

## THINGS TO TRY

This section could give some ideas of things for the user to try to do (move sliders, switches, etc.) with the model.

## EXTENDING THE MODEL

This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.

## NETLOGO FEATURES

This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.

## RELATED MODELS

This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.

## CREDITS AND REFERENCES

This section could contain a reference to the model's URL on the web if it has one, as well as any other necessary credits or references.
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

backyard
true
0
Circle -955883 true false 73 101 64
Rectangle -955883 true false 105 137 165 240
Rectangle -2674135 true false 84 91 127 112
Circle -1184463 true false 92 205 38
Circle -1184463 true false 138 205 40
Circle -11221820 true false 91 116 14
Polygon -1184463 true false 64 120 45 135 75 150 75 120

bird side
false
0
Polygon -7500403 true true 0 120 45 90 75 90 105 120 150 120 240 135 285 120 285 135 300 150 240 150 195 165 255 195 210 195 150 210 90 195 60 180 45 135
Circle -16777216 true false 38 98 14

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

duck
true
3
Circle -6459832 true true 150 45 60
Circle -6459832 true true 74 74 122
Rectangle -955883 true false 150 45 165 60
Line -955883 false 75 150 60 150
Line -955883 false 75 120 60 120
Line -955883 false 90 105 60 105
Line -955883 false 90 165 60 165
Line -955883 false 60 105 60 120
Line -955883 false 60 150 60 165
Rectangle -955883 true false 60 105 75 120
Rectangle -955883 true false 75 105 90 120
Rectangle -955883 true false 60 150 90 165
Rectangle -11221820 true false 180 60 195 75

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

farm
true
0
Circle -1184463 true false 73 101 64
Rectangle -1184463 true false 105 137 165 240
Rectangle -2674135 true false 84 91 127 112
Circle -955883 true false 92 205 38
Circle -955883 true false 138 205 40
Circle -11221820 true false 91 116 14
Polygon -955883 true false 64 120 45 135 75 150 75 120

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
  <experiment name="BaselineModel" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-transmissions-and-contaminations</final>
    <metric>ticks</metric>
    <metric>ntrans:road-flock</metric>
    <metric>ntrans:after-visit</metric>
    <metric>ntrans:visitor-flock</metric>
    <metric>ntrans:eggtrader-flock</metric>
    <metric>ntrans:livetrader-flock</metric>
    <metric>ntrans:arena-fc</metric>
    <metric>ntrans:fc-flock</metric>
    <metric>ncont:flock-livetrader</metric>
    <metric>ncont:flock-eggtrader</metric>
    <metric>ncont:flock-visitor</metric>
    <metric>ncont:flock-road</metric>
    <metric>ncont:flock-field</metric>
    <metric>map [item 1 ?] table:to-list days-on-field</metric>
    <metric>count barns with [pcontam-until &gt; ticks]</metric>
    <metric>count rice-fields with [ (field-state = "harvested") and (pcontam-until &gt; ticks)]</metric>
    <metric>count fgducks with [not (member? flu-state "eir")]</metric>
    <metric>e:fgducks</metric>
    <metric>i:fgducks</metric>
    <metric>r:fgducks</metric>
    <metric>count bychickens with [not (member? flu-state "eid")]</metric>
    <metric>e:bychickens</metric>
    <metric>i:bychickens</metric>
    <metric>d:bychickens</metric>
    <metric>count byducks with [not (member? flu-state "eir")]</metric>
    <metric>e:byducks</metric>
    <metric>i:byducks</metric>
    <metric>r:byducks</metric>
    <metric>count farms with [not (member? flu-state "eird")]</metric>
    <metric>e:farms</metric>
    <metric>i:farms</metric>
    <metric>dr:farms</metric>
    <metric>e:poultry</metric>
    <metric>i:poultry</metric>
    <metric>r:poultry</metric>
    <metric>count eggtraders</metric>
    <enumeratedValueSet variable="road-pen-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="road-byc-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="from-visit-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="to-visit-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-eggtrader-contam-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eggtrader-flock-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-livetrader-contam-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="livetrader-flock-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-chicken">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-duck">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline">
      <value value="&quot;pert&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="visitorInfectProb" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-transmissions-and-contaminations</final>
    <metric>table:to-list transmissions</metric>
    <metric>table:to-list contaminations</metric>
    <metric>count rice-fields with [ (field-state = "harvested") and (pcontam-until &gt; ticks)]</metric>
    <metric>count fgducks with [not (member? flu-state "eir")]</metric>
    <metric>count fgducks with [flu-state = "e"]</metric>
    <metric>count fgducks with [flu-state = "i"]</metric>
    <metric>count fgducks with [flu-state = "r"]</metric>
    <metric>count bychickens with [not (member? flu-state "eid")]</metric>
    <metric>count bychickens with [flu-state = "e"]</metric>
    <metric>count bychickens with [flu-state = "i"]</metric>
    <metric>count bychickens with [flu-state = "d"]</metric>
    <metric>count byducks with [not (member? flu-state "eir")]</metric>
    <metric>count byducks with [flu-state = "e"]</metric>
    <metric>count byducks with [flu-state = "i"]</metric>
    <metric>count byducks with [flu-state = "r"]</metric>
    <metric>count farms with [not (member? flu-state "eird")]</metric>
    <metric>count farms with [flu-state = "e"]</metric>
    <metric>count farms with [flu-state = "i"]</metric>
    <metric>count farms with [member? flu-state "dr"]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "e")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "i")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "r")]</metric>
    <metric>count eggtraders</metric>
    <enumeratedValueSet variable="baseline">
      <value value="&quot;pert&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="road-pen-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="road-byc-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="from-visit-infect-pct">
      <value value="98"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="to-visit-infect-pct">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-eggtrader-contam-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eggtrader-flock-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-livetrader-contam-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="livetrader-flock-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-chicken">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-duck">
      <value value="7"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="fgdFieldBarn" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-transmissions-and-contaminations</final>
    <metric>table:to-list transmissions</metric>
    <metric>table:to-list contaminations</metric>
    <metric>map [item 1 ?] table:to-list days-on-field</metric>
    <metric>count barns with [pcontam-until &gt; ticks]</metric>
    <metric>count rice-fields with [ (field-state = "harvested") and (pcontam-until &gt; ticks)]</metric>
    <metric>count fgducks with [not (member? flu-state "eir")]</metric>
    <metric>count fgducks with [flu-state = "e"]</metric>
    <metric>count fgducks with [flu-state = "i"]</metric>
    <metric>count fgducks with [flu-state = "r"]</metric>
    <metric>count bychickens with [not (member? flu-state "eid")]</metric>
    <metric>count bychickens with [flu-state = "e"]</metric>
    <metric>count bychickens with [flu-state = "i"]</metric>
    <metric>count bychickens with [flu-state = "d"]</metric>
    <metric>count byducks with [not (member? flu-state "eir")]</metric>
    <metric>count byducks with [flu-state = "e"]</metric>
    <metric>count byducks with [flu-state = "i"]</metric>
    <metric>count byducks with [flu-state = "r"]</metric>
    <metric>count farms with [not (member? flu-state "eird")]</metric>
    <metric>count farms with [flu-state = "e"]</metric>
    <metric>count farms with [flu-state = "i"]</metric>
    <metric>count farms with [member? flu-state "dr"]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "e")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "i")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "r")]</metric>
    <metric>count eggtraders</metric>
    <enumeratedValueSet variable="baseline">
      <value value="&quot;pert&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="road-pen-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="road-byc-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="from-visit-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="to-visit-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-eggtrader-contam-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eggtrader-flock-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-livetrader-contam-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="livetrader-flock-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-chicken">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-duck">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-fgd-barntofield">
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="eggtraderLinks" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-transmissions-and-contaminations</final>
    <metric>table:to-list transmissions</metric>
    <metric>table:to-list contaminations</metric>
    <metric>count rice-fields with [ (field-state = "harvested") and (pcontam-until &gt; ticks)]</metric>
    <metric>count fgducks with [not (member? flu-state "eir")]</metric>
    <metric>count fgducks with [flu-state = "e"]</metric>
    <metric>count fgducks with [flu-state = "i"]</metric>
    <metric>count fgducks with [flu-state = "r"]</metric>
    <metric>count bychickens with [not (member? flu-state "eid")]</metric>
    <metric>count bychickens with [flu-state = "e"]</metric>
    <metric>count bychickens with [flu-state = "i"]</metric>
    <metric>count bychickens with [flu-state = "d"]</metric>
    <metric>count byducks with [not (member? flu-state "eir")]</metric>
    <metric>count byducks with [flu-state = "e"]</metric>
    <metric>count byducks with [flu-state = "i"]</metric>
    <metric>count byducks with [flu-state = "r"]</metric>
    <metric>count farms with [not (member? flu-state "eird")]</metric>
    <metric>count farms with [flu-state = "e"]</metric>
    <metric>count farms with [flu-state = "i"]</metric>
    <metric>count farms with [member? flu-state "dr"]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "e")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "i")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "r")]</metric>
    <metric>count eggtraders</metric>
    <enumeratedValueSet variable="baseline">
      <value value="&quot;pert&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="road-pen-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="road-byc-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="from-visit-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="to-visit-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-eggtrader-contam-pct">
      <value value="88"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eggtrader-flock-infect-pct">
      <value value="85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-livetrader-contam-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="livetrader-flock-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-chicken">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-duck">
      <value value="7"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="duckShedding" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-transmissions-and-contaminations</final>
    <metric>table:to-list transmissions</metric>
    <metric>table:to-list contaminations</metric>
    <metric>count rice-fields with [ (field-state = "harvested") and (pcontam-until &gt; ticks)]</metric>
    <metric>count fgducks with [not (member? flu-state "eir")]</metric>
    <metric>count fgducks with [flu-state = "e"]</metric>
    <metric>count fgducks with [flu-state = "i"]</metric>
    <metric>count fgducks with [flu-state = "r"]</metric>
    <metric>count bychickens with [not (member? flu-state "eid")]</metric>
    <metric>count bychickens with [flu-state = "e"]</metric>
    <metric>count bychickens with [flu-state = "i"]</metric>
    <metric>count bychickens with [flu-state = "d"]</metric>
    <metric>count byducks with [not (member? flu-state "eir")]</metric>
    <metric>count byducks with [flu-state = "e"]</metric>
    <metric>count byducks with [flu-state = "i"]</metric>
    <metric>count byducks with [flu-state = "r"]</metric>
    <metric>count farms with [not (member? flu-state "eird")]</metric>
    <metric>count farms with [flu-state = "e"]</metric>
    <metric>count farms with [flu-state = "i"]</metric>
    <metric>count farms with [member? flu-state "dr"]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "e")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "i")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "r")]</metric>
    <metric>count eggtraders</metric>
    <enumeratedValueSet variable="baseline">
      <value value="&quot;pert&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="road-pen-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="road-byc-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="from-visit-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="to-visit-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-eggtrader-contam-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eggtrader-flock-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-livetrader-contam-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="livetrader-flock-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-chicken">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-duck">
      <value value="7"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="chickenShedding" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-transmissions-and-contaminations</final>
    <metric>table:to-list transmissions</metric>
    <metric>table:to-list contaminations</metric>
    <metric>count barns with [pcontam-until &gt; ticks]</metric>
    <metric>count rice-fields with [ (field-state = "harvested") and (pcontam-until &gt; ticks)]</metric>
    <metric>count fgducks with [not (member? flu-state "eir")]</metric>
    <metric>count fgducks with [flu-state = "e"]</metric>
    <metric>count fgducks with [flu-state = "i"]</metric>
    <metric>count fgducks with [flu-state = "r"]</metric>
    <metric>count bychickens with [not (member? flu-state "eid")]</metric>
    <metric>count bychickens with [flu-state = "e"]</metric>
    <metric>count bychickens with [flu-state = "i"]</metric>
    <metric>count bychickens with [flu-state = "d"]</metric>
    <metric>count byducks with [not (member? flu-state "eir")]</metric>
    <metric>count byducks with [flu-state = "e"]</metric>
    <metric>count byducks with [flu-state = "i"]</metric>
    <metric>count byducks with [flu-state = "r"]</metric>
    <metric>count farms with [not (member? flu-state "eird")]</metric>
    <metric>count farms with [flu-state = "e"]</metric>
    <metric>count farms with [flu-state = "i"]</metric>
    <metric>count farms with [member? flu-state "dr"]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "e")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "i")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "r")]</metric>
    <metric>count eggtraders</metric>
    <enumeratedValueSet variable="baseline">
      <value value="&quot;pert&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-chicken">
      <value value="1"/>
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="roadInfectBYC" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-transmissions-and-contaminations</final>
    <metric>table:to-list transmissions</metric>
    <metric>table:to-list contaminations</metric>
    <metric>count rice-fields with [ (field-state = "harvested") and (pcontam-until &gt; ticks)]</metric>
    <metric>count fgducks with [not (member? flu-state "eir")]</metric>
    <metric>count fgducks with [flu-state = "e"]</metric>
    <metric>count fgducks with [flu-state = "i"]</metric>
    <metric>count fgducks with [flu-state = "r"]</metric>
    <metric>count bychickens with [not (member? flu-state "eid")]</metric>
    <metric>count bychickens with [flu-state = "e"]</metric>
    <metric>count bychickens with [flu-state = "i"]</metric>
    <metric>count bychickens with [flu-state = "d"]</metric>
    <metric>count byducks with [not (member? flu-state "eir")]</metric>
    <metric>count byducks with [flu-state = "e"]</metric>
    <metric>count byducks with [flu-state = "i"]</metric>
    <metric>count byducks with [flu-state = "r"]</metric>
    <metric>count farms with [not (member? flu-state "eird")]</metric>
    <metric>count farms with [flu-state = "e"]</metric>
    <metric>count farms with [flu-state = "i"]</metric>
    <metric>count farms with [member? flu-state "dr"]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "e")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "i")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "r")]</metric>
    <metric>count eggtraders</metric>
    <enumeratedValueSet variable="baseline">
      <value value="&quot;pert&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="road-pen-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="road-byc-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="from-visit-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="to-visit-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-eggtrader-contam-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eggtrader-flock-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-livetrader-contam-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="livetrader-flock-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-chicken">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-duck">
      <value value="7"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="roadPenProb" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-transmissions-and-contaminations</final>
    <metric>table:to-list transmissions</metric>
    <metric>table:to-list contaminations</metric>
    <metric>count rice-fields with [ (field-state = "harvested") and (pcontam-until &gt; ticks)]</metric>
    <metric>count fgducks with [not (member? flu-state "eir")]</metric>
    <metric>count fgducks with [flu-state = "e"]</metric>
    <metric>count fgducks with [flu-state = "i"]</metric>
    <metric>count fgducks with [flu-state = "r"]</metric>
    <metric>count bychickens with [not (member? flu-state "eid")]</metric>
    <metric>count bychickens with [flu-state = "e"]</metric>
    <metric>count bychickens with [flu-state = "i"]</metric>
    <metric>count bychickens with [flu-state = "d"]</metric>
    <metric>count byducks with [not (member? flu-state "eir")]</metric>
    <metric>count byducks with [flu-state = "e"]</metric>
    <metric>count byducks with [flu-state = "i"]</metric>
    <metric>count byducks with [flu-state = "r"]</metric>
    <metric>count farms with [not (member? flu-state "eird")]</metric>
    <metric>count farms with [flu-state = "e"]</metric>
    <metric>count farms with [flu-state = "i"]</metric>
    <metric>count farms with [member? flu-state "dr"]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "e")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "i")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "r")]</metric>
    <metric>count eggtraders</metric>
    <enumeratedValueSet variable="baseline">
      <value value="&quot;pert&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="road-pen-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="road-byc-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="from-visit-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="to-visit-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-eggtrader-contam-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eggtrader-flock-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-livetrader-contam-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="livetrader-flock-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-chicken">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-duck">
      <value value="7"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="initialInfectionSource" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-transmissions-and-contaminations</final>
    <metric>table:to-list transmissions</metric>
    <metric>table:to-list contaminations</metric>
    <metric>map [item 1 ?] table:to-list days-on-field</metric>
    <metric>count rice-fields with [ (field-state = "harvested") and (pcontam-until &gt; ticks)]</metric>
    <metric>count fgducks with [not (member? flu-state "eir")]</metric>
    <metric>count fgducks with [flu-state = "e"]</metric>
    <metric>count fgducks with [flu-state = "i"]</metric>
    <metric>count fgducks with [flu-state = "r"]</metric>
    <metric>count bychickens with [not (member? flu-state "eid")]</metric>
    <metric>count bychickens with [flu-state = "e"]</metric>
    <metric>count bychickens with [flu-state = "i"]</metric>
    <metric>count bychickens with [flu-state = "d"]</metric>
    <metric>count byducks with [not (member? flu-state "eir")]</metric>
    <metric>count byducks with [flu-state = "e"]</metric>
    <metric>count byducks with [flu-state = "i"]</metric>
    <metric>count byducks with [flu-state = "r"]</metric>
    <metric>count farms with [not (member? flu-state "eird")]</metric>
    <metric>count farms with [flu-state = "e"]</metric>
    <metric>count farms with [flu-state = "i"]</metric>
    <metric>count farms with [member? flu-state "dr"]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "e")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "i")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "r")]</metric>
    <metric>count eggtraders</metric>
    <enumeratedValueSet variable="baseline">
      <value value="&quot;pert&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infection-source">
      <value value="&quot;fgduck&quot;"/>
      <value value="&quot;farm&quot;"/>
      <value value="&quot;byduck&quot;"/>
      <value value="&quot;bychicken&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="road-pen-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="road-byc-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="from-visit-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="to-visit-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-eggtrader-contam-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eggtrader-flock-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-livetrader-contam-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="livetrader-flock-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-chicken">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-duck">
      <value value="7"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="highEggtraderContamInfect" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-transmissions-and-contaminations</final>
    <metric>table:to-list transmissions</metric>
    <metric>table:to-list contaminations</metric>
    <metric>count barns with [pcontam-until &gt; ticks]</metric>
    <metric>count rice-fields with [ (field-state = "harvested") and (pcontam-until &gt; ticks)]</metric>
    <metric>count fgducks with [not (member? flu-state "eir")]</metric>
    <metric>count fgducks with [flu-state = "e"]</metric>
    <metric>count fgducks with [flu-state = "i"]</metric>
    <metric>count fgducks with [flu-state = "r"]</metric>
    <metric>count bychickens with [not (member? flu-state "eid")]</metric>
    <metric>count bychickens with [flu-state = "e"]</metric>
    <metric>count bychickens with [flu-state = "i"]</metric>
    <metric>count bychickens with [flu-state = "d"]</metric>
    <metric>count byducks with [not (member? flu-state "eir")]</metric>
    <metric>count byducks with [flu-state = "e"]</metric>
    <metric>count byducks with [flu-state = "i"]</metric>
    <metric>count byducks with [flu-state = "r"]</metric>
    <metric>count farms with [not (member? flu-state "eird")]</metric>
    <metric>count farms with [flu-state = "e"]</metric>
    <metric>count farms with [flu-state = "i"]</metric>
    <metric>count farms with [member? flu-state "dr"]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "e")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "i")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "r")]</metric>
    <metric>count eggtraders</metric>
    <enumeratedValueSet variable="baseline">
      <value value="&quot;pert&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="road-pen-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="road-byc-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="from-visit-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="to-visit-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-eggtrader-contam-pct">
      <value value="88"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eggtrader-flock-infect-pct">
      <value value="88"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-livetrader-contam-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="livetrader-flock-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-chicken">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-duck">
      <value value="7"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="highLivetraderContamInfect" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-transmissions-and-contaminations</final>
    <metric>table:to-list transmissions</metric>
    <metric>table:to-list contaminations</metric>
    <metric>count barns with [pcontam-until &gt; ticks]</metric>
    <metric>count rice-fields with [ (field-state = "harvested") and (pcontam-until &gt; ticks)]</metric>
    <metric>count fgducks with [not (member? flu-state "eir")]</metric>
    <metric>count fgducks with [flu-state = "e"]</metric>
    <metric>count fgducks with [flu-state = "i"]</metric>
    <metric>count fgducks with [flu-state = "r"]</metric>
    <metric>count bychickens with [not (member? flu-state "eid")]</metric>
    <metric>count bychickens with [flu-state = "e"]</metric>
    <metric>count bychickens with [flu-state = "i"]</metric>
    <metric>count bychickens with [flu-state = "d"]</metric>
    <metric>count byducks with [not (member? flu-state "eir")]</metric>
    <metric>count byducks with [flu-state = "e"]</metric>
    <metric>count byducks with [flu-state = "i"]</metric>
    <metric>count byducks with [flu-state = "r"]</metric>
    <metric>count farms with [not (member? flu-state "eird")]</metric>
    <metric>count farms with [flu-state = "e"]</metric>
    <metric>count farms with [flu-state = "i"]</metric>
    <metric>count farms with [member? flu-state "dr"]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "e")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "i")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "r")]</metric>
    <metric>count eggtraders</metric>
    <enumeratedValueSet variable="baseline">
      <value value="&quot;pert&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="road-pen-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="road-byc-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="from-visit-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="to-visit-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-eggtrader-contam-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eggtrader-flock-infect-pct">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flock-livetrader-contam-pct">
      <value value="95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="livetrader-flock-infect-pct">
      <value value="92"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-chicken">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-duck">
      <value value="7"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="roadFlock" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-transmissions-and-contaminations</final>
    <metric>table:to-list transmissions</metric>
    <metric>table:to-list contaminations</metric>
    <metric>map [item 1 ?] table:to-list days-on-field</metric>
    <metric>count barns with [pcontam-until &gt; ticks]</metric>
    <metric>count rice-fields with [ (field-state = "harvested") and (pcontam-until &gt; ticks)]</metric>
    <metric>count fgducks with [not (member? flu-state "eir")]</metric>
    <metric>count fgducks with [flu-state = "e"]</metric>
    <metric>count fgducks with [flu-state = "i"]</metric>
    <metric>count fgducks with [flu-state = "r"]</metric>
    <metric>count bychickens with [not (member? flu-state "eid")]</metric>
    <metric>count bychickens with [flu-state = "e"]</metric>
    <metric>count bychickens with [flu-state = "i"]</metric>
    <metric>count bychickens with [flu-state = "d"]</metric>
    <metric>count byducks with [not (member? flu-state "eir")]</metric>
    <metric>count byducks with [flu-state = "e"]</metric>
    <metric>count byducks with [flu-state = "i"]</metric>
    <metric>count byducks with [flu-state = "r"]</metric>
    <metric>count farms with [not (member? flu-state "eird")]</metric>
    <metric>count farms with [flu-state = "e"]</metric>
    <metric>count farms with [flu-state = "i"]</metric>
    <metric>count farms with [member? flu-state "dr"]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "e")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "i")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "r")]</metric>
    <metric>count eggtraders</metric>
    <enumeratedValueSet variable="baseline">
      <value value="&quot;mode&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;road-flock(high)&quot;"/>
      <value value="&quot;road-flock(low)&quot;"/>
      <value value="&quot;road-flock(zero)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-fgd-barntofield">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-chicken">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-duck">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-cooperatives">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="visit" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-transmissions-and-contaminations</final>
    <metric>table:to-list transmissions</metric>
    <metric>table:to-list contaminations</metric>
    <metric>map [item 1 ?] table:to-list days-on-field</metric>
    <metric>count barns with [pcontam-until &gt; ticks]</metric>
    <metric>count rice-fields with [ (field-state = "harvested") and (pcontam-until &gt; ticks)]</metric>
    <metric>count fgducks with [not (member? flu-state "eir")]</metric>
    <metric>count fgducks with [flu-state = "e"]</metric>
    <metric>count fgducks with [flu-state = "i"]</metric>
    <metric>count fgducks with [flu-state = "r"]</metric>
    <metric>count bychickens with [not (member? flu-state "eid")]</metric>
    <metric>count bychickens with [flu-state = "e"]</metric>
    <metric>count bychickens with [flu-state = "i"]</metric>
    <metric>count bychickens with [flu-state = "d"]</metric>
    <metric>count byducks with [not (member? flu-state "eir")]</metric>
    <metric>count byducks with [flu-state = "e"]</metric>
    <metric>count byducks with [flu-state = "i"]</metric>
    <metric>count byducks with [flu-state = "r"]</metric>
    <metric>count farms with [not (member? flu-state "eird")]</metric>
    <metric>count farms with [flu-state = "e"]</metric>
    <metric>count farms with [flu-state = "i"]</metric>
    <metric>count farms with [member? flu-state "dr"]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "e")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "i")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "r")]</metric>
    <metric>count eggtraders</metric>
    <enumeratedValueSet variable="baseline">
      <value value="&quot;mode&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;visit(veryhigh)&quot;"/>
      <value value="&quot;visit(high)&quot;"/>
      <value value="&quot;visit(low)&quot;"/>
      <value value="&quot;visit(zero)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-fgd-barntofield">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-chicken">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-duck">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-cooperatives">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="eggtrade" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-transmissions-and-contaminations</final>
    <metric>table:to-list transmissions</metric>
    <metric>table:to-list contaminations</metric>
    <metric>map [item 1 ?] table:to-list days-on-field</metric>
    <metric>count barns with [pcontam-until &gt; ticks]</metric>
    <metric>count rice-fields with [ (field-state = "harvested") and (pcontam-until &gt; ticks)]</metric>
    <metric>count fgducks with [not (member? flu-state "eir")]</metric>
    <metric>count fgducks with [flu-state = "e"]</metric>
    <metric>count fgducks with [flu-state = "i"]</metric>
    <metric>count fgducks with [flu-state = "r"]</metric>
    <metric>count bychickens with [not (member? flu-state "eid")]</metric>
    <metric>count bychickens with [flu-state = "e"]</metric>
    <metric>count bychickens with [flu-state = "i"]</metric>
    <metric>count bychickens with [flu-state = "d"]</metric>
    <metric>count byducks with [not (member? flu-state "eir")]</metric>
    <metric>count byducks with [flu-state = "e"]</metric>
    <metric>count byducks with [flu-state = "i"]</metric>
    <metric>count byducks with [flu-state = "r"]</metric>
    <metric>count farms with [not (member? flu-state "eird")]</metric>
    <metric>count farms with [flu-state = "e"]</metric>
    <metric>count farms with [flu-state = "i"]</metric>
    <metric>count farms with [member? flu-state "dr"]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "e")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "i")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "r")]</metric>
    <metric>count eggtraders</metric>
    <enumeratedValueSet variable="baseline">
      <value value="&quot;mode&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;eggtrade(high)&quot;"/>
      <value value="&quot;eggtrade(low)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-fgd-barntofield">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-chicken">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-duck">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-cooperatives">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="aibaseline" repetitions="500" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-transmissions-and-contaminations</final>
    <metric>table:to-list transmissions</metric>
    <metric>table:to-list contaminations</metric>
    <metric>map [item 1 ?] table:to-list days-on-field</metric>
    <metric>count barns with [pcontam-until &gt; ticks]</metric>
    <metric>count rice-fields with [ (field-state = "harvested") and (pcontam-until &gt; ticks)]</metric>
    <metric>count fgducks with [not (member? flu-state "eir")]</metric>
    <metric>count fgducks with [flu-state = "e"]</metric>
    <metric>count fgducks with [flu-state = "i"]</metric>
    <metric>count fgducks with [flu-state = "r"]</metric>
    <metric>count bychickens with [not (member? flu-state "eid")]</metric>
    <metric>count bychickens with [flu-state = "e"]</metric>
    <metric>count bychickens with [flu-state = "i"]</metric>
    <metric>count bychickens with [flu-state = "d"]</metric>
    <metric>count byducks with [not (member? flu-state "eir")]</metric>
    <metric>count byducks with [flu-state = "e"]</metric>
    <metric>count byducks with [flu-state = "i"]</metric>
    <metric>count byducks with [flu-state = "r"]</metric>
    <metric>count farms with [not (member? flu-state "eird")]</metric>
    <metric>count farms with [flu-state = "e"]</metric>
    <metric>count farms with [flu-state = "i"]</metric>
    <metric>count farms with [member? flu-state "dr"]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "e")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "i")]</metric>
    <metric>count turtles with [(member? breed poultry-types) and (flu-state = "r")]</metric>
    <metric>count eggtraders</metric>
    <enumeratedValueSet variable="baseline">
      <value value="&quot;mode&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-fgd-barntofield">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-chicken">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shedding-period-duck">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-cooperatives">
      <value value="0"/>
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
1
@#$#@#$#@
