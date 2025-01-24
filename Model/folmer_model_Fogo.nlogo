extensions [gis matrix csv table]
;; Model for exploring the mechanisms of IFD undermatching through social attraction, interference competition, and density-dependent effects

breed [caribous caribou]

; model parameters that will be used for each cell
globals [

  ;attack_rate           ;; a from the Folmer paper, assumed = 1
  handling_time         ;; h from the Folmer paper, assumed = 1
  total_number_of_patches       ;; N in the paper define the number of patches
  total_number_of_caribou
  number_of_rows ;;
  number_of_columns ;;
                    ;  interference_rate ;; q in the Folmer paper
                    ;  intensity_of_conspecific_attraction ;; s in the Folmer paper
  spatial_weights_matrix ;; W in the paper
  gaussian_filter ;; 2D gaussian filter that uses r to obtain the values
                  ;; kernel_range  ;; r in the paper for Gaussian filter
  ; the_percentage ;; the percentage of leaders
]


; define new property for patches object i.e. cells
patches-own [
  is_border ; designate border 0 means world 1 means border
  expected_consumption_rate ;; C_i in the Folmer paper
                            ;; store a list for each animal so that it can be called later and also updated
  conspecific_attractiveness ;; S_i in the Folmer paper
  total_expected_attractiveness ;; T_i in the Folmer paper
  true_amount_of_food       ;; amount of food available on the patch, updates with consumption
  expected_amount_of_food   ;; estimated amount of food, generated from normal distribution with mean at true_amount_of_food truncated at 0
  normalized_spatial_weight ;; normalized weight for the particular cell/patch
                            ; conspecific_on_cell ;; P_j or P_i in the paper call count caribou-here for cell specific or count caribou-on neighbor for adjacent
  landed ;; counts the number of caribou that visit the patch over the trial
  final_caribou_count ;; the total number of caribou present on the patch at the end of the trial
  initial_food ;; initial food present on the patch before consumption
]

caribous-own [
  ;; expected_food_at_cell is a single long list
  ;; index 5 means row 0 column 4, as netlogo starts index at 0
  is_leader ;; are they leader or follower
  current_expected_consumption_rate ;; a caribou's individual anticipated consumption rate based on functional response
  current_total_expected_attractiveness ;; a caribou's individual total attractedness to a patch, sum of consumption rate and social attraction
  total_food_consumed ;; running total for all food consumed during the sim
  residence_time  ;; the number of time steps that a caribou stays on an individual patch
  food_and_residence_time_list ;; list that contains the xcor, ycor, and residence time, is a dimension is MAX_TICKS x 3
  has_relocated ;; checks whether they relocate at each time step
]
;;;******************************************************** SIMULATION FUNCTIONS *********************************************************;;;
to setup
  clear-all
  reset-ticks
  clear-output
  ;  random-seed 0 ;; set for dev purposes but comment out when running tests
  ; set the_percentage 0.2 ;; the percentage of leaders ;; made into slider for sensitivity analysis
  ; set attack_rate 1 ;; set by the Folmer paper ;; made into slider for sensitivity analysis
  set handling_time 1 ;; set by the Folmer paper

  set number_of_rows  (max-pycor + 1)
  set number_of_columns (max-pxcor + 1)
  set total_number_of_patches number_of_rows * number_of_columns
  ifelse kernel_range = 3 [
    set total_number_of_caribou density * (((number_of_rows - 2) * (number_of_columns - 2)) / 25)
  ]
  [
    set total_number_of_caribou density * (((number_of_rows - 4) * (number_of_columns - 4)) / 25)
  ]

  ; these are fixed through the simulation
  set_world_border ;; this defines which patches are border and which are world
  set_true_amount_of_food ;; true amount of food changes with consumption

  set_spatial_weights_for_each_cell ;; spatial weights are fixed
  colour_grass ;; colour in the food

  let leader_surrounding_cells_list [] ;;create list of leader spaces that followers can choose from
  let location []

  create-caribous total_number_of_caribou [
    set has_relocated false
    set residence_time 1
    set shape "cow"

  ;; setting each caribou as leader or follower based on probability of drawing a value above or below assigned percentage
   ifelse (random-float 1 < the_percentage)
   [
      set is_leader true
    ]
    [
      set is_leader false
    ]

    ; now we determine where the caribou should go
   if (is_leader = true) [
      set color black
      setxy random-xcor random-ycor
      while [pcolor = black] ;; because of border cells, move any individuals that randomly occur in these spaces to actual green spaces
      [
      setxy random-xcor random-ycor
      ]

      set leader_surrounding_cells_list lput (patches in-radius 2 with [is_border = false]) leader_surrounding_cells_list
    ]

    set total_food_consumed 0
  ]


  let neighbor_cells []
  ; loop through followers
  ask caribous with [is_leader = false] [
    set color white
    ; select a leader for each follower
    set neighbor_cells one-of leader_surrounding_cells_list

    ; randomly move follower to one of the neighboring cells as long
    ; as the number of caribou on the cell is less then 10
    move-to one-of neighbor_cells with [count caribous-here < 10]

  ]

  ask patches [
    set initial_food true_amount_of_food
  ]

  let tmp matrix:make-constant (MAX_TICKS + 1) 2 0

  ;; give caribou starting conditions for current patch
  ask caribous [
    set food_and_residence_time_list matrix:to-row-list tmp
    let food_and_res_time (list true_amount_of_food residence_time)
    set food_and_residence_time_list (replace-item ticks food_and_residence_time_list food_and_res_time)
  ]
end

to relocate

  if (ticks > MAX_TICKS) [
    ask patches [
      set final_caribou_count count caribous-here
      ;print (final_caribou_count)
    ]
    stop
  ]

  if (ticks < MAX_TICKS) [

    ask caribous [
      let old-patch patch-here ;; assign patch location to current location
      reevaluate_current_location ;; determine attractivness of current patch
      update_neighboring_patch_values ;; determine attractiveness of neighboring patches

      ifelse (is_leader = true) [
        relocate_leader
      ]
      [ ;; caribou is follower
        relocate_follower
      ]
;      colour_grass ;; only have toggled on if watching on interface, leave off for behaviourspace

      if old-patch != patch-here [ ;; determine if animal has relocated after relocation attempt

        ask patch-here [
          set landed landed + 1 ;; add a patch visit to the new patch
                                ;print (word ticks " "  pxcor " "  pycor  " " landed )
        ]
      ]
    ]


    ask caribous [
      ifelse has_relocated = true [
        set residence_time 1
      ]
      [ ;; else stayed at current patch
        set residence_time residence_time + 1 ;; increase residence time if caribou hasn't relocated
      ]
      set has_relocated false
      let food_and_res_time (list true_amount_of_food residence_time) ;;at each time step, recording residence time at patch value
      set food_and_residence_time_list (replace-item ticks food_and_residence_time_list food_and_res_time)
    ]
  ]
  tick

end


to relocate_follower

  let threshold current_total_expected_attractiveness
  ;; get the neighboring cells with expected expected consumption rate greater than the current patch
  let new_locations neighbors with [total_expected_attractiveness >  threshold]
  ;    let new_locations max-one-of neighbors [expected_consumption_rate]

  ;; if any new location candidates exists
  if any? new_locations [
    ;; we move the the one with the highest expected total attractiveness including social attraction
    move-to one-of (max-n-of 1 new_locations [total_expected_attractiveness])

    ;; update total attractiveness
    set current_total_expected_attractiveness total_expected_attractiveness

    ;; update current consumption rate
    set current_expected_consumption_rate expected_consumption_rate
    set has_relocated true
  ]

  ;; depletion
  set true_amount_of_food true_amount_of_food - (current_expected_consumption_rate / 1000)
  ;; update total food consumed
  set total_food_consumed total_food_consumed + (current_expected_consumption_rate / 1000)
  if not (true_amount_of_food > 0.01) [
    set true_amount_of_food 0
  ]
  ;;


end


to relocate_leader

  let threshold current_expected_consumption_rate

  ;; get the neighboring cells with expected expected consumption rate greater than the current patch
  let new_locations neighbors with [expected_consumption_rate > threshold ]
  ;    let new_locations max-one-of neighbors [expected_consumption_rate]

  ;; if any new location candidates exists
  if any? new_locations [
    ;; we move the the one with the highest expected consumption rate
    move-to one-of (max-n-of 1 new_locations [expected_consumption_rate])
    ;; update current comsumption rate
    set current_expected_consumption_rate expected_consumption_rate
    set has_relocated true
  ]

   ;; depletion
  set true_amount_of_food true_amount_of_food - (current_expected_consumption_rate / 1000)
    ;; update total food consumed
  set total_food_consumed total_food_consumed + (current_expected_consumption_rate / 1000)
  ;; the small non zero value is for floating point arithmatic
  if not (true_amount_of_food > 0.01) [
    set true_amount_of_food 0
  ]

end

to post_run
   ; let random_value random-normal 10 5
  export-world (word "trial_"  density "_" interference_rate "_" intensity_of_conspecific_attraction "_" behaviorspace-run-number ".csv")
end

;;;************************************************************************************************************************************;;;
;;;******************************************************** CARIBOU FUNCTIONS *********************************************************;;;
;;;************************************************************************************************************************************;;;

to reevaluate_current_location
  ;; before caribou can make decision on where to move. They reevaluate their current location
  ;; update expected amount of food matrix with realized food values once caribou have entered a cell
  ;; if caribou have imperfect information then we need to update the list based on where they are

  ask patch-here [

    let random_value random-normal true_amount_of_food theta

    ; value must be non-zero
    while [random_value < 0] [
      set random_value random-normal true_amount_of_food theta
    ]

    set expected_amount_of_food random_value
  ]

  ;; S_i in the Folmer paper. Each caribou stores this information as the weight matrix info is stored on the patch
  let new_conspecific_attractiveness_value 0
  ask patch-here [

    ;; count the number of caribou in the current patch, this is P_i in the in the Folmer paper
    let number_of_caribou_on_cell count caribous-here

    ;; also count the number of caribou in the neighbour patches, this is P_j
    let number_of_caribou_on_neighbors count caribous-on neighbors with [is_border = false]
;    print word "caribou on neighbors " number_of_caribou_on_neighbors
    ;; numerator is \sum
    let numerator (normalized_spatial_weight * (number_of_caribou_on_cell + number_of_caribou_on_neighbors))
    let denominator ( numerator + 1 )

    ;; compute new conspecific attractiveness value at current location
    set new_conspecific_attractiveness_value ( ( intensity_of_conspecific_attraction * numerator ) / denominator )
    set conspecific_attractiveness new_conspecific_attractiveness_value
  ]

  ;; C_i in the paper
  let number_of_caribou_on_cell 0
    let new_expected_consumption_rate 0
  ask patch-here [
    ;; count the number of caribou in the current patch, this is P_i in the in the paper
    set number_of_caribou_on_cell count caribous-here
  ]

  ;; compute a*R_i
  let numerator (expected_amount_of_food * attack_rate)

  ;; compute  q*P_i + a*h*R_i + 1
  let denominator ( (number_of_caribou_on_cell * interference_rate) + (numerator * handling_time) + 1)
  set new_expected_consumption_rate (numerator / denominator)

  ; update the expected consumption rate at this cell
  set expected_consumption_rate new_expected_consumption_rate

  ;; update the specific cell with new value
  set total_expected_attractiveness (conspecific_attractiveness + expected_consumption_rate) ;; T_i in the paper
  set current_expected_consumption_rate expected_consumption_rate
  set current_total_expected_attractiveness total_expected_attractiveness

end

;;;************************************************************************************************************************************;;;
;;;******************************************************** CARIBOU FUNCTIONS END *********************************************************;;;
;;;************************************************************************************************************************************;;;


;;;************************************************************************************************************************************;;;
;;;******************************************************** PATCH FUNCTIONS *********************************************************;;;
;;;************************************************************************************************************************************;;;

to set_spatial_weights_for_each_cell
  ;; NOTE: This function is only run at setup and does not need to be called again

  ;; Set the values of W_ij in the spatial weight matrix with first-order queen contiguity
  ;; W_ij = 1 if there is a common border or vertex and W_ii = 1 as the model allows for
  ;; conspecific attraction within the cell. Then, W_ij = 0 elsewhere.
  ;; The wights are also row normalized, i.e., the sum of each row is 1

  ;; since the weight matrix is normalized the sum_j^N W_ij*Pj = W_i * sum_(j in neighbors of i) P_j as W_ij is 0 everywhere
  let spatial_weight_matrix matrix:make-constant number_of_rows number_of_columns 0

  ;; set border cells
  ask patches with [is_border = true] [
    set normalized_spatial_weight 0
  ]

  ;; set non border cells
  ask patches with [is_border = false][
    ;; As the weight matrix is  sparse. We store the data in the patch
    set normalized_spatial_weight (1 / ( (count neighbors with [is_border = false]) + 1) ) ;; we add 1 to the count of neighbors to include the current patch
    matrix:set spatial_weight_matrix pycor pxcor normalized_spatial_weight
  ]

end

;; TO-DO: change these to to-report functions so that they can be used with ask caribous
to set_true_amount_of_food
  ;; code follows how the 3x3 kernal would be computed using the matlab implementation
  ;random-seed food_seed_value
  ask patches
  [
;    let random_value random-normal 0 1
    let random_value random-float 13
    set true_amount_of_food random_value
  ]

  padding
  smooth_true_amount_of_food

  ask patches with [true_amount_of_food < 0 ] [
      set true_amount_of_food 0
  ]

  padding

end


to neighbors_update_expected_amount_of_food
  ask neighbors with [is_border = false][

    let random_value random-normal true_amount_of_food theta
    ; value must be non-zero
    while [random_value < 0] [
      set random_value random-normal true_amount_of_food theta
    ]

    set expected_amount_of_food random_value
  ]

end


to neighbors_update_expected_consumption_rate

  ask neighbors with [is_border = false][

    ;; count the number of caribou in the current patch, this is P_i in the in the paper
    let number_of_caribou_on_cell count caribous-here

    ;; compute a*R_i
    let numerator (expected_amount_of_food * attack_rate)

    ;; compute  q*P_i + a*h*R_i + 1
    let denominator ( (number_of_caribou_on_cell * interference_rate) + (numerator * handling_time) + 1)
    let new_expected_consumption_rate_at_cell (numerator / denominator)

    ; update patch information
    set expected_consumption_rate new_expected_consumption_rate_at_cell

  ]

end

to neighbors_update_conspecific_attractiveness
  let new_conspecific_attractiveness_value 0

  ask neighbors with [is_border = false][

    ;; count the number of caribou in the current patch, this is P_i in the in the paper
    let number_of_caribou_on_cell count caribous-here

    ;; also count the number of caribou in the neighbour patches, this is P_j
    let number_of_caribou_on_neighbors count caribous-on neighbors with [is_border = false]

    ;; numerator is \sum
    let numerator (normalized_spatial_weight * (number_of_caribou_on_cell + number_of_caribou_on_neighbors))
    let denominator ( numerator + 1 )

    ;; compute new conspecific attractiveness value at current location
    set new_conspecific_attractiveness_value ( ( intensity_of_conspecific_attraction * numerator ) / denominator )

    ; update patch information
    set conspecific_attractiveness new_conspecific_attractiveness_value

  ]

end

to neighbors_update_total_expected_attractiveness
    ask neighbors with [is_border = false] [
        set total_expected_attractiveness (expected_consumption_rate + conspecific_attractiveness)
    ]

end

to update_neighboring_patch_values
      ;; we update all the patch variables regardless of leader or follower status

      neighbors_update_expected_amount_of_food

      ;; update the consumption rate of neighbors, to determine if the caribou moves from the current cell
      neighbors_update_expected_consumption_rate

      ;; update the conspecific attractiveness of neighbors, now that the caribou is on that cell
      neighbors_update_conspecific_attractiveness

      ;; update the total attractiveness of neighbor cells that the caribou is on the cell
      ;; total must always be updated last, it uses expected_consumption_rate and conspecific attractiveness values
      neighbors_update_total_expected_attractiveness

end

;;;************************************************************************************************************************************;;;
;;;******************************************************** PATCH FUNCTIONS END *******************************************************;;;
;;;************************************************************************************************************************************;;;

;;;************************************************************************************************************************************;;;
;;;******************************************************** HELPER FUNCTIONS *********************************************************;;;
;;;************************************************************************************************************************************;;;
;; These functions either help with colouring patches, colouring borders, setting values at borders, etc
;; anything that may be needed to support the setup and execution of the simulation but isn't described in the paper goes in this section

to show_caribou_count_on_patches
  ask patches with [is_border = false] [
    set plabel (count caribous-here)
    set plabel-color red
  ]

end

to hide_caribou_count_on_patches
  ask patches with [is_border = false] [
    set plabel ""
  ]

end

to hide_all_caribou
  ask caribous [
    hide-turtle
  ]
end

to show_all_caribou
  ask caribous [
    show-turtle
  ]
end


to toggle_leaders_visibility
  ask caribous with [is_leader = true][
    ifelse hidden? = false [
      hide-turtle
    ]
    [
      show-turtle
    ]
  ]
end

to toggle_followers_visibility
  ask caribous with [is_leader = false][
    ifelse hidden? = false [
      hide-turtle
    ]
    [
      show-turtle
    ]
  ]
end

to set_world_border

  ;; we manually do zero padding for our world
  ;; based on kernel size
  ask patches [
    set is_border false
  ]

  if (kernel_range > 0) [
    ;; we manually do zero padding for our world
    ;; based on 3x3 kernel size
    ask patches with [
      (pxcor = max-pxcor) or
      (pxcor = min-pxcor) or
      (pycor = max-pycor) or
      (pycor = min-pycor) ] [
      set is_border true
    ]
  ]
  ;; if we have kernel range 5 or 7 this must trigger
  if (kernel_range > 3) [
    ;; we manually do zero padding for our world
    ;; based on 5x5 kernel size
    ask patches with [
      (pxcor = max-pxcor - 1) or
      (pxcor = min-pxcor + 1) or
      (pycor = max-pycor - 1) or
      (pycor = min-pycor + 1) ] [
      set is_border true
    ]
  ]
  ;; if we have kernel range 7 this must trigger
  if (kernel_range > 5) [

      ;; we manually do zero padding for our world
      ;; based on 5x5 kernel size
      ask patches with [

        (pxcor = max-pxcor - 2) or
        (pxcor = min-pxcor + 2) or
        (pycor = max-pycor - 2) or
        (pycor = min-pycor + 2)] [
        set is_border true
      ]
  ]

end


to colour_grass

  ask patches with [is_border = false] [
    set pcolor (60 - true_amount_of_food)
  ]
end

to create_gaussian_filter
  ;; assume kernel range is 5 and we change if that is not the case
  let kernel [[-2 -1 0 1 2] [-2 -1 0 1 2] [-2 -1 0 1 2] [-2 -1 0 1 2] [-2 -1 0 1 2]]
  let vertical_kernel matrix:from-row-list kernel
  let vertical_kernel_squared matrix:times-element-wise vertical_kernel vertical_kernel

  ;; horizontal kernel
  set kernel [[-2 -2 -2 -2 -2] [-1 -1 -1 -1 -1] [0 0 0 0 0] [1 1 1 1 1] [2 2 2 2 2]]
  let horizontal_kernel matrix:from-row-list kernel
  let horizontal_kernel_squared matrix:times-element-wise horizontal_kernel horizontal_kernel

  if kernel_range = 3 [
    set kernel [[-1 0 1] [-1 0 1] [-1 0 1]]
    set vertical_kernel matrix:from-row-list kernel
    set vertical_kernel_squared matrix:times-element-wise vertical_kernel vertical_kernel

    ;; horizontal kernel
    set kernel [ [-1 -1 -1] [0 0 0] [1 1 1] ]
    set horizontal_kernel matrix:from-row-list kernel
    set horizontal_kernel_squared matrix:times-element-wise horizontal_kernel horizontal_kernel

  ]
  if kernel_range = 7 [
    set kernel [[-3 -2 -1 0 1 2 3] [-3 -2 -1 0 1 2 3] [-3 -2 -1 0 1 2 3] [-3 -2 -1 0 1 2 3] [-3 -2 -1 0 1 2 3] [-3 -2 -1 0 1 2 3] [-3 -2 -1 0 1 2 3]]
    set vertical_kernel matrix:from-row-list kernel
    set vertical_kernel_squared matrix:times-element-wise vertical_kernel vertical_kernel

    ;; horizontal kernel
    set kernel [[-3 -3 -3 -3 -3 -3 -3] [-2 -2 -2 -2 -2 -2 -2] [-1 -1 -1 -1 -1 -1 -1] [0 0 0 0 0 0 0] [1 1 1 1 1 1 1] [2 2 2 2 2 2 2] [3 3 3 3 3 3 3]]
    set horizontal_kernel matrix:from-row-list kernel
    set horizontal_kernel_squared matrix:times-element-wise horizontal_kernel horizontal_kernel

  ]

  ;; create exponent matrix
  let numerator matrix:plus horizontal_kernel_squared vertical_kernel_squared
  set numerator matrix:times-scalar numerator -1
  let denominator 2 * kernel_range * kernel_range
  let exponent matrix:times-scalar numerator  (1 / denominator )

  ;; compute exp( exponent matrix )
  set gaussian_filter matrix:map exp exponent

  ;; get normalize factor by summing all values in matrix
  let row_list matrix:to-row-list gaussian_filter
  let row_sums []
  ; compute row sums
  foreach row_list [
    row ->
    set row_sums lput (sum row) (row_sums)
  ]

  ;; compute sum of entire matrix
  let matrix_sum (sum row_sums)

  ;; produce gaussian filter
  set gaussian_filter matrix:times-scalar gaussian_filter  (1 / matrix_sum )
end

to padding

  ask patches with [ is_border = true ] [
    set true_amount_of_food 0
    set is_border true
  ]
end

to smooth_true_amount_of_food
  ; required to use gis:patch-dataset
  gis:set-transformation (list min-pxcor max-pxcor min-pycor max-pycor) (list min-pxcor max-pxcor min-pycor max-pycor)

  ; convert data stored in true_amount_of_food to raster dataset for gis
  let dataset gis:patch-dataset true_amount_of_food

  create_gaussian_filter

  ;; convert the nested list into a single list
  let row_list matrix:to-row-list gaussian_filter
  let kernel []
  foreach row_list [
    row -> foreach row [
      value ->
      set kernel lput value kernel
    ]
  ]
  let convoled_true_food 0


  (ifelse kernel_range = 3 [
    ;; for a 3x3 kernel the pixels at the border result in NaN
    ;; values after gis:convolve is applied. As a work around
    ;; we pad the world before hand with 0 around the border

    set convoled_true_food gis:convolve dataset 3 3 kernel 1 1
    ]
    kernel_range = 5 [
      ; for a 5x5 kernel padding around the border needs
      ;; to be 2 pixels wide on each side.
      ;; the pixels at the border result in NaN
      ;; values after gis:convolve is applied. As a work around
      ;; we pad the world before hand with 0 around the border
      set convoled_true_food gis:convolve dataset 5 5 kernel 2 2
    ]
    ; else kernel_range = 7 [
    [;; for a 7x7 kernel padding around the border needs
     ;; to be 3 pixels wide on each side.
     ;; the pixels at the border result in NaN
     ;; values after gis:convolve is applied. As a work around
     ;; we pad the world before hand with 0 around the border
      set convoled_true_food gis:convolve dataset 7 7 kernel 3 3
  ])

  ;; return rastered dataset to patch variable
  gis:apply-raster convoled_true_food true_amount_of_food

end
@#$#@#$#@
GRAPHICS-WINDOW
257
10
832
586
-1
-1
7.0
1
13
1
1
1
0
0
0
1
0
80
0
80
0
0
1
ticks
30.0

BUTTON
17
16
83
49
Setup
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
15
67
96
100
NIL
relocate
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
14
107
152
152
kernel_range
kernel_range
3 5 7
1

CHOOSER
14
161
152
206
theta
theta
0 0.5 1 1.5 2
2

SLIDER
14
216
194
249
interference_rate
interference_rate
0.1
1
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
14
305
195
338
density
density
0
5
0.4
0.1
1
NIL
HORIZONTAL

INPUTBOX
11
522
172
582
MAX_TICKS
8760.0
1
0
Number

BUTTON
109
66
194
99
NIL
relocate
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
260
242
293
intensity_of_conspecific_attraction
intensity_of_conspecific_attraction
0
1
1.0
0.1
1
NIL
HORIZONTAL

BUTTON
853
47
1084
80
Show caribou count on patches
show_caribou_count_on_patches\nhide_all_caribou
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
855
106
991
139
Show all caribou
show_all_caribou
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
854
171
1080
204
Hide caribou count on patches
hide_caribou_count_on_patches\nshow_all_caribou
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
855
232
987
265
Hide all caribou
hide_all_caribou
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
856
294
1015
327
Show/Hide Leaders 
toggle_leaders_visibility
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
857
346
1022
379
Show/Hide Followers
toggle_followers_visibility
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
349
187
382
attack_rate
attack_rate
0.5
1.5
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
13
399
185
432
the_percentage
the_percentage
0
1
0.2
0.1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

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
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment1" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>relocate</go>
    <postRun>post_run</postRun>
    <metric>count turtles</metric>
    <metric>min [current_expected_consumption_rate] of turtles</metric>
    <metric>max [current_expected_consumption_rate] of turtles</metric>
    <metric>mean [current_expected_consumption_rate] of turtles</metric>
    <metric>min [total_food_consumed] of turtles</metric>
    <metric>max [total_food_consumed] of turtles</metric>
    <metric>mean [total_food_consumed] of turtles</metric>
    <metric>mean [residence_time] of turtles</metric>
    <enumeratedValueSet variable="density">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interference_rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="intensity_of_conspecific_attraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MAX_TICKS">
      <value value="8760"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="theta">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kernel_range">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="low density experiment" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>relocate</go>
    <postRun>post_run</postRun>
    <metric>count turtles</metric>
    <metric>min [current_expected_consumption_rate] of turtles</metric>
    <metric>max [current_expected_consumption_rate] of turtles</metric>
    <metric>mean [current_expected_consumption_rate] of turtles</metric>
    <metric>min [total_food_consumed] of turtles</metric>
    <metric>max [total_food_consumed] of turtles</metric>
    <metric>mean [total_food_consumed] of turtles</metric>
    <metric>mean [residence_time] of turtles</metric>
    <enumeratedValueSet variable="density">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interference_rate">
      <value value="0.1"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="intensity_of_conspecific_attraction">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MAX_TICKS">
      <value value="8760"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="theta">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kernel_range">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="medium density experiment" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>relocate</go>
    <postRun>post_run</postRun>
    <metric>count turtles</metric>
    <metric>min [current_expected_consumption_rate] of turtles</metric>
    <metric>max [current_expected_consumption_rate] of turtles</metric>
    <metric>mean [current_expected_consumption_rate] of turtles</metric>
    <metric>min [total_food_consumed] of turtles</metric>
    <metric>max [total_food_consumed] of turtles</metric>
    <metric>mean [total_food_consumed] of turtles</metric>
    <metric>mean [residence_time] of turtles</metric>
    <enumeratedValueSet variable="density">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interference_rate">
      <value value="0.1"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="intensity_of_conspecific_attraction">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MAX_TICKS">
      <value value="8760"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="theta">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kernel_range">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="high density experiment" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>relocate</go>
    <postRun>post_run</postRun>
    <metric>count turtles</metric>
    <metric>min [current_expected_consumption_rate] of turtles</metric>
    <metric>max [current_expected_consumption_rate] of turtles</metric>
    <metric>mean [current_expected_consumption_rate] of turtles</metric>
    <metric>min [total_food_consumed] of turtles</metric>
    <metric>max [total_food_consumed] of turtles</metric>
    <metric>mean [total_food_consumed] of turtles</metric>
    <metric>mean [residence_time] of turtles</metric>
    <enumeratedValueSet variable="density">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interference_rate">
      <value value="0.1"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="intensity_of_conspecific_attraction">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MAX_TICKS">
      <value value="8760"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="theta">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kernel_range">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="full experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>relocate</go>
    <postRun>post_run</postRun>
    <metric>count turtles</metric>
    <metric>min [current_expected_consumption_rate] of turtles</metric>
    <metric>max [current_expected_consumption_rate] of turtles</metric>
    <metric>mean [current_expected_consumption_rate] of turtles</metric>
    <metric>min [total_food_consumed] of turtles</metric>
    <metric>max [total_food_consumed] of turtles</metric>
    <metric>mean [total_food_consumed] of turtles</metric>
    <metric>mean [residence_time] of turtles</metric>
    <enumeratedValueSet variable="density">
      <value value="0.4"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interference_rate">
      <value value="0.1"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="intensity_of_conspecific_attraction">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MAX_TICKS">
      <value value="8760"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="theta">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kernel_range">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="FogoSensitivityModel" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>relocate</go>
    <postRun>post_run</postRun>
    <metric>count turtles</metric>
    <metric>min [current_expected_consumption_rate] of turtles</metric>
    <metric>max [current_expected_consumption_rate] of turtles</metric>
    <metric>mean [current_expected_consumption_rate] of turtles</metric>
    <metric>min [total_food_consumed] of turtles</metric>
    <metric>max [total_food_consumed] of turtles</metric>
    <metric>mean [total_food_consumed] of turtles</metric>
    <metric>mean [residence_time] of turtles</metric>
    <enumeratedValueSet variable="density">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interference_rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="intensity_of_conspecific_attraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MAX_TICKS">
      <value value="8760"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="theta">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kernel_range">
      <value value="5"/>
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
