breed [houses house]
breed [people person]
breed [facilities facility]
breed [neighbourhoods neighbourhood]
breed [regions region]
breed [giniLabels giniLabel]
globals [downpayment maxDTI leavingCity inflation overalInflation cityGini goodNeighbourhoods moderateNeighbourhoods badNeighbourhoods lowIncome mediumIncome highIncome topIncome originalCPI CPIchange sellingPeriod governmentUpgrade satisfaction incomeProportion]


houses-own [type-of-house lifetime price nbhdQ locQ interestedBuyers owner forSale initialPrice]
people-own[income isOwner savings savingsRate interested-houses nbhdQ-weight locQ-weight composite-weight primaryBid secondaryBid wantsToSell canSell waitingTime mpi outstandingLoan maturity experienced likelihoodToSell]
neighbourhoods-own [id nhbq crimeRate giniCoefficient meanIncome]
regions-own [id giniCoefficient listOfNeighbourhoods locQuality]


;The city is set up with the initial neighborhoods, regions and houses with their prices, global variables are initialised
to setup
  clear-all
  setupNeighbourhoods
  setupregions
  ask regions [ht]
  ask patches [
    set pcolor 37
    ifelse random 100 < 30 [
      sprout-houses 1[
        set color white
        set shape "house"
        set type-of-house decideTypeOfHouse
        set lifetime 0
        set owner nobody
        set forSale true
        set nbhdQ 0.01
        set locQ getlocQ self
        set interestedBuyers []
        set price 0
        set initialPrice 0
      ]
    ][
      if random 100 < 5 [
        sprout-facilities 1[
          set color violet
          set shape "square"
        ]
      ]
    ]

  ]
  set goodNeighbourhoods 0
  set moderateNeighbourhoods 0
  set badNeighbourhoods 0
  set downpayment 0.1
  set cityGini 0
  set satisfaction 0
  set maxDTI 0.5
  set leavingCity 0
  set inflation 0
  set overalInflation 1
  set lowIncome 20000
  set mediumIncome 40000
  set highIncome 60000
  set topIncome 80000
  set incomeProportion [0 0 0 0]
  set sellingPeriod 0
  set governmentUpgrade 1
  assign_initial_price

  set originalCPI mean [price] of houses
  set CPIchange 1.0
  initial_investors 400
  reset-ticks


end

;Given an house it returns the neighbourhood quality for that house based on the neighbourhood it resides in
to-report getNbhdQ [theHouse]
  let belonged  neighbourhoods with [[xcor] of theHouse < xcor + 6 and [xcor] of theHouse >= xcor and [ycor] of theHouse > ycor -  6 and [ycor] of theHouse <= ycor]
  ifelse count belonged = 0 [
    report 0
  ][
    report [nhbq] of one-of belonged
  ]
end

;Given an house it reurns the location quality ofr that house based on the region it resides in.
to-report getlocQ [theHouse]
  let belonged  regions with [[xcor] of theHouse < xcor + 12 and [xcor] of theHouse >= xcor and [ycor] of theHouse > ycor -  12 and [ycor] of theHouse <= ycor]
  ifelse count belonged = 0 [
    report 0
  ][
    report [locQuality] of one-of belonged
  ]
end

;the go procedure managers the timeing of the events that take place in te vrtual city,
; every tick the lifetime of the house is updated and checks if outstanding loan for the house as reached the its contract maturity
;when tick is even the potential buyers make their bids, when tick is odd real estate agent decided the outcome of te bid, waiting time is updated for the potential buyers and then new buyers enter the city
;Every month (which corresponds to 10 ticks) the owners information is updated, they make a decison whether to sell, price decreased for houses that are for sale based on time on the market
;Every year new houses are added to the city, neighbourhood quality is updated, inflation rate for a year is generated which affcect the prices of the houses, and income band
;at the end of the simulation after 8 years gini coefficient is calculated and then stopped
;
to go
  ;initial_investors
  if ticks = 1 and  crimeIncreaseProb + crimeDecreaseProb > 100 [
    set crimeIncreaseProb 60
    set crimeDecreaseProb 30
  ]


  ifelse ticks <= 359[
    houses_eligibility
    realtorsReview
    if ticks = 359 [
      ;removeWaiting

      calculate_nhbq
    ]
  ][
    if ticks mod 480 = 0 [
      set governmentUpgrade governmentUpgrade + 0.02
      updateIncomeBand
    ]
    if ticks mod 120 = 0[
      calculate_nhbq
      updateInflationRate
      impactOfInflation
      ;updateIncomeBand
    ]

    if ticks < 840 and ticks mod 60 = 0 [
      newHouses
    ]
    ifelse ticks mod 60 = 0 [
      set sellingPeriod ticks + 10
      decreaseSalePrice
      removeWaiting
      ;newHouses
      initial_investors 60
    ][
      ifelse ticks < sellingPeriod[
        selling_houses
        ][
        houses_eligibility
        realtorsReview
      ]
    ]





    if ticks = 1320 [
      updateGini
      classificationOfNeighbourhoods
      calculateSatisfaction
      %_per_incomeBand
      stop
      ]
    ]
  updateHouseInfo





  tick
end


to removeWaiting
  ask people with [isOwner = false][
    set leavingCity leavingCity + 1
    die


  ]

end

;sets up the regions for the virtual city, the are and assign a locaqtion quality which would reain the same throughou the simulation
to setupRegions
  let x min-pxcor
  let y max-pxcor
  let num 0
  let ids ["NW" "N" "NE" "W" "C" "E" "SW" "S" "SE"]
  loop [
    if y < min-pycor + 6 [stop]
    while [ x <= (max-pxcor - 12)][
      create-regions 1 [
        set color red
        ;set heading 90
        ;set hidden? true
        set shape "line half"
        set label (item num ids)
        set xcor x
        set ycor y
        set size 0.5
        draw-square 12
        ;set xcor x + 3
        ;set ycor y - 3
        set label-color black
        set heading 90
        set size 2
        set pen-size 10
        set id label
        ;set xcor x + 1
        set giniCoefficient 0
        set locQuality precision ((random-float 0.5) + 0.3) 2
        set listOfNeighbourhoods neighbourhoods with [xcor < [xcor] of myself + 12 and xcor >= [xcor] of myself and ycor > [ycor] of myself -  12 and ycor <= [ycor] of myself]
        set xcor x + 1
      ]
      set x x + 12
      set num num + 1
    ]
    set x min-pxcor
    set y y - 12
  ]
end

;sets up the neighbourhood for the virtual city
to setupNeighbourhoods
  let x min-pxcor
  let y max-pxcor
  let num 0
  loop [
    if y < min-pycor + 3 [stop]
    while [ x <= (max-pxcor - 6)][
      create-neighbourhoods 1 [
        set color red
        ;set heading 90
        ;set hidden? true
        set shape "line half"
        set label word "N" num
        set xcor x
        set ycor y
        set size 0.5
        draw-square 6
        ;set xcor x + 3
        ;set ycor y - 3
        set label-color black
        set heading 90
        set size 2
        set pen-size 10
        set id label
        set nhbq 0.01
        ;set xcor x + 1
        set crimeRate assignInitialCrimeRate
        set giniCoefficient 0
        set meanIncome 0
      ]
      set x x + 6
      set num num + 1
    ]
    set x min-pxcor
    set y y - 6
  ]
end

;draws a square in the grid to show an area of territory
to draw-square [radius]    ;; observer procedure
;; draw the circumference of a circle at the given radius
  hatch 1 [
    let ini xcor
    set pen-size 1 set color red ;set heading -90 fd radius
    set heading 0
    set pen-size 2
    pendown
    fd (-1 * radius)
    rt 90
    fd radius
    rt -90
    fd radius
    rt -90
    fd radius

    ;set head
    ;while [xcor <= ini + radius ] [ fd 1 ]
    die
   ]
end


;assigns prices for houses based on the type of house
to assign_initial_price
  ask houses with [type-of-house = "Detached" and price = 0] [set price (342800 + random 14000) * overalInflation set initialPrice price]
  ask houses with [type-of-house = "Semi-detached" and price = 0] [set price (212600 + random 8700) * overalInflation set initialPrice price]
  ask houses with [type-of-house = "Terraced" and price = 0] [set price (182900 + random 7400) * overalInflation set initialPrice price]
  ask houses with [type-of-house = "flat/maisonette" and price = 0][set price (195000 + random 8000) * overalInflation set initialPrice price]
 ; ask detached [set price 342800 + random 14000]
 ; ask semi-detached [set price 212600 + random 8700]
 ; ask terraced [set price 182900 + random 7400 ]
 ; ask flats[set price 195000 + random 8000 ]
end

;assign savings for potential homeowners, taking in consideration the CPI
to-report assign_savings [thePerson]
  ifelse [income] of thePerson = lowIncome [
    report precision ((governmentUpgrade * (15000 + random 10000)) / overalInflation) 2
  ][
    ifelse [income] of thePerson = mediumIncome [
      report precision ((governmentUpgrade * (25000 + random 7000)) / overalInflation) 2
      ][
      ifelse [income] of thePerson = highIncome [
        report precision ((governmentUpgrade * (32000 + random 8000)) / overalInflation) 2
      ][
        report precision ((governmentUpgrade * (40000 + random 5000))/ overalInflation) 2
      ]
    ]
  ]
  ;ask people with [income = 20000] [set savings 15000 + random 10000]
  ;ask people with [income = 40000] [set savings 25000 + random 7000]
  ;ask people with [income = 60000] [set savings 32000 + random 8000]
  ;ask people with [income = 80000] [set savings 40000 + random 5000]

end

;assigns income for homeowners based on microdata that was collected
to-report assign_income
  let num random-float 1
  ifelse num < 0.25 [
    report lowIncome
  ][
    ifelse num < 0.74[
      report mediumIncome
    ][
      ifelse num < 0.94[
        report highIncome
      ][
        report topIncome
      ]
    ]
  ]
end

;assign colour to represent different income bands
to-report assignColour [the_income]
  ifelse the_income = lowIncome[
    report red
  ][
    ifelse the_income = mediumIncome[
      report yellow
    ][
      ifelse the_income = highIncome[
        report 56
      ][
        report 62
      ]
    ]
  ]

end

;assigns preference rating  toindicate level of imprtance for each homeowner regarding neighbourhood quality, location quality and composite. rating from 1 to 5 where 1 indicates really important and 5 insignificant factor
to-report assignPreference [the_income]
  let nbhdQ-weights 1 + random 4
  let locQ-weights 1 + random 4
  let composite-weights 1 + random 4
  ;let final (list nbhdQ-weights locQ-weights composite-weights)
  if the_income = highIncome[
      set locQ-weights 1 + random 3
    ]
  if the_income = topIncome[
    set locQ-weights 1 + random 2
    set nbhdQ-weights 1 + random 1
    ]
  report (list nbhdQ-weights locQ-weights composite-weights)


end

;real estate checks transaction for each house where there is a level of interest from potential homeowners
;if only one person is interested in the house the primary bid is taken from that person as the purchasing price and it becoms the owner
;if multiple people interested in the house the person with the highest secndary bid becomes the owner, and that bid is the price of the house
;if an  house is bought from another person who put the huse for sale the money from the sale is give to them
to realtorsReview
  ask houses with [interestedBuyers  != []][
    let oldOwner owner

    if owner != nobody [
      ask owner [
        if canSell = true [
          ifelse random-float 1 < 0.05 [
            die
            set leavingCity leavingCity + 1
          ][
            move-to patch 0 0
            set interested-houses []
            set wantsToSell false
            set isOwner false
            set canSell false
            set waitingTime 0
            set experienced true
          ]
        ]


      ]
    ]
    let theHouse self
    let thePatch patch pxcor pycor
    let thePrice price
    ifelse length interestedBuyers = 1 [
      let thePerson first interestedBuyers
      ask thePerson [
        set isOwner true
        move-to thePatch
        set savings round (savings - ( thePrice * downpayment ))
        set interested-houses []
        set interested-houses insert-item 0 [] theHouse
        ifelse experienced = false [
          let monthlyInterestRate  interestRate / 1200
          set mpi  ((thePrice * (1 - downpayment)) * (monthlyInterestRate * (1 + monthlyInterestRate) ^ 360 ) / (((1 + monthlyInterestRate) ^ 360 ) - 1))
          set maturity 360
          set outstandingLoan thePrice * (1 - downpayment)
        ][
          set canSell true
        ]

      ]
     set owner thePerson
    ][
      let orderedBuyers sort-by [ [person1 person2] -> [secondaryBid] of person1 > [secondaryBid] of person2 ] interestedBuyers
      foreach interestedBuyers [x -> ask x [
        set interested-houses remove-item 0 interested-houses
      ]]
      let thePerson first interestedBuyers
      ask thePerson [
        move-to thePatch
        set isOwner true
        set savings round(savings - (secondaryBid * downpayment ))
        set thePrice secondaryBid
        set interested-houses insert-item 0 [] theHouse
        ifelse experienced = false [
          let monthlyInterestRate  interestRate / 1200
          set mpi  ((thePrice * (1 - downpayment)) * (monthlyInterestRate * (1 + monthlyInterestRate) ^ 360 ) / (((1 + monthlyInterestRate) ^ 360 ) - 1))
          set maturity 360
          set outstandingLoan thePrice * (1 - downpayment)
        ][
          set canSell true
        ]

      ]
      set owner thePerson
    ]
    set interestedBuyers []
    set forSale false
    set price thePrice
    set initialPrice thePrice
    if oldOwner != nobody [

      ask oldOwner [
        if experienced = true [
          set savings savings + thePrice

        ]

      ]
    ]

    set color black
  ]

end

;decide wha type of house an house would be based on a probability
to-report decideTypeOfHouse
  let typeProbability random-float 1
  ifelse typeProbability < 0.18 [
    report "Detached"
  ][
    ifelse typeProbability < 0.45 [
      report "Semi-detached"
    ][
      ifelse typeProbability < 0.74 [
        report "Terraced"
      ][
        report "flat/maisonette"
      ]
    ]
  ]

end

;update the waiting time for people that are looking for an house but they havent got one yet, if the waiting time gets to to 3 months (30 ticks) they exit the market
to updateWaitingtime
  ask people with [isOwner = false][
    set waitingTime waitingTime + 2
    if waitingTime > 29 [
      set leavingCity leavingCity + 1
      die
    ]
  ]

end

;show the people leaving the city without finding an house
to-report  showLeaving
  report leavingCity
end

;calculated and stores the satisfaction level in the city, which is the the percentage of people who are not looking to sell their houses
to calculateSatisfaction
  let happy count people with [isOwner = true and wantsToSell = false]
  let all count people with [isOwner = true]
  set satisfaction ((happy / all) * 100)
end

;returns the percentage of owners in the city that are part of the lowest income band.
to-report %_of_low
  report (item 0 incomeProportion )
end

;returns the percentage of owners in the city that are part of the medium income band
to-report %_of_medium
  report (item 1 incomeProportion )
end

;returns the percentage of owners in the city that are part of the high-income band
to-report %_of_high
  report (item 2 incomeProportion )
end

;returns the percentage of owners in the city that are part of the top income band.
to-report %_of_top
  report (item 3 incomeProportion )
end

;calculates the percentage of owners for each income band
to %_per_incomeBand
  let all count people with [isOwner = true]
  let low ((count people with [isOwner = true and income = lowIncome]) / all) * 100
  let medium ((count people with [isOwner = true and income = mediumIncome]) / all) * 100
  let high ((count people with [isOwner = true and income = highIncome]) / all) * 100
  let top ((count people with [isOwner = true and income = topIncome]) / all) * 100
  set incomeProportion (list low medium high top)

end

;returns the percentage of people who are not looking to sell their houses.
to-report showSatisfaction
  report satisfaction
end


;create people that would partecipate in the market initialising their properies
to initial_investors [numOfPeople]
  create-people numOfPeople [
    let preferences assignPreference income
    set income assign_income
    set size 0.5
    set color assignColour income
    set shape "circle"
    set nbhdQ-weight item 0 preferences
    set locQ-weight item 1 preferences
    set composite-weight item 2 preferences
    set interested-houses []
    set mpi 0
    set maturity 0
    set experienced false
    set outstandingLoan 0
    set canSell false
    set wantsToSell false
    set isOwner false
    set waitingTime 0
    set likelihoodToSell 0.005
    set savings assign_savings self
    set savingsRate precision random-float 0.3 2
  ]


end


;for each person find out the list of afforable houses, sort them based on preference and submit primary and secondary bids
to houses_eligibility
  ask people with [isOwner = false] [
    let the_savings savings
    let the_income income
    let weight1 nbhdQ-weight
    let weight2 locQ-weight
    let weight3 composite-weight
    let interested interested-houses
    if length interested = 0 [
      ifelse experienced = true [
        let deposit the_savings * 0.9
        set interested houses with [ forSale = true and deposit > price]

      ][
        let monthlyInterestRate  interestRate / 1200
        set interested houses with [ forSale = true and price * downpayment < the_savings and ((price * (1 - downpayment)) * (monthlyInterestRate * (1 + monthlyInterestRate) ^ 360 ) / (((1 + monthlyInterestRate) ^ 360 ) - 1)) / (the_income / 12)  <= maxDTI]
      ]

      ifelse count interested >= 2 [
        ifelse experienced = true [
          let deposit the_savings * 0.9
          set interested-houses sort-by [ [house1 house2] -> [nbhdQ] of house1 ^ weight1 * [locQ] of house1 ^ weight2 * (1 - ([price] of house1) / deposit) ^ weight3 > [nbhdQ] of house2 ^ weight1 * [locQ] of house2 ^ weight2 *(1 - ([price] of house2 / deposit)) ^ weight3 ] interested
        ][
          let monthlyInterestRate  interestRate / 1200
          set interested-houses sort-by [ [house1 house2] -> [nbhdQ] of house1 ^ weight1 * [locQ] of house1 ^ weight2 * (1 - ((([price] of house1 * (1 - downpayment)) * (monthlyInterestRate * (1 + monthlyInterestRate) ^ 360 ) / (((1 + monthlyInterestRate) ^ 360 ) - 1)) / (the_income / 12))) ^ weight3 > [nbhdQ] of house2 ^ weight1 * [locQ] of house2 ^ weight2 * ( 1 - (([price] of house2 * (1 - downpayment)) * (monthlyInterestRate * (1 + monthlyInterestRate) ^ 360 ) / (((1 + monthlyInterestRate) ^ 360 ) - 1)) / (the_income / 12)) ^ weight3  ] interested
        ]
      ][
        ifelse count interested = 1 [
          set interested-houses insert-item 0 [] one-of interested
        ][
          set interested-houses []
        ]
      ]
    ]

    set interested-houses filter [s -> [forSale] of s = true] interested-houses
    ifelse length interested-houses >= 2 [
      set primaryBid [price] of item 0 interested-houses
      let house1 item 0 interested-houses
      ask house1 [
        set interestedBuyers insert-item 0 interestedBuyers myself
      ]
      let house2 item 1 interested-houses
      let pointFound false
      let increaseBid  1.005
      ifelse experienced = true [
        while [ pointFound = false ] [
          let deposit the_savings * 0.9
          ifelse [nbhdQ] of house1 ^ weight1 * [locQ] of house1 ^ weight2 * (1 - ([price] of house1) / deposit) ^ weight3 <= [nbhdQ] of house2 ^ weight1 * [locQ] of house2 ^ weight2 * (1 - ([price] of house2 / deposit)) ^ weight3  or increaseBid >= 1.15 [
            set pointFound true
          ][
            set increaseBid increaseBid + 0.005
          ]
        ]
      ][
        while [ pointFound = false ] [
          let monthlyInterestRate  interestRate / 1200
          ifelse [nbhdQ] of house1 ^ weight1 * [locQ] of house1 ^ weight2 * (1 - ((([price] of house1 * (1 - downpayment)) * (monthlyInterestRate * (1 + monthlyInterestRate) ^ 360 ) / (((1 + monthlyInterestRate) ^ 360 ) - 1)) / (the_income / 12))) ^ weight3 <= [nbhdQ] of house2 ^ weight1 * [locQ] of house2 ^ weight2 * ( 1 - ((([price] of house2 * (1 - downpayment)) * (monthlyInterestRate * (1 + monthlyInterestRate) ^ 360 ) / (((1 + monthlyInterestRate) ^ 360 ) - 1)) / (the_income / 12))) ^ weight3 or increaseBid >= 1.15 [
            set pointFound true
          ][
            set increaseBid increaseBid + 0.005
          ]
        ]
      ]

      set secondaryBid round( primaryBid * precision increaseBid 3)
    ][
      ifelse length interested-houses = 1 [
        ;set interested-houses insert-item 0 one-of interested []
        ask first interested-houses  [
          set interestedBuyers insert-item 0 interestedBuyers myself
        ]
        set primaryBid [price] of item 0 interested-houses
        set secondaryBid round( primaryBid * 1.025)
      ][
        set primaryBid 0
        set secondaryBid 0

      ]

    ]

  ]


end

;if an homeowner can sell the house baed on a probability they decide whther to put the house for sale
to selling_houses
  ask people with [isOwner = true and canSell = true and likelihoodToSell > 0][
    if random-float 1 < likelihoodToSell [
      set wantsToSell true
      ask first interested-houses [
        set color white
        set forSale true
        set initialPrice initialPrice  * ( 1 + (random 5 / 100))
        set price initialPrice
      ]

    ]
  ]

end


;decreases the price of the houses if they are not sold ever month, deducting 3 % of the initial price each time
to decreaseSalePrice
  ask houses with [forSale = True][
    if lifetime mod 30 = 0 [
      if price > initialPrice * 0.7 [
        set price round (price - (initialPrice * 0.03))
      ]

    ]
  ]
end

;updates information about the house and check whether they have paid their mortgage
to updateHouseInfo
  ask houses[
    set lifetime lifetime + 1
    if owner != nobody [
      ask owner [
        if experienced = false [
          ifelse maturity > 0 [
            set maturity maturity - 1
            if maturity mod 10 = 0 [
              set outstandingLoan (1 + interestRate / 120) * (outstandingLoan - mpi)
            ]
          ][
            set outstandingLoan 0
            set canSell true
          ]

        ]




      ]
    ]
  ]
end

;while owning an house their savings increaes and model the possibility of homeowners to move to a different income band
to updateOwnersinfo
  ask people with [isOwner = True ][
    set savings round(savings + ( savingsRate * (income / 12)))
    if income <= lowIncome and random-float 1 < 0.03 [
      set income mediumIncome
      set color yellow
    ]
    if income > lowIncome and income <= mediumIncome and random-float 1 < 0.01 [
      set income highIncome
      set color 56
    ]
    if income > mediumIncome and income <= highIncome and random-float 1 < 0.001 [
      set income topIncome
      set color 62
    ]


  ]

end

;genrates new houses, that are put for sale in the market
to newHouses
  ask patches with [count turtles-here = 0 ][
    ifelse random 100 < 10 [
      sprout-houses 1[
        set color white
        set shape "house"
        set type-of-house decideTypeOfHouse
        set lifetime 0
        set owner nobody
        set forSale true
        set nbhdQ getNbhdQ self
        set locQ getlocQ self
        set interestedBuyers []
        set price 0
        set initialPrice 0
      ]
    ][
      if random 100 < facilitiesConstructionProb [
        sprout-facilities 1[
          set color violet
          set shape "square"
        ]
      ]
    ]
  ]
  assign_initial_price

end


;houses are adjusted to inflation, savings rate are adjusted as well.
to impactOfInflation
  ask houses [
    set price round ( price * (1 + inflation / 100 ))
    set initialPrice round (initialPrice * (1 + inflation / 100 ))
  ]

  ask people [
    set savingsRate savingsRate * ( 1 - inflation / 100 )
  ]



end

;inflation rate for a given year is generated
to updateInflationRate
  let inflationRange random-float 1
  ifelse inflationRange < 0.25[
    set inflation precision random-float (averageInflationRate * 5 / 9) 1
  ][
    ifelse inflationRange < 0.5[
      set inflation precision ( random-float ( averageInflationRate * 4 / 9 ) + (averageInflationRate * 5 / 9)) 1
    ][
      ifelse inflationRange < 0.75[
        set inflation precision (random-float ( averageInflationRate / 11 ) + (averageInflationRate )) 1
      ][
        ifelse inflationRange < 0.98[
          set inflation precision (random-float ( averageInflationRate * 10 / 11 ) + (averageInflationRate * 12 / 11)) 1
        ][
          set inflation precision (random-float ( averageInflationRate * 7 / 4 ) + (averageInflationRate * 2)) 1
        ]
      ]
    ]
  ]
  set overalInflation overalInflation * (1 + inflation / 100)
end

;income band is updatedd according to the CPI
to updateIncomeBand
  let lowIncomeBand round (20000 * governmentUpgrade)
  ask people with [income = lowIncome][
    set income lowIncomeBand
  ]
  set lowIncome lowIncomeBand
  let mediumIncomeBand round (40000 * governmentUpgrade)
  ask people with [income = mediumIncome][
    set income mediumIncomeBand
  ]
  set mediumIncome mediumIncomeBand
  let highIncomeBand round (60000 * governmentUpgrade)
  ask people with [income = highIncome][
    set income highIncomeBand
  ]
  set highIncome highIncomeBand
  let topIncomeBand round (80000 * governmentUpgrade )
 ask people with [income = topIncome][
    set income topIncomeBand
  ]
  set topIncome topIncomeBand
end

;calculates neighbourhood quality based on mean income, facilities rate and crime rate. if it changes it affects directly the price of the house
to calculate_nhbq
  ask neighbourhoods [
    let numOfReseidents 0
    let totalncome 0
    let totalFacilities 0
    ask houses with [owner != nobody and xcor < [xcor] of myself + 6 and xcor >= [xcor] of myself and ycor > [ycor] of myself -  6 and ycor <= [ycor] of myself][
      set numOfReseidents numOfReseidents + 1
      set totalncome totalncome + [income] of owner
    ]

    ifelse numOfReseidents > 0[
      set meanIncome totalncome / numOfReseidents
    ][
      set meanIncome 0
    ]
    let theProbability random 100
    ifelse theProbability < crimeIncreaseProb [
      set crimeRate crimeRate * (1 + crimeRateIncrease)
    ][
      if theProbability < (crimeIncreaseProb + crimeDecreaseProb )[
        set crimeRate crimeRate *  (1 - crimeRateDecrease )
      ]
    ]
  ]

  affluentareas
  let maxIncome [meanIncome] of max-one-of neighbourhoods [meanIncome]

  if maxIncome > 0 [
    ask neighbourhoods [

      let facilitiesRate 0
      if count facilities > 0 [
        set facilitiesRate count facilities with [xcor < [xcor] of myself + 6 and xcor >= [xcor] of myself and ycor > [ycor] of myself -  6 and ycor <= [ycor] of myself] / count facilities
      ]
      set nhbq meanIncome / maxIncome + facilitiesRate -  (2 * crimeRate)
      let newQuality nhbq
      ask houses with [owner != nobody and xcor < [xcor] of myself + 6 and xcor >= [xcor] of myself and ycor > [ycor] of myself -  6 and ycor <= [ycor] of myself][
        let oldQuality nbhdQ
        if ticks > 360 [
          set price price * (1 + (((newQuality - oldQuality) * 5 ) / 100))
          ask owner [
          set likelihoodToSell (likelihoodToSell - ((newQuality - oldQuality) ^ nbhdQ-weight ))
            ]
        ]
        set nbhdQ newQuality
      ]
    ]
  ]




end

;check most affluent areas and based on a probability builds more facilities
to affluentareas
  let theList max-n-of 6 neighbourhoods [meanIncome]
  ask max-n-of 5 neighbourhoods [meanIncome][
    ask patches with [count turtles-here = 0 and pxcor < [xcor] of myself + 6 and pxcor >= [xcor] of myself and pycor > [ycor] of myself -  6 and pycor <= [ycor] of myself][
      if random 100 < facilitiesConstructionProb [
        sprout-facilities 1[
          set color violet
          set shape "square"
          ]
        ]
      ]
    ]
end

;assigns initial crime rate based on micro data
to-report assignInitialCrimeRate
  let theValue random-float 1
  ifelse theValue < 0.25 [
    report precision ((random 40 + 40) / 1000) 3
  ][
    ifelse theValue < 0.5 [
      report precision ((random 25 + 80) / 1000) 3
    ][
      ifelse theValue < 0.75 [
        report precision ((random 4 + 105) / 1000) 3
      ][
        ifelse theValue < 0.98 [
          report precision ((random 41 + 109) / 1000) 3
        ][
          report precision((random 10 + 150) / 1000) 3
        ]
      ]
    ]

  ]


end

;assigns the percentage increase in crime based on micro data
to-report crimeRateIncrease
  let theValue random-float 1
  ifelse theValue < 0.25 [
    report precision (random-float 0.03 + 0.01) 3
  ][
    ifelse theValue < 0.5 [
      report precision (random-float 0.05 + 0.03) 3
    ][
      ifelse theValue < 0.75 [
        report precision (random-float 0.05 + 0.08) 3
      ][
        ifelse theValue < 0.98 [
          report precision (random-float 0.15 + 0.13) 3
        ][
          report precision (random-float 0.02 + 0.28) 3
        ]
      ]
    ]
  ]
end

;assigns the percentage decrease in crime based on micro data
to-report crimeRateDecrease
  let theValue random-float 1
  ifelse theValue < 0.25 [
    report precision (random-float 0.01 + 0.01) 3
  ][
    ifelse theValue < 0.5 [
      report precision (random-float 0.02 + 0.02) 3
    ][
      ifelse theValue < 0.75 [
        report precision (random-float 0.05 + 0.04) 3
      ][
        ifelse theValue < 0.98 [
          report precision (random-float 0.09 + 0.09) 3
        ][
          report precision (random-float 0.07 + 0.18) 3
        ]
      ]
    ]
  ]
end

;return the Gini index fiven a list of values representing the income of the owners
to-report gini-index [ lst ]
;; reports the gini index of the values in the given list
;; Actually returns the gini coefficient (between 0 and 1)
;;  the gini index is a percentage

  let sorted sort lst
  let total sum sorted
  let items length lst
  let sum-so-far 0
  let index 0
  let gini 0
  repeat items [
    set sum-so-far sum-so-far + item index sorted
    set index index + 1
    set gini  gini + (index / items) - (sum-so-far / total)
  ]
  ; only accurate if items is large
  ;show gini / items
  ifelse items = 0 [
    report 0
  ][
    report 2 * (gini / items)
  ]

end


;plots the an histogram based on the income band of a group of owners passed as input.
to plotHistogram [Population]
  set-current-plot "Histogram"
  set-current-plot-pen "ck"
  set-plot-pen-interval (topIncome / 4) + 1000
  set-plot-x-range 0 topIncome + (topIncome / 4)
  histogram [income] of Population
end


;plots the Lorenze curve for a neighbourhood, by considering the income band of the owners present in such neghbourhood.
to plotLorenzeCurve
  set-current-plot "Lorenze plot"
  ;plot-pen-reset
  clear-plot
  set-current-plot-pen "perfect distribution"
  plotxy 0 0
  plotxy 100 100
  ask neighbourhoods with [id = nb1][
    let peopleInNeighbourhood []
    set peopleInNeighbourhood people with [isOwner = true and xcor < [xcor] of myself + 6 and xcor >= [xcor] of myself and ycor > [ycor] of myself -  6 and ycor <= [ycor] of myself]
    set-current-plot-pen "lorenz line of nb1"
    plotTheGraph peopleInNeighbourhood
    plotHistogram peopleInNeighbourhood
  ]
end

;plot the x and y cordinates for the Lorenze curve given a list of the owners in a paricular neighbourhood
to plotTheGraph [thePeople]
  let population count thePeople
  let totalIncome sum [income] of thePeople
  let offset population mod 10
  let interval floor ( population / 10)
  let subPopulation 0
  let x 0
  let y 0
  plotxy 0 0
  while [x < 100 ][
    ifelse offset > 0[
      set subPopulation subPopulation + interval + 1
      set y round (((sum [income] of (min-n-of (subpopulation) thePeople [income])) / totalIncome) * 100)

      set offset offset - 1
    ][
      set subPopulation subPopulation + interval
      set y round (((sum [income] of (min-n-of (subpopulation ) thePeople [income])) / totalIncome) * 100)
    ]

    set x x + 10
    plotxy x y

  ]
end


;calculates the gini coefficient for a neighbourhood, updaiting at the regional and city level as well.
to updateGini
  ask neighbourhoods [
    let peopleInNeighbourhood people with [isOwner = true and xcor < [xcor] of myself + 6 and xcor >= [xcor] of myself and ycor > [ycor] of myself -  6 and ycor <= [ycor] of myself]
    set giniCoefficient precision (gini-index [income] of peopleInNeighbourhood) 2
  ]
  ask regions[
    set giniCoefficient precision ((sum [giniCoefficient] of listOfNeighbourhoods) / 4 ) 2
  ]
  set cityGini precision ((sum [giniCoefficient] of regions) / 9 ) 2
end


;Display the view of the city showing the houses , owners, faclities hiding Gini coefficent values.
to seeCityView
  ask houses [st]
  ask people [st]
  ask facilities [st]
  ask giniLabels [ht]

end
;return the gini coefficient at the city level
to-report showCityGini
  report cityGini
end

;classifies neighbourhood based on the gini coefficient, into thre main categories good neighbourhoods, moderate neighbourhoods and bad neighbourhoods.
to classificationOfNeighbourhoods
  set goodNeighbourhoods neighbourhoods with [giniCoefficient < 0.1]
  set moderateNeighbourhoods neighbourhoods with [giniCoefficient >= 0.1 and giniCoefficient < 0.2]
  set badNeighbourhoods neighbourhoods with [giniCoefficient >= 0.2]
end

;display regional view of the city, showing the regions with their corresponding Gini index alongside the neighbouroods that belong to each region
to seeGiniOfRegions
  ask houses [ht]
  ask people [ht]
  ask facilities [ht]
  ask neighbourhoods [ht]
  ask giniLabels [ht]
  ask regions [st]
  ask patches [set pcolor 37]
  let x min-pxcor
  let y max-pxcor
  let num 0
  let ids ["NW" "N" "NE" "W" "C" "E" "SW" "S" "SE"]
  loop [
    if y < min-pycor + 6 [stop]
    while [ x <= (max-pxcor - 12)][
      let theIndex item num ids


      create-giniLabels 1 [
        set shape "line half"
        set xcor x + 6
        set ycor y - 6
        set size 0.5
        set label-color black
        set label first [giniCoefficient] of regions with [id = theIndex]
      ]

      set x x + 12
      set num num + 1
    ]
    set x min-pxcor
    set y y - 12
  ]
end

;display neighbourhood view of the city, showing the neighbourhoods with their corresponding Gini index.
to seeGiniOfNeighbourhoods
  ;clear-all
  ask houses [ht]
  ask people [ht]
  ask facilities [ht]
  ask giniLabels [ht]
  ask neighbourhoods [st]
  ask regions [ht]
  ask patches [set pcolor 37]
  let x min-pxcor
  let y max-pxcor
  let num 0
  loop [
    if y < min-pycor + 3 [stop]
    while [ x <= (max-pxcor - 6)][
      let theIndex word "N" num


      create-giniLabels 1 [
        set shape "line half"
        set xcor x + 4
        set ycor y - 3
        set size 0.5
        set label-color black
        set label first [giniCoefficient] of neighbourhoods with [id = theIndex]
      ]

      set x x + 6
      set num num + 1
    ]
    set x min-pxcor
    set y y - 6
  ]
end

;returns the percentage of neighbourhoods considered to have good diversity
to-report %_of_good_nhbd
  report round((count goodNeighbourhoods / 36) * 100)
end

;returns the percentage of neighbourhoods considered to have moderate diversity (with Gini coefficient of between 0.1 and 0.2).
to-report %_of_moderate_nhbd
  report round ((count moderateNeighbourhoods / 36) * 100)
end

;returns the percentage of neighbourhoods considered to have bad diversity
to-report %_of_bad_nhbd
  report round ((count badNeighbourhoods / 36) * 100)
end

;returns the mean income of the owners that live in neighbourhoods with good diversity.
to-report avg_income_of_good_nhbd
  let avgIncome 0
  if count goodNeighbourhoods > 0 [
    set avgIncome precision (mean [meanIncome] of goodNeighbourhoods) 2
  ]
  report avgIncome
end

;returns the mean income of the owners that live in neighbourhoods with moderate diversity.
to-report avg_income_of_moderate_nhbd
  let avgIncome 0
  if count moderateNeighbourhoods > 0 [
    set avgIncome precision (mean [meanIncome] of moderateNeighbourhoods) 2
  ]
  report avgIncome
end

;returns the mean income of the owners that live in neighbourhoods with bad diversity.
to-report avg_income_of_bad_nhbd
  let avgIncome 0
  if count badNeighbourhoods > 0 [
    set avgIncome precision (mean [meanIncome] of badNeighbourhoods) 2
  ]
  report avgIncome
end

;returns the average price of the houses located in neighbourhoods with good diversity.
to-report avg_house_price_of_good_nhbd
  let avgPrice 0
  if count goodNeighbourhoods > 0 [
    ask goodNeighbourhoods [
      let total mean [price] of houses with [owner != nobody and xcor < [xcor] of myself + 6 and xcor >= [xcor] of myself and ycor > [ycor] of myself -  6 and ycor <= [ycor] of myself]
      set avgPrice avgPrice + total
    ]
    set avgPrice precision ( avgPrice / count goodNeighbourhoods) 2
  ]
  report avgPrice
end

;returns the average price of the houses located in neighbourhoods with moderate diversity.
to-report avg_house_price_of_moderate_nhbd
  let avgPrice 0
  if count moderateNeighbourhoods > 0 [
    ask moderateNeighbourhoods [
      let total mean [price] of houses with [owner != nobody and xcor < [xcor] of myself + 6 and xcor >= [xcor] of myself and ycor > [ycor] of myself -  6 and ycor <= [ycor] of myself]
      set avgPrice avgPrice + total
    ]
    set avgPrice precision ( avgPrice / count moderateNeighbourhoods) 2
  ]

  report avgPrice
end

;returns the average price of the houses located in neighbourhoods with bad diversity
to-report avg_house_price_of_bad_nhbd
  let avgPrice 0
  if count badNeighbourhoods > 0 [
    ask badNeighbourhoods [
      let total mean [price] of houses with [owner != nobody and xcor < [xcor] of myself + 6 and xcor >= [xcor] of myself and ycor > [ycor] of myself -  6 and ycor <= [ycor] of myself]
      set avgPrice avgPrice + total
    ]
    set avgPrice precision ( avgPrice / count badNeighbourhoods) 2
  ]

  report avgPrice
end

;returns the percentage of regions considered to have good diversity
to-report %_of_good_regions
  report round((count(regions with [giniCoefficient >= 0.2]) / 9) * 100)
end

;returns the percentage of regions considered to have moderate diversity
to-report %_of_moderate_regions
  report round((count(regions with [giniCoefficient >= 0.1 and giniCoefficient < 0.2]) / 9) * 100)
end

;returns the percentage of regions considered to have bad diversity
to-report %_of_bad_regions
  report round((count(regions with [giniCoefficient < 0.1]) / 9) * 100)
end

;returns the highest Gini coefficient that was found across all neighbourhoods in the city
to-report gini_of_best_nhbd
  report max [giniCoefficient] of neighbourhoods
end

;returns the lowest Gini coefficient that was found across all neighbourhoods in the city
to-report gini_of_worst_nhbd
  report min [giniCoefficient] of neighbourhoods
end
@#$#@#$#@
GRAPHICS-WINDOW
251
10
695
455
-1
-1
11.8
1
20
1
1
1
0
1
1
1
-18
18
-18
18
0
0
1
ticks
30.0

BUTTON
31
281
92
314
setup
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
121
282
184
315
Go
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

MONITOR
701
10
810
55
NIL
showLeaving
17
1
11

SLIDER
23
21
195
54
averageInflationRate
averageInflationRate
0
10
2.4
0.1
1
NIL
HORIZONTAL

SLIDER
22
67
194
100
interestRate
interestRate
0
8
4.2
0.1
1
NIL
HORIZONTAL

SLIDER
22
112
194
145
facilitiesConstructionProb
facilitiesConstructionProb
0
12
5.0
1
1
NIL
HORIZONTAL

SLIDER
21
154
193
187
crimeIncreaseProb
crimeIncreaseProb
0
100
20.0
1
1
NIL
HORIZONTAL

SLIDER
23
196
195
229
crimeDecreaseProb
crimeDecreaseProb
0
100
20.0
1
1
NIL
HORIZONTAL

PLOT
716
235
1037
473
Lorenze plot
population percentile
income share
0.0
100.0
0.0
100.0
true
true
"" ""
PENS
"perfect distribution" 1.0 0 -11085214 true "" ""
"lorenz line of nb1" 1.0 0 -2674135 true "" ""

BUTTON
702
61
922
94
see Gini index per neighbourhood
seeGiniOfNeighbourhoods
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
704
145
801
178
Display city
seeCityView
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
1054
237
1154
297
nb1
N1
1
0
String

BUTTON
1174
250
1237
283
Plot
plotLorenzeCurve
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
811
194
961
222
Ideal Gini index for a neighbourhood is 0.25
11
0.0
0

BUTTON
703
103
835
136
NIL
seeGiniOfRegions
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1046
319
1246
469
Histogram
NIL
NIL
0.0
1000000.0
0.0
50.0
true
false
"" ""
PENS
"ck" 20000.0 1 -16777216 true "" ""

MONITOR
706
183
792
228
NIL
showCityGini
2
1
11

MONITOR
859
10
968
55
NIL
showSatisfaction
2
1
11

MONITOR
997
89
1068
134
NIL
%_of_low
2
1
11

MONITOR
1090
91
1186
136
NIL
%_of_medium
2
1
11

MONITOR
998
148
1072
193
NIL
%_of_high
2
1
11

MONITOR
1092
151
1162
196
NIL
%_of_top
2
1
11

TEXTBOX
41
332
191
444
Warning: When selecting probability for crime increase and decrease ensure that addition of the probabilities is less or equal to 100 otherwise deafult values of 60 and 30 will be used as values for these parameters
11
15.0
1

@#$#@#$#@
## WHAT IS IT?

An agent-based model of the housing market is developed, to investigate how certain factors affect income diversity; the factors being inflation, interest rate, facilities construction rate and crime changes. Gini coefficent is the measurement of income diversity , the higher the better.

## HOW IT WORKS

Potential buyers partecipate in the market by placing bids on houses of interested, an homeowner can decide to sell once they have repayed the moertgage for the house.
Real estate agent regulate the market evaluating the bids and deciding who gets to aquire the houses.
Banks provide loans to potential buyers if their financial situation satisfies their predefined eligibility criteria.

## HOW TO USE IT

End user decide on the input regarding average inflation rate, interest rate, facilities construction, crime increase and decrease probability. 
To initialise the simulation with the parameters the setup button has to be clicked, and to allow the simulation to run for the defined amount of ticks the go button has to be clicked.
Having clicked go, in the middle you should start seing chnages in the virual city, simulation is run for a total of 1320 ticks after which some monitors would display bits of information.

## THINGS TO NOTICE

At the end of the simulation run, there are buttons on the right to change the view of the city. Deciding to focuus on neighborhoods aspect, regional aspect , or city aspect.

## THINGS TO TRY

To analyse a particular neighbourhood diversity in depth, click on the neighbourhood view of the city to identify the id of the neighbourhood, input the ID in the input bar with title nb1 and click on plot to see the lorenze curve and histogram of the owners income band.

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
  <experiment name="Default Runs" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>showCityGini</metric>
    <metric>showLeaving</metric>
    <metric>showSatisfaction</metric>
    <metric>%_of_low</metric>
    <metric>%_of_medium</metric>
    <metric>%_of_high</metric>
    <metric>%_of_top</metric>
    <metric>%_of_good_nhbd</metric>
    <metric>avg_income_of_good_nhbd</metric>
    <metric>avg_house_price_of_good_nhbd</metric>
    <metric>%_of_moderate_nhbd</metric>
    <metric>avg_income_of_moderate_nhbd</metric>
    <metric>avg_house_price_of_moderate_nhbd</metric>
    <metric>%_of_bad_nhbd</metric>
    <metric>avg_income_of_bad_nhbd</metric>
    <metric>avg_house_price_of_bad_nhbd</metric>
    <metric>%_of_good_regions</metric>
    <metric>%_of_moderate_regions</metric>
    <metric>%_of_bad_regions</metric>
    <metric>gini_of_best_nhbd</metric>
    <metric>gini_of_worst_nhbd</metric>
    <enumeratedValueSet variable="averageInflationRate">
      <value value="2.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeIncreaseProb">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeDecreaseProb">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interestRate">
      <value value="2.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="facilitiesConstructionProb">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment Set 1" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>showCityGini</metric>
    <metric>showLeaving</metric>
    <metric>showSatisfaction</metric>
    <metric>%_of_low</metric>
    <metric>%_of_medium</metric>
    <metric>%_of_high</metric>
    <metric>%_of_top</metric>
    <metric>%_of_good_nhbd</metric>
    <metric>avg_income_of_good_nhbd</metric>
    <metric>avg_house_price_of_good_nhbd</metric>
    <metric>%_of_moderate_nhbd</metric>
    <metric>avg_income_of_moderate_nhbd</metric>
    <metric>avg_house_price_of_moderate_nhbd</metric>
    <metric>%_of_bad_nhbd</metric>
    <metric>avg_income_of_bad_nhbd</metric>
    <metric>avg_house_price_of_bad_nhbd</metric>
    <metric>%_of_good_regions</metric>
    <metric>%_of_moderate_regions</metric>
    <metric>%_of_bad_regions</metric>
    <metric>gini_of_best_nhbd</metric>
    <metric>gini_of_worst_nhbd</metric>
    <steppedValueSet variable="averageInflationRate" first="0" step="0.5" last="10"/>
    <enumeratedValueSet variable="crimeIncreaseProb">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeDecreaseProb">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interestRate">
      <value value="2.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="facilitiesConstructionProb">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment Set 2" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>showCityGini</metric>
    <metric>showLeaving</metric>
    <metric>showSatisfaction</metric>
    <metric>%_of_low</metric>
    <metric>%_of_medium</metric>
    <metric>%_of_high</metric>
    <metric>%_of_top</metric>
    <metric>%_of_good_nhbd</metric>
    <metric>avg_income_of_good_nhbd</metric>
    <metric>avg_house_price_of_good_nhbd</metric>
    <metric>%_of_moderate_nhbd</metric>
    <metric>avg_income_of_moderate_nhbd</metric>
    <metric>avg_house_price_of_moderate_nhbd</metric>
    <metric>%_of_bad_nhbd</metric>
    <metric>avg_income_of_bad_nhbd</metric>
    <metric>avg_house_price_of_bad_nhbd</metric>
    <metric>%_of_good_regions</metric>
    <metric>%_of_moderate_regions</metric>
    <metric>%_of_bad_regions</metric>
    <metric>gini_of_best_nhbd</metric>
    <metric>gini_of_worst_nhbd</metric>
    <enumeratedValueSet variable="averageInflationRate">
      <value value="2.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeIncreaseProb">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeDecreaseProb">
      <value value="30"/>
    </enumeratedValueSet>
    <steppedValueSet variable="interestRate" first="0.5" step="0.5" last="8"/>
    <enumeratedValueSet variable="facilitiesConstructionProb">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment Set 3" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>showCityGini</metric>
    <metric>showLeaving</metric>
    <metric>showSatisfaction</metric>
    <metric>%_of_low</metric>
    <metric>%_of_medium</metric>
    <metric>%_of_high</metric>
    <metric>%_of_top</metric>
    <metric>%_of_good_nhbd</metric>
    <metric>avg_income_of_good_nhbd</metric>
    <metric>avg_house_price_of_good_nhbd</metric>
    <metric>%_of_moderate_nhbd</metric>
    <metric>avg_income_of_moderate_nhbd</metric>
    <metric>avg_house_price_of_moderate_nhbd</metric>
    <metric>%_of_bad_nhbd</metric>
    <metric>avg_income_of_bad_nhbd</metric>
    <metric>avg_house_price_of_bad_nhbd</metric>
    <metric>%_of_good_regions</metric>
    <metric>%_of_moderate_regions</metric>
    <metric>%_of_bad_regions</metric>
    <metric>gini_of_best_nhbd</metric>
    <metric>gini_of_worst_nhbd</metric>
    <enumeratedValueSet variable="averageInflationRate">
      <value value="2.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeIncreaseProb">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeDecreaseProb">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interestRate">
      <value value="2.9"/>
    </enumeratedValueSet>
    <steppedValueSet variable="facilitiesConstructionProb" first="0" step="1" last="12"/>
  </experiment>
  <experiment name="Experiment Set 4 part 1" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>showCityGini</metric>
    <metric>showLeaving</metric>
    <metric>showSatisfaction</metric>
    <metric>%_of_low</metric>
    <metric>%_of_medium</metric>
    <metric>%_of_high</metric>
    <metric>%_of_top</metric>
    <metric>%_of_good_nhbd</metric>
    <metric>avg_income_of_good_nhbd</metric>
    <metric>avg_house_price_of_good_nhbd</metric>
    <metric>%_of_moderate_nhbd</metric>
    <metric>avg_income_of_moderate_nhbd</metric>
    <metric>avg_house_price_of_moderate_nhbd</metric>
    <metric>%_of_bad_nhbd</metric>
    <metric>avg_income_of_bad_nhbd</metric>
    <metric>avg_house_price_of_bad_nhbd</metric>
    <metric>%_of_good_regions</metric>
    <metric>%_of_moderate_regions</metric>
    <metric>%_of_bad_regions</metric>
    <metric>gini_of_best_nhbd</metric>
    <metric>gini_of_worst_nhbd</metric>
    <enumeratedValueSet variable="averageInflationRate">
      <value value="2.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeIncreaseProb">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeDecreaseProb">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interestRate">
      <value value="2.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="facilitiesConstructionProb">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment Set 4 part 2" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>showCityGini</metric>
    <metric>showLeaving</metric>
    <metric>showSatisfaction</metric>
    <metric>%_of_low</metric>
    <metric>%_of_medium</metric>
    <metric>%_of_high</metric>
    <metric>%_of_top</metric>
    <metric>%_of_good_nhbd</metric>
    <metric>avg_income_of_good_nhbd</metric>
    <metric>avg_house_price_of_good_nhbd</metric>
    <metric>%_of_moderate_nhbd</metric>
    <metric>avg_income_of_moderate_nhbd</metric>
    <metric>avg_house_price_of_moderate_nhbd</metric>
    <metric>%_of_bad_nhbd</metric>
    <metric>avg_income_of_bad_nhbd</metric>
    <metric>avg_house_price_of_bad_nhbd</metric>
    <metric>%_of_good_regions</metric>
    <metric>%_of_moderate_regions</metric>
    <metric>%_of_bad_regions</metric>
    <metric>gini_of_best_nhbd</metric>
    <metric>gini_of_worst_nhbd</metric>
    <enumeratedValueSet variable="averageInflationRate">
      <value value="2.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeIncreaseProb">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeDecreaseProb">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interestRate">
      <value value="2.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="facilitiesConstructionProb">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment Set 4 part 3" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>showCityGini</metric>
    <metric>showLeaving</metric>
    <metric>showSatisfaction</metric>
    <metric>%_of_low</metric>
    <metric>%_of_medium</metric>
    <metric>%_of_high</metric>
    <metric>%_of_top</metric>
    <metric>%_of_good_nhbd</metric>
    <metric>avg_income_of_good_nhbd</metric>
    <metric>avg_house_price_of_good_nhbd</metric>
    <metric>%_of_moderate_nhbd</metric>
    <metric>avg_income_of_moderate_nhbd</metric>
    <metric>avg_house_price_of_moderate_nhbd</metric>
    <metric>%_of_bad_nhbd</metric>
    <metric>avg_income_of_bad_nhbd</metric>
    <metric>avg_house_price_of_bad_nhbd</metric>
    <metric>%_of_good_regions</metric>
    <metric>%_of_moderate_regions</metric>
    <metric>%_of_bad_regions</metric>
    <metric>gini_of_best_nhbd</metric>
    <metric>gini_of_worst_nhbd</metric>
    <enumeratedValueSet variable="averageInflationRate">
      <value value="2.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeIncreaseProb">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeDecreaseProb">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interestRate">
      <value value="2.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="facilitiesConstructionProb">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment Set 4 part 4" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>showCityGini</metric>
    <metric>showLeaving</metric>
    <metric>showSatisfaction</metric>
    <metric>%_of_low</metric>
    <metric>%_of_medium</metric>
    <metric>%_of_high</metric>
    <metric>%_of_top</metric>
    <metric>%_of_good_nhbd</metric>
    <metric>avg_income_of_good_nhbd</metric>
    <metric>avg_house_price_of_good_nhbd</metric>
    <metric>%_of_moderate_nhbd</metric>
    <metric>avg_income_of_moderate_nhbd</metric>
    <metric>avg_house_price_of_moderate_nhbd</metric>
    <metric>%_of_bad_nhbd</metric>
    <metric>avg_income_of_bad_nhbd</metric>
    <metric>avg_house_price_of_bad_nhbd</metric>
    <metric>%_of_good_regions</metric>
    <metric>%_of_moderate_regions</metric>
    <metric>%_of_bad_regions</metric>
    <metric>gini_of_best_nhbd</metric>
    <metric>gini_of_worst_nhbd</metric>
    <enumeratedValueSet variable="averageInflationRate">
      <value value="2.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeIncreaseProb">
      <value value="33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeDecreaseProb">
      <value value="33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interestRate">
      <value value="2.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="facilitiesConstructionProb">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment Set 4 part 5" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>showCityGini</metric>
    <metric>showLeaving</metric>
    <metric>showSatisfaction</metric>
    <metric>%_of_low</metric>
    <metric>%_of_medium</metric>
    <metric>%_of_high</metric>
    <metric>%_of_top</metric>
    <metric>%_of_good_nhbd</metric>
    <metric>avg_income_of_good_nhbd</metric>
    <metric>avg_house_price_of_good_nhbd</metric>
    <metric>%_of_moderate_nhbd</metric>
    <metric>avg_income_of_moderate_nhbd</metric>
    <metric>avg_house_price_of_moderate_nhbd</metric>
    <metric>%_of_bad_nhbd</metric>
    <metric>avg_income_of_bad_nhbd</metric>
    <metric>avg_house_price_of_bad_nhbd</metric>
    <metric>%_of_good_regions</metric>
    <metric>%_of_moderate_regions</metric>
    <metric>%_of_bad_regions</metric>
    <metric>gini_of_best_nhbd</metric>
    <metric>gini_of_worst_nhbd</metric>
    <enumeratedValueSet variable="averageInflationRate">
      <value value="2.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeIncreaseProb">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeDecreaseProb">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interestRate">
      <value value="2.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="facilitiesConstructionProb">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment Set 4 part 6" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>showCityGini</metric>
    <metric>showLeaving</metric>
    <metric>showSatisfaction</metric>
    <metric>%_of_low</metric>
    <metric>%_of_medium</metric>
    <metric>%_of_high</metric>
    <metric>%_of_top</metric>
    <metric>%_of_good_nhbd</metric>
    <metric>avg_income_of_good_nhbd</metric>
    <metric>avg_house_price_of_good_nhbd</metric>
    <metric>%_of_moderate_nhbd</metric>
    <metric>avg_income_of_moderate_nhbd</metric>
    <metric>avg_house_price_of_moderate_nhbd</metric>
    <metric>%_of_bad_nhbd</metric>
    <metric>avg_income_of_bad_nhbd</metric>
    <metric>avg_house_price_of_bad_nhbd</metric>
    <metric>%_of_good_regions</metric>
    <metric>%_of_moderate_regions</metric>
    <metric>%_of_bad_regions</metric>
    <metric>gini_of_best_nhbd</metric>
    <metric>gini_of_worst_nhbd</metric>
    <enumeratedValueSet variable="averageInflationRate">
      <value value="2.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeIncreaseProb">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeDecreaseProb">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interestRate">
      <value value="2.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="facilitiesConstructionProb">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment Set 4 part 7" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>showCityGini</metric>
    <metric>showLeaving</metric>
    <metric>showSatisfaction</metric>
    <metric>%_of_low</metric>
    <metric>%_of_medium</metric>
    <metric>%_of_high</metric>
    <metric>%_of_top</metric>
    <metric>%_of_good_nhbd</metric>
    <metric>avg_income_of_good_nhbd</metric>
    <metric>avg_house_price_of_good_nhbd</metric>
    <metric>%_of_moderate_nhbd</metric>
    <metric>avg_income_of_moderate_nhbd</metric>
    <metric>avg_house_price_of_moderate_nhbd</metric>
    <metric>%_of_bad_nhbd</metric>
    <metric>avg_income_of_bad_nhbd</metric>
    <metric>avg_house_price_of_bad_nhbd</metric>
    <metric>%_of_good_regions</metric>
    <metric>%_of_moderate_regions</metric>
    <metric>%_of_bad_regions</metric>
    <metric>gini_of_best_nhbd</metric>
    <metric>gini_of_worst_nhbd</metric>
    <enumeratedValueSet variable="averageInflationRate">
      <value value="2.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeIncreaseProb">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeDecreaseProb">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interestRate">
      <value value="2.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="facilitiesConstructionProb">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment Set 4 part 8" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>showCityGini</metric>
    <metric>showLeaving</metric>
    <metric>showSatisfaction</metric>
    <metric>%_of_low</metric>
    <metric>%_of_medium</metric>
    <metric>%_of_high</metric>
    <metric>%_of_top</metric>
    <metric>%_of_good_nhbd</metric>
    <metric>avg_income_of_good_nhbd</metric>
    <metric>avg_house_price_of_good_nhbd</metric>
    <metric>%_of_moderate_nhbd</metric>
    <metric>avg_income_of_moderate_nhbd</metric>
    <metric>avg_house_price_of_moderate_nhbd</metric>
    <metric>%_of_bad_nhbd</metric>
    <metric>avg_income_of_bad_nhbd</metric>
    <metric>avg_house_price_of_bad_nhbd</metric>
    <metric>%_of_good_regions</metric>
    <metric>%_of_moderate_regions</metric>
    <metric>%_of_bad_regions</metric>
    <metric>gini_of_best_nhbd</metric>
    <metric>gini_of_worst_nhbd</metric>
    <enumeratedValueSet variable="averageInflationRate">
      <value value="2.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeIncreaseProb">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeDecreaseProb">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interestRate">
      <value value="2.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="facilitiesConstructionProb">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment Set 4 part 9" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>showCityGini</metric>
    <metric>showLeaving</metric>
    <metric>showSatisfaction</metric>
    <metric>%_of_low</metric>
    <metric>%_of_medium</metric>
    <metric>%_of_high</metric>
    <metric>%_of_top</metric>
    <metric>%_of_good_nhbd</metric>
    <metric>avg_income_of_good_nhbd</metric>
    <metric>avg_house_price_of_good_nhbd</metric>
    <metric>%_of_moderate_nhbd</metric>
    <metric>avg_income_of_moderate_nhbd</metric>
    <metric>avg_house_price_of_moderate_nhbd</metric>
    <metric>%_of_bad_nhbd</metric>
    <metric>avg_income_of_bad_nhbd</metric>
    <metric>avg_house_price_of_bad_nhbd</metric>
    <metric>%_of_good_regions</metric>
    <metric>%_of_moderate_regions</metric>
    <metric>%_of_bad_regions</metric>
    <metric>gini_of_best_nhbd</metric>
    <metric>gini_of_worst_nhbd</metric>
    <enumeratedValueSet variable="averageInflationRate">
      <value value="2.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeIncreaseProb">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeDecreaseProb">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interestRate">
      <value value="2.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="facilitiesConstructionProb">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment Set 4 part 10" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>showCityGini</metric>
    <metric>showLeaving</metric>
    <metric>showSatisfaction</metric>
    <metric>%_of_low</metric>
    <metric>%_of_medium</metric>
    <metric>%_of_high</metric>
    <metric>%_of_top</metric>
    <metric>%_of_good_nhbd</metric>
    <metric>avg_income_of_good_nhbd</metric>
    <metric>avg_house_price_of_good_nhbd</metric>
    <metric>%_of_moderate_nhbd</metric>
    <metric>avg_income_of_moderate_nhbd</metric>
    <metric>avg_house_price_of_moderate_nhbd</metric>
    <metric>%_of_bad_nhbd</metric>
    <metric>avg_income_of_bad_nhbd</metric>
    <metric>avg_house_price_of_bad_nhbd</metric>
    <metric>%_of_good_regions</metric>
    <metric>%_of_moderate_regions</metric>
    <metric>%_of_bad_regions</metric>
    <metric>gini_of_best_nhbd</metric>
    <metric>gini_of_worst_nhbd</metric>
    <enumeratedValueSet variable="averageInflationRate">
      <value value="2.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeIncreaseProb">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeDecreaseProb">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interestRate">
      <value value="2.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="facilitiesConstructionProb">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment Set 4 part 11" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>showCityGini</metric>
    <metric>showLeaving</metric>
    <metric>showSatisfaction</metric>
    <metric>%_of_low</metric>
    <metric>%_of_medium</metric>
    <metric>%_of_high</metric>
    <metric>%_of_top</metric>
    <metric>%_of_good_nhbd</metric>
    <metric>avg_income_of_good_nhbd</metric>
    <metric>avg_house_price_of_good_nhbd</metric>
    <metric>%_of_moderate_nhbd</metric>
    <metric>avg_income_of_moderate_nhbd</metric>
    <metric>avg_house_price_of_moderate_nhbd</metric>
    <metric>%_of_bad_nhbd</metric>
    <metric>avg_income_of_bad_nhbd</metric>
    <metric>avg_house_price_of_bad_nhbd</metric>
    <metric>%_of_good_regions</metric>
    <metric>%_of_moderate_regions</metric>
    <metric>%_of_bad_regions</metric>
    <metric>gini_of_best_nhbd</metric>
    <metric>gini_of_worst_nhbd</metric>
    <enumeratedValueSet variable="averageInflationRate">
      <value value="2.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeIncreaseProb">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeDecreaseProb">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interestRate">
      <value value="2.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="facilitiesConstructionProb">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment Set 4 part 12" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>showCityGini</metric>
    <metric>showLeaving</metric>
    <metric>showSatisfaction</metric>
    <metric>%_of_low</metric>
    <metric>%_of_medium</metric>
    <metric>%_of_high</metric>
    <metric>%_of_top</metric>
    <metric>%_of_good_nhbd</metric>
    <metric>avg_income_of_good_nhbd</metric>
    <metric>avg_house_price_of_good_nhbd</metric>
    <metric>%_of_moderate_nhbd</metric>
    <metric>avg_income_of_moderate_nhbd</metric>
    <metric>avg_house_price_of_moderate_nhbd</metric>
    <metric>%_of_bad_nhbd</metric>
    <metric>avg_income_of_bad_nhbd</metric>
    <metric>avg_house_price_of_bad_nhbd</metric>
    <metric>%_of_good_regions</metric>
    <metric>%_of_moderate_regions</metric>
    <metric>%_of_bad_regions</metric>
    <metric>gini_of_best_nhbd</metric>
    <metric>gini_of_worst_nhbd</metric>
    <enumeratedValueSet variable="averageInflationRate">
      <value value="2.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeIncreaseProb">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crimeDecreaseProb">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interestRate">
      <value value="2.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="facilitiesConstructionProb">
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
