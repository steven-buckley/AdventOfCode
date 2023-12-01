

# Description -------------------------------------------------------------



# Load input --------------------------------------------------------------
monkeys_test = list(m0 = list(79,98),
               m1 = list(54,65,75,74),
               m2 = list(79,60,97),
               m3 = list(74),
               mi0 = 0,
               mi1 = 0,
               mi2 = 0,
               mi3 = 0
               )


monkeys = list(m0 = list(84,72,58,51),
               m1 = list(88,58,58),
               m2 = list(93,82,71,77,83,53,71,89),
               m3 = list(81,68,65,81,73,77,96),
               m4 = list(75,80,50,73,88),
               m5 = list(59,72,99,87,91,81),
               m6 = list(86,69),
               m7 = list(91),
               mi0 = 0,
               mi1 = 0,
               mi2 = 0,
               mi3 = 0,
               mi4 = 0,
               mi5 = 0,
               mi6 = 0,
               mi7 = 0
)




# Solution: Test Data -----------------------------------------------------
# create function to run through one round of the process
f_round_test <- function(monkeys, part){
  
  # MONKEY 0
  if(length(monkeys[["m0"]]) > 0){
    for(i in 1:length(monkeys[["m0"]])){
      monkeys[["mi0"]] = monkeys[["mi0"]] + 1
      # operation : new = old * 19
      wl = as.numeric(monkeys[["m0"]][[i]])
      wl = wl * 19
      if(part == "A"){
        wl = floor(wl / 3)
      }else{
        wl = wl %% (23*19*13*17)
      }
      # test divisible by 23
      if(wl%%23 == 0){
        monkeys[["m2"]] = append(monkeys[["m2"]], wl)
      } else {
        monkeys[["m3"]] = append(monkeys[["m3"]], wl)
      }
    }
    monkeys[["m0"]] = list()
  }
  
  
  # MONKEY 1
  if(length(monkeys[["m1"]]) > 0){
    for(i in 1:length(monkeys[["m1"]])){
      monkeys[["mi1"]] = monkeys[["mi1"]] + 1
      # operation : new = old + 6
      wl = as.numeric(monkeys[["m1"]][[i]])
      wl = wl + 6
      if(part == "A"){
        wl = floor(wl / 3)
      }else{
        wl = wl %% (23*19*13*17)
      }
      # test divisible by 19
      if(wl%%19 == 0){
        monkeys[["m2"]] = append(monkeys[["m2"]], wl)
      } else {
        monkeys[["m0"]] = append(monkeys[["m0"]], wl)
      }
    }
    monkeys[["m1"]] = list()
  }
  

  # MONKEY 2
  if(length(monkeys[["m2"]]) > 0){
    for(i in 1:length(monkeys[["m2"]])){
      monkeys[["mi2"]] = monkeys[["mi2"]] + 1
      # operation : new = old * old
      wl = as.numeric(monkeys[["m2"]][[i]])
      wl = wl * wl
      if(part == "A"){
        wl = floor(wl / 3)
      }else{
        wl = wl %% (23*19*13*17)
      }
      # test divisible by 13
      if(wl%%13 == 0){
        monkeys[["m1"]] = append(monkeys[["m1"]], wl)
      } else {
        monkeys[["m3"]] = append(monkeys[["m3"]], wl)
      }
    }
    monkeys[["m2"]] = list()
  }
  
  
  # MONKEY 3
  if(length(monkeys[["m3"]]) > 0){
    for(i in 1:length(monkeys[["m3"]])){
      monkeys[["mi3"]] = monkeys[["mi3"]] + 1
      # operation : new = old + 3
      wl = as.numeric(monkeys[["m3"]][[i]])
      wl = wl + 3
      if(part == "A"){
        wl = floor(wl / 3)
      }else{
        wl = wl %% (23*19*13*17)
      }
      # test divisible by 17
      if(wl%%17 == 0){
        monkeys[["m0"]] = append(monkeys[["m0"]], wl)
      } else {
        monkeys[["m1"]] = append(monkeys[["m1"]], wl)
      }
    }
    monkeys[["m3"]] = list()
  }
  
  return(monkeys)
}  

# create a function to run the game
f_day11_test <- function(monkeys, rounds, part){
  
  # call the rounds function for the required number of rounds
  for(r in 1:rounds){
    monkeys <- f_round_test(monkeys, part)
  }
  
  # find the product of the top two values
  inspections <- c(monkeys[["mi0"]], monkeys[["mi1"]], monkeys[["mi2"]], monkeys[["mi3"]])
  answer <- max(inspections) * sort(inspections,partial=3)[3]
  
  return(answer)
}



# Day11 - Solution --------------------------------------------------------
f_round <- function(monkeys, part){
  
  # MONKEY 0
  if(length(monkeys[["m0"]]) > 0){
    for(i in 1:length(monkeys[["m0"]])){
      monkeys[["mi0"]] = monkeys[["mi0"]] + 1
      wl = as.numeric(monkeys[["m0"]][[i]])
      wl = wl * 3
      if(part == "A"){
        wl = floor(wl / 3)
      }else{
        wl = wl %% (13*2*7*17*5*11*3*19)
      }
      if(wl%%13 == 0){
        monkeys[["m1"]] = append(monkeys[["m1"]], wl)
      } else {
        monkeys[["m7"]] = append(monkeys[["m7"]], wl)
      }
    }
    monkeys[["m0"]] = list()
  }
  
  # MONKEY 1
  if(length(monkeys[["m1"]]) > 0){
    for(i in 1:length(monkeys[["m1"]])){
      monkeys[["mi1"]] = monkeys[["mi1"]] + 1
      wl = as.numeric(monkeys[["m1"]][[i]])
      wl = wl + 8
      if(part == "A"){
        wl = floor(wl / 3)
      }else{
        wl = wl %% (13*2*7*17*5*11*3*19)
      }
      if(wl%%2 == 0){
        monkeys[["m7"]] = append(monkeys[["m7"]], wl)
      } else {
        monkeys[["m5"]] = append(monkeys[["m5"]], wl)
      }
    }
    monkeys[["m1"]] = list()
  }
  
  # MONKEY 2
  if(length(monkeys[["m2"]]) > 0){
    for(i in 1:length(monkeys[["m2"]])){
      monkeys[["mi2"]] = monkeys[["mi2"]] + 1
      wl = as.numeric(monkeys[["m2"]][[i]])
      wl = wl * wl
      if(part == "A"){
        wl = floor(wl / 3)
      }else{
        wl = wl %% (13*2*7*17*5*11*3*19)
      }
      if(wl%%7 == 0){
        monkeys[["m3"]] = append(monkeys[["m3"]], wl)
      } else {
        monkeys[["m4"]] = append(monkeys[["m4"]], wl)
      }
    }
    monkeys[["m2"]] = list()
  }
  
  # MONKEY 3
  if(length(monkeys[["m3"]]) > 0){
    for(i in 1:length(monkeys[["m3"]])){
      monkeys[["mi3"]] = monkeys[["mi3"]] + 1
      wl = as.numeric(monkeys[["m3"]][[i]])
      wl = wl + 2
      if(part == "A"){
        wl = floor(wl / 3)
      }else{
        wl = wl %% (13*2*7*17*5*11*3*19)
      }
      if(wl%%17 == 0){
        monkeys[["m4"]] = append(monkeys[["m4"]], wl)
      } else {
        monkeys[["m6"]] = append(monkeys[["m6"]], wl)
      }
    }
    monkeys[["m3"]] = list()
  }
  
  # MONKEY 4
  if(length(monkeys[["m4"]]) > 0){
    for(i in 1:length(monkeys[["m4"]])){
      monkeys[["mi4"]] = monkeys[["mi4"]] + 1
      wl = as.numeric(monkeys[["m4"]][[i]])
      wl = wl + 3
      if(part == "A"){
        wl = floor(wl / 3)
      }else{
        wl = wl %% (13*2*7*17*5*11*3*19)
      }
      if(wl%%5 == 0){
        monkeys[["m6"]] = append(monkeys[["m6"]], wl)
      } else {
        monkeys[["m0"]] = append(monkeys[["m0"]], wl)
      }
    }
    monkeys[["m4"]] = list()
  }
  
  # MONKEY 5
  if(length(monkeys[["m5"]]) > 0){
    for(i in 1:length(monkeys[["m5"]])){
      monkeys[["mi5"]] = monkeys[["mi5"]] + 1
      wl = as.numeric(monkeys[["m5"]][[i]])
      wl = wl * 17
      if(part == "A"){
        wl = floor(wl / 3)
      }else{
        wl = wl %% (13*2*7*17*5*11*3*19)
      }
      if(wl%%11 == 0){
        monkeys[["m2"]] = append(monkeys[["m2"]], wl)
      } else {
        monkeys[["m3"]] = append(monkeys[["m3"]], wl)
      }
    }
    monkeys[["m5"]] = list()
  }
  
  # MONKEY 6
  if(length(monkeys[["m6"]]) > 0){
    for(i in 1:length(monkeys[["m6"]])){
      monkeys[["mi6"]] = monkeys[["mi6"]] + 1
      wl = as.numeric(monkeys[["m6"]][[i]])
      wl = wl + 6
      if(part == "A"){
        wl = floor(wl / 3)
      }else{
        wl = wl %% (13*2*7*17*5*11*3*19)
      }
      if(wl%%3 == 0){
        monkeys[["m1"]] = append(monkeys[["m1"]], wl)
      } else {
        monkeys[["m0"]] = append(monkeys[["m0"]], wl)
      }
    }
    monkeys[["m6"]] = list()
  }
  
  # MONKEY 7
  if(length(monkeys[["m7"]]) > 0){
    for(i in 1:length(monkeys[["m7"]])){
      monkeys[["mi7"]] = monkeys[["mi7"]] + 1
      wl = as.numeric(monkeys[["m7"]][[i]])
      wl = wl + 1
      if(part == "A"){
        wl = floor(wl / 3)
      }else{
        wl = wl %% (13*2*7*17*5*11*3*19)
      }
      if(wl%%19 == 0){
        monkeys[["m2"]] = append(monkeys[["m2"]], wl)
      } else {
        monkeys[["m5"]] = append(monkeys[["m5"]], wl)
      }
    }
    monkeys[["m7"]] = list()
  }
  
  return(monkeys)

}

# create a function to run the game
f_day11 <- function(monkeys, rounds, part){
  
  # call the rounds function for the required number of rounds
  for(r in 1:rounds){
    monkeys <- f_round(monkeys, part)
  }
  
  # find the product of the top two values
  inspections <- c(monkeys[["mi0"]], monkeys[["mi1"]], monkeys[["mi2"]], monkeys[["mi3"]], monkeys[["mi4"]], monkeys[["mi5"]], monkeys[["mi6"]], monkeys[["mi7"]])
  answer <- max(inspections) * sort(inspections,partial=7)[7]
  
  return(answer)
}



# Output ------------------------------------------------------------------
f_day11_test(monkeys_test, 20, "A") # 10605
f_day11(monkeys, 20, "A") # 55458

# problem is the numbers get too big
# beyond my knowledge by tips suggest that some form of modulo calculation is needed
# we don't really care about the value, just if it is divisible by a value
# therefore we can use the modulo function to divide by the product of all the test values
# e.g. in the test case for x%%23 we can just keep track of the number modulo 23
# (x*23)%%(y*23) will always return 0
# prior to the check we can find the modulo after dividing by the product of the test cases
# for all the cases we can just keep track of the number modulo (23*19*13*17)

f_day11_test(monkeys_test, 10000, "B") # 2713310158
f_day11(monkeys, 10000, "B") # 14508081294
