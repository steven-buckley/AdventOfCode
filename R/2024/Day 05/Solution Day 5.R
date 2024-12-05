# https://adventofcode.com/2024/day/5


# Description -------------------------------------------------------------
# Part 1
# Spilt input into rules and updates
# For each of the updates step through the numbers and check each pair against the rules
#
# Part 2
# Need to allow for cases where only a single level in list causes an issue
# For unsafe entries recall test after removing a single entry in the list
# Repeat until removing an entry gives a safe result


# Load Packages -------------------------------------------------------------
library(dplyr)
library(stringr)

# Load input --------------------------------------------------------------
df_example = read.table('./data/2024/Day 05/input_example.txt',sep='\t', col.names = "INPUT")
df_part1 = read.table('./data/2024/Day 05/input_part1.txt',sep='\t', col.names = "INPUT")


# Solution (Part 1)-------------------------------------------------------------

f_day5_check_rule <- function(seq, rules){

  # initialise vector
  chk <- c()
  
  # loop through each entry in the sequence
  for(i in 1:(length(seq)-1)){
    # loop through each pair
    for(c in (i+1):length(seq)){
      chk = c(chk,paste0(seq[i],'|',seq[c]))
    }
  }
  
  # check if all rules are met
  if(all(chk %in% rules)){
    return(as.numeric(seq[(length(seq)+1)/2]))
  } else {
    return(0)
  }
  
}

f_day5_part1 <- function(df){
  
  # split dataframe into rules and updates
  rules <- df |> dplyr::filter(grepl('\\|',INPUT)) |> dplyr::pull()
  updates <- df |> dplyr::filter(grepl(',',INPUT))
  
  updates <- updates |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      OUTPUT = f_day5_check_rule(unlist(stringr::str_split(INPUT,",")),rules)
    )
  
  return(sum(updates$OUTPUT))
  
}

# Solution (Part 2)-------------------------------------------------------------
f_day5_check_and_reorder <- function(seq, rules){
  
  # initialise vector
  chk <- c()
  
  # loop through each entry in the sequence
  for(i in 1:(length(seq)-1)){
    # loop through each pair
    for(c in (i+1):length(seq)){
      # get the pages to compare and build string
      p1 <- seq[i]
      p2 <- seq[c]
      check = paste0(p1,'|',p2)
      # if the combination is not in the rules flip the entries around
      if(!check %in% rules){
        seq[i] <- p2
        seq[c] <- p1
        check = paste0(p2,'|',p1)
      }
      # append the output to the vectr
      chk = c(chk,check)
    }
  }
  
  # check if all rules are met : return middle number
  if(all(chk %in% rules)){
    return(as.numeric(seq[(length(seq)+1)/2]))
  } else {
    return(0)
  }
  
}

f_day5_part2 <- function(df){
  
  # split dataframe into rules and updates
  rules <- df |> dplyr::filter(grepl('\\|',INPUT)) |> dplyr::pull()
  updates <- df |> dplyr::filter(grepl(',',INPUT))
  
  updates <- updates |> 
    dplyr::rowwise() |> 
    # run PART 1
    dplyr::mutate(
      PART1 = f_day5_check_rule(unlist(stringr::str_split(INPUT,",")),rules)
    ) |> 
    # filter to those failing Part 1 and run through Part 2
    dplyr::filter(PART1 == 0) |> 
    dplyr::mutate(
      PART2 = f_day5_check_and_reorder(unlist(stringr::str_split(INPUT,",")),rules)
    )
  
  return(sum(updates$PART2))
  
}

# Output ------------------------------------------------------------------
f_day5_part1(df_example)
f_day5_part1(df_part1)

f_day5_part2(df_example)
f_day5_part2(df_part1)

 

