# https://adventofcode.com/2024/day/2


# Description -------------------------------------------------------------
# Part 1
# How many reports are safe?
# Safe are where all values in a list are either increasing or decreasing and adjacent values differ by between 1 and 3
#
# Part 2
# Need to allow for cases where only a single level in list causes an issue
# For unsafe entries recall test after removing a single entry in the list
# Repeat until removing an entry gives a safe result


# Load Packages -------------------------------------------------------------
library(dplyr)
library(stringr)

# Load input --------------------------------------------------------------
df_example = read.table('./data/2024/Day 02/input_example.txt',sep='\t', col.names = "INPUT")
df_input_1 = read.table('./data/2024/Day 02/input_part1.txt',sep='\t', col.names = "INPUT")


# Solution ----------------------------------------------------------------

# function to split a string to numeric vector
f_split_str_to_numeric_vector <- function(str){
  # split the string to a numeric vector
  return(as.numeric(unlist(strsplit(str, " "))))
}


# function to perform tests of list of levels
f_test_safe <- function(lvls){
  
  # Test 1: Difference between levels is always between 1 and 3
  T1 <- all(between(abs(lvls[2:length(lvls)] - lvls[1:length(lvls)-1]),1,3))
  
  # Test 2: Levels are always increasing or decreasing
  T1I <- all(lvls[2:length(lvls)] > lvls[1:length(lvls)-1])
  T1D <- all(lvls[2:length(lvls)] < lvls[1:length(lvls)-1])
  
  # return boolean based on T1 and either T1I or T1D
  return(T1 & (T1I | T1D))
}



f_day2_part1_test <- function(str){
  
  # split input to vector of numbers
  lvls <- f_split_str_to_numeric_vector(str)
  
  # call test function
  test <- f_test_safe(lvls)
  
  return(test)
}

f_day2_part1 <- function(df){
  
  # apply test function to each row
  df <- df |> 
    rowwise() |> 
    dplyr::mutate(
      TEST = f_day2_part1_test(INPUT)
    )
  
  # sum the TRUE entries
  return(sum(df$TEST))
  
}

f_day2_part2_test <- function(str){
  
  # split input to vector of numbers
  lvls <- f_split_str_to_numeric_vector(str)
  
  # run the initial test on the full list of levels, return if TRUE
  if(f_test_safe(lvls)){
    return(TRUE)
  }
  
  # if full list is not safe, retest removing single character at a time
  # return if any is TRUE
  for (i in 1:length(lvls)){
    if(f_test_safe(lvls[-i])){
      return(TRUE)
    }
  }
  
  # point only reached if tests fail
  return(FALSE)
}

f_day2_part2 <- function(df){
  
  # apply test function to each row
  df <- df |> 
    rowwise() |> 
    dplyr::mutate(
      TEST = f_day2_part2_test(INPUT)
    )
  
  # sum the TRUE entries
  return(sum(df$TEST))
  
}


# Output ------------------------------------------------------------------
f_day2_part1(df_example)
f_day2_part1(df_input_1)

f_day2_part2(df_example)
f_day2_part2(df_input_1)
