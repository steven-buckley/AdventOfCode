# https://adventofcode.com/2024/day/1


# Description -------------------------------------------------------------
# Part 1
# Given two list of numbers, sort both lists and calculate the absolute difference between each pair of numbers
#
# Part 2
# This time, you'll need to figure out exactly how often each number from the left list appears in the right list. 
# Calculate a total similarity score by adding up each number in the left list after multiplying it by the number 
# of times that number appears in the right list.

# Load Packages -------------------------------------------------------------
library(dplyr)
library(stringr)

# Load input --------------------------------------------------------------
df_example = read.table('./data/2024/Day 01/input_example.txt',sep='\t', col.names = "INPUT")
df_input_1 = read.table('./data/2024/Day 01/input_part1.txt',sep='\t', col.names = "INPUT")
df <- df_example

# Solution ----------------------------------------------------------------
split_numeric_columns <- function(df){
  
  df <- df|> 
    dplyr::mutate(
      f1 = as.numeric(stringr::word(INPUT,1)),
      f2 = as.numeric(stringr::word(INPUT,-1))
    )
    
  return(df)
}
  
f_2024_d1_a <- function(df){
  
  # split the input into two sorted lists
  lst_a <- sort(split_numeric_columns(df)$f1)
  lst_b <- sort(split_numeric_columns(df)$f2)
  
  # calculate the sum of the differences
  sum_diff <- sum(abs(lst_b - lst_a))
  
  return(sum_diff)
}


f_2024_d1_b <- function(df){
  
  # split the input into two sorted lists
  lst_a <- sort(split_numeric_columns(df)$f1)
  lst_b <- sort(split_numeric_columns(df)$f2)
  
  # group by column 1, sum occurences of number in second list
  df <- split_numeric_columns(df) |> 
    dplyr::group_by(f1) |> 
    dplyr::mutate(
      occurences = sum(f1 == lst_b),
      score = f1 * occurences
    )
  
  # total the sum
  return(sum(df$score))
}

# Output ------------------------------------------------------------------
f_2024_d1_a(df_example)
f_2024_d1_a(df_input_1)

f_2024_d1_b(df_example)
f_2024_d1_b(df_input_1)