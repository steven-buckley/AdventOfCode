# https://adventofcode.com/2024/day/3


# Description -------------------------------------------------------------
# Part 1
# extract pattern mul(1,2) and multiply pairs of numbers before summing products
#
# Part 2
# also check for do() and don't()
# iterate through instructions keeping only those after a do() and not after don't
# recode to 0,0


# Load Packages -------------------------------------------------------------
library(readr)


# Load input --------------------------------------------------------------
inputExample <- readr::read_file('./data/2024/Day 03/input_example.txt')
inputPart1 <- readr::read_file('./data/2024/Day 03/input_part1.txt')
inputExample2 <- readr::read_file('./data/2024/Day 03/input_example2.txt')


# Solution (Part 1)-------------------------------------------------------------

f_day3_part1 <- function(str){
  
  # extract the instructions: mul(1,2)
  instructions <- unlist(stringr::str_extract_all(str, pattern = "mul\\([0-9]+,[0-9]+\\)"))
  
  # extract the first part
  i1 <- as.numeric(stringr::str_extract_all(instructions, pattern = "[0-9]+(?=,)"))
  
  # extract the second part
  i2 <- as.numeric(stringr::str_extract_all(instructions, pattern = "(?<=,)[0-9]+"))
  
  # sum the product of the two values
  result <- sum(i1 * i2)
  
  return(result)
}

# Solution (Part 2)-------------------------------------------------------------
f_day3_part2 <- function(str){
  # retain only the instructions we want
  # recode all others to 0,0 so they will have no effect on calculation
  
  # extract the instructions: mul(1,2) or do() or don't()
  instructions <- unlist(stringr::str_extract_all(str, pattern = "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)"))
  
  # initialise tracker
  use_instruction = TRUE
  
  # loop though each of the commands
  for(i in 1:length(instructions)){
    
    # if command is do() update tracker and reset command to 0,0
    if(instructions[i] == "do()"){
      use_instruction = TRUE
      instructions[i] = "0,0"
    }
    
    # if command is don't() update tracker and reset command to 0,0
    if(instructions[i] == "don't()"){
      use_instruction = FALSE
      instructions[i] = "0,0"
    }
    
    # if current tracker is don't, reset command to 0,0
    if(!use_instruction){
      instructions[i] = "0,0"
    }
  }
  
  # extract the first part
  i1 <- as.numeric(stringr::str_extract_all(instructions, pattern = "[0-9]+(?=,)"))
  
  # extract the second part
  i2 <- as.numeric(stringr::str_extract_all(instructions, pattern = "(?<=,)[0-9]+"))
  
  # sum the product of the two values
  result <- sum(i1 * i2)
  
  return(result)
}


# Output ------------------------------------------------------------------
f_day3_part1(inputExample)
f_day3_part1(inputPart1)

f_day3_part2(inputExample2)
f_day3_part2(inputPart1)

