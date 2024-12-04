# https://adventofcode.com/2024/day/4


# Description -------------------------------------------------------------
# Part 1
# Wordsearch
# horizontal: combine matrix row and count occurences (forward and reverse string)
# vertical: combine matrix column and count occurences (forward and reverse string)
# diagonal: use stepwise approach or shift rows/columns to align??
#
# Part 2
# Only need to look at diaganols
# find A and then look in all directions


# Load Packages -------------------------------------------------------------
library(readr)


# Load input --------------------------------------------------------------
df_example <- as.matrix(do.call(rbind,strsplit(readr::read_lines('./data/2024/Day 04/input_example.txt'),'',fixed=T)), stringsAsFactors = FALSE)
df_part1 <- as.matrix(do.call(rbind,strsplit(readr::read_lines('./data/2024/Day 04/input_part1.txt'),'',fixed=T)), stringsAsFactors = FALSE)




# Solution (Part 1)-------------------------------------------------------------
f_day4_part1 <- function(matrix){

  # initialise counter
  word_count = 0
  
  # check horizontals
  for(r in 1:nrow(matrix)){
    word_count = word_count + stringr::str_count(paste(matrix[r,], collapse = ""), "XMAS")
    word_count = word_count + stringr::str_count(paste(matrix[r,], collapse = ""), "SAMX")
  }
  
  # check horizontals
  for(c in 1:ncol(matrix)){
    word_count = word_count + stringr::str_count(paste(matrix[,c], collapse = ""), "XMAS")
    word_count = word_count + stringr::str_count(paste(matrix[,c], collapse = ""), "SAMX")
  }
  
  # check diagonals : iterate through each cell and only check those starting with X
  for(r in 1:nrow(matrix)){
    for(c in 1:ncol(matrix)){
      if(matrix[r,c] == 'X'){
        # down-right (only if not in far-right or bottom columns)
        if(r<=nrow(matrix)-3 & c<=ncol(matrix)-3){
          if(paste0(matrix[r,c], matrix[r+1,c+1], matrix[r+2,c+2], matrix[r+3,c+3]) == 'XMAS'){word_count = word_count + 1}
        }
        # down-left (only if not in far-left or bottom columns)
        if(r>=4 & c<=ncol(matrix)-3){
          if(paste0(matrix[r,c], matrix[r-1,c+1], matrix[r-2,c+2], matrix[r-3,c+3]) == 'XMAS'){word_count = word_count + 1}
        }
        # up-right (only if not in far-right or top columns)
        if(r<=nrow(matrix)-3 & c>=4){
          if(paste0(matrix[r,c], matrix[r+1,c-1], matrix[r+2,c-2], matrix[r+3,c-3]) == 'XMAS'){word_count = word_count + 1}
        }
        # up-left (only if not in far-left or top columns)
        if(r>=4 & c>=4){
          if(paste0(matrix[r,c], matrix[r-1,c-1], matrix[r-2,c-2], matrix[r-3,c-3]) == 'XMAS'){word_count = word_count + 1}
        }
      }
    }
  }
  
  return(word_count)
}


# Solution (Part 2)-------------------------------------------------------------
f_day4_part2 <- function(matrix){
  
  # initialise counter
  word_count = 0
  
  
  # check diagonals : iterate through each cell and only check those starting with A
  # ignore the outside edge for iterating as need to step out 1 in all directions
  for(r in 2:nrow(matrix)-1){
    for(c in 2:ncol(matrix)-1){
      if(matrix[r,c] == 'A'){
        
        # down-right / up-left
        if(
          paste0(matrix[r-1,c-1], matrix[r,c], matrix[r+1,c+1]) %in% c('MAS','SAM') &
          paste0(matrix[r+1,c-1], matrix[r,c], matrix[r-1,c+1]) %in% c('MAS','SAM')
        ){
          word_count = word_count + 1
        }
        
      }
    }
  }
  
  return(word_count)
}

# Output ------------------------------------------------------------------
f_day4_part1(df_example)
f_day4_part1(df_part1)

f_day4_part2(df_example)
f_day4_part2(df_part1)
