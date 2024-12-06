# https://adventofcode.com/2024/day/6


# Description -------------------------------------------------------------
# Part 1
# Walk through the maze to exit
# Value . = empty space
# Value # = dead-end
# Value ^ = start point (assume this will always start pointing north)
# Adding a frame around matrix will make it easier to see when end is reached
#
# Part 2
# Add return function to Part 1 to flag if steps are retraced (track via placing NESW for steps)
# Try all possible new block locations was slow. Only need to add block were step might be made
# 


# Load Packages -------------------------------------------------------------
library(readr)


# Load input --------------------------------------------------------------
df_example <- as.matrix(do.call(rbind,strsplit(readr::read_lines('./data/2024/Day 06/input_example.txt'),'',fixed=T)), stringsAsFactors = FALSE)
df_part1 <- as.matrix(do.call(rbind,strsplit(readr::read_lines('./data/2024/Day 06/input_part1.txt'),'',fixed=T)), stringsAsFactors = FALSE)

make_turn <- list(
  "N" = "E",
  "E" = "S",
  "S" = "W",
  "W" = "N"
)

step_direction <- list(
  "N" = c(-1,0),
  "E" = c(0,1),
  "S" = c(1,0),
  "W" = c(0,-1)
)

# Solution Part 1 ---------------------------------------------------------
f_day06_part1 <- function(mtrx){
  
  # add frame for exits (X)
  mtrx <- rbind(
    matrix(rep("X",ncol(mtrx)),nrow=1),
    mtrx,
    matrix(rep("X",ncol(mtrx)),nrow=1)
  )
  mtrx <- cbind(
    matrix(rep("X",nrow(mtrx)),ncol=1),
    mtrx,
    matrix(rep("X",nrow(mtrx)),ncol=1)
  )
  
  # find the starting point
  cpos <- which(mtrx == '^',arr.ind = TRUE)
  dir <- 'N'
  
  # loop to make steps
  repeat {
    if(mtrx[cpos[1]+step_direction[[dir]][1],cpos[2]+step_direction[[dir]][2]] == "X"){
      # if at exit
      return(length(which(mtrx %in% c("N","E","S","W"))))
    } else if(mtrx[cpos[1]+step_direction[[dir]][1],cpos[2]+step_direction[[dir]][2]] == "#"){
      # if at blocker
      dir = make_turn[[dir]]
    } else if(mtrx[cpos[1]+step_direction[[dir]][1],cpos[2]+step_direction[[dir]][2]] == dir){
      # if step would retrace previous steps and create loop
      return(-1)
    } else {
      # if space ahead
      cpos[1] = cpos[1]+step_direction[[dir]][1]
      cpos[2] = cpos[2]+step_direction[[dir]][2]
      mtrx[cpos[1],cpos[2]] = dir
    }  
  }  
  
}

# Solution Part 2 ---------------------------------------------------------
f_day06_part2 <- function(mtrx){
  
  orig_mtrx <- mtrx
  loop_counter <- 0
  
  # find all empty spaces
  empty_spaces = which(mtrx == ".")
  
  for(t in empty_spaces){
    
    # find the starting point
    mtrx <- orig_mtrx
    mtrx[t] <- "#"
    if(f_day06_part1(mtrx) == -1){
      loop_counter = loop_counter + 1
      print(paste0("Test: ",t, " : Loop Counter =", loop_counter))
    }
  }
  
  return(loop_counter)
}


# Output ------------------------------------------------------------------
f_day06_part1(df_example)
f_day06_part1(df_part1)

f_day06_part2(df_example)
f_day06_part2(df_part1)
