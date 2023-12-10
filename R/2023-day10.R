# https://adventofcode.com/2023/day/10


# Description -------------------------------------------------------------
# Need to find the shape for S (look at the surrounding values)
# 
# starting from S loop round the path and distance is half of path length
#
# Part 2 : where to start
# If replace whole of path with X could use this as a boundary
# No idea how to decide inside loop or between two outside parts of pipe


# Load input --------------------------------------------------------------
df_test1 <- as.matrix(do.call(rbind,strsplit(readr::read_lines('./data/day-10-test1.txt'),'',fixed=T)), stringsAsFactors = FALSE)
df_input <- as.matrix(do.call(rbind,strsplit(readr::read_lines('./data/day-10-input.txt'),'',fixed=T)), stringsAsFactors = FALSE)

# Solution ----------------------------------------------------------------

f_day10_next_step <- function(val, dir){
  
  # based on the current direction and the pipe value decide the next step
  if(val == "|" & dir == "N"){NEXT_STEP <- c("N",-1,0)}
  if(val == "|" & dir == "S"){NEXT_STEP <- c("S",1,0)}
  
  if(val == "-" & dir == "E"){NEXT_STEP <- c("E",0,1)}
  if(val == "-" & dir == "W"){NEXT_STEP <- c("W",0,-1)}
  
  if(val == "L" & dir == "S"){NEXT_STEP <- c("E",0,1)}
  if(val == "L" & dir == "W"){NEXT_STEP <- c("N",-1,0)}
  
  if(val == "J" & dir == "S"){NEXT_STEP <- c("W",0,-1)}
  if(val == "J" & dir == "E"){NEXT_STEP <- c("N",-1,0)}
  
  if(val == "7" & dir == "N"){NEXT_STEP <- c("W",0,-1)}
  if(val == "7" & dir == "E"){NEXT_STEP <- c("S",1,0)}
  
  if(val == "F" & dir == "N"){NEXT_STEP <- c("E",0,1)}
  if(val == "F" & dir == "W"){NEXT_STEP <- c("S",1,0)}
  
  return(NEXT_STEP)
}
m<-df_test1

f_day10_part1 <- function(m){

  # add a boundary around the values to prevent erros from end of range
  m <- rbind(m, c(rep(".",ncol(m))))
  m <- rbind(c(rep(".",ncol(m))), m)
  m <- cbind(c(rep(".",nrow(m))), m)
  m <- cbind(m, c(rep(".",nrow(m))))
    
  # find the start position in the matrix (S)
  SPOS <- data.frame(which(m == "S", arr.ind = TRUE))
  
  # identify valid pipes in each direction from a point
  val_N <- c("|","F","7")
  val_S <- c("|","J","L")
  val_E <- c("-","J","7")
  val_W <- c("-","F","L")
  
  # initialist some counters
  STEPS <- 0
  cur_r <- SPOS$row
  cur_c <- SPOS$col
  REACHED_END = FALSE
  
  # identfy what shape pipe the start position is based on the valid pipes around it and start clockwise
  if(m[(cur_r-1), cur_c] %in% val_N & m[(cur_r+1), cur_c] %in% val_S) {
    SPOS_VAL = "|"
    NEXT_STEP <- c("N",-1,0)
  } else if(m[cur_r, (cur_c-1)] %in% val_W & m[cur_r, (cur_c+1)] %in% val_E) {
    SPOS_VAL = "-"
    NEXT_STEP <- c("E",0,1)
  } else if(m[(cur_r-1), cur_c] %in% val_N & m[cur_r, (cur_c+1)] %in% val_E) {
    SPOS_VAL = "L"
    NEXT_STEP <- c("E",0,1)
  } else if(m[(cur_r-1), cur_c] %in% val_N & m[cur_r, (cur_c-1)] %in% val_W) {
    SPOS_VAL = "J"
    NEXT_STEP <- c("N",-1,0)
  } else if(m[(cur_r+1), cur_c] %in% val_S & m[cur_r, (cur_c+1)] %in% val_E) {
    SPOS_VAL = "F"
    NEXT_STEP <- c("E",0,1)
  } else if(m[cur_r, (cur_c-1)] %in% val_W & m[(cur_r+1), cur_c] %in% val_S) {
    SPOS_VAL = "7"
    NEXT_STEP <- c("S",0,-1)
  }
  
  # run through the maze until the start point is reached again
  while(REACHED_END == FALSE){
    # increment the steps
    STEPS <- STEPS + 1
    # update the position
    cur_r <- cur_r + as.numeric(NEXT_STEP[[2]])
    cur_c <- cur_c + as.numeric(NEXT_STEP[[3]])
    CUR_POS <- m[cur_r, cur_c]
    if(CUR_POS == "S"){
      REACHED_END <- TRUE
    } else {
      NEXT_STEP <- f_day10_next_step(CUR_POS, NEXT_STEP[[1]])  
    }
  }
  
  # the furthest point is the half way mark
  return(STEPS / 2)

}


# Output ------------------------------------------------------------------
f_day10_part1(df_test1)
f_day10_part1(df_input)