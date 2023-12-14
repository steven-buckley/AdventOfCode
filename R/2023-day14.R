# https://adventofcode.com/2023/day/14


# Description -------------------------------------------------------------
# Part 1:
# seems to be basic sorting problem
# within each column split between the cubes (#) and sort so rocks (O) rise to top
# flip the column so can use the row number to assign the value to sum up

# Part 2:
# Initially got really lost
# repeated function to mimic other directions (could probably fix code so allow direction as parameter)
# figures must have a repeating pattern so identify that and then identify which stage in pattern would be the relevant cycle


# Load input --------------------------------------------------------------
df_test1 <- as.matrix(do.call(rbind,strsplit(readr::read_lines('./data/day-14-test1.txt'),'',fixed=T)), stringsAsFactors = FALSE)
df_input <- as.matrix(do.call(rbind,strsplit(readr::read_lines('./data/day-14-input.txt'),'',fixed=T)), stringsAsFactors = FALSE)

# Solution ----------------------------------------------------------------
f_day14_part1 <- function(m){
  
  # initialise a results vector
  res <- 0
  
  # replace characters in the matrix with numbers
  m <- gsub('O',1,m)
  m <- gsub('\\.',2,m)
  m <- gsub('#',3,m)
  
  # convert to numerics
  class(m) <- "numeric"
  
  # handle each column
  for(c in 1:ncol(m)){
    # isolate the column
    col <- m[,c]
    # find the number of cubes (value of 3)
    cubes <- which(col == 3)
    # check for cubes
    if(length(cubes)==0){
      # if no cubes sort the whole column
      m[,c] <- sort(m[,c])
    } else {
      # cycle through gaps between cubes
      srow <- 1
      for(r in cubes){
        # sort up to the cube
        m[srow:r,c] <- sort(m[srow:r,c])
        # increment to the next start position
        srow <- r + 1
      }
      # sort everything after the last cube (min of last cube + 1 or boundary)
      srow <- min(cubes[length(cubes)]+1,nrow(m))
      
      # sort the column
      m[srow:nrow(m),c] <- sort(m[srow:nrow(m),c])
    }
    
    # flip the column and sum up the position values of the round rocks (value of 1)
    res[c] <- sum(which(rev(m[,c])==1))
  }
  
  return(sum(res))
}

f_day14_part2_north <- function(m){
  
  # handle each column
  for(c in 1:ncol(m)){
    # isolate the column
    col <- m[,c]
    # find the number of cubes (value of 3)
    cubes <- which(col == 3)
    # check for cubes
    if(length(cubes)==0){
      # if no cubes sort the whole column
      m[,c] <- sort(m[,c])
    } else {
      # cycle through gaps between cubes
      srow <- 1
      for(r in cubes){
        # sort up to the cube
        m[srow:r,c] <- sort(m[srow:r,c])
        # increment to the next start position
        srow <- r + 1
      }
      # sort everything after the last cube (min of last cube + 1 or boundary)
      srow <- min(cubes[length(cubes)]+1,nrow(m))
      
      # sort the column
      m[srow:nrow(m),c] <- sort(m[srow:nrow(m),c])
    }

  }
  
  return(m)
}

f_day14_part2_south <- function(m){

  # handle each column
  for(c in 1:ncol(m)){
    # isolate the column
    col <- m[,c]
    # find the number of cubes (value of 3)
    cubes <- which(col == 3)
    # check for cubes
    if(length(cubes)==0){
      # if no cubes sort the whole column
      m[,c] <- sort(m[,c], decreasing = T)
    } else {
      # cycle through gaps between cubes
      srow <- 1
      for(r in cubes){
        # sort up to the cube
        m[srow:(r-1),c] <- sort(m[srow:(r-1),c], decreasing = T)
        # increment to the next start position
        srow <- r + 1
      }
      # sort everything after the last cube (min of last cube + 1 or boundary)
      srow <- min(cubes[length(cubes)]+1,nrow(m))
      
      # sort the column
      m[srow:nrow(m),c] <- sort(m[srow:nrow(m),c], decreasing = T)
    }
    
  }
  
  return(m)
}

f_day14_part2_east <- function(m){
 
  # handle each row
  for(r in 1:nrow(m)){
    # isolate the row
    row <- m[r,]
    # find the number of cubes (value of 3)
    cubes <- which(row == 3)
    # check for cubes
    if(length(cubes)==0){
      # if no cubes sort the whole row
      m[r,] <- sort(m[r,], decreasing = T)
    } else {
      # cycle through gaps between cubes
      scol <- 1
      for(c in cubes){
        # sort up to the cube
        m[r,scol:(c-1)] <- sort(m[r,scol:(c-1)], decreasing = T)
        # increment to the next start position
        scol <- c + 1
      }
      # sort everything after the last cube (min of last cube + 1 or boundary)
      scol <- min(cubes[length(cubes)]+1,ncol(m))
      
      # sort the row
      m[r,scol:ncol(m)] <- sort(m[r,scol:ncol(m)], decreasing = T)
    }
    
  }
  
  return(m)
}

f_day14_part2_west <- function(m){
  
  # handle each row
  for(r in 1:nrow(m)){
    # isolate the row
    row <- m[r,]
    # find the number of cubes (value of 3)
    cubes <- which(row == 3)
    # check for cubes
    if(length(cubes)==0){
      # if no cubes sort the whole row
      m[r,] <- sort(m[r,])
    } else {
      # cycle through gaps between cubes
      scol <- 1
      for(c in cubes){
        # sort up to the cube
        m[r,scol:(c-1)] <- sort(m[r,scol:(c-1)])
        # increment to the next start position
        scol <- c + 1
      }
      # sort everything after the last cube (min of last cube + 1 or boundary)
      scol <- min(cubes[length(cubes)]+1,ncol(m))
      
      # sort the column
      m[r,scol:ncol(m)] <- sort(m[r,scol:ncol(m)])
    }
    
  }
  
  return(m)
}

f_day14_part2 <- function(m, cycles){
  
  output <- 0
  res <- 0
  
  # replace characters in the matrix with numbers
  m <- gsub('O',1,m)
  m <- gsub('\\.',2,m)
  m <- gsub('#',3,m)
  
  # convert to numerics
  class(m) <- "numeric"
  
  m_bck <- m
  
  # looping through all the cycles would take too long
  # once the same end of cycle is reached it will loop
  for(x in 1:cycles){
    m <- f_day14_part2_north(m)
    m <- f_day14_part2_west(m)
    m <- f_day14_part2_south(m)
    m <- f_day14_part2_east(m)
    
    # save the output state
    output[x] <- sum(which(m==1))
    
    # break loop if output state already exists
    if(x > 2 & output[x] %in% output[1:(x-1)]){break}
  }
  
  # find the start point and length of the cycle
  cyc_st <- which(output == output[length(output)])[1]
  cyc_len <- length(output) - cyc_st
  
  # find the relative position in the cycle for the required number of cycles
  req_cycles <- (((cycles - cyc_st) %% cyc_len) + cyc_st)
  
  for(x in 1:req_cycles){
    m_bck <- f_day14_part2_north(m_bck)
    m_bck <- f_day14_part2_west(m_bck)
    m_bck <- f_day14_part2_south(m_bck)
    m_bck <- f_day14_part2_east(m_bck)
  }
  
  for(rcol in 1:ncol(m_bck)){
    # flip the column and sum up the position values of the round rocks (value of 1)
    res[rcol] <- sum(which(rev(m_bck[,rcol])==1))
  }
  
  return(sum(res))
}
  
# Output ------------------------------------------------------------------
f_day14_part1(df_test1)
f_day14_part1(df_input)

f_day14_part2(df_test1, 1000000000)
f_day14_part2(df_input, 1000000000)
