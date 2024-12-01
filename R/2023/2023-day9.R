# https://adventofcode.com/2023/day/9


# Description -------------------------------------------------------------



# Load input --------------------------------------------------------------
df_test1 = read.table('./data/day-9-test1.txt',sep='\t', col.names = "INPUT")
df_input = read.table('./data/day-9-input.txt',sep='\t', col.names = "INPUT")



# Solution ----------------------------------------------------------------
f_day9_identify_next_num <- function(str){
  
  # convert the number string to a list of numbers
  hist <- list(as.numeric(unlist(strsplit(str, " "))))
  
  # create the sequences until the full list is 0
  while(sum(hist[[length(hist)]]) != 0){
    
    # create placeholder for list
    nl <- c()
    
    # loop through each of the list elements to identify the difference
    for(i in 2:length(hist[[length(hist)]])){
      nl[i-1] <- hist[[length(hist)]][i] - hist[[length(hist)]][i-1]
    }
    
    # append the new sequence to the list
    hist <- append(hist,list(nl))
    
  }
  
  # append the initial 0 to the final sequence
  hist[[length(hist)]] <- append(hist[[length(hist)]], 0)
  
  # for each of the sequences to the top
  for(i in (length(hist)-1):1){
    hist[[i]] <- append(hist[[i]], (tail(hist[[i]],1) + tail(hist[[i+1]],1)))
  }
  
  return(as.numeric(tail(hist[[1]],1)))
  
}


f_day9_part1 <- function(df){
  
  # create a new column for the next number in each sequence
  df$NEXT <- lapply(df$INPUT, f_day9_identify_next_num)
  
  # return the sum of all the next numbers
  return(sum(as.numeric(df$NEXT)))
  
}

f_day9_identify_prev_num <- function(str){
  
  # convert the number string to a list of numbers
  hist <- list(as.numeric(unlist(strsplit(str, " "))))
  
  # create the sequences until the full list is 0
  while(sum(hist[[length(hist)]]) != 0){
    
    # create placeholder for list
    nl <- c()
    
    # loop through each of the list elements to identify the difference
    for(i in 2:length(hist[[length(hist)]])){
      nl[i-1] <- hist[[length(hist)]][i] - hist[[length(hist)]][i-1]
    }
    
    # append the new sequence to the list
    hist <- append(hist,list(nl))
    
  }
  
  # append the initial 0 to the front of the final sequence
  hist[[length(hist)]] <- append(0, hist[[length(hist)]])
  
  # for each of the sequences to the top
  for(i in (length(hist)-1):1){
    hist[[i]] <- append((hist[[i]][1] - hist[[i+1]][1]), hist[[i]])
  }
  
  return(as.numeric(hist[[1]][1]))
  
}

f_day9_part2 <- function(df){
  
  # create a new column for the next number in each sequence
  df$PREV <- lapply(df$INPUT, f_day9_identify_prev_num)
  
  # return the sum of all the next numbers
  return(sum(as.numeric(df$PREV)))
  
}

# Output ------------------------------------------------------------------
f_day9_part1(df_test1)
f_day9_part1(df_input)

f_day9_part2(df_test1)
f_day9_part2(df_input)