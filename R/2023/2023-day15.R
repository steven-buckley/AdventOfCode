# https://adventofcode.com/2023/day/15


# Description -------------------------------------------------------------
# Part 1 looks straightforward
# Create function to calculate the hash value for a string
# Apply this to each comma seperated string in the input


# Load input --------------------------------------------------------------
df_test1 = read.table('./data/day-15-test1.txt',sep=',', col.names = "INPUT")
df_input = read.table('./data/day-15-input.txt',sep=',', col.names = "INPUT")

# Solution ----------------------------------------------------------------
f_day15_hash <- function(str){
  
  # initialise value
  hash_val <- 0
  
  # loop through each character in the string
  for(c in strsplit(str,"")[[1]]){
    # increment value by ascii value of character
    hash_val <- hash_val + utf8ToInt(c)
    # multiply by multiplier
    hash_val <- hash_val * 17
    # get remainder after dividing by 256
    hash_val <- hash_val %% 256
  }
  
  return(hash_val)
  
}
  
f_day15_part1 <- function(df){
  
  # update the df with the hash calculations
  df$HASH_VAL <- apply(df, 1, f_day15_hash)
  
  # return the sum of the HASH_VAL column
  return(sum(df$HASH_VAL))
}


# Output ------------------------------------------------------------------


f_day15_hash("HASH")
f_day15_part1(df_test1)
f_day15_part1(df_input)

