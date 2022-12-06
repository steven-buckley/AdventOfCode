

# Description -------------------------------------------------------------
# https://adventofcode.com/2022/day/6


# Load input --------------------------------------------------------------
df_test1 <- "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
df_test2 <- "bvwbjplbgvbhsrlpgdmjqwftvncz"
df_test3 <- "nppdvjthqldpwncqszvftbrmjlhg"
df_test4 <- "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
df_test5 <- "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

df_input <- readr::read_file("./Data/Day6.txt")


# Solution ----------------------------------------------------------------

f_day6_a <- function(str){

  # split the string into a vector of characters
  v_chars <- as.vector(stringr::str_split_fixed(str, pattern = "", n = nchar(str)))
  
  # loop over each group of 4 characters
  for(x in 4:length(v_chars)){
    
    if ((length(v_chars[(x-3):x]) == length(table(v_chars[(x-3):x])))){
      return(x)
    }
  }
  
}

f_day6_b <- function(str){
  
  # split the string into a vector of characters
  v_chars <- as.vector(stringr::str_split_fixed(str, pattern = "", n = nchar(str)))
  
  # loop over each group of 4 characters
  for(x in 14:length(v_chars)){
    
    if ((length(v_chars[(x-13):x]) == length(table(v_chars[(x-13):x])))){
      return(x)
    }
  }
  
}

f_day6 <- function(str, mlen){
  
  # split the string into a vector of characters
  v_chars <- as.vector(stringr::str_split_fixed(str, pattern = "", n = nchar(str)))
  
  # loop over each group of 4 characters
  for(x in mlen:length(v_chars)){
    
    if ((length(v_chars[(x-(mlen-1)):x]) == length(table(v_chars[(x-(mlen-1)):x])))){
      return(x)
    }
  }
  
}


# Output ------------------------------------------------------------------
f_day6_a(df_test1) # 7
f_day6_a(df_test2) # 5
f_day6_a(df_test3) # 6
f_day6_a(df_test4) # 10
f_day6_a(df_test5) # 11

f_day6_b(df_test1) # 19
f_day6_b(df_test2) # 23
f_day6_b(df_test3) # 23
f_day6_b(df_test4) # 29
f_day6_b(df_test5) # 26

f_day6_a(df_input) # 1640
f_day6(df_input, 4)

f_day6_b(df_input) # 1640
f_day6(df_input, 14) # 3613