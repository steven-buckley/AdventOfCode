# https://adventofcode.com/2023/day/2


# Load packages -----------------------------------------------------------
library(stringr)


# Load input --------------------------------------------------------------
df_input = read.table('./data/day-2-input.txt',sep='\t', col.names = "INPUT")
df_test1 = read.table('./data/day-2-test1.txt',sep='\t', col.names = "INPUT")
df_test2 = read.table('./data/day-1-test2.txt',sep='\t', col.names = "INPUT")


# Recreate game -----------------------------------------------------------
# each game has a number of turns
# each turn pulls a random combination of red, green and blue balls from bag
# balls are returned
# need to know which games are possible with a bag of 12 red, 13 green and 14 blue
# take these as parameters in case the rules change
# part 2, need to know the product of the maximum number of each colour pulled in a game

f_day2_game <- function(df, max_r = 0, max_g = 0, max_b = 0){
  # create a function to play the game at retain the outcomes
  
  # add game id and placeholder for results
  df$GAME <- seq.int(nrow(df))
  df$OUTCOME <- FALSE
  
  # iterate through each game (row in data)
  for(game in 1:nrow(df)){
    
    # reset counters
    gcubes <- 0
    rcubes <- 0
    bcubes <- 0
    
    # split all the turns in the game
    turns <- strsplit(df[game,1], ";")
    
    # iterate through each turn (draw of group of colour cubes)
    for(turn in 1:lengths(turns)){
      
      # split all the groups in the turn
      grps <- strsplit(turns[[1]][turn], ",")
      
      # iterate through each group
      for(grp in 1:lengths(grps)){
        
        # clear out the game label if required
        chk <- gsub(".*:","",grps[[1]][grp])
        
        # check the colour for the current group
        # check the value and replace counter if larger
        if(grepl("red", chk, fixed = TRUE)){
          rtmp <- as.numeric(gsub("\\D","", chk))
          if(rtmp > rcubes){rcubes <- rtmp}
        }
        if(grepl("green", chk, fixed = TRUE)){
          gtmp <- as.numeric(gsub("\\D","", chk))
          if(gtmp > gcubes){gcubes <- gtmp}
        }
        if(grepl("blue", chk, fixed = TRUE)){
          btmp <- as.numeric(gsub("\\D","", chk))
          if(btmp > bcubes){bcubes <- btmp}
        }
        
      }
    }
    
    # append max colour counts to data
    df[game,"RED"] <- rcubes
    df[game,"GREEN"] <- gcubes
    df[game,"BLUE"] <- bcubes
    
    df[game,"PRODUCT"] <- df[game,"RED"]  * df[game,"GREEN"] * df[game,"BLUE"]
    
    # check if the counters have exceeded the threshold
    if(gcubes <= max_g & rcubes <= max_r & bcubes <= max_b){
      df[game,"OUTCOME"] = TRUE
    }
  }
  
  # return the updated dataframe
  return(df)

}

f_day2_part1 <- function(df){
  # play the game for the input
  # filter results to only outcomes that were possible based on supplied max values
  # sum up game IDs of valid games
  
  f_day2_game(df, max_r = 12, max_g = 13, max_b = 14) |> 
    dplyr::filter(OUTCOME == TRUE) |> 
    dplyr::summarise(sum(GAME))
  
}

f_day2_part2 <- function(df){
  # play the game for the input
  # sum up the products of the maximum number of each ball pulled in any turn
  
  f_day2_game(df) |> 
    dplyr::summarise(sum(PRODUCT))
  
}



# Output ------------------------------------------------------------------

f_day2_part1(df_test1)
f_day2_part1(df_input)
f_day2_part2(df_test1)
f_day2_part2(df_input)
