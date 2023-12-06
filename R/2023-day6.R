# https://adventofcode.com/2023/day/6

# Description -------------------------------------------------------------
# Seems straightforward to simulate each race for each of the different push times
# see how many of the race simulations beat the record and update accordingly


# Load input --------------------------------------------------------------
df_test1 = read.table('./data/day-6-test1.txt',sep='\t', col.names = "INPUT")
df_input = read.table('./data/day-6-input.txt',sep='\t', col.names = "INPUT")



# Solution Functions ------------------------------------------------------

f_day6_tidy_input <- function(df){
  
  # create a frame
  df_output <- data.frame(
    RACE_NUMBER = numeric(),
    RACE_TIME = numeric(),
    RACE_RECORD = numeric()
  )
  
  # check how many race inputs exist
  INPUT_LENGTH <- length(strsplit(gsub("\\s+", "-", df[1,1]),"-")[[1]])
  
  # add new records for each race
  for(i in 2:(INPUT_LENGTH)){
    df_output <- rbind(
      df_output, 
      data.frame(
        RACE_NUMBER = c(i-1),
        RACE_TIME = c(as.numeric(strsplit(gsub("\\s+", "-", df[1,1]),"-")[[1]][i])),
        RACE_RECORD = c(as.numeric(strsplit(gsub("\\s+", "-", df[2,1]),"-")[[1]][i]))
      )
    )
  }
  
  # append the number of winning scenarios based on the race simulations
  df_output <- df_output |> 
    dplyr::rowwise() |> 
    dplyr::mutate(WINS = f_day6_simulate_race_wins(RACE_TIME, RACE_RECORD))
  
  return(df_output)
}

f_day6_simulate_race_wins <- function(RACE_TIME, RACE_RECORD){
  
  # initialise race win counter
  WINS = 0
  
  # simulate all of the potential options
  for(j in 1:(RACE_TIME-1)){
    PUSH_TIME <- j
    TRAVEL_TIME <- RACE_TIME - PUSH_TIME
    RACE_DIST <- TRAVEL_TIME * PUSH_TIME
    if(RACE_DIST > RACE_RECORD){WINS <- WINS + 1}
  }
  
  return(WINS)
}


# Part 1 ------------------------------------------------------------------
# get the results and then get the product of all the wins
prod(f_day6_tidy_input(df_test1)[,"WINS"])

prod(f_day6_tidy_input(df_input)[,"WINS"])


# Part 2 ------------------------------------------------------------------
# can simply reuse the function to simulate the races passing the full details
f_day6_simulate_race_wins(
  as.numeric(strsplit(gsub("\\s+", "", df_test1[1,1]), ":")[[1]][2]),
  as.numeric(strsplit(gsub("\\s+", "", df_test1[2,1]), ":")[[1]][2])
)

f_day6_simulate_race_wins(
  as.numeric(strsplit(gsub("\\s+", "", df_input[1,1]), ":")[[1]][2]),
  as.numeric(strsplit(gsub("\\s+", "", df_input[2,1]), ":")[[1]][2])
)