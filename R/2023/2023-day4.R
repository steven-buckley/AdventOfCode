# https://adventofcode.com/2023/day/2


# Load packages -----------------------------------------------------------


# Description -------------------------------------------------------------
# the input has details of scratchcards in format card_num : winning_numbers | your_numbers
# data can include additional whitespace
# need to check how many numbers are winners on each card
# the card value is 2 ^ (winning numbers - 1)
#
# for winning cards you need to update the number of cards below it equal to the number of winners
# the card count should be updated by the number of winning cards above



# Load input --------------------------------------------------------------
df_input = read.table('./data/day-4-input.txt',sep='\t', col.names = "INPUT")
df_test1 = read.table('./data/day-4-test1.txt',sep='\t', col.names = "INPUT")


# Solution ----------------------------------------------------------------
f_day4_part1 <- function(df){
  
  # split the entry into two parts, the winning numbers and your numbers
  df <- df |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      # for card number take everything before : and convert digits to number
      CARD_NUMBER = as.numeric(gsub("\\D","", strsplit(INPUT, ":")[[1]][1])),
      # for winning numbers take everything between : and |, trim whitespace and remove multiple spaces
      WINNING_NUMBERS = gsub("\\s+", ",", trimws(strsplit(strsplit(INPUT, ":")[[1]][2], "\\|")[[1]][1])),
      # for numbers to check take everything efte the |, trim whitespace and remove multiple spaces
      YOUR_NUMBERS = gsub("\\s+", ",", trimws(strsplit(INPUT, "\\|")[[1]][2]))
    ) |> 
    dplyr::mutate(NUM_WINNERS = f_day4_check_winners(WINNING_NUMBERS, YOUR_NUMBERS),
                  CARD_VALUE = ifelse(NUM_WINNERS == 0, 0, 2^(NUM_WINNERS-1))
                  )
  
  return(df)
  
}

f_day4_check_winners <- function(str_win, str_chk){
  # take two strings of numbers and check how many numbers from one list appear in the other
  
  # split the strings into lists of numbers
  l_check <- strsplit(str_chk, ",")[[1]]
  l_winner <- strsplit(str_win, ",")[[1]]
  
  # initialise a win counter
  wincount <- 0
  
  # loop through each of the numbers to check
  for(chk in l_check){
    # check if the number is in the list of winning numbers and update counter
    if(chk %in% l_winner){wincount <- wincount + 1}
  }
  
  return(wincount)
  
}

f_day4_part2<- function(df){
  
  # call the part1 function to set up the details of the winners
  df <- f_day4_part1(df)
  
  # add a new column for count of cards
  df$CARD_COUNT = 1
  
  # loop through each of the cards
  for(i in 1:nrow(df)){
    
    # check the number of winning numbers
    NUM_WINNERS = df[[i, "NUM_WINNERS"]]
    C_COUNT = df[[i, "CARD_COUNT"]]
    
    # if the card is a winner
    if(NUM_WINNERS > 0){
      # iterate for each of the next cards based on number of winners
      for(nw in 1:NUM_WINNERS)
        # increment the card count by the number of winning cards
        df[(i+nw),"CARD_COUNT"] <- df[[(i+nw),"CARD_COUNT"]] + C_COUNT
    }
  }
  
  return(df)
  
}

# Output ------------------------------------------------------------------
sum(f_day4_part1(df_test1)$CARD_VALUE)
sum(f_day4_part1(df_input)$CARD_VALUE)

sum(f_day4_part2(df_test1)$CARD_COUNT)
sum(f_day4_part2(df_input)$CARD_COUNT)