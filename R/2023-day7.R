# https://adventofcode.com/2023/day/7


# Description -------------------------------------------------------------



# Load input --------------------------------------------------------------
df_test1 = read.table('./data/day-7-test1.txt',sep='\t', col.names = "INPUT")
df_input = read.table('./data/day-7-input.txt',sep='\t', col.names = "INPUT")

# Solution ----------------------------------------------------------------
f_day7_get_card_value <- function(cnum, acehigh = TRUE, jacks_wild = FALSE){
# function to assign a value for a card (handle ace high/low)  
  if(cnum == 'A'){
    CVAL = ifelse(acehigh == TRUE, 13, 1)
  } else if(cnum == 'K'){
    CVAL = ifelse(acehigh == TRUE, 12, 13)
  } else if(cnum == 'Q'){
    CVAL = ifelse(acehigh == TRUE, 11, 12)
  } else if(cnum == 'J'){
    CVAL = ifelse(jacks_wild == TRUE, 0, ifelse(acehigh == TRUE, 10, 11))
  } else if(cnum == 'T'){
    CVAL = ifelse(acehigh == TRUE, 9, 10)
  } else {
    CVAL = ifelse(acehigh == TRUE, as.numeric(cnum)-1, as.numeric(cnum))
  }
  
  return(CVAL)
  
}


f_day7_identify_hand_type <- function(cardhand, jacks_wild = FALSE){
  
  # initialise a vector to store a count of each card
  CCOUNT <- c(rep(0,13))
  
  if(jacks_wild){
    # strip out any jacks (J) from the cardhand
    jacks = gsub("[^J]+","",cardhand)
    cardhand = gsub("J","",cardhand)
  }
  
  
  # loop through each card in the hand and assign a value to update the card count
  for(c in 1:nchar(cardhand)){
    
    CVAL = f_day7_get_card_value(substr(cardhand,c,c), FALSE)
    
    CCOUNT[CVAL] <- CCOUNT[CVAL] + 1
    
  }
  
  cardhand <- paste(sort(CCOUNT[CCOUNT != 0], decreasing = TRUE), collapse = "")
  
  if(jacks_wild){
    # combine out any jacks (J) from the cardhand
    cardhand = paste0(jacks,cardhand)
  }
  
  # combine the card counts (excluding 0) to get a pattern of cards
  HAND_TYPE <- switch(
    cardhand,
    "5" = "1 : Five of a kind",
    "JJJJJ" = "1 : Five of a kind",
    "J4" = "1 : Five of a kind",
    "JJ3" = "1 : Five of a kind",
    "JJJ2" = "1 : Five of a kind",
    "JJJJ1" = "1 : Five of a kind",
    "41" = "2 : Four of a kind",
    "J31" = "2 : Four of a kind",
    "JJ21" = "2 : Four of a kind",
    "JJJ11" = "2 : Four of a kind",
    "32" = "3 : Full house",
    "J22" = "3 : Full house",
    "311" = "4 : Three of a kind",
    "J211" = "4 : Three of a kind",
    "JJ111" = "4 : Three of a kind",
    "221" = "5 : Two pair",
    "2111" = "6: One pair",
    "J1111" = "6: One pair",
    "11111" = "7 : High card"
  )
  
  return(HAND_TYPE)
}


f_day7 <- function(df, jw = FALSE){
  
  df <- df |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      HAND = strsplit(INPUT, " ")[[1]][1],
      WAGER = as.numeric(strsplit(INPUT, " ")[[1]][2]),
      HANDTYPE = f_day7_identify_hand_type(HAND, jacks_wild = jw),
      C1_VAL = f_day7_get_card_value(substr(HAND,1,1), jacks_wild = jw),
      C2_VAL = f_day7_get_card_value(substr(HAND,2,2), jacks_wild = jw),
      C3_VAL = f_day7_get_card_value(substr(HAND,3,3), jacks_wild = jw),
      C4_VAL = f_day7_get_card_value(substr(HAND,4,4), jacks_wild = jw),
      C5_VAL = f_day7_get_card_value(substr(HAND,5,5), jacks_wild = jw)
    ) |> 
    dplyr::arrange(
      desc(HANDTYPE), 
      C1_VAL,
      C2_VAL,  
      C3_VAL,
      C4_VAL,
      C5_VAL
    ) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      HAND_RANK = dplyr::row_number(),
      WINNINGS = WAGER * HAND_RANK
    )
  
  return(df)
}


# Output ------------------------------------------------------------------
sum(f_day7(df_test1)$WINNINGS)
sum(f_day7(df_input)$WINNINGS)

sum(f_day7(df_test1, jw = TRUE)$WINNINGS)
sum(f_day7(df_input, jw = TRUE)$WINNINGS)

