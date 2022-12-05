

# Description -------------------------------------------------------------
# https://adventofcode.com/2022/day/5


# Load input --------------------------------------------------------------
test_input = list(c("Z","N"),
             c("M","C","D"),
             c("P")
             )
df_test_movelist <- data.frame(text = c("move 1 from 2 to 1",
                                        "move 3 from 1 to 3",
                                        "move 2 from 2 to 1",
                                        "move 1 from 1 to 2"
                                        )
                               )

day5_input <- list(
  c("B","S","V","Z","G","P","W"),
  c("J","V","B","C","Z","F"),
  c("V","L","M","H","N","Z","D","C"),
  c("L","D","M","Z","P","F","J","B"),
  c("V","F","C","G","J","B","Q","H"),
  c("G","F","Q","T","S","L","B"),
  c("L","G","C","Z","V"),
  c("N","L","G"),
  c("J","F","H","C")
)

day5_movelist <- read.csv("./Data/Day5_moves.csv",
                          header = FALSE,
                          col.names = "text"
)


# Solution ----------------------------------------------------------------
# cleanse the movelist
f_day5_cleanmoves <- function(df){
  df |> 
  dplyr::mutate(moves = as.numeric(sub(" .*", "", sub("move ", "", text))),
                from = as.numeric(sub(" .*","", sub(".*from ", "", text))),
                to = as.numeric(sub(" .*","", sub(".*to ", "", text)))
                )
  
}

# function to move boxes : one box at a time from top of stack
f_day5_move_single <- function(stacks, moves, from, to){
  
  # repeat action for x moves
  for(x in 1:moves){
    # add the "box" to the back of the "to" stack
    stacks[[to]] <- append(stacks[[to]], stacks[[from]][length(stacks[[from]])])
    # remove the "box" from the "from" stack
    stacks[[from]] <- stacks[[from]][-length(stacks[[from]])]
  }
  
  # pass back the updated input
  return(stacks)
}

# function to complete part A of problem
f_day5_a <- function(stacks, df_moves){
  # call function to clean dataframe
  df_moves <- f_day5_cleanmoves(df_moves)
  
  # call function to apply moves
  for(x in 1:nrow(df_moves)){
    stacks <- f_day5_move_single(stacks, df_moves$moves[[x]], df_moves$from[[x]], df_moves$to[[x]])
  }
  
  # return the final output
  return(stacks)
}

# function to move boxes : x boxes at a time in stack order
f_day5_move_multi <- function(stacks, moves, from, to){
  
  # move a stack of boxes in the current order
  stacks[[to]] <- append(stacks[[to]], stacks[[from]][(length(stacks[[from]])-moves+1):(length(stacks[[from]]))])
  
  # repeat action for x moves
  for(x in 1:moves){
    # remove the "box" from the "from" stack
    stacks[[from]] <- stacks[[from]][-length(stacks[[from]])]
  }
  
  # pass back the updated input
  return(stacks)
}

# function to complete part B of problem
f_day5_b <- function(stacks, df_moves){
  # call function to clean dataframe
  df_moves <- f_day5_cleanmoves(df_moves)
  
  # call function to apply moves
  for(x in 1:nrow(df_moves)){
    stacks <- f_day5_move_multi(stacks, df_moves$moves[[x]], df_moves$from[[x]], df_moves$to[[x]])
  }
  
  # return the final output
  return(stacks)
}

# Output ------------------------------------------------------------------
test <- f_day5_a(test_input, df_test_movelist) #CMZ
test_b <- f_day5_b(test_input, df_test_movelist) #MCD

day5_a <- f_day5_a(day5_input, day5_movelist) #VJSFHWGFT
day5_b <- f_day5_b(day5_input, day5_movelist) #LCTQFBVZV
