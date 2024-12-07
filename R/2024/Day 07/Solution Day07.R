# https://adventofcode.com/2024/day/7


# Description -------------------------------------------------------------
# Part 1
# Spilt input into result and number combination
# split the numbers and merge with all combinations of operators
# use brackets to ensure step-by-step calculations rather than precedence
#
# Part 2
# amended part 1 but misunderstood the brief
# abandoned as past self-imposed time limit
# would have been simply if set up calculation to run step by step instead of single formula


# Load Packages -------------------------------------------------------------
library(dplyr)
library(stringr)
library(DescTools)

# Load input --------------------------------------------------------------
df_example = read.table('./data/2024/Day 07/input_example_day_07.txt',sep='\t', col.names = "INPUT")
df_part1 = read.table('./data/2024/Day 07/input_part1_day_07.txt',sep='\t', col.names = "INPUT")


# Support Functions -------------------------------------------------------

f_generate_operator_combinations <- function(operators, entries){
  # create a list of permutations of supplied operators
  return(DescTools::CombSet(operators, entries, repl=TRUE, ord=TRUE, as.list = TRUE))
}


f_check_possible_calculations <- function(RESULT, CALC_NUMS, OPLIST = c("+","*")){
  
  # replace spaces with ) to ensure step by step evaluation
  CALC_NUMS <- stringr::str_replace_all(CALC_NUMS," ",") ")
  
  # convert string to vector
  CALC_NUMS <- unlist(stringr::str_split(CALC_NUMS," "))
  
  # get list of operator combinations
  CALC_OPS <- f_generate_operator_combinations(OPLIST,length(CALC_NUMS)-1)
  
  # test the possible calculations
  for(i in 1:length(CALC_OPS)){
    
    # create the calculation by combining the vector or numbers with the permuations
    T_CALC <- paste(c(rbind(CALC_NUMS,c(CALC_OPS[[i]],""))),collapse = "")
    
    # check for the concatenate (using c instead of || for now)
    T_CALC <- stringr::str_replace_all(T_CALC,"\\)c","")
    
    # wrap in opening/closing brackets
    CALC <- eval(
      rlang::parse_expr(
        paste0(
          paste0(rep("(",stringr::str_count(T_CALC,"\\)")+1),collapse = ""),
          T_CALC,
          ")"
        )
      )
    )
    
    if(RESULT == CALC){
      return(TRUE)  
    }
  }
  
  return(FALSE)
}




# Solution Part 1 ---------------------------------------------------------

f_day07_solution <- function(df, OPLIST = c("+","*")){

  # split the data into result and calculation numbers
  df <- df |> 
    dplyr::rowwise() |>
    dplyr::mutate(
      RESULT = as.numeric(stringr::str_split(INPUT,": ")[[1]][1]),
      CALC_NUMS = stringr::str_split(INPUT,": ")[[1]][2],
    ) |> 
    # perform the possible calculations to check if any are true
    dplyr::mutate(
      VALID = f_check_possible_calculations(RESULT, CALC_NUMS, OPLIST)
    )
  
  return(sum(df$RESULT[df$VALID]))
  
}





# Output ------------------------------------------------------------------
f_day07_solution(df_example, OPLIST = c("+","*"))
p1 <- f_day07_solution(df_part1)

f_day07_solution(df_example, OPLIST = c("+","*","c"))
p2 <- f_day07_solution(df_part1)

