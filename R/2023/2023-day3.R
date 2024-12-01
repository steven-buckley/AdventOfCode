# https://adventofcode.com/2023/day/2


# Load packages -----------------------------------------------------------
library(dplyr)
library(readr)

# Description -------------------------------------------------------------
# data is in a matrix so read it in so each character has its own row/column
# cycle through each entry to classify the entrys
# if it is a symbol record this and the location reference
# if it is a number check next cell(s) to get the full number and save this along with the start and end column
# for all numbers check if it is valid by checking the cells around it for symbols
#
# for part 2, gears are identified as the * symbol adjacent to exactly two part numbers
# the gear ratio for a gear is the product of the two adjacent part numbers
# already classified the symbols and parts so just need to check for part numbers next to a *


# Load input --------------------------------------------------------------
df_test1 <- as.data.frame(do.call(rbind,strsplit(readr::read_lines('./data/day-3-test1.txt'),'',fixed=T)), stringsAsFactors = FALSE)
df_input <- as.data.frame(do.call(rbind,strsplit(readr::read_lines('./data/day-3-input.txt'),'',fixed=T)), stringsAsFactors = FALSE)

# Solution ----------------------------------------------------------------
f_day3_classify_matrix <- function (df){
  
  # add a frame around the data so don't have to worry about hitting edges
  
  #add a new coluumn at start and end
  df$first = "."
  df$flast = "."
  df <- df |> 
    dplyr::select(first,everything())
  
  # add new row at the end
  df[nrow(df)+1,] <- "."
  
  # add new row at top (create new df)
  df <- rbind(
    df[nrow(df),],
    df
  )
  
  # rename rows and columns
  rownames(df) <- 1:nrow(df)
  colnames(df) <- 1:ncol(df)
  
  # create an output dataframe to hold classifications of data
  df_results <- data.frame(
    TYPE =  character(),
    PART_NUM = numeric(),
    VALID_PART = character(),
    SYMBOL = character(),
    ROW_ID = numeric(), 
    SCOL_ID = numeric(),
    ECOL_ID = numeric(),
    stringsAsFactors = FALSE
  ) 
  
  # cycle through the matrix
  for(r in 1:nrow(df)){
    
    # start a counter for the column reference
    c <- 1
    
    # cycle through the columns until the last column in the data
    while(c <= ncol(df)){
      
      # check the contents : action required if not "."
      val <- df[r,c]
      if(val != "."){
        # check if the value is a number
        if(val %in% c(0,1,2,3,4,5,6,7,8,9)){
          
          # create variable to hold the part number
          pnum <- ''
          
          # set variable for part number start column
          pst <- c
          
          # continue along the row appending each numeric in sequence
          while(df[r,c] %in% c(0,1,2,3,4,5,6,7,8,9)){
            
            # append the number to the part number
            pnum <- paste0(pnum,df[r,c])
            
            # increment to the next column
            c <- c + 1
            
          }
          
          # reduce the column increment as stepped too far during while loop
          c <- c-1
          
          # call function to check if part is valid
          val_part <- f_day3_checkpart(df, r, pst, c)
          
          # store the details of part number
          df_results <- rbind(df_results, setNames(data.frame('PART',as.numeric(pnum),val_part,NA,r,pst,c),names(df_results)))
          
        } else {
          
          # store the details of the symbols
          df_results <- rbind(df_results, setNames(data.frame('SYM',NA,NA,val,r,c,c),names(df_results)))
          
        }
      }
      
      # increment the counter
      c <- c + 1
      
    }
  }
  
  # return the dataframe
  return(df_results)
}

f_day3_checkpart <- function(df, row_id, start_col, end_col){
  
  # default is that a entry is not a valid part
  val_part = "N"
  
  # cycle through the row above and below the one with the number
  for(chkr in (row_id-1):(row_id+1)){
    
    # cycle through the columns from one before to one after number
    for(chkc in (start_col-1):(end_col+1)){
      
      # check if the value is a symbol (not number or .)
      if(!(df[chkr,chkc] %in% c(".",1,2,3,4,5,6,7,8,9,0))){
        val_part = "Y"
      }
    }
  }
  
  return(val_part)
  
}

f_day3_gear_ratio <- function(df_parts, row_id, col_id){
  
  # filter the supplied parts list to only include those aligned with the coords
  # need to consider the length of a number as could be adjacent to any part of it
  df_parts <- df_parts |> 
    dplyr::filter(ROW_ID >= (row_id - 1) & ROW_ID <= (row_id + 1)) |> 
    dplyr::filter((SCOL_ID -1) <= col_id & (ECOL_ID + 1) >= col_id)
  
  # check the number of adjacent parts and if exactly 2 get the product of the part numbers
  if(nrow(df_parts) == 2){
    gear_ratio <- df_parts[1,"PART_NUM"] * df_parts[2,"PART_NUM"]
  } else {
    gear_ratio <- 0
  }
  
  return(gear_ratio)
    
}


f_day3_part1 <- function(df){
  
  f_day3_classify_matrix(df) |>
    dplyr::filter(VALID_PART == "Y")|> 
    dplyr::summarise(sum(PART_NUM))
}

f_day3_part2 <- function(df){
  
  # summarise the supplied matrix to get parts and gear symbols
  df_summary <- f_day3_classify_matrix(df)
  
  # isolate the part list
  df_parts <- df_summary |> 
    dplyr::filter(VALID_PART == 'Y')
  
  # isolate the gears and calculate the ratios by supply details to the function
  df_gears <- df_summary |> 
    dplyr::filter(SYMBOL == "*") |> 
    dplyr::rowwise() |> 
    dplyr::mutate(GEAR_RATIO = f_day3_gear_ratio(df_parts, ROW_ID, SCOL_ID)) |> 
    dplyr::ungroup() |> 
    dplyr::summarise(sum(GEAR_RATIO))
  
  return(df_gears)
}


# Output ------------------------------------------------------------------
f_day3_part1(df_test1)
f_day3_part1(df_input)
f_day3_part2(df_test1)
f_day3_part2(df_input)