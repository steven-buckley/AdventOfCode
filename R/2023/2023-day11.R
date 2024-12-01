# https://adventofcode.com/2023/day/11


# Description -------------------------------------------------------------



# Load input --------------------------------------------------------------
df_test1 <- as.matrix(do.call(rbind,strsplit(readr::read_lines('./data/day-11-test1.txt'),'',fixed=T)), stringsAsFactors = FALSE)
df_input <- as.matrix(do.call(rbind,strsplit(readr::read_lines('./data/day-11-input.txt'),'',fixed=T)), stringsAsFactors = FALSE)


# Solution ----------------------------------------------------------------


f_day11_part1 <- function(m){
  
  # replace the characters in the matrix with numeric values
  m <- gsub("#",1,m)
  m <- gsub("\\.",0,m)
  
  # convert class for matrix
  class(m) <- "numeric"
  
  # loop through each row in the matrix and append to a new matrix
  for(r in 1:nrow(m)){
    
    # add the row to a new dataframe
    if(r == 1){
      m2 <- m[r,]
    } else {
      m2 <- rbind(m2, m[r,])
    }
    
    # check for empty rows and add them again
    if(sum(m[r,])==0){
      m2 <- rbind(m2, m[r,])
    }
    
  }
  
  # loop through each column in the matrix and append to a new matrix
  for(c in 1:ncol(m2)){
    
    # add the column to a new dataframe
    if(c == 1){
      m3 <- m2[,c]
    } else {
      m3 <- rbind(m3, m2[,c])
    }
    
    # check for empty rows and add them again
    if(sum(m[,c])==0){
      m3 <- rbind(m3, m2[,c])
    }
    
  }
  
  # identify the reference points of all galaxies (where cell is 1)
  output <- data.frame(which(m3 == 1, arr.ind = TRUE)) |> 
    dplyr::mutate(
      SID = dplyr::row_number(),
      SROW = row,
      SCOL = col
    ) |> 
    dplyr::select(SID, SROW, SCOL)
  
  # join the output to itself to get all combinations
  output <- output |>
    dplyr::cross_join(
      y = output |> dplyr::rename(DID = SID, DROW = SROW, DCOL = SCOL)
    ) |> 
    # identify unique combinations of different points
    dplyr::filter(SID != DID) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      MIN = min(SID, DID),
      MAX = max(SID, DID)
    ) |> 
    dplyr::group_by(MIN,MAX) |> 
    dplyr::mutate(PAIR_RANK = dplyr::row_number()) |> 
    dplyr::ungroup() |> 
    dplyr::filter(PAIR_RANK == 1) |> 
    # calculate shortest distance between each pair of points
    dplyr::mutate(DIST = abs(SROW - DROW) + abs(SCOL - DCOL))
  
  return(output)
}


f_day11_part2 <- function(m, multiplier){
# getting messy with counting rows multiple times
# find a better way

  # find which rows need expanding
  emptyrows <- which(rowSums(m == '#') == 0)
  emptycols <- which(colSums(m == '#') == 0)
  
  # identify the reference points of all galaxies (where cell is 1)
  output <- data.frame(which(m == '#', arr.ind = TRUE)) |> 
    dplyr::mutate(
      SID = dplyr::row_number(),
      SROW = row,
      SCOL = col
    ) |> 
    dplyr::select(SID, SROW, SCOL)
  
  # join the output to itself to get all combinations
  output <- output |>
    dplyr::cross_join(
      y = output |> dplyr::rename(DID = SID, DROW = SROW, DCOL = SCOL)
    ) |> 
    # identify unique combinations of different points
    dplyr::filter(SID != DID) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      MIN = min(SID, DID),
      MAX = max(SID, DID)
    ) |> 
    dplyr::group_by(MIN,MAX) |> 
    dplyr::mutate(PAIR_RANK = dplyr::row_number()) |> 
    dplyr::ungroup() |> 
    dplyr::filter(PAIR_RANK == 1) |> 
    dplyr::rowwise() |> 
    # calculate shortest distance between each pair of points
    dplyr::mutate(
      # number of steps
      DIST = abs(SROW - DROW) + abs(SCOL - DCOL),
      # deduct rows that have expanded
      DIST = DIST - sum(emptyrows %in% seq(SROW,DROW)),
      # replace with number of expanded rows by multiplier
      DIST = DIST + (sum(emptyrows %in% seq(SROW,DROW))*multiplier),
      # deduct cols that have expanded
      DIST = DIST - sum(emptycols %in% seq(SCOL,DCOL)),
      # replace with number of expanded cols by multiplier
      DIST = DIST + (sum(emptycols %in% seq(SCOL,DCOL))*multiplier)
      )
  
  return(output)

}


# Output ------------------------------------------------------------------
sum(f_day11_part1(df_test1)$DIST)
sum(f_day11_part1(df_input)$DIST)

sum(f_day11_part2(df_test1,10)$DIST)
sum(f_day11_part2(df_test1,100)$DIST)
sum(f_day11_part2(df_input,1000000)$DIST)

