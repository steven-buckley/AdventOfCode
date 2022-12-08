

# Description -------------------------------------------------------------
# https://adventofcode.com/2022/day/8


# Load input --------------------------------------------------------------
# input data already split into individual columns in csv files

df_day8_test <- read.csv("./Data/Day8_test.csv",
                         header = FALSE,
                         colClasses = c("numeric")
)

df_day8_input <- read.csv("./Data/Day8.csv",
                          header = FALSE,
                          colClasses = c("numeric")
                          )



# Solution ----------------------------------------------------------------
f_day8_a <- function(df_i){
  
  # count the size of the tree grid
  gridw <- nrow(df_i)
  gridh <- ncol(df_i)
  
  # count trees in outside of grid
  treecount <- (gridw*2)+(gridh*2)-4
  
  # loop through all the interior trees
  # loop rows
  for(r in 2:(gridw-1)){
    # loop cols
    for(c in 2:(gridh-1)){
      # identify the tree size
      treesize = df_i[r,c]
      
      # compare against trees in each direction
      max_n = max(df_i[1:(r-1), c])
      max_e = max(df_i[r, (c+1):gridw])
      max_s = max(df_i[(r+1):gridh, c])
      max_w = max(df_i[r, 1:(c-1)])
      
      # if the tree size is larger than the max in any other direction
      if(treesize >  max_n |
         treesize >  max_e |
         treesize >  max_s |
         treesize >  max_w 
      ){
        # increment the tree counter
        treecount <- treecount + 1
      }
    }
  }
  
  return(treecount)
}


f_day8_b <- function(df_i){
  # count the size of the tree grid
  gridw <- nrow(df_i)
  gridh <- ncol(df_i)
  
  # initialise score
  max_viewscore = 0
  
  # loop through all the interior trees
  # loop rows
  for(r in 2:(gridw-1)){
    # loop cols
    for(c in 2:(gridh-1)){
      # identify the tree size
      treesize = df_i[r,c]
      # initialise counters
      treecount_n = 0
      treecount_s = 0
      treecount_e = 0
      treecount_w = 0
      
      # look North
      for(n in (r-1):1){
        if(df_i[n,c] < treesize){
          treecount_n = treecount_n + 1
        } else {
          treecount_n = treecount_n + 1
          break
        }
      }
      
      # look South
      for(s in (r+1):gridh){
        if(df_i[s,c] < treesize){
          treecount_s = treecount_s + 1
        } else {
          treecount_s = treecount_s + 1
          break
        }
      }
      
      # look East
      for(e in (c+1):gridw){
        if(df_i[r,e] < treesize){
          treecount_e = treecount_e + 1
        } else {
          treecount_e = treecount_e + 1
          break
        }
      }
      
      # look West
      for(w in (c-1):1){
        if(df_i[r,w] < treesize){
          treecount_w = treecount_w + 1
        } else {
          treecount_w = treecount_w + 1
          break
        }
      }
      
      # calculate the viewscore
      viewscore = (treecount_n * treecount_s * treecount_e * treecount_w)
      if(viewscore > max_viewscore){max_viewscore = viewscore}
      
    }
  }
  
  return(max_viewscore)
}




# Output ------------------------------------------------------------------
f_day8_a(df_day8_test) # 21
f_day8_a(df_day8_input) # 1736

f_day8_b(df_day8_test) # 8
f_day8_b(df_day8_input) # 268800


