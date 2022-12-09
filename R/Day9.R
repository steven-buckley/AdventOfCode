

# Description -------------------------------------------------------------
# https://adventofcode.com/2022/day/9


# Load input --------------------------------------------------------------
df_day9_test <- read.csv("./Data/Day9_test.csv",
                         header = TRUE,
                         colClasses = c("character","numeric")
)

df_day9_input <- read.csv("./Data/Day9.csv",
                          header = TRUE,
                          colClasses = c("character","numeric")
)

df_day9_test_b <- read.csv("./Data/Day9_test_b.csv",
                         header = TRUE,
                         colClasses = c("character","numeric")
)


# Solution: Part A --------------------------------------------------------

f_day9_a <- function(df_i){
  # initialise positions
  # HEAD (H) relative position (x,y)
  Hx <- 0
  Hy <- 0
  # TAIL (T) relative position (x,y)
  Tx <- 0
  Ty <- 0
  # TAIL position log
  Tpl <- data.frame(x = Tx, y = Ty)
  
  # loop through the instructions
  for(i in 1:nrow(df_i)){
    # repeat for the required distance
    for(m in 1:df_i$distance[i]){
      # move HEAD in the required direction
      switch(df_i$direction[i],
             "U" = {Hy = Hy + 1},
             "D" = {Hy = Hy - 1},
             "R" = {Hx = Hx + 1},
             "L" = {Hx = Hx - 1}
      )
      # move TAIL, if required, in the required direction
      switch(paste0(Hx-Tx,",",Hy-Ty),
             # move horizontal or vertical
             "0,2" = {Ty = Ty + 1}, # move 1 step up
             "0,-2"= {Ty = Ty - 1}, # move 1 step down
             "2,0" = {Tx = Tx + 1}, # move 1 step right
             "-2,0"= {Tx = Tx - 1}, # move 1 step left
             # move on diagonal
             "1,2" = {Tx = Tx + 1
             Ty = Ty + 1},
             "2,1" = {Tx = Tx + 1
             Ty = Ty + 1},
             "-1,2"= {Tx = Tx - 1
             Ty = Ty + 1},
             "-2,1"= {Tx = Tx - 1
             Ty = Ty + 1},
             "-2,-1"= {Tx = Tx - 1
             Ty = Ty - 1},
             "-1,-2"= {Tx = Tx - 1
             Ty = Ty - 1},
             "1,-2" = {Tx = Tx + 1
             Ty = Ty - 1},
             "2,-1" = {Tx = Tx + 1
             Ty = Ty - 1}
      )
      # add the TAIL position to the tracker
      Tpl = rbind(Tpl, data.frame(x = Tx, y = Ty))  
    }
  }
  
  # return the number of distinct tail position list
  return(nrow(unique(Tpl)))
}

# Solution: Part B --------------------------------------------------------
# The HEAD still moves the same but the body includes multiple knots for the TAIL
# Each knot will act the same way as the HEAD/TAIL previously
# Create a function to move the TAIL based on knot ahead of it in pairs

# create function to move tail, following parent know (or HEAD)
# allows it be called called for each knot stage
# need to add direct diagonal moves
f_movetail <- function(df_pos, tpos){
  # HEAD (H) relative position (x,y)
  Hx <- df_pos$x[df_pos$id == (tpos-1)]
  Hy <- df_pos$y[df_pos$id == (tpos-1)]
  # TAIL (T) relative position (x,y)
  Tx <- df_pos$x[df_pos$id == tpos]
  Ty <- df_pos$y[df_pos$id == tpos]
  # move TAIL, if required, in the required direction
  switch(paste0(Hx-Tx,",",Hy-Ty),
         "0,2" = {Ty = Ty + 1}, # move 1 step up
         "0,-2"= {Ty = Ty - 1}, # move 1 step down
         "2,0" = {Tx = Tx + 1}, # move 1 step right
         "-2,0"= {Tx = Tx - 1}, # move 1 step left
         "1,2" = {Tx = Tx + 1
         Ty = Ty + 1},
         "2,1" = {Tx = Tx + 1
         Ty = Ty + 1},
         "2,2" = {Tx = Tx + 1
         Ty = Ty + 1},
         "-1,2"= {Tx = Tx - 1
         Ty = Ty + 1},
         "-2,1"= {Tx = Tx - 1
         Ty = Ty + 1},
         "-2,2"= {Tx = Tx - 1
         Ty = Ty + 1},
         "-2,-1"= {Tx = Tx - 1
         Ty = Ty - 1},
         "-1,-2"= {Tx = Tx - 1
         Ty = Ty - 1},
         "-2,-2"= {Tx = Tx - 1
         Ty = Ty - 1},
         "1,-2" = {Tx = Tx + 1
         Ty = Ty - 1},
         "2,-1" = {Tx = Tx + 1
         Ty = Ty - 1},
         "2,-2" = {Tx = Tx + 1
         Ty = Ty - 1}
  )
  # update the tail reference
  df_pos$x[df_pos$id == tpos] <- Tx
  df_pos$y[df_pos$id == tpos] <- Ty
  # return update reference
  return(df_pos)
  
}

# new process function that calls f_movetail
f_day9 <- function(df_i, tlength){
  
  # create frame to store relative positions of head and each knot
  df_pos <- data.frame(id = c(seq(0,tlength)),
                       x = c(rep(0,tlength+1)),
                       y = c(rep(0,tlength+1))
  )

  # TAIL position log tracker : start at 0,0
  Tpl <- data.frame(x = 0, y = 0)
  
  # loop through the instructions
  for(i in 1:nrow(df_i)){
    # repeat for the required distance
    for(m in 1:df_i$distance[i]){
      # move HEAD in the required direction
      # HEAD is always in the first row of df_pos
      switch(df_i$direction[i],
             "U" = {df_pos$y[1] = df_pos$y[1] + 1},
             "D" = {df_pos$y[1] = df_pos$y[1] - 1},
             "R" = {df_pos$x[1] = df_pos$x[1] + 1},
             "L" = {df_pos$x[1] = df_pos$x[1] - 1}
      )
      # move TAIL, if required, in the required direction
      # repeat for each knot in tail
      for(k in 1:tlength){
        df_pos <- f_movetail(df_pos, k)
      }
      
      # add the chosen TAIL position to the tracker
      Tpl = rbind(Tpl, data.frame(x = df_pos$x[df_pos$id == tlength], y = df_pos$y[df_pos$id == tlength]))  
    }
  }
  
  # return the number of distinct tail position list
  return(nrow(unique(Tpl)))
}


  
# Output ------------------------------------------------------------------
f_day9(df_day9_test,1) # 13
f_day9(df_day9_input,1) # 6181

f_day9(df_day9_test_b,9) # 36
f_day9(df_day9_input,9) # 2386
