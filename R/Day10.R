

# Description -------------------------------------------------------------



# Load input --------------------------------------------------------------
df_day10_demo <- read.csv("./Data/Day10_Demo.csv",
                         header = FALSE,
                         colClasses = c("character"),
                         col.names = "input"
)

df_day10_test <- read.csv("./Data/Day10_Test.csv",
                          header = FALSE,
                          colClasses = c("character"),
                          col.names = "input"
)

df_day10 <- read.csv("./Data/Day10.csv",
                          header = FALSE,
                          colClasses = c("character"),
                          col.names = "input"
)

# Solution ----------------------------------------------------------------
df_day10_a <- function(df_i){
  
  # parse the instructions into command and value
  df_i <- df_i |> 
    dplyr::mutate(command = stringr::str_extract(input, "[^ ]+"),
                  value = as.numeric(stringr::str_extract(input, "[^ ]+$"))
    )
  
  # setup counters
  x = 1
  cycle = 1
  x_track = list()
  ss = 0
  
  # loop through each of the input commands
  for(i in 1:nrow(df_i)){
    
    # check the input command
    if(df_i$command[i] == "noop"){
      # nothing changes during the cycle
      x_track[cycle] = x
      cycle = cycle + 1
    } else if(df_i$command[i] == "addx"){
      # first cycle does nothing
      x_track[cycle] = x
      cycle = cycle + 1
      # second cycle increments the x value
      x = x + df_i$value[i]
      x_track[cycle] = x
      cycle = cycle + 1
    }
  }
  
  # loop through each cycle entry
  for(c in 1:length(x_track)){
    # want value for cycle 20 then every 40 places onwards (20,60,100)
    # so (x+20)%%40 == 0
    # actually want the value during the cycle so value as of [c-1]
    if((c+20)%%40==0){ss = ss+(c*as.numeric(x_track[c-1]))}
  }
  
  # return value
  return(ss)
  
}

df_day10_b <- function(df_i){
  
  # parse the instructions into command and value
  df_i <- df_i |> 
    dplyr::mutate(command = stringr::str_extract(input, "[^ ]+"),
                  value = as.numeric(stringr::str_extract(input, "[^ ]+$"))
    )
  
  # setup counters
  x = 1
  cycle = 1
  x_track = list()
  ss = 0
  sprite = 1
  display = '#'
  
  # loop through each of the input commands
  for(i in 1:nrow(df_i)){
    
    # check the input command
    if(df_i$command[i] == "noop"){
      # nothing changes during the cycle
      x_track[cycle] = x
      cycle = cycle + 1
    } else if(df_i$command[i] == "addx"){
      # first cycle does nothing
      x_track[cycle] = x
      cycle = cycle + 1
      # second cycle increments the x value
      x = x + df_i$value[i]
      x_track[cycle] = x
      cycle = cycle + 1
    }
    
    
  }
  
  f_draw_output(x_track[1:40])
  f_draw_output(x_track[41:80])
  f_draw_output(x_track[81:120])
  f_draw_output(x_track[121:160])
  f_draw_output(x_track[161:200])
  f_draw_output(x_track[201:240])
  
}

f_draw_output <- function(x_track){
  display = '#'
  
  for(x in 1:40){
    if(x_track[x] >= (x-1) & x_track[x] <= (x+1)){
      display = paste0(display,'#')
    }else{
      display = paste0(display,'.')
    }
  }
  
  print(display)
}




# Output ------------------------------------------------------------------
df_day10_a(df_day10_test) # 13140
df_day10_a(df_day10) # 14820

df_day10_b(df_day10_test)
df_day10_b(df_day10) # RZEKEFHA
