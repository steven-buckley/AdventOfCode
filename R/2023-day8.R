# https://adventofcode.com/2023/day/



# Required packages and libraries -----------------------------------------
install.packages("numbers")


# Description -------------------------------------------------------------



# Load input --------------------------------------------------------------
df_test1 = read.table('./data/day-8-test1.txt',sep='\t', col.names = "INPUT")
df_input = read.table('./data/day-8-input.txt',sep='\t', col.names = "INPUT")
df_test2 = read.table('./data/day-8-test2.txt',sep='\t', col.names = "INPUT")

# Solution ----------------------------------------------------------------
f_day8_part1 <- function(df){
  
  # tidy input to extract the instructions and leave the nodes and directions
  direction_list = df[1,]
  df_nodes = data.frame(INPUT = df[-1,]) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(INPUT = gsub("\\(|\\)| ","",INPUT))|> 
    dplyr::mutate(
      NODE = strsplit(INPUT, "=")[[1]][1],
      L = strsplit(strsplit(INPUT, "=")[[1]][2], ",")[[1]][1],
      R =  strsplit(INPUT, ",")[[1]][2]
    ) |> 
    dplyr::ungroup() |> 
    as.data.frame()
  
  # push the NODE values to be the row ID
  rownames(df_nodes) <- df_nodes[,"NODE"]
  
  # initialise counters
  CUR_NODE = "AAA"
  CUR_STEPS = 0
  INSTR_STEPS = 0
  
  # loop through the instructions until reaching ZZZ
  while(CUR_NODE != 'ZZZ'){
    # take a step
    CUR_STEPS = CUR_STEPS + 1
    INSTR_STEPS = ifelse(INSTR_STEPS == nchar(direction_list),1,INSTR_STEPS + 1)
    # identify direction to take
    DIR <- substr(direction_list, INSTR_STEPS,INSTR_STEPS)
    # move to the destination
    CUR_NODE <- df_nodes[[CUR_NODE,DIR]]
  }
  
  return(CUR_STEPS)
}

f_day8_part2_wrong <- function(df){
  # this approach is not going to work if the number of steps is going to be huge
  # must be a pattern instead
  
  # tidy input to extract the instructions and leave the nodes and directions
  direction_list = df[1,]
  df_nodes = data.frame(INPUT = df[-1,]) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(INPUT = gsub("\\(|\\)| ","",INPUT))|> 
    dplyr::mutate(
      NODE = strsplit(INPUT, "=")[[1]][1],
      L = strsplit(strsplit(INPUT, "=")[[1]][2], ",")[[1]][1],
      R =  strsplit(INPUT, ",")[[1]][2],
      START_NODE = ifelse(substr(NODE,3,3)=="A",1,0),
      END_NODE = ifelse(substr(NODE,3,3)=="Z",1,0)
    ) |> 
    dplyr::ungroup() |> 
    as.data.frame()
  
  # push the NODE values to be the row ID
  rownames(df_nodes) <- df_nodes[,"NODE"]
  
  # identify all the start points
  df_routes <- df_nodes |> 
    dplyr::filter(START_NODE == 1) |> 
    dplyr::select(NODE, END_NODE)
  
  #
  CUR_NODES <- df_routes$NODE
  CUR_ENDPOINTS <- df_routes$END_NODE
  
  # initialise counters
  CUR_STEPS = 0
  INSTR_STEPS = 0
  NUM_ROUTES = nrow(df_routes)
  NUM_Z = 0
  
  # loop through the instructions until all routes reach end points
  while(NUM_Z != NUM_ROUTES){
    
    # take a step
    CUR_STEPS = CUR_STEPS + 1
    INSTR_STEPS = ifelse(INSTR_STEPS == nchar(direction_list),1,INSTR_STEPS + 1)
    # identify direction to take
    DIR <- substr(direction_list, INSTR_STEPS,INSTR_STEPS)
    
    CUR_NODES <- sapply(CUR_NODES,f_update, DIR, df_nodes)
    CUR_ENDPOINTS <- sapply(CUR_NODES, f_check_endpoint, df_nodes)
    
    NUM_Z <- sum(CUR_ENDPOINTS)
    
    if(NUM_Z > 0){
      print(paste0("Number of steps=",CUR_STEPS, " : No of routes at endpoint=", NUM_Z,"/",NUM_ROUTES))  
    }
    
  }
  
  return(CUR_STEPS)
}


f_day8_part2 <- function(df){
  # running each route for n endpoints showed that each route cycles
  # so just need to find the length for each route then the least common multiple (LCM)
  
  # tidy input to extract the instructions and leave the nodes and directions
  direction_list = df[1,]
  df_nodes = data.frame(INPUT = df[-1,]) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(INPUT = gsub("\\(|\\)| ","",INPUT))|> 
    dplyr::mutate(
      NODE = strsplit(INPUT, "=")[[1]][1],
      L = strsplit(strsplit(INPUT, "=")[[1]][2], ",")[[1]][1],
      R =  strsplit(INPUT, ",")[[1]][2],
      START_NODE = ifelse(substr(NODE,3,3)=="A",1,0),
      END_NODE = ifelse(substr(NODE,3,3)=="Z",1,0)
    ) |> 
    dplyr::ungroup() |> 
    as.data.frame()
  
  # push the NODE values to be the row ID
  rownames(df_nodes) <- df_nodes[,"NODE"]
  
  # identify all the start points
  df_routes <- df_nodes |> 
    dplyr::filter(START_NODE == 1) |> 
    dplyr::select(NODE, END_NODE)
  
  # setup vector for route lengths
  ROUTE_STEPS = df_routes$END_NODE
  
  # run each route individually
  for(r in 1:nrow(df_routes)){
    
    # get the route start point
    CUR_NODE <- df_routes$NODE[r]
    CUR_ENDPOINT <- df_routes$END_NODE[r]
    
    # initialise counters
    CUR_STEPS = 0
    INSTR_STEPS = 0
    
    # loop through the instructions until route reaches end point
    while(CUR_ENDPOINT != 1){
      
      # take a step
      CUR_STEPS = CUR_STEPS + 1
      INSTR_STEPS = ifelse(INSTR_STEPS == nchar(direction_list),1,INSTR_STEPS + 1)
      # identify direction to take
      DIR <- substr(direction_list, INSTR_STEPS,INSTR_STEPS)
      
      # update the current node counters
      CUR_NODE <- sapply(CUR_NODE,f_update, DIR, df_nodes)
      CUR_ENDPOINT <- sapply(CUR_NODE, f_check_endpoint, df_nodes)
      
    }
    
    # return the LCM for all route steps
    ROUTE_STEPS[r] = CUR_STEPS
  }
  
  return(numbers::mLCM(ROUTE_STEPS))
  
}


# Output ------------------------------------------------------------------
f_day8_part1(df_test1)
f_day8_part1(df_input)

f_day8_part2(df_test2)
f_day8_part2(df_input)
