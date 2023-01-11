

# Description -------------------------------------------------------------



# Load input --------------------------------------------------------------

file <- './Data/Day12_test.txt'
test_input <- as.matrix(read.fwf(file, widths = rep(1, nchar(readLines(file, n = 1)))))

file <- './Data/Day12.txt'
input_data <- as.matrix(read.fwf(file, widths = rep(1, nchar(readLines(file, n = 1)))))

# Solution ----------------------------------------------------------------
# can network analysis be used?
# need to create a node and edge dataset
create_nodes <- function(input){
  
  df_nodes <- data.frame(node_id = as.numeric(),
                         node_ref = as.character(),
                         node_desc = as.character(),
                         node_text = as.character()
                         )
  
  node_count = 1
  for(y in 1:nrow(input)){
    for(x in 1:ncol(input)){
      # identify the type of step
      if(input[y,x] == "S"){
        node_type = "START"
      } else if(input[y,x] == "E"){
        node_type = "END"
      } else {
        node_type = "STEP"
      }
      
      df_nodes <- rbind(df_nodes, data.frame(node_id = node_count,
                                             node_ref = paste0('[',x,',',y,']'),
                                             node_desc = node_type,
                                             node_text = input[y,x]
                                             )
                        )
      
      node_count = node_count + 1
    }
  }
  
  return(df_nodes)
}
  
create_edges <- function(input, df_nodes){
  
  df_edges <- data.frame(node_from = as.numeric(),
                         node_to = as.numeric()
                         )
  
  for(y in 1:nrow(input)){
    for(x in 1:ncol(input)){
      
      
      if(input[y,x] == "S"){
        input[y,x] = as.numeric(1)
      } else if(input[y,x] == "E"){
        input[y,x] = 26
      } else {
        input[y,x] = utf8ToInt(input[y,x])-96
      }
    }
  }
  class(input) <- "numeric"
  
  for(y in 1:nrow(input)){
    for(x in 1:ncol(input)){
      
      cur_val <- as.numeric(input[y,x])
      cur_node <- df_nodes$node_id[df_nodes$node_ref == paste0('[',x,',',y,']')]
      
      # check up
      if((y-1)>0){
        pot_val <- as.numeric(input[(y-1),x])
        pot_node <- df_nodes$node_id[df_nodes$node_ref == paste0('[',x,',',y-1,']')]
        if((pot_val - cur_val) <= 1){
          df_edges <- rbind(df_edges, data.frame(node_from = cur_node, node_to = pot_node))
        }
      }
      
      # check down
      if((y+1) <= nrow(input)){
        pot_val <- as.numeric(input[(y+1),x])
        pot_node <- df_nodes$node_id[df_nodes$node_ref == paste0('[',x,',',y+1,']')]
        if((pot_val - cur_val) <= 1){
          df_edges <- rbind(df_edges, data.frame(node_from = cur_node, node_to = pot_node))
        }
      }
      
      # check right
      if((x+1) <= ncol(input)){
        pot_val <- as.numeric(input[y,(x+1)])
        pot_node <- df_nodes$node_id[df_nodes$node_ref == paste0('[',x+1,',',y,']')]
        if((pot_val - cur_val) <= 1){
          df_edges <- rbind(df_edges, data.frame(node_from = cur_node, node_to = pot_node))
        }
      }
      
      # check left
      if((x-1)>0){
        pot_val <- as.numeric(input[y,(x-1)])
        pot_node <- df_nodes$node_id[df_nodes$node_ref == paste0('[',x-1,',',y,']')]
        if((pot_val - cur_val) <= 1){
          df_edges <- rbind(df_edges, data.frame(node_from = cur_node, node_to = pot_node))
        }
      }
      
    }
  }
  
  return(df_edges)
}

no_of_steps <- function(df_nodes, df_edges){
  # create the iGraph object
  # must be directed as one way paths
  G <- igraph::graph_from_data_frame(d=df_edges, vertices=df_nodes, directed=TRUE)
  
  # find the shortest path from start to end
  SOURCE_ID <- igraph::V(G)[node_desc =="START"]
  TARGET_ID <- igraph::V(G)[node_desc =="END"]
  shortest_path <- igraph::get.shortest.paths(G, SOURCE_ID, TARGET_ID)$vpath
  
  # no. of steps is path length - 1
  return(length(shortest_path[[1]])-1)
}

no_of_steps_b <- function(df_nodes, df_edges, target){
  # create the iGraph object
  # must be directed as one way paths
  G <- igraph::graph_from_data_frame(d=df_edges, vertices=df_nodes, directed=TRUE)
  
  # set the target to beat
  shortest_path = target
  
  
  # find the potential starting points
  start_points <- igraph::V(G)[node_text =="a"]
  # set target destination
  TARGET_ID <- igraph::V(G)[node_desc =="END"]
  
  # loop through each start point
  for(i in start_points){
    # find the shortest path
    path <- igraph::get.shortest.paths(G, i, TARGET_ID)$vpath
    path_length = length(path[[1]])-1
    # check if the path is shorter than the target
    # some start points will not have a valid path so check it is above 0
    if(path_length < shortest_path & path_length > 0){shortest_path = path_length}
  }
  
  return(shortest_path)
}

# Output ------------------------------------------------------------------
test_nodes <- create_nodes(test_input)
test_edges <- create_edges(test_input, test_nodes)
no_of_steps(test_nodes, test_edges) # 31
no_of_steps_b(test_nodes, test_edges, 31)


nodes <- create_nodes(input_data)
edges <- create_edges(input_data, nodes)
no_of_steps(nodes, edges) # 425
no_of_steps_b(nodes, edges, 425) # 418
