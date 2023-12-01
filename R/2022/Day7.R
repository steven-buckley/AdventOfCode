

# Description -------------------------------------------------------------
# https://adventofcode.com/2022/day/7


# Load input --------------------------------------------------------------
df_day7_test <- read.csv("./Data/Day7_test.csv",
                         header = FALSE,
                         col.names = "terminal")

df_day7_input <- read.csv("./Data/Day7.csv",
                          header = FALSE,
                          col.names = "terminal")

# Build file structure ----------------------------------------------------
f_Day7_str <- function(tlist){
  # Mutate input into structured format:
  # | PATH | TYPE | NAME | SIZE
  # PATH : path to folder/file
  # TYPE: file / folder
  # NAME: name of file or sub-folder
  # SIZE: for files it is the size of the file and for folders the size of all files/folders within
  
  # create and initialise data frame
  df_structure <- data.frame(PATH = "",
                             TYPE = "folder",
                             NAME = ".",
                             SIZE = as.numeric(NA),
                             LEVEL = 0
  )
  
  # initialise position identifiers
  cur_path = "."

  # iterate through each row in the input
  for(i in 1:nrow(tlist)){
    # parse the text in the record and act accordingly
    if(tlist$terminal[i] == "$ cd /"){
      # navigate to root
      cur_path = "."
    } else if(tlist$terminal[i] == "$ cd .."){
      # navigate up one folder
      cur_path = dirname(cur_path)
    } else if(substr(tlist$terminal[i],1,5) == "$ cd "){
      # move down one folder
      cur_path = paste0(cur_path,"/",gsub("\\$ cd ","",tlist$terminal[i]))
    } else if(substr(tlist$terminal[i],1,4) == "$ ls"){
      # do nothing
    } else if(substr(tlist$terminal[i],1,3) == "dir"){
      # create a new folder entry
      df_structure <- rbind(df_structure, data.frame(PATH = paste0(cur_path,"/"),
                                                     TYPE = "folder",
                                                     NAME = stringr::word(tlist$terminal[i],-1),
                                                     SIZE = as.numeric(NA),
                                                     LEVEL = stringr::str_count(paste0(cur_path,"/"), "/")
      )
      )
    } else {
      # create a new file entry
      df_structure <- rbind(df_structure, data.frame(PATH = paste0(cur_path,"/"),
                                                     TYPE = "file",
                                                     NAME = stringr::word(tlist$terminal[i],-1),
                                                     SIZE = as.numeric(stringr::word(tlist$terminal[i],1)),
                                                     LEVEL = stringr::str_count(paste0(cur_path,"/"), "/")
      )
      )
    }
  }
  
  # iterate over the structure (lowest level to root)
  df_structure <- df_structure |> dplyr::arrange(desc(LEVEL))
  for(i in 1:nrow(df_structure)){
    if(df_structure$TYPE[i]=="folder"){
      # identify a search path (all files/folders sharing the path)
      spath = paste0(df_structure$PATH[i],df_structure$NAME[i],"/")
      # aggregate the size of the files/folders
      df_structure$SIZE[i] =  sum(df_structure[df_structure$PATH == spath,]$SIZE)
    }
  }
  
  # return the formatted df
  return(df_structure)
}



# Part 1 ------------------------------------------------------------------
# find the sum of the directories
f_Day7_a <- function(df_str, max_size){
  df_str <- df_str |> 
    dplyr::filter(TYPE == "folder" &
                    SIZE <= max_size) |> 
    dplyr::summarise(sum(SIZE)) |> 
    dplyr::pull()
  
  # return the minimum size needed
  return(df_str)
  
}

# Part 2 ------------------------------------------------------------------
# find the required filespace
f_Day7_b <- function(df_str, total_space, req_space){
  # calculate the required additional space
  cur_free_space = total_space - sum(df_str[df_str$PATH == "",]$SIZE)
  req_space = req_space - cur_free_space
  
  # filter the structure to only folders, smaller than the required space
  df_str <- df_str |> 
    dplyr::filter(TYPE == "folder" & SIZE >= req_space) |> 
    dplyr::arrange(SIZE) |> 
    dplyr::top_n(n=-1, SIZE) |> 
    dplyr::pull(SIZE)
  
  # return the minimum size needed
  return(df_str)
}



# Results -----------------------------------------------------------------
# test case
f_Day7_a(f_Day7_str(df_day7_test), 100000) # 95437
f_Day7_b(f_Day7_str(df_day7_test), 70000000, 30000000) # 24933642

#actual
f_Day7_a(f_Day7_str(df_day7_input), 100000) # 1428881
f_Day7_b(f_Day7_str(df_day7_input), 70000000, 30000000) # 10475598

