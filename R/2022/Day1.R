
# Day1 --------------------------------------------------------------------


# Description -------------------------------------------------------------
# https://adventofcode.com/2022/day/1
# https://adventofcode.com/2022/day/1#part2


# Load input --------------------------------------------------------------
df_day1 <- read.csv("./Data/Day1.csv",
                    col.names = "calories",
                    blank.lines.skip = FALSE
)


# Solution ----------------------------------------------------------------
# need to identify each elf - split by spaces
df_day1 <- df_day1 |> 
  dplyr::mutate(elf = 1)

# initialise counter
elf_id = 1

# loop through and update counter
for(x in 1:nrow(df_day1)){
  if(is.na(df_day1$calories[[x]])){
    elf_id = elf_id + 1
    df_day1$elf[[x]] = 0
  } else {
    df_day1$elf[[x]] = elf_id
  }
  
}

# calculte cumulative calories
df_day1 <- df_day1 |> 
  dplyr::group_by(elf) |> 
  dplyr::mutate(total_calories = sum(calories)) |> 
  dplyr::ungroup()



# Output ------------------------------------------------------------------
# find the total calories for the elf with the most calories
df_day1 |> 
  dplyr::slice_max(total_calories, n = 1) |> 
  dplyr::summarise(max(total_calories))
  
# find the total calories for the top 3 elves with the most calories
df_day1 |> 
  dplyr::select(elf, total_calories) |> 
  dplyr::distinct() |> 
  dplyr::arrange(desc(total_calories)) |> 
  dplyr::slice_max(total_calories, n=3) |> 
  dplyr::summarise(sum(total_calories))
  


