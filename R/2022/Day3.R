

# Description -------------------------------------------------------------
https://adventofcode.com/2022/day/3



# Load input --------------------------------------------------------------
df_day3_input <- read.csv("./Data/Day3.csv",
                    header = FALSE,
                    col.names = "input")
                    

df_test_input <- data.frame(input = c("vJrwpWtwJgWrhcsFMMfFFhFp",
                                "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
                                "PmmdzqPrVvPwwTWBwg",
                                "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
                                "ttgJtRGJQctTZtZT",
                                "CrZsJsPPZsGzwwsLwLmpwMDw"
                                )
                      )



# Solution: Part A  -------------------------------------------------------

# function to find common characters between two strings
matchwords <- function(str1, str2) {
  mapply(function(x, y) paste0(intersect(x, y),collapse = " "), 
         strsplit(str1, ''), strsplit(str2, ''))
}


f_day3_a <- function(df){
  
  df <- df |> 
    # split the input column into two even parts
    dplyr::mutate(bag1 = substr(input, 1, nchar(input)/2),
                  bag2 = substr(input, (nchar(input)/2)+1, nchar(input)),
                  # apply function to find common words
                  common = matchwords(bag1,bag2),
                  # calculate value
                  value = match(common, c(letters[1:26],toupper(letters[1:26])))
    )
  
  # calculate the sum of the value column
  sum(df$value)

}


# Solution: Part B --------------------------------------------------------

# extend function to compare three strings
matchwords3 <- function(str1, str2, str3) {
  mapply(function(x, y, z) paste0(intersect(intersect(x, y), z),collapse = " "), 
         strsplit(str1, ''), strsplit(str2, ''), strsplit(str3, ''))
}



# function for day 3 part a
f_day3_b <- function(df){
  
  df <- df |> 
    # set row ids, use lag to populate previous 2 rows data
    dplyr::mutate(id = dplyr::row_number(),
                  bag1 = input,
                  bag2 = dplyr::lag(input, n=1),
                  bag3 = dplyr::lag(input, n=2)
    ) |> 
    # only keep each third record (to group by three)
    dplyr::filter(id%%3 == 0) |> 
    # apply function to find common character and calculate value
    dplyr::mutate(common = matchwords3(bag1,bag2, bag3),
                  value = match(common, c(letters[1:26],toupper(letters[1:26])))
    )
    
  # calculate the sum of the value column
  sum(df$value)
  
}


# Output ------------------------------------------------------------------
f_day3_a(df_test_input) # 157
f_day3_a(df_day3_input) # 8018

f_day3_b(df_test_input) # 70
f_day3_b(df_day3_input) # 2518