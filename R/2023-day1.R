# https://adventofcode.com/2023/day/1


# Load packages -----------------------------------------------------------
library(dplyr)
library(stringr)


# Load input --------------------------------------------------------------
df_input = read.table('./data/day-1-input.txt',sep='\t', col.names = "INPUT")
df_test1 = read.table('./data/day-1-test1.txt',sep='\t', col.names = "INPUT")
df_test2 = read.table('./data/day-1-test2.txt',sep='\t', col.names = "INPUT")


# Solution - Part 1 -------------------------------------------------------
# The newly-improved calibration document consists of lines of text; 
# each line originally contained a specific calibration value that the Elves now 
# need to recover. On each line, the calibration value can be found by combining 
# the first digit and the last digit (in that order) to form a single two-digit number.
# e.g. 1abc2 = 12 & treb7uchet = 77

f_part1 <- function(df){
  
  df <- df|> 
    dplyr::mutate(FIRST_DIGIT = stringr::str_extract(INPUT, "(\\d)"),
                  LAST_DIGIT = stringr::str_extract(INPUT, stringr::regex("(\\d)(?!.*\\d)")),
                  COMBINE_DIGITS = as.numeric(paste0(FIRST_DIGIT,LAST_DIGIT))
    )
  
  return(sum(df$COMBINE_DIGITS))
}


# Solution - Part 2 -------------------------------------------------------
# Your calculation isn't quite right. It looks like some of the digits are actually 
# spelled out with letters: one, two, three, four, five, six, seven, eight, and nine 
# also count as valid "digits".
# Equipped with this new information, you now need to find the real first and last 
# digit on each line. For example:
# two1nine = 29

# converting the text to numbers causes issues with overlap of text numbers
# keep the text and append the number
# put the number in the middle to preserve start and end

f_part2 <- function(df){
  df <- df |> 
    dplyr::mutate(REVISED_INPUT = gsub("nine","nine9nine",
                                       gsub("eight","eight8eight",
                                            gsub("seven","seven7seven",
                                                 gsub("six","six6six",
                                                      gsub("five","five5five",
                                                           gsub("four","four4four",
                                                                gsub("three","three3three",
                                                                     gsub("two","two2two",
                                                                          gsub("one","one1one",INPUT))))))))),
                  FIRST_DIGIT = stringr::str_extract(REVISED_INPUT, "(\\d)"),
                  LAST_DIGIT = stringr::str_extract(REVISED_INPUT, stringr::regex("(\\d)(?!.*\\d)")),
                  COMBINE_DIGITS = as.numeric(paste0(FIRST_DIGIT,LAST_DIGIT))
    )
  
  return(sum(df$COMBINE_DIGITS))
    
}


# Output ------------------------------------------------------------------
f_part1(df_test1)
f_part1(df_input)
f_part2(df_test2)
f_part2(df_input)