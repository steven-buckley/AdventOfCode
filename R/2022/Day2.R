

# Description -------------------------------------------------------------
https://adventofcode.com/2022/day/2
https://adventofcode.com/2022/day/2#part2


# Load input --------------------------------------------------------------
df_day2 <- read.csv("./Data/Day2.csv", 
                    sep = " ",
                    col.names = c("opp", "me"),
                    header = FALSE
)

# Solution: Part A --------------------------------------------------------
df_day2a <- df_day2 |> 
  # convert to matching classification
  dplyr::mutate(me = dplyr::case_when(me == "X" ~ "A",
                                      me == "Y" ~ "B",
                                      me == "Z" ~ "C",
                                      TRUE ~ me)
                ) |> 
  dplyr::mutate(score_choice = dplyr::case_when(me == "A" ~ 1,
                                                me == "B" ~ 2,
                                                me == "C" ~ 3,
                                                TRUE ~ 0
                                                ),
                score_outcome = dplyr::case_when(opp == me ~ 3,
                                                 opp == "A" & me == "B" ~ 6,
                                                 opp == "A" & me == "C" ~ 0,
                                                 opp == "B" & me == "A" ~ 0,
                                                 opp == "B" & me == "C" ~ 6,
                                                 opp == "C" & me == "A" ~ 6,
                                                 opp == "C" & me == "B" ~ 0
                                                 ),
                score_overall = score_choice + score_outcome
                )

# Solution: Part B --------------------------------------------------------
df_day2b <- df_day2 |> 
  # rename the fields
  dplyr::rename(outcome = me) |> 
  # convert to matching classification
  dplyr::mutate(me = dplyr::case_when(outcome == "Y" ~ opp,
                                      outcome == "X" & opp == "A" ~ "C",
                                      outcome == "X" & opp == "B" ~ "A",
                                      outcome == "X" & opp == "C" ~ "B",
                                      outcome == "Z" & opp == "A" ~ "B",
                                      outcome == "Z" & opp == "B" ~ "C",
                                      outcome == "Z" & opp == "C" ~ "A"
                                      )
  ) |> 
  dplyr::mutate(score_choice = dplyr::case_when(me == "A" ~ 1,
                                                me == "B" ~ 2,
                                                me == "C" ~ 3,
                                                TRUE ~ 0
  ),
  score_outcome = dplyr::case_when(outcome == "Y" ~ 3,
                                   outcome == "X" ~ 0,
                                   outcome == "Z" ~ 6
  ),
  score_overall = score_choice + score_outcome
  )

# Output ------------------------------------------------------------------
sum(df_day2a$score_overall)
sum(df_day2b$score_overall)




