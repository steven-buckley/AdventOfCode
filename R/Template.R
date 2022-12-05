

# Description -------------------------------------------------------------



# Load input --------------------------------------------------------------
df_day4_input <- read.csv("./Data/Day4.csv",
                          header = FALSE,
                          col.names = c("A", "B")
)


df_test_input <- data.frame("A" = c("2-4","2-3","5-7","2-8","6-6","2-6"),
                            "B" = c("6-8","4-5","7-9","3-7","4-6","4-8")
)

# Solution ----------------------------------------------------------------
f_day4_a <- function(df){
  
  df |> 
    # split the ranges
    dplyr::mutate(A1 = stringr::str_extract(A, "[^-]+"),
                  A2 = stringr::str_extract(A, "[^-]+$"),
                  B1 = stringr::str_extract(B, "[^-]+"),
                  B2 = stringr::str_extract(B, "[^-]+$")
    ) |>
    # check for overlap
    dplyr::mutate(overlap_flag = dplyr::case_when((A1>=B1 & A2<=B2) ~1,
                                                  (B1>=A1 & B2<=A2) ~1,
                                                  TRUE ~ 0
    )
    ) |> 
    dplyr::summarise(sum(overlap_flag)) |> 
    dplyr::pull()
    
}

df_test <- df_day4_input |> 
  # split the ranges
  dplyr::mutate(A1 = stringr::str_extract(A, "[^-]+"),
                A2 = stringr::str_extract(A, "[^-]+$"),
                B1 = stringr::str_extract(B, "[^-]+"),
                B2 = stringr::str_extract(B, "[^-]+$")
                ) |>
  dplyr::mutate(overlap_flag = dplyr::case_when((A1>=B1 & A2<=B2) ~1,
                                                (B1>=A1 & B2<=A2) ~1,
                                                TRUE ~ 0
                                                )
                )

|> 
  dplyr::summarise(sum(overlap_flag)) |> 
  dplyr::pull()
  


# Output ------------------------------------------------------------------
f_day4_a(df_test_input) # 2
f_day4_a(df_day4_input)



