# https://adventofcode.com/2023/day/5


# Description -------------------------------------------------------------
# probably a nice way to split the input data but just did it manually for now
# checking the actual input the values are too big to create lists of each number as in the example
# could fix the joins to use a function rather than repeating some steps


# Load input --------------------------------------------------------------
df_test_seeds = read.table('./data/day-5-test-seeds.txt',sep='\t', col.names = "SEED")
df_test_seed_to_soil = read.table('./data/day-5-test-seed-to-soil.txt',sep='\t', col.names = "INPUT")
df_test_soil_to_fertilizer = read.table('./data/day-5-test-soil-to-fertilizer.txt',sep='\t', col.names = "INPUT")
df_test_fertilizer_to_water = read.table('./data/day-5-test-fertilizer-to-water.txt',sep='\t', col.names = "INPUT")
df_test_water_to_light = read.table('./data/day-5-test-water-to-light.txt',sep='\t', col.names = "INPUT")
df_test_light_to_temperature = read.table('./data/day-5-test-light-to-temperature.txt',sep='\t', col.names = "INPUT")
df_test_temperature_to_humidity = read.table('./data/day-5-test-temperature-to-humidity.txt',sep='\t', col.names = "INPUT")
df_test_humidity_to_location = read.table('./data/day-5-test-humidity-to-location.txt',sep='\t', col.names = "INPUT")

df_seeds = read.table('./data/day-5-seeds.txt',sep='\t', col.names = "SEED")
df_seed_to_soil = read.table('./data/day-5-seed-to-soil.txt',sep='\t', col.names = "INPUT")
df_soil_to_fertilizer = read.table('./data/day-5-soil-to-fertilizer.txt',sep='\t', col.names = "INPUT")
df_fertilizer_to_water = read.table('./data/day-5-fertilizer-to-water.txt',sep='\t', col.names = "INPUT")
df_water_to_light = read.table('./data/day-5-water-to-light.txt',sep='\t', col.names = "INPUT")
df_light_to_temperature = read.table('./data/day-5-light-to-temperature.txt',sep='\t', col.names = "INPUT")
df_temperature_to_humidity = read.table('./data/day-5-temperature-to-humidity.txt',sep='\t', col.names = "INPUT")
df_humidity_to_location = read.table('./data/day-5-humidity-to-location.txt',sep='\t', col.names = "INPUT")


# Solution ----------------------------------------------------------------
f_day5_clean_map <- function(df){
  
  # clean the dataframe up to get a range start, end and the mapping adjustment
  df <- df |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      DEST = as.numeric(strsplit(INPUT," ")[[1]][1]),
      SOURCE = as.numeric(strsplit(INPUT, " ")[[1]][2]),
      LENGTH = as.numeric(strsplit(INPUT, " ")[[1]][3]),
      MAPPING = DEST - SOURCE,
      RANGE_START = SOURCE,
      RANGE_END = SOURCE + LENGTH - 1
    ) |> 
    dplyr::select(RANGE_START, RANGE_END, MAPPING)
}


# Part 1 Test -------------------------------------------------------------

part1_test <- df_test_seeds |> 
  # perform seed-to-soil mapping
  dplyr::left_join(
    f_day5_clean_map(df_test_seed_to_soil),
    dplyr::join_by(SEED >= RANGE_START, SEED <= RANGE_END)
  ) |> 
  dplyr::mutate(SOIL = ifelse(is.na(MAPPING), SEED, SEED + MAPPING)) |> 
  dplyr::select(SEED, SOIL) |> 
  # perform soil-to-fertilizer mapping
  dplyr::left_join(
    f_day5_clean_map(df_test_soil_to_fertilizer),
    dplyr::join_by(SOIL >= RANGE_START, SOIL <= RANGE_END)
  ) |> 
  dplyr::mutate(FERT = ifelse(is.na(MAPPING), SOIL, SOIL + MAPPING)) |> 
  dplyr::select(SEED, SOIL, FERT) |> 
  # perform fertilizer-to-water mapping
  dplyr::left_join(
    f_day5_clean_map(df_test_fertilizer_to_water),
    dplyr::join_by(FERT >= RANGE_START, FERT <= RANGE_END)
  ) |> 
  dplyr::mutate(WATER = ifelse(is.na(MAPPING), FERT, FERT + MAPPING)) |> 
  dplyr::select(SEED, SOIL, FERT, WATER) |> 
  # perform water-to-light mapping
  dplyr::left_join(
    f_day5_clean_map(df_test_water_to_light),
    dplyr::join_by(WATER >= RANGE_START, WATER <= RANGE_END)
  ) |> 
  dplyr::mutate(LIGHT = ifelse(is.na(MAPPING), WATER, WATER + MAPPING)) |> 
  dplyr::select(SEED, SOIL, FERT, WATER, LIGHT) |> 
  # perform light-to-temperature mapping
  dplyr::left_join(
    f_day5_clean_map(df_test_light_to_temperature),
    dplyr::join_by(LIGHT >= RANGE_START, LIGHT <= RANGE_END)
  ) |> 
  dplyr::mutate(TEMP = ifelse(is.na(MAPPING), LIGHT, LIGHT + MAPPING)) |> 
  dplyr::select(SEED, SOIL, FERT, WATER, LIGHT, TEMP) |> 
  # perform temperature-to-humidity mapping
  dplyr::left_join(
    f_day5_clean_map(df_test_temperature_to_humidity),
    dplyr::join_by(TEMP >= RANGE_START, TEMP <= RANGE_END)
  ) |> 
  dplyr::mutate(HUMIDITY = ifelse(is.na(MAPPING), TEMP, TEMP + MAPPING)) |> 
  dplyr::select(SEED, SOIL, FERT, WATER, LIGHT, TEMP, HUMIDITY) |>
  # perform humidity-to-location mapping
  dplyr::left_join(
    f_day5_clean_map(df_test_humidity_to_location),
    dplyr::join_by(HUMIDITY >= RANGE_START, HUMIDITY <= RANGE_END)
  ) |> 
  dplyr::mutate(LOCATION = ifelse(is.na(MAPPING), HUMIDITY, HUMIDITY + MAPPING)) |> 
  dplyr::select(SEED, SOIL, FERT, WATER, LIGHT, TEMP, HUMIDITY, LOCATION)

min(part1_test$LOCATION)


# Part 1 ------------------------------------------------------------------
part1 <- df_seeds |> 
  # perform seed-to-soil mapping
  dplyr::left_join(
    f_day5_clean_map(df_seed_to_soil),
    dplyr::join_by(SEED >= RANGE_START, SEED <= RANGE_END)
  ) |> 
  dplyr::mutate(SOIL = ifelse(is.na(MAPPING), SEED, SEED + MAPPING)) |> 
  dplyr::select(SEED, SOIL) |> 
  # perform soil-to-fertilizer mapping
  dplyr::left_join(
    f_day5_clean_map(df_soil_to_fertilizer),
    dplyr::join_by(SOIL >= RANGE_START, SOIL <= RANGE_END)
  ) |> 
  dplyr::mutate(FERT = ifelse(is.na(MAPPING), SOIL, SOIL + MAPPING)) |> 
  dplyr::select(SEED, SOIL, FERT) |> 
  # perform fertilizer-to-water mapping
  dplyr::left_join(
    f_day5_clean_map(df_fertilizer_to_water),
    dplyr::join_by(FERT >= RANGE_START, FERT <= RANGE_END)
  ) |> 
  dplyr::mutate(WATER = ifelse(is.na(MAPPING), FERT, FERT + MAPPING)) |> 
  dplyr::select(SEED, SOIL, FERT, WATER) |> 
  # perform water-to-light mapping
  dplyr::left_join(
    f_day5_clean_map(df_water_to_light),
    dplyr::join_by(WATER >= RANGE_START, WATER <= RANGE_END)
  ) |> 
  dplyr::mutate(LIGHT = ifelse(is.na(MAPPING), WATER, WATER + MAPPING)) |> 
  dplyr::select(SEED, SOIL, FERT, WATER, LIGHT) |> 
  # perform light-to-temperature mapping
  dplyr::left_join(
    f_day5_clean_map(df_light_to_temperature),
    dplyr::join_by(LIGHT >= RANGE_START, LIGHT <= RANGE_END)
  ) |> 
  dplyr::mutate(TEMP = ifelse(is.na(MAPPING), LIGHT, LIGHT + MAPPING)) |> 
  dplyr::select(SEED, SOIL, FERT, WATER, LIGHT, TEMP) |> 
  # perform temperature-to-humidity mapping
  dplyr::left_join(
    f_day5_clean_map(df_temperature_to_humidity),
    dplyr::join_by(TEMP >= RANGE_START, TEMP <= RANGE_END)
  ) |> 
  dplyr::mutate(HUMIDITY = ifelse(is.na(MAPPING), TEMP, TEMP + MAPPING)) |> 
  dplyr::select(SEED, SOIL, FERT, WATER, LIGHT, TEMP, HUMIDITY) |>
  # perform humidity-to-location mapping
  dplyr::left_join(
    f_day5_clean_map(df_humidity_to_location),
    dplyr::join_by(HUMIDITY >= RANGE_START, HUMIDITY <= RANGE_END)
  ) |> 
  dplyr::mutate(LOCATION = ifelse(is.na(MAPPING), HUMIDITY, HUMIDITY + MAPPING)) |> 
  dplyr::select(SEED, SOIL, FERT, WATER, LIGHT, TEMP, HUMIDITY, LOCATION)

min(part1$LOCATION)


