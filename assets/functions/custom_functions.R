
# About -------------------------------------------------------------------

# This file contains the functions for 
# the website Vårcupen. Vårcupen is a
# competition hosted by the fencing club
# FFF in Stockholm, Sweden.


# Reshape  data, get data to long format----------------------

# reshapes a tibble to long format
to_long_format <- function(df){
  df |> 
    # make long dataframe
    pivot_longer(cols = starts_with("no_"), 
                 names_to = "comp_no",
                 names_prefix = "no_",
                 values_to = "place") |> 
    filter(comp_no <= {{ current_comp_no }}) |> 
    mutate(comp_no = as.numeric(comp_no),
           place_char = as.character(place))
}


# Calculate points --------------------------------------------------------

# This function takes a tibble with a variable called "curr_comp" and calculates points
calc_points <- function(df) {
  tot <- df %>% 
    mutate(points = case_match(place_char,
                               "1" ~ 32,
                               "2" ~ 26,
                               "3" ~ 21,
                               "4" ~ 19,
                               c("5", "6", "7", "8") ~ 14,
                               c("9", "10", "11", "12", "13", "14", "15", "16") ~  8,
                               c("17", "18", "19", "20", "21", "22", "23", "24", 
                                 "25", "26", "27", "28", "29", "30", "31", "32") ~ 4
    ))
}

# Create current comp table -----------------------------------------------

current_comp_table <- function(df) {
   df |> 
    filter(comp_no == current_comp_no) |> 
    select(-c(gender, comp_no, place_char)) |> 
    arrange(place) |> 
    na.omit() |> 
    rename(
      Namn = name,
      Placering = place,
      Poäng = points
    ) |>
    gt() |> 
    tab_header(title = paste0("Resultat efter den ", 
                              current_comp_no, ":a deltävlingen"))
}


