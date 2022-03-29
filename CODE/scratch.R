suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(naturalsort))

input_test_names <- c("PV-S", "LW-S", "QRF-S", "WRF-S")
norm_type <- "grade"
input_file_path <- "INPUT-FILES/TOD-S/CHILD-GRADE/"

# read the input files (test>>grade) into a list.
input_files <- map(
  input_test_names,
  ~
    suppressMessages(read_csv(here(str_c(
      input_file_path, .x, "-", norm_type, ".csv"
    ))))
) %>% 
  set_names(input_test_names)

# fix col names and store them in vectors: add leading zeros so they'll sort
# properly
new_names_input <- input_files[[1]] %>%
  names() %>%
  tibble() %>%
  mutate(across(.,
                ~
                  case_when(
                    str_detect(., "^(?!(r|K|10|11|12)).*$") ~ str_c("0", .),
                    TRUE ~ .
                  ))) %>%
  pull(.)
