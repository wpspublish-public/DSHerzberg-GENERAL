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


temp2 <- input_files[[1]] %>%
  names() %>%
  tibble() %>% 
  rename(grade_strat = ".") %>% 
  arrange(grade_strat)



temp3 <- input_files[[1]] %>%
  names() %>%
  tibble() %>% 
  rename(grade_strat = ".") %>% 
  arrange(match(temp1, grade_strat))

temp4 <- input_files[[1]] %>%
  names() %>%
  tibble() %>% 
  rename(grade_strat = ".") %>% 
  arrange(match(order(.$grade_strat, decreasing = FALSE), grade_strat))

temp5 <- input_files[[1]] %>%
  names() %>%
  tibble() %>% 
  rename(grade_strat = ".") %>% 
  arrange(match(grade_strat, grade_strat))

temp6 <- input_files[[1]] %>%
  names() %>%
  tibble() %>% 
  rename(grade_strat = ".") 
  
temp7 <- temp6 %>% 
  arrange(match(grade_strat, grade_strat))

temp8 <- temp6 %>% 
  arrange(match(1:27, grade_strat))


urlRemote_path  <- "https://raw.github.com/"
github_path <- "wpspublish-public/DSHerzberg-GENERAL/master/INPUT-FILES/TOD-S/CHILD-GRADE/"

input_files <- map(
  input_test_names,
  ~
    suppressMessages(read_csv(url(str_c(
      urlRemote_path, github_path, .x, "-", norm_type, ".csv"
    ))))
) %>% 
  set_names(input_test_names)


