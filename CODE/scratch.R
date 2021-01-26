suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

# read single col input data
temp1 <-
  suppressMessages(read_csv(here(
    "INPUT-FILES/summary-evaluation-data-1-col.csv"
  )))

# extract column label for output by replacing white space with underscore.
temp1_name <- str_replace_all(names(temp1), " ", "_")

# use `tidyr::separate` to split a single column containing a long string into
# several cols.
temp2 <- temp1 %>%
  separate(names(temp1),
           c("q1", "r1", "q2", "r2", "q3", "r3", "q4", "r4"),
           "([:,])",
           remove = FALSE)

  temp3 <- temp2 %>% 
  select(q1, q2, q3, q4) %>% 
  mutate(across(
    everything(),
    ~ str_replace(., " ", "") %>% 
      as.integer()
  ))

vec1 <- temp2 %>% 
  select(A, B, C, D) %>% 
  filter(row_number() == 1) %>% 
  as.character() %>% 
  str_replace_all(" ", "_")

vec2 <- str_c(temp1_name, vec1, sep = "_") %>% 
  str_replace(., "__", "_")

temp4 <- temp3 %>% 
  set_names(vec2)

write_csv(temp4,
          here("OUTPUT-FILES/lms-output-test.csv"),
          na = "")
