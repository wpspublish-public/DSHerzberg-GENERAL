suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

# read input data
input <-
  suppressMessages(read_csv(here(
    "INPUT-FILES/summary-evaluation-data.csv"
  )))

names_input <- names(input)

# process date-time data
date_col <- input %>% 
  select(Completed) %>% 
  transmute(completed = lubridate::mdy_hm(Completed))

# process super-sub cols
super_sub_cols <- input %>% 
  select("Quality of Instruction – Instructor Ratings":"Learning Objectives")

# SINGLE COLUMN SCRIPT ----------------------------------------------------

# read single col input data
input <-
  suppressMessages(read_csv(here(
    "INPUT-FILES/summary-evaluation-data-1-col.csv"
  )))

# extract superordinate question name for output by replacing white space with
# underscore, adding : as a separator for the subordinate question name, col
# labels on output table are long labels containing both super- and sub- questions.
q_name <- str_c(str_replace_all(names(input), " ", "_"), ":")

# use `tidyr::separate` to split a single column containing a long string into
# several cols. 1st arg names col to be split; 2nd arg is vec of column names to
# hold the 8 parts of the split string; 3rd arg is regex specifying the two
# characters to split on; 4th arg keeps input col in output df
temp1 <- input %>%
  separate(names(input),
           c("q1", "r1", "q2", "r2", "q3", "r3", "q4", "r4"),
           "([:,])",
           remove = FALSE)

# The cells of the final output need to be the numerical response values to the
# questions. Next snippet selects() the cols holding these response values,
# strips out white space, and formats values as integers.
r_cols <- temp1 %>% 
  select(r1, r2, r3, r4) %>% 
  mutate(across(
    everything(),
    ~ str_replace(., " ", "") %>% 
      as.integer()
  ))

# in temp1, the sub-question names exist as duplicate cell values in cols, but
# we need them to be in a vec so they can be used as part of col names on the
# final output table. We select the sub-question cols, filter only the first row
# to capture just the names we need without any dups, apply as.character to
# transform the df row into a vec, and strip out whitespace.
sub_q_names <- temp1 %>% 
  select(q1, q2, q3, q4) %>% 
  filter(row_number() == 1) %>% 
  as.character() %>% 
  str_replace_all(" ", "_")

# now we create the vector of col names for output by concatenating the
# super-ordinate q_name (which gets recycled), with the sub-ordinate q_names,
col_names <- str_c(q_name, sub_q_names, sep = "_") %>% 
  str_replace(., "__", "_")

# we name the cols of numerical responses with the vec of names we just created.
output <- r_cols %>% 
  set_names(col_names)

write_csv(output,
          here("OUTPUT-FILES/lms-output-test.csv"),
          na = "")
