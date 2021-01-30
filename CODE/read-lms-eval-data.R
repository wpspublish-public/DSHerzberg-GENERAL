suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

# read input data
input <-
  suppressMessages(read_csv(here(
    "INPUT-FILES/summary-evaluation-data.csv"
  )))

names_input <- names(input)

# segregate super-sub cols
df_super_sub_cols <- input %>% 
  select("General Live Webinar Experience":"Usefulness of Content")

# segregate and process date-time col
date_col <- input %>% 
  select(Completed) %>% 
  transmute(completed = lubridate::mdy_hm(Completed))

# segregate LHS and RHS side cols to leave intact for final output

lhs_cols <- input %>% 
  select("First Name", "Last Name", "Email", "Credits") %>% 
  mutate(across(where(is.logical), as.character))

rhs_cols <- input %>% 
  select("How will you use the knowledge gained from this course within your practice?": 
         "I certify that I am the person who attended the live webinar and completed this evaluation.")

# extract superordinate question name for output by replacing white space with
# underscore, adding : as a separator for the subordinate question name, col
# labels on output table are long labels containing both super- and sub- questions.
q_name <- str_c(str_replace_all(names(df_super_sub_cols), " ", "_"), ":")

# use `tidyr::separate` to split a single column containing a long string into
# several cols. 1st arg names col to be split; 2nd arg is vec of column names to
# hold the 8 parts of the split string; 3rd arg is regex specifying the two
# characters to split on; 4th arg drops input col in output df. In this input
# file, there are six super-sub questions to process, so we map() over the names
# of those six questions. In addition, those six questions have differing
# numbers of subquestions. The way to handle this is to create enough ouput cols
# (e.g., q1-r1  pairs) to handle question with the MOST subquestions. The ones
# with fewer subquestions will have all NA in those extra columns, and the empty
# cols can be dropped with janitor::remove_empty(). Here map returns a list of
# six dfs, each containing the separated subquestions and responses
# corresponding to one super-ordinate question.
list_super_sub_cols <- map(
  names(df_super_sub_cols),
  ~ df_super_sub_cols %>%
    select(!!sym(.x)) %>%
    separate(
      !!sym(.x),
      c("q1", "r1", "q2", "r2", "q3", "r3", "q4", "r4", "q5", "r5"),
      # document this regex, especially the "look around" operation
      "(:|(?<=[:digit:]),)",
      remove = TRUE
    ) %>% 
    janitor::remove_empty("cols")
)

# The cells of the final output need to be the numerical response values to the
# questions. Next snippet selects() the cols holding these response values,
# strips out white space, and formats values as integers. Here we are mapping
# over a list containing dfs, and feeding one data frame per iteration with .x
list_r_cols <- map(list_super_sub_cols,
                   ~ .x %>%
                     select(contains("r")) %>%
                     mutate(across(
                       everything(),
                       ~ str_replace(., " ", "") %>%
                         as.integer()
                     )))

# In the list of dfs, the sub-question names exist as duplicate cell values in
# cols, but we need them to be in a vec so they can be used as part of col names
# on the final output table. We select the sub-question cols, filter only the
# first row to capture just the names we need without any dups, apply
# as.character to transform the df row into a vec, and strip out whitespace.
# Here we are mapping over a list containing dfs, and feeding one data frame per
# iteration with .x. Map returns a list of vecs containing the subq names
# associated with each df.
list_sub_q_names <- map(
  list_super_sub_cols,
  ~ .x %>%
    select(contains("q")) %>%
    filter(row_number() == 1) %>%
    as.character() %>%
    str_replace_all(" ", "_")
)

# now we create the vector of col names for output by concatenating the
# super-ordinate q_name (which gets recycled), with the sub-ordinate q_names. We
# use map2() to map over a list of char vecs (.x) and a vec (.y). As we iterate
# and concatenate the col names, the current .y element recycles and gets joined
# to each of the current .x elements.
list_col_names <- map2(list_sub_q_names,
                       q_name,
                       ~
                         str_c(.y, .x, sep = "_") %>%
                         str_replace(., "__", "_"))

# we name the cols of numerical responses with the vec of names we just created.
# As previously, we map2() over the list of col names, and the list of dfs
# containing the sub question responses. This returns a list of dfs with the
# cols named as required. We used bind_cols() to bind the separate dfs into a
# single df.
named_super_sub_r_cols <- map2(list_r_cols,  list_col_names,
                                    ~ .x %>%
                                      set_names(.y)) %>% 
  bind_cols()

# configure final output

output <- bind_cols(
  lhs_cols,
  date_col,
  named_super_sub_r_cols,
  rhs_cols
)

# write output to .csv
write_csv(output,
          here("OUTPUT-FILES/lms-output.csv"),
          na = "")
