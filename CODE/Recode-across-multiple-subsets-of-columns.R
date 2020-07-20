suppressMessages(library(here))
suppressMessages(library(tidyverse))

# read input file where cases are missing for complete subsets of columns, with
# cases missing one, two or three subsets.
input_orig <- suppressMessages(read_csv(
  here(
    "INPUT-FILES/Recode-across-multiple-subsets-of-columns-dataOrig.csv"
  )
))

# Read BLIMP output from processing input_orig. In this output, there is no
# missing data; all missing cells have been imputed by BLIMP.
blimp_ouput <- suppressMessages(read_csv((
  here("INPUT-FILES/BLIMP-impute-output.csv")
))) %>%
  pivot_wider(names_from = item,
              values_from = response) %>%
  setNames(names(input_orig))

# Initialize char vec containing the subsets of columns that may be missing in
# input_orig
col_range <- c("i001:i050", "i051:i084", "i085:i114", "i115:i185", 
               "i186:i206", "i207:i251", "i252:i293")

# this section now has code to handle when a single case has up to three column
# ranges that need to be recoded - these are now identified in recode_cols1,
# recode_cols2, recode_cols3. 
miss_recode <- col_range %>%
  # map over the column ranges, map_df() returns a data frame
  map_df(
    ~
      input_orig %>%
      # Because the elements of col_range need to be processed as R expressions
      # (e.g., "i001:i050" uses `:` to express a range of columns), we use
      # `rlang::parse_expr()` to transform the char string into and expression,
      # and unquote it using `!!`. Now across() designates the subset of columns
      # to be processed by filter. filter() retains only rows that are NA on all
      # of the cols in each subset.
      filter(across(!!rlang::parse_expr(.x),
                    ~ is.na(.))) %>%
      # mutate creates a new col that holds the label of the subset of cols
      # where that row has all NA. mutate() takes `.x` without any
      # transformation, because the cell values of the new var are char strings
      mutate(recode_cols1 = .x) %>%
      select(ID, recode_cols1)
  ) %>%
  # at this point, if a case has two or three missing subsets, the object has
  # dup rows for that case. We sort by ID so those dup rows will be adjacent to
  # each other
  arrange(ID) %>%
  # runner::streak_run() processes down the rows and gives a running count of
  # consecutive identical values of ID.
  mutate(
    streak = runner::streak_run(ID),
    # Now we create two new cols which hold, for cases that have more than one
    # subset missing, the labels for those 2nd and 3rd subsets. We use
    # lead(streak) to populate recode_cols2 with the appropriate label, by
    # addressing and grabbing values from one row ahead Similarly, we use lead(n
    # = 2) to address and grab from two rows ahead.
    recode_cols2 = case_when(lead(streak) == 2 ~ lead(recode_cols1),
                             T ~ NA_character_),
    recode_cols3 = case_when(lead(streak, 2) == 3 ~ lead(recode_cols1, 2),
                             T ~ NA_character_)
  ) %>% 
  # The streak var makes it easy to keep only the first row of each set of dups,
  # simply filter() for streak == 1. Then drop the streak var as it is no longer
  # needed.
  filter(streak == 1) %>% 
  select(-streak)
 
# ########## NEXT EXCHANGE LATEST CODE WITH TOD-SPECIFIC SCRIPT

temp4 <- temp2 %>%
  left_join(miss_recode, by = "ID") %>%
  relocate(c(recode_cols1, recode_cols2), .after = "ID") %>%
  pivot_longer(cols = c(-ID, -recode_cols1,-recode_cols2),
               names_to = c("item")) %>%
  extract(
    recode_cols1,
    into = c("start1", "end1"),
    "([:alnum:]{4})?\\:?(.*)",
    remove = F
  ) %>%
  extract(
    recode_cols2,
    into = c("start2", "end2"),
    "([:alnum:]{4})?\\:?(.*)",
    remove = F
  ) %>%
  group_by(ID) %>%
  mutate(
    recode_run =
      case_when(
        start1 == item ~ "recode1",
        end1 == item ~ "recode1",
        start2 == item ~ "recode2",
        end2 == item ~ "recode2",
        T ~ NA_character_
      ),
    across(c(recode_run),
           ~ runner::fill_run(., only_within = T)),
    across(c(value),
           ~ case_when(
             recode_run %in% c("recode1", "recode2") ~ NA_real_,
             T ~ value
           ))
  ) %>%
  select(ID, item, value) %>%
  pivot_wider(
    names_from = item,
    values_from = value) %>%
  unnest()

write_csv(temp4, here(
  str_c(
    "MISSING-DATA-BLIMP/",
    file_name,
    "-noMiss-",
    format(Sys.Date(), "%Y-%m-%d"),
    ".csv"
  )
),
na = ""
)

