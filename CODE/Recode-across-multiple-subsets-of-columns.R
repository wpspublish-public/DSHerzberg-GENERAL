# This is a demo of a method for recoding all values within multiple subsets of
# columns. The need for this method arose in the TOD project in 2020. In those
# data sets, we needed to use BLIMP to impute missing values for the entire data
# set, but then go back and recode to missing any cases who didn't take entire
# subtest(s). Thus to execute this method, we need both the original input data,
# so we can identify the cases that need recoding, and the imputed data set with
# now missing values, because that is the file where the column subsets need to
# be recoded to missing.

suppressMessages(library(here))
suppressMessages(library(tidyverse))

# read data from remote URL
urlRemote_path  <- "https://raw.githubusercontent.com/"
github_path <- "DSHerzberg/GENERAL/master/INPUT-FILES/"

# read input file where cases are missing for complete subsets of columns, with
# cases missing one, two or three subsets.
input_orig <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, 
        "Recode-across-multiple-subsets-of-columns-dataOrig.csv")
)))

# Read the output file that BLIMP generates when it imputes the missing data in
# `input_orig`. In this output, there is no missing data. NOTE: in this demo,
# "BLIMP-impute-output.csv" has column names. Usually, the raw output from a
# BLIMP imputation doesn't have column names. In this case, we use the argument
# `col_names = F` in the call of `read_csv()`, and we name the cols by
# insertating `setNames(c("ID", "item", "response"))` into the pipeline.
blimp_output <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, 
        "BLIMP-impute-output.csv")
))) %>% 
  pivot_wider(names_from = item,
              values_from = response) %>%
  setNames(names(input_orig))

# Initialize char vec containing the subsets of columns that may be missing in
# input_orig
col_subsets <- c("i001:i050", "i051:i084", "i085:i114", "i115:i185", 
               "i186:i206", "i207:i251", "i252:i293")

# this section has code to handle when a single case has up to three column
# subsets that need to be recoded - these are now identified in recode_cols1,
# recode_cols2, recode_cols3. 
miss_recode <- col_subsets %>%
  # map over the column subsets, map_df() returns a data frame
  map_df(
    ~
      input_orig %>%
      # Because the elements of col_subsets need to be processed as R
      # expressions (e.g., "i001:i050" uses `:` to express a range of columns),
      # we use `rlang::parse_expr()` to transform the char string into an
      # expression, and we then unquote it using `!!`. Now across() designates
      # the subset of columns to be processed by filter. filter() retains only
      # rows that are NA on all of the cols in each subset.
      filter(across(!!rlang::parse_expr(.x),
                    ~ is.na(.))) %>%
      # mutate creates a new col `recode_cols1` that holds the label of the
      # first subset of cols where that row has all NA in `input_orig`.
      # `mutate()` can evaluate `.x` without any NSE transformation, because the
      # cell values of `recode_cols1` are intended to be char strings, and we
      # are mapping over a char vector `col_subsets`,
      mutate(recode_cols1 = .x) %>%
      select(ID, recode_cols1)
  ) %>%
  # at this point, if a case has two or three missing subsets, the object has
  # dup rows for that case. We sort by ID so those dup ID rows will be adjacent to
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

# This next section handles the recoding of col subsets in the BLIMP imputed
# data set. First we join the `recode_cols` variables to blimp_output, thus
# labeling the cases that need recoding.
blimp_recode <- blimp_output %>%
  left_join(miss_recode, by = "ID") %>%
  relocate(c(recode_cols1, recode_cols2, recode_cols3), .after = "ID") %>%
  # We now pivot the data object to the tall (multilevel) format. This enables
  # us to perform recoding within a single `item` column, rather than across
  # multiple columns.
  pivot_longer(cols = c(-ID, -recode_cols1, -recode_cols2, -recode_cols3),
               names_to = c("item")) %>%
  # we now use `tidyr::extract()` to extract the start and end item of each
  # column subset, and place those values into new `start` and `end` cols.
  # `"([:alnum:]{4})?\\:?(.*)"` is a regular expression that tells how and where
  # to split the string contained in `recode_cols` into two separate strings.
  # `remove = F` leaves the input column in place. As a result, we can now
  # identify the rows to start and end the item recoding, with a simple logical
  # predicate
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
  extract(
    recode_cols3,
    into = c("start3", "end3"),
    "([:alnum:]{4})?\\:?(.*)",
    remove = F
  ) %>%
  group_by(ID) %>%
  # we now create a new col `recode_run` to label the rows that need recoding.
  # We mark this column where the `start` and `end` cols indicate the start and
  # end of the run of rows that needs recoding.
  mutate(
    recode_run =
      case_when(
        start1 == item ~ "recode1",
        end1 == item ~ "recode1",
        start2 == item ~ "recode2",
        end2 == item ~ "recode2",
        start3 == item ~ "recode3",
        end3 == item ~ "recode3",
        T ~ NA_character_
      ),
    # Here we use `runner::fill_run()` to fill in the recode_run column between
    # the labeled start and end rows. `only_within = T` limits the fill to the
    # span of rows between the start and end markers. The rows that need
    # recoding are now completely labeled in the recode_run column.
    across(c(recode_run),
           ~ runner::fill_run(., only_within = T)),
    # We use `case_when()` to recode only those labeled rows to `NA`, leaving
    # the remainder of the `values` col unchanged.
    across(c(value),
           ~ case_when(
             recode_run %in% c("recode1", "recode2", "recode3") ~ NA_real_,
             T ~ value
           ))
  ) %>%
  # We drop all cols except those needed to recreate the original wide format,
  # in which we had only the ID var and item cols. We pivot the data object back
  # to the wide format, and write the output for downstream analysis.
  select(ID, item, value) %>%
  pivot_wider(
    names_from = item,
    values_from = value) %>%
  ungroup()

write_csv(blimp_recode,
          here("OUTPUT-FILES/output-col-subsets-recoded.csv"),
          na = "")

