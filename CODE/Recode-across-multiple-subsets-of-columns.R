suppressMessages(library(here))
suppressMessages(library(tidyverse))

urlRemote_path  <- "https://raw.githubusercontent.com/"
github_path <- "DSHerzberg/GENERAL/master/INPUT-FILES/"

input_orig <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, 
        "Recode-across-multiple-subsets-of-columns-dataOrig.csv")
)))

blimp_output <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, 
        "BLIMP-impute-output.csv")
))) %>% 
  pivot_wider(names_from = item,
              values_from = response) %>%
  setNames(names(input_orig))

col_subsets <- c("i001:i050", "i051:i084", "i085:i114", "i115:i185", 
               "i186:i206", "i207:i251", "i252:i293")

miss_recode <- col_subsets %>%
  map_df(
    ~
      input_orig %>%
      filter(across(!!rlang::parse_expr(.x),
                    ~ is.na(.))) %>%
      mutate(recode_cols1 = .x) %>%
      select(ID, recode_cols1)
  ) %>%
  arrange(ID) %>%
   mutate(
    streak = runner::streak_run(ID),
     recode_cols2 = case_when(lead(streak) == 2 ~ lead(recode_cols1),
                             T ~ NA_character_),
    recode_cols3 = case_when(lead(streak, 2) == 3 ~ lead(recode_cols1, 2),
                             T ~ NA_character_)
  ) %>% 
  filter(streak == 1) %>% 
  select(-streak) 

knitr::kable(slice(miss_recode, 7:9))

blimp_recode <- blimp_output %>%
  left_join(miss_recode, by = "ID") %>%
  relocate(c(recode_cols1, recode_cols2, recode_cols3), .after = "ID") %>%
  pivot_longer(cols = c(-ID, -recode_cols1, -recode_cols2, -recode_cols3),
               names_to = c("item")) %>%
  extract(
    recode_cols1,
    into = c("start1", "end1"),
    "([:alnum:]{4}):(.*)",
    remove = F
  ) %>%
  extract(
    recode_cols2,
    into = c("start2", "end2"),
    "([:alnum:]{4}):(.*)",
    remove = F
  ) %>%
  extract(
    recode_cols3,
    into = c("start3", "end3"),
    "([:alnum:]{4}):(.*)",
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

