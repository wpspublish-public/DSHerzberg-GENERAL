suppressMessages(library(here))
suppressMessages(library(tidyverse))

urlRemote_path  <- "https://raw.github.com/"
github_path <- "wpspublish/DSHerzberg-GENERAL/master/INPUT-FILES/"

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
                             TRUE ~ NA_character_),
    recode_cols3 = case_when(lead(streak, 2) == 3 ~ lead(recode_cols1, 2),
                             TRUE ~ NA_character_)
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
    "([[:alnum:]]{4}):(.*)",
    remove = FALSE
  ) %>%
  extract(
    recode_cols2,
    into = c("start2", "end2"),
    "([[:alnum:]]{4}):(.*)",
    remove = FALSE
  ) %>%
  extract(
    recode_cols3,
    into = c("start3", "end3"),
    "([[:alnum:]]{4}):(.*)",
    remove = FALSE
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
        TRUE ~ NA_character_
      ),
     across(recode_run,
           ~ runner::fill_run(., only_within = TRUE)),
     across(value,
           ~ case_when(
             recode_run %in% c("recode1", "recode2", "recode3") ~ NA_real_,
             TRUE ~ value
           ))
  ) %>%
  select(ID, item, value) %>%
  pivot_wider(
    names_from = item,
    values_from = value) %>%
  ungroup()

write_csv(blimp_recode,
          here("OUTPUT-FILES/output-col-subsets-recoded.csv"),
          na = "")

