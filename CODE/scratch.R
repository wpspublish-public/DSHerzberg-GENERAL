suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(runner))

urlRemote_path  <- "https://raw.github.com/"
github_path <- "wpspublish/DSHerzberg-GENERAL/master/INPUT-FILES/"
fileName_path   <- "recode-above-ceiling-input.csv"

input <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, fileName_path)
)))

input_tall <- input %>%
  pivot_longer(
    cols = -ID,
    names_to = c("pre", "num"),
    names_sep = 3
  )  %>%
  group_by(ID, pre) %>%
  mutate(
    streak_val = case_when(value == 0 ~ streak_run(value, na_rm = F),
                           TRUE ~ NA_integer_),
    ceiling = case_when(
      streak_val == 5 ~ 1,
      TRUE ~ 0)
  )

ceiling <-  input_tall %>% 
  group_by(ID, pre) %>% 
  summarise(ceiling_count = sum(ceiling)) %>% 
  mutate(ceiling_reached = case_when(
    ceiling_count >= 1 ~ 1,
    TRUE ~ NA_real_
  )) %>% 
  select(-ceiling_count)

recode_output1 <- input_tall %>% 
  left_join(ceiling, by = c("ID", "pre")) %>%
  group_by(ID) %>%
  mutate(
    NA_status = case_when(
      (pre != lead(pre) | is.na(lead(pre))) & is.na(value) & ceiling_reached == 1 ~ "offset_NA",
      # WHAT IS THE DIFFERENCE BETWEEN NEXT TWO LINES, IN TERMS OF WHICH ROWS THEY CATCH?
      # is.na(value) & !is.na(lag(value)) & pre == lag(pre) & ceiling_reached == 1 ~ "onset_NA",
      lag(ceiling) == 1 & pre == lag(pre) & ceiling_reached == 1~ "onset_NA",
      TRUE ~ NA_character_
    )
  ) %>% 
  group_by(ID, pre) %>% 
  mutate(
    across(
      c(NA_status),
      ~ fill_run(.)
    )
  ) %>% 
  mutate(new_val = case_when(NA_status %in% c("onset_NA", "offset_NA") ~ 0,
                             TRUE ~ value)) %>%
  pivot_wider(
    id_cols = ID,
    names_from = c(pre, num),
    names_sep = "",
    values_from = new_val
  )



temp1 <- recode_output %>% filter(
  NA_status == "onset_NA" | lead(NA_status) == "onset_NA" | lag(NA_status) == "onset_NA"
)

temp2 <- recode_output %>% filter(
  (
    streak_val == 1 | is.na(streak_val) & lead(streak_val) == 1 |
      is.na(streak_val) & lag(streak_val) == 1
  ) &
    (
      NA_status == "onset_NA" |
        lead(NA_status) == "onset_NA" | lag(NA_status) == "onset_NA"
    )
)

temp3 <- recode_output %>%
  mutate(laglead = case_when(
    lag(NA_status) == "onset_NA" |
      lead(NA_status) == "onset_NA" ~ 1,
    TRUE ~ NA_real_
  )) %>% 
  filter(
    NA_status == "onset_NA" | laglead == 1
  )

temp4 <- temp3 %>%
  filter(
    (NA_status == "onset_NA" & lag(streak_val) == 1) | laglead == 1
  )

temp5 <- input_tall %>%
  mutate(laglead = case_when(
    lag(NA_status) == "onset_NA" |
      lead(NA_status) == "onset_NA" ~ 1,
    TRUE ~ NA_real_
  ))

temp6 <- input_tall %>% 
  filter(
    ceiling == 1 & streak_val != 5
  )
