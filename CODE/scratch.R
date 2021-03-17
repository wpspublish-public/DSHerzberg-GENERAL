suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(runner))

# special cases that this code needs to handle 

# 1. examiner stops admin before
# reaching stop rule, so streak_run never reaches 5 - we handle this by not
# enabling NA_onset until streak run reaches 5. 

# 2. examiner keeps administering after reaching stop rule, so streak_run is 6+.
# In this case we "correct" the record of an examiner who kept administering
# after the stop rule was reached. For example, an examiner who administers six
# fails in a row and then stops, when the stop rule requires stopping after five
# fails in a row.  Our correction is to change the 0 code in the 6th consecutive fail
# to NA, which is what it should have been if the examiner correctly applied the
# stop rule.


# 3. examiner admins all items without ever activating stop rule (and streak_run
# never meets that criterion). In this case handle this by not enabling
# NA_onset until streak run reaches 5.

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
      # next line will NOT code 1 if ceiling is attained on last item of subtest
      streak_val == 5 & lead(pre) == pre ~ 1,
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
      # WHAT IS THE DIFFERENCE BETWEEN NEXT TWO LINES, IN TERMS OF WHICH ROWS THEY CATCH?
      # (pre != lead(pre) | is.na(lead(pre))) & is.na(value) & ceiling_reached == 1 ~ "offset_NA",
      (pre != lead(pre) | is.na(lead(pre))) & ceiling_reached == 1 ~ "offset_NA",

      
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


temp1 <- recode_output1 %>% 
  filter(
    lag(pre) != pre | lead(pre) != pre
  )

temp2 <- recode_output1 %>% 
  filter(
    lag(pre) != pre | lead(pre) != pre | lag(pre, 2) != pre | lead(pre, 2) != pre
  )
