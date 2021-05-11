suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(blockTools))
suppressMessages(library(writexl))

# read input, correct bad codes, keep columns needed for downstream
input <- suppressMessages(
  read_csv(
    here("INPUT-FILES/osel-wps-r1-data.csv"
         ))) %>% 
  mutate(age_years = as.integer(trunc(ageinyears)),
         across(clinical, ~ case_when(
           . == 3 ~ 2,
           TRUE ~ .))) %>% 
  select(ID, age_years, clinical)

# block the data by age and clinical status, and assign blocked data to three
# treatment conditions (or here, three coders)
set.seed(12345)
blocks <- block(input, n.tr = 3, id.vars = "ID", 
                block.vars = c("age_years", "clinical"))

# randomly assign cases to blocks
assignments <- assignment(blocks, seed = 12345)

# create a sequenced block object, which is a list containing a single df
# holding the cases assigned to each coder. These cases were randomly selected
# from within the stratification structure (by age year and clinical status))
set.seed(12345)
output_list <- block2seqblock(blocks, assignments, input, 
                              trn = c("coder1", "coder2", "coder3"))

# extract the df, and format the columns
output_df <- output_list[["x"]] %>% 
  rename(coder = Tr) %>% 
  relocate(coder, .after = "ID")

# write the single df of randomized assignments to .csv
write_csv(output_df,
          here("OUTPUT-FILES/BLOCK-RANDOMIZATION/osel-wps-r1-coder-assignments.csv"),
          na = "")

# create a list of three dfs, in which the cases are split by coder.
coder_df_list <- map(
  c("coder1", "coder2", "coder3"),
  ~ output_df %>%
    filter(coder == .x)
) %>% set_names(c("coder1", "coder2", "coder3"))

# Write assignments by coder into tabbed, xlsx workbook. To create named tabs,
# supply writexl::write_xlsx() with a named list of dfs for each tab, tab names
# will be the names of the list elements
write_xlsx(coder_df_list,
           here("OUTPUT-FILES/BLOCK-RANDOMIZATION/osel-wps-r1-coder-assignments_writexl.xlsx"))

# create a summary table showing n of cases assigned to each stratification
# bucket
output_summ <- output_df %>% 
  group_by(coder) %>% 
  count(age_years, clinical) %>% 
  mutate(coder = case_when(
    lag(coder) == coder ~ NA_character_,
    TRUE ~ coder),
    age_years = case_when(
      lag(age_years) == age_years ~ NA_integer_,
      TRUE ~ age_years),
  )

# write the summary table
write_csv(output_summ,
          here("OUTPUT-FILES/BLOCK-RANDOMIZATION/osel-wps-r1-coder-assign-strat-summ.csv"),
          na = "")


