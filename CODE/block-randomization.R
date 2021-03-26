suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(blockTools))

input <- suppressMessages(
  read_csv(
    here("INPUT-FILES/osel-wps-r1-data.csv"
         ))) %>% 
  mutate(age_years = as.integer(trunc(ageinyears)),
         across(clinical, ~ case_when(
           . == 3 ~ 2,
           TRUE ~ .))) %>% 
  select(ID, age_years, clinical)

blocks <- block(input, n.tr = 3, id.vars = "ID", 
                block.vars = c("age_years", "clinical"))

assignments <- assignment(blocks, seed = 12345)

output_list <- block2seqblock(blocks, assignments, input, 
                              trn = c("coder1", "coder2", "coder3"))

output_df <- output_list[["x"]] %>% 
  rename(coder = Tr) %>% 
  relocate(coder, .after = "ID")

write_csv(output_df,
          here("OUTPUT-FILES/osel-wps-r1-coder-assignments.csv"),
          na = "")

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

write_csv(output_summ,
          here("OUTPUT-FILES/osel-wps-r1-coder-assign-strat-summ.csv"),
          na = "")
