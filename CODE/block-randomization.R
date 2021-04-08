suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(blockTools))
suppressMessages(library(xlsx))

# read input, correct bad codes, keep columns needed for downstream
input <- suppressMessages(
  read_csv(
    here("INPUT-FILES/osel-wps-r1-data.csv"
         ))) %>% 
  mutate(age_years = as.integer(trunc(ageinyears)),
         across(clinical, ~ case_when(
           . == 3 ~ 2,
           TRUE ~ .))) %>% 
  select(ID, age_years, clinical, OSEL_retest)

# isolate ID numbers and retest status for downstream
input_retest <- input %>% select(ID, OSEL_retest) %>% 
  rename(retest = OSEL_retest)

# block the data by age and clinical status, and assign blocked data to three
# treatment conditions (or here, three coders)
blocks <- block(input, n.tr = 3, id.vars = "ID", 
                block.vars = c("age_years", "clinical"))

# randomly assign cases to blocks
assignments <- assignment(blocks, seed = 12345)

# create a sequenced block object, which is a list containing a single df
# holding the cases assigned to each coder. These cases were randomly selected
# from within the stratification structure (by age year and clinical status))
output_list <- block2seqblock(blocks, assignments, input, 
                              trn = c("coder1", "coder2", "coder3"))

# extract the df, and format the columns
output_df <- output_list[["x"]] %>% 
  rename(coder = Tr) %>% 
  relocate(coder, .after = "ID")

# NOT SURE IF THIS BUCKET STRUCTURE IS NEEDED AFTER ALL?

# create a new df that has one row per each unique crossing (present in the
# data) of coder x age_year x clinical. These are stratification "buckets". Label
# the buckets with sequential integers.
strat_buckets <- output_df %>% 
  select(-ID) %>% 
  group_by(coder, age_years, clinical) %>% 
  count() %>%
  ungroup %>% 
  mutate(bucket = row_number())

# create a new df holding 10% of the original input cases, with this subsample
# randomly selected from within the stratification bucket structure. Label these
# cases as interrater cases, and keep only ID and the interrater label column
set.seed(12345)
input_interrater <- output_df %>% 
  left_join(strat_buckets, by = c("coder", "age_years", "clinical")) %>% 
  slice_sample(prop = .1, weight_by = clinical, replace = FALSE) %>%
  mutate(interrater = 1) %>% 
  ungroup() %>% 
  select(ID, interrater)

# create a list of three dfs, in which the cases are split by coder, and bring
# in the coding on retest and interrater to these new dfs.
coder_df_list <- map(
  c("coder1", "coder2", "coder3"),
  ~ output_df %>%
    left_join(input_retest, by = "ID") %>%
    left_join(input_interrater, by = "ID") %>%
    replace_na(list(interrater = 0)) %>% 
    filter(coder == .x)
) %>% set_names(c("coder1", "coder2", "coder3"))

# write the single df of randomized assigments to .csv
write_csv(output_df,
          here("OUTPUT-FILES/osel-wps-r1-coder-assignments.csv"),
          na = "")

# Write assignments by coder into tabbed, xlsx workbook
map2(
  c("coder1", "coder2", "coder3"),
  c(FALSE, TRUE, TRUE),
  ~
    write.xlsx(
      coder_df_list[[.x]],
      here("OUTPUT-FILES/osel-wps-r1-coder-assignments.xlsx"),
      sheetName = .x,
      row.names = FALSE,
      append = .y
    )
)

# create a summary table showing n of cases assigned to each bucket
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
          here("OUTPUT-FILES/osel-wps-r1-coder-assign-strat-summ.csv"),
          na = "")


