suppressMessages(library(here))
suppressMessages(library(tidyverse))
set.seed(123)

# create two input data sets
input1 <- tribble(
  ~group, ~score, ~label,
  1, 10, 'A',
  1, 20, 'B',
  1, 30, 'C',
  1, 40, 'D',
  2, 11, 'A',
  2, 21, 'B',
  2, 31, 'C',
  2, 41, 'D',
  3, 12, 'A',
  3, 22, 'B',
  4, 13, 'A',
  4, 23, 'B',
  4, 33, 'C',
  4, 43, 'D'
)

input2 <- tibble(
  ID = rep(1:5, each = 2),
  a = sample(1:100, 10), 
  b = sample(1:100, 10), 
  c = sample(1:100, 10), 
  d = sample(1:100, 10), 
  e = sample(1:100, 10)
) #%>% 
  # mutate(
  #   across(
  #     everything(),
  #     as.numeric
  #   )
  # )

# demo mutate_at() vs mutate(across()) for SINGLE COL: output1_at, output1_across are identical -----------

output1_at <- input1 %>% 
  mutate_at(
  vars(label),
  ~ case_when(
    group == 2 ~ "Z",
    T ~ .x
  )
)

output1_across <- input1 %>%
  mutate(across(
    c(label),
         ~ case_when(
           group == 2 ~ "Z",
                     T ~ .x
                     )
    ))

identical(output1_across, output1_at)

# demo mutate_at() vs mutate(across()) for MULTIPLE CONSECUTIVE COLS: output2_at, output2_across are identical -------
# If recoding to NA, NA type must match input type (e.g., integer with NA_integer_)

output2_at <- input2 %>% 
  mutate_at(
    vars(b:d),
    ~ case_when(
      ID == 2 ~ NA_integer_,
      T ~ .x
    )
  )

output2_across <- input2 %>% 
  mutate(across(
    c(b:d),
    ~ case_when(
      ID == 2 ~ NA_integer_,
      T ~ .x
    ))
  )

identical(output2_across, output2_at)
