suppressMessages(library(here))
suppressMessages(library(tidyverse))

input <- suppressMessages(read_csv(
  here(
    "INPUT-FILES/ABAS-3-oes-data-download.csv"
  )
)) %>% 
  # select(-FirstName, -LastName, -Location) %>% 
  select(-FirstName, -LastName) %>% 
  rename(
    id = CaseId,
    scale = Title,
    item_text = label,
    item_num = ResponseItemNo,
    item_response_text = ResponseLabel,
    item_response_scored = ResponseValue
    ) %>% 
  arrange(id, AdministrationFormId) %>% 
  group_by(id, AdministrationFormId) %>% 
  summarize()

dup <- input %>% 
  filter(CaseId == 312274 & TestFormSectionQuestionId == 10488)

write_csv(dup, here("dup.csv"))
  
output <- input %>% 
  select(id, AdministrationFormId, TestFormSectionQuestionId, item_response_scored) %>% 
  pivot_wider(
    names_from = TestFormSectionQuestionId,
    values_from = item_response_scored,
  )

