suppressMessages(library(here))
suppressMessages(library(tidyverse))

input <- suppressMessages(read_csv(
  here(
    "INPUT-FILES/ABAS-3-oes-data-download.csv"
  )
)) %>% 
  select(-FirstName, -LastName, -Location) %>%
  rename(
    id = CaseId,
    scale = Title,
    item_text = label,
    item_num = ResponseItemNo,
    item_response_text = ResponseLabel,
    item_response_scored = ResponseValue
    ) %>% 
  arrange(id, AdministrationFormId)

# map TestFormSectionID onto Title: attach labels for ABAS-3 form sections.
scale_labels_lookup <- bind_cols(unique(input$TestFormSectionId), unique(input$scale)) %>% 
  set_names(c("TestFormSectionID", "ABAS3_scale"))

write_csv(scale_labels_lookup, here(
  "OUTPUT-FILES/abas3-scale-labels-lookup.csv"
))


output <- input %>%
  select(id, TestFormId, scale, item_num, item_response_scored) %>%
  pivot_wider(
    names_from = c(scale, item_num),
    names_sep = "_",
    values_from = item_response_scored,
  )


# %>% 
#   group_by(id, AdministrationFormId) %>% 
#   summarize()

dup <- input %>%
  filter(id == 312274 & TestFormSectionQuestionId == 10488)

write_csv(dup, here("dup.csv"))




