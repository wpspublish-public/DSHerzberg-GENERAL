temp_input <- input_orig %>% 
  filter(ID == 230010) %>% 
  select(ID, i051:i060) %>% 
  mutate(data = "input_orig") %>% 
  relocate(data, .after = ID)

temp_blimp_out <- blimp_output %>% 
  filter(ID == 230010) %>% 
  select(ID, i051:i060) %>% 
  mutate(data = "blimp_output") %>% 
  relocate(data, .after = ID)

temp_blimp_recode <- blimp_recode %>% 
  filter(ID == 230010) %>% 
  select(ID, i051:i060) %>% 
  mutate(data = "blimp_recode") %>% 
  relocate(data, .after = ID)

comp <- bind_rows(temp_input,
                  temp_blimp_out,
                  temp_blimp_recode)
knitr::kable(comp)
