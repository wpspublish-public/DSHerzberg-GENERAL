temp1 <- suppressMessages(
  read_csv(
    (here("INPUT-FILES/TOD-impute-2020-07-17-1.csv")), col_names = F)) %>% 
  setNames(c("ID", "item", "response"))

temp2 <- input_orig %>% 
  left_join(temp1, by = "ID") %>% 
  select(ID, item, response)




temp3 <- temp2 %>% 
  pivot_wider(
    names_from = item,
    values_from = response
  ) 

NA_count <- sum(is.na(temp3))



names(temp1) <- c("ID", "item", "response")
temp2 <- temp1 %>% 
  pivot_wider(
    names_from = item,
    values_from = response
  ) 
names(temp2) <- names(input_orig)
