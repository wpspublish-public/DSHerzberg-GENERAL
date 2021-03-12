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

  
