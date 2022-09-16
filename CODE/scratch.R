test_names <-
  c(
    "iws",
    "bln",
    "seg",
    "iwr",
    "lem",
    "pan",
    "lvc",
    "rws",
    "sub",
    "del",
    "nwr",
    "wom",
    "gea",
    "ssl"
  )
temp1 <- input %>% 
  select(contains("iws")) %>% 
  filter(if_any(everything(), ~ !is.na(.)))

list <- map(
  test_names,
  ~
    input %>% 
    select(ID, contains(.x)) %>% 
    filter(if_all(everything(), ~ is.na(.)))
  )



data <- tribble(
  ~colA, ~colB, ~colC,
  NA, NA, NA,
  2, NA, NA,
  3, "c", NA,
  4, "d", "red"
)

temp_filter <- data %>% 
  filter(if_any(everything(), ~ !is.na(.))) #captures rows with at least one non-NA
# filter(if_all(everything(), ~ !is.na(.))) #captures rows with no NA
# filter(if_any(everything(), ~ is.na(.))) #captures rows with at least one NA
# filter(if_all(everything(), ~ is.na(.))) #captures  rows that are all NA


