suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(randomizr))
suppressMessages(library(blockTools))

input <- suppressMessages(
  read_csv(
    here("INPUT-FILES/osel-wps-r1-data.csv"
         ))) %>% 
  select(ID, ageinyears, ageinmonths, clinical) %>% 
  mutate(age = as.integer(trunc(ageinyears)))

blocks <- block(input, n.tr = 3, id.vars = "ID", 
             block.vars = c("age", "clinical"))

block_df <- blocks[["blocks"]][["1"]]

assign <- assignment(blocks, seed = 24)

df <- block2seqblock(blocks, assign, input)

df1 <- df[["x"]]
