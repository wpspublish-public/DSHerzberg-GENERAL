suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(runner))

urlRemote_path  <- "https://raw.githubusercontent.com/"
github_path <- "DSHerzberg/TOD-R/master/INPUT-FILES/"
fileName_path   <- "TOD-E-recode-above-ceiling-input.csv"

input <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, fileName_path)
)))


INPUT-FILES/test1.csv
https://github.com/wpspublish/DSHerzberg-TOD-R/blob/4b927433b7a70f3d4ed71ff61129c5f7ee3918c0/INPUT-FILES/test1.csv


urlRemote_path  <- "https://raw.github.com/"
github_path <- "wpspublish/DSHerzberg-TOD-R/master/INPUT-FILES/"
fileName_path   <- "test1.csv"

input <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, fileName_path)
)))


