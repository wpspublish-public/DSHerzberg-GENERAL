###### LOAD PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

### READ DATA ------------------------------

Teen_1221_Home_Stand <- suppressMessages(read_csv(
  here(
"INPUT-FILES/Teen-1221-Home-T-Scores-per-case-4080T.csv"
)))

Teen_1221_School_Stand <- suppressMessages(read_csv(
  here(
"INPUT-FILES/Teen-1221-School-T-Scores-per-case-4080T.csv"
)))

Teen_1221_Self_Stand <- suppressMessages(read_csv(
  here(
"INPUT-FILES/Teen-1221-Self-T-Scores-per-case-4080T.csv"
)))

### BUILD DATA FRAME WITH FORM, ITEM, MEDIAN COLS ------------------------------

# Create char vec with names of three data files to be processed. ls() returns
# names of objects in environment. pattern allows subsetting of objects by
# matching patterns in their names.
data <- c(ls(pattern = "Stand"))

# Create char vec with names of three forms in the three data files. Use stringr
# functions to extract those names from the data vec, which contains the file
# names.
forms <- str_replace(str_sub(data, 1, -7), "_", "-")

# The desired output is a data frame in which there is a row for each item and
# its median score, and the rows corresponding to the three forms are stacked on
# top of one another. purrr::pmap_df() is used here because it can handle
# multiple input lists and or vectors, and because it returns a dataframe.

# The two inputs are the three data sets and the three form names. mget() takes
# a char vec as its argument, and returns the objects named by the elements of
# the char vec. We need to input the data objects themselves, not their names,
# into pmap(). The elements of the forms vec are used by the mapped function as
# quoted strings, so we can use the char vec itself be the input to pmap().
item_medians_stand <- list(
  mget(data),
  forms
) %>% 
  pmap_df(
    # pmap() takes a list of objects (lists, data frames, vectors, etc) as its
    # input. ..1 is the token for the first element of that input list, which in
    # this case is a list of data frames.
    ~ ..1 %>%
      # Because our goal is to obtain the median score for each item, we need
      # only process the item cols, which we select() using the tidyselect
      # helper contains(), which in this instance keeps only the item cols, all
      # of which are named with strings that begin with "q0".
      select(contains("q0")) %>% 
      # pivot_longer transforms the data object, which now consists only of the
      # items cols, from a wide format to a long format. In the long format, the
      # rows of the input object are transformed into a pair of columns, here
      # consisting of the item name and its response for each case. Although
      # IDNumber is not part of the object, the items are nested within cases,
      # because each row of the input object represented one case. In the long
      # (tall) data object, the entire run of item numbers keeps repeating going
      # down the rows, because cases are stacked on top of one another in the
      # long format.
      pivot_longer(everything(), names_to = "item") %>% 
      # We now group the data by item name, because our intent is to get a
      # summary measure across all cases for each item. In this instance, the
      # summary measure we want is the median score of the item across all
      # cases. summarize() thus transforms the tall data object with thousands
      # of rows into a summary object with 80 rows, one for each item.
      group_by(item) %>% 
      summarize(median = round(median(value))) %>% 
      # We add a new column with mutate(). This column holds the form name in
      # the first row of the table corresponding to each form. Thus, when the
      # three form tables are stacked on top of one another in the output, there
      # is a demarcation where the medians of one form stop, and those of the
      # next form begin. case_when() is used to place the form name in the first
      # row only, where rownames(.)  == "1" returns TRUE. When that logical
      # condition obtains, case_when() inserts the value (for that iteration of
      # pmap()) of the input char vec `forms`, represented in the function by
      # the token ..2.
      mutate(form = case_when(
        rownames(.) == "1" ~ ..2,
        T ~ NA_character_
      )) %>%
      # relocate() is more efficient that select() for resequencing columns
      relocate(form, .before = item)
  )

###### WRITE OUTPUT -----------------------------------------------------------

write_csv(
  item_medians_stand,
  here(
    "OUTPUT-FILES/SPM2-item-medians-stand.csv"
  ),
  na = ""
)
