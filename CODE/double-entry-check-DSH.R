suppressMessages(suppressWarnings(library(tidyverse)))

# Create two data sets
df1 <- tribble(
  ~ID, ~aa, ~bb, ~cc, ~dd,
  "A1", 10, "book", TRUE, 2.5, 
  "B1", 20, "pen", FALSE, 8, 
  "C1", 30, "textbook", TRUE, 10, 
  "D1", 20, "phone", FALSE, 12
)

df2 <- tribble(
  ~ID, ~aa, ~bb, ~cc, ~dd,
  "A1", 10, "book", TRUE, 2.5, 
  "B1", 5, "pen", FALSE, 3, 
  "C1", 40, "textbook", TRUE, 10, 
  "D1", 40, "pen_case", TRUE, 7
)

df1_lessCols <- df1 %>% 
  select(-aa)

df1_lessRows <- df1 %>% 
  filter(ID != "D1")

df1_diffNames <- df1 %>% 
  rename(ee = dd)

# The lastest function for checking double entry (considering all condtions)
changes_by_col <- function(data1, data2){
  counter_1=c()
  counter_2=c()
  if(length(names(data1)) < length(names(data2))){
    for (i in names(data2)){
      if(!(i %in% names(data1))){
        counter_1[length(counter_1)+1] = i
      }
    }
    cat("The variable(s)",paste0(counter_1,sep = ","),"is (are) not in data1.","\n")
    stop("Warning: The datasets have different number of columns.")
  }
  else if(length(names(data1)) > length(names(data2))){
    for(j in names(data1)){
      if(!(j %in% names(data2))){
        counter_2[length(counter_2)+1] = j
      }
    }
    cat("The variable(s)",paste0(counter_2,sep = ","),"is (are) not in data2.","\n")
    stop("Warning: The datasets have different number of columns.")
  }
  else {
    print("The data sets have the same number of columns.")
  }
  
  if (!identical(names(data1),names(data2))){
    a <- names(data1)!= names(data2)
    cat("The name(s)in data1 is/are:", paste0(names(data1[a])),'\n')
    cat("The name(s)in data2 is/are:", paste0(names(data2[a])),'\n')
    stop("Warning: The datasets have different column names.")
  }
  else {
    print("The data sets have the same column names.")
  }
  if (nrow(data1)!= nrow(data2)){
    cat("Data1 has", paste0(nrow(data1)),"cases.",'\n')
    cat("Data2 has", paste0(nrow(data2)),"cases.",'\n')
    stop("Warning: The datasets have different number of cases.")
  }
  else {
    print("The data sets have the same number of cases.")
  }
  if(is.factor(data1)){
    as.character(data1) != as.character(data2)
  }else{
    data1 != data2
  }
}

# call the above function
change1 <- changes_by_col(df1,df2)
change2 <- changes_by_col(df1_lessCols, df2)
change3 <- changes_by_col(df1_lessRows, df2)
change4 <- changes_by_col(df1_d, df2)
