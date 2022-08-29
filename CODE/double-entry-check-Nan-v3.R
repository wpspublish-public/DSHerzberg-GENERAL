# Create two data sets
ID <- c("A1", "B1", "C1",'D1')
aa <- c(10,20,30,20)
bb <- c('book', 'pen', 'textbook','phone')
cc <- c(TRUE,FALSE,TRUE,FALSE)
dd <- c(2.5, 8, 10, 12)
df1 <- data.frame(ID,aa,bb,cc,dd)

ID <- c("A1", "B1", "C1", "D1")
aa <- c(10,5,40,40)
bb <- c('book', 'pen', 'textbook', 'pen_case')
cc <- c(TRUE,FALSE,TRUE,TRUE)
dd <- c(2.5, 3, 10, 7)
df2 <- data.frame(ID,aa,bb,cc,dd)

# The lastest function for checking double entry (considering all condtions)
changes_by_col <- function(data1, data2){
  counter_1=c()
  counter_2=c()
  if(length(names(data1)) < length(names(data2))){
    for (i in names(data2)){
      if(!(i %in% names(data1))){
        counter_1[length(counter_1)+1] = i
        #cat("The variable",paste0(i),"is not in data1.","\n")
      }
    }
    cat("The variable(s)",paste0(counter_1,sep = ","),"is (are) not in data1.","\n")
    cat("Warning: The datasets have different number of columns.",'\n')
    #stop("Warning: The datasets have different number of columns.")
  }
  else if(length(names(data1)) > length(names(data2))){
    for(j in names(data1)){
      if(!(j %in% names(data2))){
        counter_2[length(counter_2)+1] = j
        #cat("The variable",paste0(j),"is not in data2.","\n")
      }
    }
    cat("The variable(s)",paste0(counter_2,sep = ","),"is (are) not in data2.","\n")
    cat("Warning: The datasets have different number of columns.",'\n')
    #stop("Warning: The datasets have different number of columns.", call. = FALSE)
  }
  else {
    print("The data sets have the same number of columns.")
  }
  if (!identical(names(data1),names(data2))){
    #a <- names(data1)!= names(data2)
    #cat(paste0("The name in data1 is: " , names(data1[a]),sep="\n",
    #"The name in data2 is: " , names(data2[a]),sep="\n"))
    cat("The name(s)in data1 is/are:", paste0(names(data1)),'\n')
    cat("The name(s)in data2 is/are:", paste0(names(data2)),'\n')
    cat("Warning: The datasets have different column names.","\n")
    #stop("Warning: The datasets have different column names.")
  }
  else {
    print("The data sets have the same column names.")
  }
  if (!identical(nrow(data1),nrow(data2))){
    cat("Data1 has", paste0(nrow(data1)),"obs.",'\n')
    cat("Data2 has", paste0(nrow(data2)),"obs.",'\n')
    cat("Warning: The datasets have different number of observations.")
    #stop("Warning: The datasets have different number of observations.")
  }
  else {
    print("The data sets have the same number of observations.")
  }
  
  if(!identical(nrow(df1),nrow(df2))|!identical(names(df1),names(df2))
     |!identical(ncol(df1),ncol(df2))){
    return(NULL)
    as.character(data1) != as.character(data2)
  }
  else{
    data1 != data2   
  }
}

# call the function
changes_by_col(df1,df2)
