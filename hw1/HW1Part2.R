####Week1 Assignment#####
###Part 2 ###



##Name: Shaoyu Feng
##NetID: sf865

# 
# You will use the Kaggle Titanic Data for this assignment. 
# Use this copy so that everyone uses the same data:
# https://drive.google.com/file/d/1iAGHV19PM8c92wWxAjDG9jDM_8YGt-iO/view?usp=sharing

#switch to source directory before code execution 

###############actual code start from here########################

##note that for this dataset 
## you will need to specify quote
## also need to specify null value includes "NA" and empty string ""


data2<- read.table("Titanic_Training_Data.csv",sep =",",
                  header = TRUE, quote ="\"'", stringsAsFactors = F, na.strings = c("", "NA"))


# a)_Print the head of the data - specifically the first 10 rows.
print("The first 10 lines of the data are:")
print(head(data2,10))

# b) print the structure of the dataset (using str in R)\
print("The data structure is:")
print(str(data2))

# c) change all char types to factors.
data2[sapply(data2, is.character)]<-lapply(data2[sapply(data2, is.character)], 
                                           as.factor)
print("The data structure after changing all char to factors is:")
print(str(data2))

# d) create a frequency table (using table) for all variables
## for numeric column use summary 
## for factor column use table function 
fre_table <-lapply(data2, function(x) {
  if (is.numeric(x)) return(summary(x))
  if (is.factor(x)) return(table(x))
}) 

print("The frequcy table is: ")
print(str(fre_table))

## e) for each variable, check is there are NA values (is.na) 
#and also add up (sum) the NA values to present (print out) a total.

NAvalue <-lapply(data2, function(x) {
  #if (is.numeric(x)) return(sum(is.na(x)))
  #if (is.factor(x)) return(sum(x==""))
  sum(is.na(x))
})

print("The summary for NA values for each column is: ")
print(NAvalue)

## f) Count the number of complete rows and print this. (Use complete.cases)
print("The number of completed rows is:")
print(sum(complete.cases(data2)))

## g) Next, think of how to best clean the data. 

## data Manupulation
### AGe: Missing value is about 20% of whole dataset, will impute the missing value by mean of the column
AgemMsiingRate<- sum(is.na(data2$Age))/length(data2$Age)
### AgeMissingRate is abour 20%
data2$Age[is.na(data2$Age)] <-mean(data2$Age, na.rm=TRUE)
## Embarked: Remove the null values with most frequent items
data2$Embarked <- replace(data2$Embarked, which(is.na(data2$Embarked)), 'S')
print(sum(is.na(data2$Embarked)))
##Name column: Since there are too many names, the most important infomation within name could be the Title 
##like Mr, Miss, Dr etc. 
names<-data2$Name

title<-  strsplit(as.character(names[1]),',')







