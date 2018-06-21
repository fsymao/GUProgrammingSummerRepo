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


###Additional package needed is 
##ggplot
##randomForest


require(ggplot2)
require(randomForest)

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

##Name column: Since there are too many names, and assumption is that women and child will win advantage over men
#in survival
#the most important infomation within name could be the Title 
##like Mr, Miss, Dr etc. 
names<-data2$Name
title<-  lapply(names, function(x) {  temp<- strsplit(as.character(x),",")[1][[1]][2] 
gsub(" ","",strsplit(as.character(temp),"\\.")[1][[1]][1]) 
})

title<-unlist(title)
data2$title <- title

table(data2$title)
data2$title[data2$title == 'Ms'] <- 'Miss'
data2$title[data2$title == 'Lady'] <- 'Miss'
data2$title[data2$title == 'Mlle'] <- 'Miss' 
data2$title[data2$title == 'Mme'] <- 'Miss'
data2$title[data2$title == 'Sir'] <- 'Mr'
data2$title[data2$title %in% c('Sir','Capt','Col','Major','Dr','Rev','Don','theCountess','Jonkheer')] <- 'Other'
table(data2$title)

## sibsp  and parch can be combined together to get family size 
data2$FamilySize<-data2$Parch+data2$SibSp+1


##drop cabin column, as the missing rate is high 
carbinmissingrate<- sum(is.na(data2$Cabin))/length(data2$Cabin)
data2<-data2[,! names(data2) %in% c('Cabin')]



##h) create a column called age group

data2$BinnedAge[data2$Age<=15]<-'Age 0-15'
data2$BinnedAge[data2$Age>15 & data2$Age<=29]<-'Age 16-29'
data2$BinnedAge[data2$Age>29 & data2$Age<=45]<-'Age 30-45'
data2$BinnedAge[data2$Age>45]<-'Age 46+'


##f) some exploratory analysis

data2 <- data2 %>%
  mutate(Survived = case_when(Survived==1 ~ "Yes", 
                              Survived==0 ~ "No"))
### from below plot we know, the higher the class, the higher the survival rate 
ggplot(data2, aes(Pclass, fill=Survived)) +
  geom_bar(position = "stack") +
  scale_fill_brewer(palette= "Set1" )+
  ylab("Survival Rate") +
  ggtitle("Survival Rate by PCClass")  

####  Female has a significantly higher rate than male 
ggplot(data2, aes(Sex, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette= "Set1" )+
  ylab("Survival Rate") +
  ggtitle("Survival Rate by Sex")  

### generallly, 0-15 has a higher survival rate, while other age groups survival rate are somewhat similar 
ggplot(data2, aes(BinnedAge, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette= "Set1" )+
  ylab("Survival Rate") +
  ggtitle("Survival Rate by Age Group")  

### Again, Miss and Mrs has a relative higher survival ratio

ggplot(data2, aes(title, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette= "Set1" )+
  ylab("Survival Rate") +
  ggtitle("Survival Rate by  Title")  

### No Conclusive findings on this data column 
ggplot(data2, aes(FamilySize, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette= "Set1" )+
  ylab("Survival Rate") +
  ggtitle("Survival Rate by Family size")  

### find the correlation matrix for numeric columns 
corr_matrix <-data2 %>%
  select(-PassengerId,-SibSp,-Parch) %>%
  select_if(is.numeric) %>%
  cor(use="complete.obs") 
print(corr_matrix)


#### doing some predictive analytics in predicting survival using the infomation given. 

### data prep 
feature<-data2[1:891,c('Pclass','Sex','BinnedAge','FamilySize','title','Embarked')]
response <- as.factor(data2$Survived)
feature$Survived=as.factor(data2$Survived)

smp_size <- floor(0.8 * nrow(feature))
set.seed(500)
ind=sample(seq_len(nrow(feature)), size = smp_size)
train_val=feature[ind,]
test_val=feature[-ind,]
## check data balance
print(round(prop.table(table(train_val$Survived)*100),digits = 1))
print(round(prop.table(table(test_val$Survived)*100),digits = 1))


## run a logistic regression model 
log_model <- glm(Survived ~ ., family = binomial(link=logit), 
               data = train_val)
summary(log.mod)
##get prediction for training data
train.probs <- predict(log_model, data=train_val,type =  "response")
## get the confusion matrix for trianing data 
table(train_val$Survived,train.probs>0.5)
predictionaccuracy<- (383+211)/712

print(predictionaccuracy)
test.probs <- predict(log_model, newdata=test_val,type =  "response")
table(test_val$Survived,test.probs>0.5)
testaccuracy<- (100+50)/179
print(testaccuracy)