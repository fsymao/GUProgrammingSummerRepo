####Week1 Assignment#####


##Name: Shaoyu Feng
##NetID: sf865


#---------------------Information about the dataset-----------------------------------------------------------------#
# Source:
#   
# Data comes from an original (non-machine-learning) study: 
# Warwick J Nash, Tracy L Sellers, Simon R Talbot, Andrew J Cawthorn and Wes B Ford (1994) 
# "The Population Biology of Abalone (_Haliotis_ species) in Tasmania. I. Blacklip Abalone (_H. rubra_) from the North Coast and Islands of Bass Strait", 
# Sea Fisheries Division, Technical Report No. 48 (ISSN 1034-3288) 

## The dataset comes from https://archive.ics.uci.edu/ml/datasets/Abalone

# Data Set Information:
#   
# Predicting the age of abalone from physical measurements. 
#The age of abalone is determined by cutting the shell through the cone, staining it, 
#and counting the number of rings through a microscope -- a boring and time-consuming task. 
#Other measurements, which are easier to obtain, are used to predict the age. 
#Further information, such as weather patterns and location (hence food availability) may be required to solve the problem. 
#From the original data examples with missing values were removed (the majority having the predicted value missing), 
#and the ranges of the continuous values have been scaled.


## Dataset Attribute Information 
#
# Given is the attribute name, attribute type, the measurement unit and a
# brief description.  The number of rings is the value to predict: either
# as a continuous value or as a classification problem.
# 
# Name		Data Type	Meas.	Description
# ----		---------	-----	-----------
#   Sex		nominal			M, F, and I (infant)
# Length		continuous	mm	Longest shell measurement
# Diameter	continuous	mm	perpendicular to length
# Height		continuous	mm	with meat in shell
# Whole weight	continuous	grams	whole abalone
# Shucked weight	continuous	grams	weight of meat
# Viscera weight	continuous	grams	gut weight (after bleeding)
# Shell weight	continuous	grams	after being dried
# Rings		integer			+1.5 gives the age in years
#---------------------------------------------------------------------------------------------------------------------#


# please switch to the source file directory as working directory before code execution

if(file.exists("MYOUTFILE.txt"))
{
  file.remove("MYOUTFILE.txt")
  file.create("MYOUTFILE.txt")
}

#---------------------Actual Code Statr from Here--------------------------------------------------------------------#
library(dplyr)
#read data from csv file
data<- read.table("DATASET1.csv",sep =",",
                  header = TRUE, quote = "", stringsAsFactors = F)
#---Part I (a)-----create new feature via bining
# bin the WholeWeight column based on quantile 
quantarr=quantile(data$WholeWeight, probs = c(.2, .4, .6,.8))
#bining the Wholeweight column using cut function
levels<- c(-Inf,quantarr,Inf)
labels <- c("A", "B", "C", "D", "E")
#new categorical column is called WeightBin
data$WeightBin <- cut(data$WholeWeight, levels,labels=labels)

sink('MYOUTFILE.txt')
cat("---This is the head () output after adding first categorical column---\n")
cat("\n")
head(data)
cat("\n")
cat("------------------------------------------------------------------------------")
cat("\n")
sink()

# adding second binary catrgorical column 
#Creating binary categorical column based on Length column
data$LongAbalone<-ifelse(data$Length>mean(data$Length), 1,0)
sink('MYOUTFILE.txt',append="True")
cat("---This is the head () output after adding second categorical column---\n")
cat("\n")
head(data)
cat("\n")
cat("------------------------------------------------------------------------------")
cat("\n")
sink()

#---Part I (b)-----create four functions to do statistical testing

sink('MYOUTFILE.txt',append="True")
cat("---Part I(b) starts from here---\n")
cat("\n")
cat("\n")
sink()


###function 1 Anova
AnovaTest<- function(factors, response, rawdata)
{
  results=aov(response ~ factors, data=rawdata)
  return (summary(results))
}

sink('MYOUTFILE.txt',append="True")
cat("---Anova Test---\n")
cat("---Feature column: WholeWeight---\n")
cat("---Response column:rings---\n")
AnovaTest(data$WholeWeight,data$Rings,data)
cat("\n")
cat("\n")
sink()

# Function 2 T test
### Independent sample T test
## need to consider numeric and binary factory case
## define one more function to 


is.binary <- function(v) {
  x <- unique(v)
  length(x) - sum(is.na(x)) == 2L && all(x[1:2] == 0:1)
}

tTest<- function(x,y)
{
if (is.binary(x))
{
  cat("---T Test for binary factor---\n")
  t.test(y~x)
}
else
{
  cat("---T Test for numeric factor---\n")
  t.test(y,x)
}
}

sink('MYOUTFILE.txt',append="True")
cat("---T Test---\n")
tTest(data$LongAbalone,data$Height)
tTest(data$WholeWeight,data$Height)

cat("\n")
cat("\n")
sink()

####Function 3: Z test

zTest = function(a, mu, var){
  zeta <- (mean(a) - mu) / (sqrt(var / length(a)))
  p_val<- pnorm(zeta,0,1)
  return(zeta)
}

sink('MYOUTFILE.txt',append="True")
cat("---Z Test---\n")
cat("---Z Test Score is: ---\n")
zTest(data$Diameter,0.4,0.1)
cat("---Z Test pvalue is: ---\n")
pnorm(zTest(data$Diameter,0.4,0.1),0,1)
cat("\n")
cat("\n")
sink()

####Function4: summary statistis for quantitative variable

sumStat = function(a){
  if (is.numeric(a))
  {
    return (summary(a))
  }
  else 
    cat("This is not a numerical column")
}

sink('MYOUTFILE.txt',append="True")
cat("---Summary Statistic---\n")
cat("---SUmmary Statistic for numeric column: ---\n")
sumStat(data$Length)
cat("---SUmmary Statistic for non-numeric column: ---\n")
sumStat(data$WeightBin)
cat("\n")
cat("\n")
sink()

#---Part I (C)-----Basic Ploting function 

# plot1: Boxplots for at least three variables on the same plot
# make use of R's ggplot library
library(ggplot2)
require(reshape2)
data_part<-data[,c("Length","Diameter","Height","WholeWeight","Sex")]
data_transformed <- melt(data_part, id.var = "Sex")

p <- ggplot(data = data_transformed,aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=Sex))
p<- p+ facet_wrap( ~ variable, scales="free")
p <- p + xlab("Ablone Weight and Height  Attribute") + ylab("Ablone Attribute Value") + ggtitle("Sex V.S. Size and Weight of Ablone")
p <- p + guides(fill=guide_legend(title="Ablone Sex"))
p

##make use of Base R ploting library
new_order <- with(data_transformed, reorder(variable , value, mean , na.rm=T))
par(mar=c(3,4,3,1))
myplot=boxplot(data_transformed$value ~ data_transformed$Sex*data_transformed$variable,data=data$data_transformed,boxwex=0.8,ylab="Ablone Attribute Value",
               main="Sex V.S. Size and Weight of Ablone" , col=c("tomato" , "green","blue"))
# Add a legend
legend("topleft", legend = c("Sex:F", "Sex:I","Sex:M"), col=c("tomato" , "green","blue"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1.2,  horiz = F, inset = c(0.1, 0.1))

# plot2: Histogram with normal curve
x<-data$Rings
h<-hist(x,breaks=10,col='blue', xlab="Ablone Rings distribution", main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=50)
yfit<-dnorm(xfit, mean=mean(x),sd=sd(x))
yfit<-yfit*diff(h$mids[1:2])*length(x)
lines(xfit,yfit,col="red",lwd=2)

#plot3: Scatter Plot between two variables
plot(data$Height,data$Rings, xlab="Ablone Height",ylab="Ablone Rings(Indicator of Age)",  col="blue", main="Scatter Plot: Height V.S Age")

#plot multiple bar plot 

data_bar <- table(data$Sex, data$Rings)
barplot(data_bar, main="Ablone Distributon by Sex and Rings",
        xlab="Numer of Rings of Ablone", ylab="Number of abloe",col=c("tomato","green","blue"),
        legend = rownames(data_bar), beside=TRUE)



#----------------------------------------------------------------------------

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


require(ggplot2)

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

agegroupcnt<-table(data2$BinnedAge)

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
summary(log_model)
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



