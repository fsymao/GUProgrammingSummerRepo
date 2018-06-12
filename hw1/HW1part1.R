####Week1 Assignment#####

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
current_dir="C:/Users/fengshaoyu/Desktop/GUSummer/GUProgrammingSummerRepo/hw1/DATASET1.csv"
#current_dir="Users/Shaoyu/Desktop/GU Summer/GUProgrammingSummerRepo/hw1/DATASET1.csv"
setwd(dirname(current_dir))

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

####Z test



















