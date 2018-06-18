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


