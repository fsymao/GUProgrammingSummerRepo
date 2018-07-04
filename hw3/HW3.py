#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jul  3 20:39:06 2018

@author: Shaoyu
"""

#%%
import pandas as pd 
import numpy as np
import sklearn
import matplotlib.pyplot as plt
from scipy import stats
from scipy.special import stdtr
from scipy.stats import ttest_ind, ttest_ind_from_stats
import seaborn as sns
#%%
from pathlib import Path

my_file = Path("OUTFILE.txt")
if my_file.is_file():
    open("OUTFILE.txt", "w").close()

#Part1: Data Pre-processing

#Read this data into Python using pandas as a dataframe.
StudentData= pd.read_csv('StudentData2.csv',sep=',')
#Clean the data according to these rules:
print(StudentData.dtypes)

#The ID must be 6 numbers in length (no more and no less).
#Remove rows with IDs that are not 6 in length.
StudentData=StudentData[(StudentData['id']>99999) & (StudentData['id']<1000000)]

#The gender is 1 for male and 2 for female. No other gender values are permitted. 
#Remove rows with genders other than 1 or 2.
StudentData=StudentData[StudentData['gender'].isin([1,2])]

#The age must be between 18 and 80. Remove rows with ages outside of this range.
StudentData=StudentData[(StudentData['age']>=18) & (StudentData['age']<=80) ]

#The gpa must be between 0 and 4.0. Remove rows with incorrect or missing gpa values.
StudentData=StudentData[(StudentData['gpa']>=0) & (StudentData['gpa']<=4.0) ]

#The sat score must be between 0 and 1600. Remove rows with sat missing or out of range.
StudentData=StudentData[(StudentData['sat']>=0) & (StudentData['sat']<=1600) ]

#There are 5 class sections (1 – 5). Remove rows with sections that are missing.
StudentData=StudentData[StudentData['section'].notnull()]

#The final can be between 0 and 100. Remove any incorrect or missing values (remove the row).
StudentData=StudentData[(StudentData['final']>=0) & (StudentData['final']<=100) ]

#The project can be between 0 and 100. Remove any incorrect or missing values (remove the row).
StudentData=StudentData[(StudentData['project']>=0) & (StudentData['project']<=100) ]

print(StudentData)

StudentData.to_csv(r'OUTFILE.txt', index=None, sep=' ', mode='a')

#%%

# =============================================================================
# Using numpy - and/or python statistical methods - perform the following statistical tests on the data:
# (a) Is there a significant difference in final scores between the two gender groups? 
# (Run an independent samples t-test and explain the p value and result. Write all results and explanations to the OUTFILE.txt.
# (b) Is there a correlation between the final and the project? 
# What is the r value? Write the results and explanations to OUTFILE.txt.
# (c) Using ANOVA, is there a significant difference between the 5 sections for final score? 
# Create box plots to visualize the 5 sections for final score.
# RE: http://www.marsja.se/four-ways-to-conduct-one- way-anovas-using-python/
# =============================================================================
#(a) Independet samples T-test 
myfile=open("OUTFILE.txt", "a")
myfile.write("-------------------------------------------------------------\n")
myfile.write("------------Part 5(a)----------------------------------------\n")
myfile.write("Null Hypothesis: There is no significant difference between  two gender groups in final score.\n ")
myfile.write("Alternative Hypothesis: There is significant difference between  two gender groups in final score\n")

## extract two data groups from pandas dataframe 
a=StudentData[StudentData['gender']==1]['final'].values
b=StudentData[StudentData['gender']==2]['final'].values


##calculate the necassary stats
abar = a.mean()
avar = a.var(ddof=1)
na = a.size
adof = na - 1

bbar = b.mean()
bvar = b.var(ddof=1)
nb = b.size
bdof = nb - 1

### calculate the t test statistics based on formula 
### reference: https://en.wikipedia.org/wiki/Welch%27s_t-test 
tf = (abar - bbar) / np.sqrt(avar/na + bvar/nb)
dof = (avar/na + bvar/nb)**2 / (avar**2/(na**2*adof) + bvar**2/(nb**2*bdof))
pf = 2*stdtr(dof, -np.abs(tf))
myfile.write("formula:  tscore = %g  pvalue = %g \n" % (tf, pf))

###verified using standard packages
# Use scipy.stats.ttest_ind.
t, p = ttest_ind(a, b, equal_var=False)
myfile.write("ttest_ind:            tscore = %g  pvalue = %g \n" % (t, p))

# Use scipy.stats.ttest_ind_from_stats.
t2, p2 = ttest_ind_from_stats(abar, np.sqrt(avar), na,
                              bbar, np.sqrt(bvar), nb,
                              equal_var=False)




myfile.write("ttest_ind_from_stats: tscore = %g  pvalue = %g \n" % (t2, p2))

myfile.write("Given p value in this T test is 0.129, we will fail to reject Null Hypothesis. \n")
myfile.write("In other words, there is no significant difference between gender groups in final score  \n")

myfile.close()
## ploting a kDE plot to visualize the distributuin of final scores in two gender groups
sns.distplot(a, hist=True, kde=True, bins=5, color = 'darkblue', label = '1')
sns.distplot(b, hist=True, kde=True, bins=5, color = 'orange', label = '2')
plt.legend(prop={'size': 16}, title = 'GenderGroups')
plt.title('Final Score of two Gender Groups')
plt.xlabel('Final Scores')

#%%
myfile=open("OUTFILE.txt", "a")
myfile.write("-------------------------------------------------------------\n")
myfile.write("------------Part 5(b)----------------------------------------\n")

#(b) Is there a correlation between the final and the project? 
##what is the r value?
myfile.write('Correlation coefficient matrix between two columns is:  \n')
myfile.write(str(np.corrcoef(StudentData['final'].values,StudentData['project'].values)))
myfile.write(' \n')
slope, intercept, r_value, p_value, std_err = stats.linregress(StudentData['final'].values,StudentData['project'])
myfile.write(' Linear regression formula is :  \n')
myfile.write("Y= %g *x  + %g \n" % (slope, intercept))
myfile.write('r-square value is :  %g \n' % r_value**2)
myfile.write("Given r^2 value of 0.81 and pvalue of near zero, we can conclude final score and project has strong correlation")
myfile.close()






