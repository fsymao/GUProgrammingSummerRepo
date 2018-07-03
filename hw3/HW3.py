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
import matplotlib.pyplot

#%%
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

StudentData.to_c










