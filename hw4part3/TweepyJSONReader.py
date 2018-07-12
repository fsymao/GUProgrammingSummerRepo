# -*- coding: utf-8 -*-
"""
Created on Tue Jul 10 15:50:32 2018

@author: fengshaoyu
"""

# -*- coding: utf-8 -*-
"""
Created on Wed Mar  8 20:53:33 2017

@author: Ami
"""

#TweepyJSONReader.py
##Reads JSON files of tweets that Tweepy collects
#Gates

##This program will read a .txt file
## that was created by TwitterMining.py
## Set the filename correctly

## This program will create two files:
## "TwitterResultsRaw.txt" which is all the words
## "TwitterWordFrq.txt" which is each word and its frequency


import json
from nltk.tokenize import word_tokenize
from nltk.tokenize import TweetTokenizer
import re
#https://docs.python.org/3/library/re.html
#%%
linecount=0
hashcount=0
wordcount=0
BagOfWords=[]
BagOfHashes=[]
BagOfLinks=[]

### SET THE FILE NAME ###

tweetsfile="file_DataScience.txt"

###################################

with open(tweetsfile, 'r') as file:
    for line in file:
        #print(line,"\n")
        tweetSplitter = TweetTokenizer(strip_handles=True, reduce_len=True)
        WordList=tweetSplitter.tokenize(line)
        #WordList2=word_tokenize(line)
        #linecount=linecount+1
        #print(WordList)
        #print(len(WordList))
        #print(WordList[0])
        #print(WordList2)
        #print(len(WordList2))
        #print(WordList2[3:6])
        #print("NEXT..........\n")
        regex1=re.compile('^#.+')
        regex2=re.compile('[^\W\d]') #no numbers
        regex3=re.compile('^http*')
        regex4=re.compile('.+\..+')
        for item in WordList:
            if(len(item)>2):
                if((re.match(regex1,item))):
                    #print(item)
                    newitem=item[1:] #remove the hash
                    BagOfHashes.append(newitem)
                    hashcount=hashcount+1
                elif(re.match(regex2,item)):
                    if(re.match(regex3,item) or re.match(regex4,item)):
                        BagOfLinks.append(item)
                    else:
                        BagOfWords.append(item)
                        wordcount=wordcount+1
                else:
                    pass
            else:
                pass
            
    
        
       
#print(linecount)            
#print(BagOfWords)
#print(BagOfHashes)
#print(BagOfLinks)
BigBag=BagOfWords+BagOfHashes




#list of words I have seen
seenit=[]
#dict of word counts
WordDict={}
Rawfilename="TwitterResultsRaw.txt"
Freqfilename="TwitterWordFrq.txt"


#FILE=open(Freqfilename,"w")
#FILE2=open(Rawfilename, "w")
R_FILE=open(Rawfilename,"w")
F_FILE=open(Freqfilename, "w")

IgnoreThese=["and", "And", "AND","THIS", "This", "this", "for", "FOR", "For", 
             "THE", "The", "the", "is", "IS", "Is", "or", "OR", "Or", "will", 
             "Will", "WILL", "God", "god", "GOD", "Bible", "bible", "BIBLE",
             "CanChew", "Download", "free", "FREE", "Free", "will", "WILL", 
             "Will", "hits", "hit", "within", "steam", "Via", "via", "know", "Study",
             "study", "unit", "Unit", "always", "take", "Take", "left", "Left",
             "lot","robot", "Robot", "Lot", "last", "Last", "Wonder", "still", "Still",
             "ferocious", "Need", "need", "food", "Food", "Flint", "MachineCredit",
             "Webchat", "luxury", "full", "fifdh17", "New", "new", "Caroline",
             "Tirana", "Shuayb", "repro", "attempted", "key", "Harrient", 
             "Chavez", "Women", "women", "Mumsnet", "Ali", "Tubman", "girl","Girl",
             "CSW61", "IWD2017", "Harriet", "Great", "great", "single", "Single", 
             "tailoring", "ask", "Ask"]
###Look at the words
for w in BigBag:
    if(w not in IgnoreThese):
        rawWord=w+" "
        R_FILE.write(rawWord)
        if(w in seenit):
            #print(w, seenit)
            WordDict[w]=WordDict[w]+1 #increment the times word is seen
        else:
            ##add word to dict and seenit
            seenit.append(w)
            WordDict[w]=1
    
#print(WordDict)  
#print(seenit)
#print(BagOfWords)



for key in WordDict:
    #print(WordDict[key])
    if(WordDict[key]>1):
        if(key not in IgnoreThese):
            #print(key)
            Key_Value=key + "," + str(WordDict[key]) + "\n"
            F_FILE.write(Key_Value)


#FILE.close()
#FILE2.close()
R_FILE.close()
F_FILE.close()