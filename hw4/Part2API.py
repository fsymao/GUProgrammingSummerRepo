#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jul  8 19:06:38 2018

@author: Shaoyu Feng
"""
import urllib
from urllib.request import urlopen
import json
from pathlib import Path
import pandas as pd
#%%

def UseUrllib(BaseURL, URLPost,outfile):
    try:
        URL=BaseURL + "?"+ urllib.parse.urlencode(URLPost)
        WebURL=urlopen(URL)
        data=WebURL.read()
        encoding = WebURL.info().get_content_charset('utf-8')
        jsontxt = json.loads(data.decode(encoding))
        col_names=['zipcode', 'DateObserved', 'state', 'City', 'AQIType', 'AQIValue']
        my_df  = pd.DataFrame(columns = col_names)
        if(len(jsontxt)!=0):
            for list in jsontxt:
                    AQIType = list['ParameterName']
                    City=list['ReportingArea']
                    AQIValue=list['AQI']
                    zipcode=URLPost['zipCode']
                    DateObserved=list['DateObserved']
                    state=list['StateCode']
                    my_df=my_df.append({col_names[0]:zipcode, col_names[1]:DateObserved ,\
                    col_names[2]:state, col_names[3]:City ,\
                    col_names[4]:AQIType, col_names[5]:AQIValue \
                    }, ignore_index=True)
                    #print("For Location ", City, " zip code ", zipcode,\
                    #" in ", state," the AQI for ", AQIType,\
                    #"on ", DateObserved, " is ", AQIValue, "\n")
            print(my_df.head(5))
            my_df.to_csv(outfile, index=None, sep=' ', mode='a',header=None)
            return 1
        else:
            return -1
                #return zipcode, DateObserved, state, City, AQIType, AQIValue
    except:
        print("No revelant zipcode infomation found. Please check your input")
        return -1
            
#%%
def main():
    outfile="OUTFILE.txt"
    my_file = Path(outfile)
    if my_file.is_file():
        open("OUTFILE.txt", "w").close()
    col_names=['zipcode', 'DateObserved', 'state', 'City', 'AQIType', 'AQIValue']
    my_df  = pd.DataFrame(columns = col_names)
    my_df.to_csv(r'OUTFILE.txt', sep=' ', mode='a')
    BaseURL="http://www.airnowapi.org/aq/observation/zipCode/current/"
    ##query tool: https://docs.airnowapi.org/CurrentObservationsByZip/query
    #http://www.airnowapi.org/aq/observation/zipCode
    #/current/?format=text/csv&zipCode=20002&distance=25&
    #API_KEY=DBECFD03-FF12-42D2-9E67-778A5211D1CD    
    URLPost = {'API_KEY': 'DBECFD03-FF12-42D2-9E67-778A5211D1CD',
               'format': 'application/json',
               'zipCode':'00000',
               'distance': '25'
               }
    print("----------------------------------------------------------\n")          
    print("The AQI index is retrieved from AirNow \n")
    print("Source Data: https://docs.airnowapi.org/CurrentObservationsByZip/query \n")
    print("Please Contact fsymao@gmail.com if there is any violation.\n \n \n")   
    var = input("Please Enter a zipcode to retrive the latest AQI information: ")   
    count=1
    while(str(var)!='quit' and count<5):
        URLPost['zipCode']=str(var)
        status=UseUrllib(BaseURL, URLPost,outfile)
        if(status!=-1):
            print("\n\n")   
        else:
            print("No revelant zipcode infomation found. Please check your input\n\n")
        if(count!=4):
            print("Enter anther zipcode to continue..... \n")
            print("Enter 'quit' to quit \n ")
            var = input("Please Enter a zipcode to retrive the latest AQI information: ")
            count+=1
        else:
            break
    if(count==4):
        print("You already reached the limit for searching AQI \n")
    print("Thank you for using the AQI retriever.")
    
if __name__ == '__main__':
    main()



    
