#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from bs4 import BeautifulSoup
import urllib3 as url
import certifi as cert
from urllib.request import urlopen
import sys 

##follwing function is to get the live stock price to yahoo
##need to get th cert to bypass the security warning imposed by the website 
def get_stock_price_yahoo(name):
    try:
        http = url.PoolManager(cert_reqs='CERT_REQUIRED', ca_certs=cert.where())
        response = http.request('GET', 'https://finance.yahoo.com/quote/' + name + '?p=' + name)
        soup = BeautifulSoup(response.data,"lxml")
        price=soup.find("span",class_="Trsdu(0.3s) Trsdu(0.3s) Fw(b) Fz(36px) Mb(-4px) D(b)").get_text()
        companyname=soup.find('h1').get_text()
        return  companyname,price
    except:
        return '-1','-1'

def get_stock_price_yahoo2(name):
    try:
        optionsUrl ='https://finance.yahoo.com/quote/' + name + '?p=' + name
        optionsPage = urlopen(optionsUrl)
        soup = BeautifulSoup(optionsPage,"html.parser")
        price=soup.find("span",class_="Trsdu(0.3s) Trsdu(0.3s) Fw(b) Fz(36px) Mb(-4px) D(b)").get_text()
        companyname=soup.find('h1').get_text()
        return companyname, price
    except:
        return '-1','-1'

#%%    


def main():
    print("----------------------------------------------------------\n")
    print("The stock price is retrieved from Yahoo Financial \n")
    print("Source Data: https://finance.yahoo.com/\n")
    print("Please Contact fsymao@gmail.com if there is any violation.\n \n \n")
    var = input("Please Enter a stock symbol to retrive the latest quote: ")
    while(str(var)!='quit'):
        companyname,temp=get_stock_price_yahoo2(str(var))
        if(temp!='-1'):
            print("The current stock price for "+str(companyname)+" is: $"+str(temp)+ "\n\n\n")
            print("Enter anther stock symbol to continue..... \n")
            print("Enter 'quit' to quit ")
            var = input("Please Enter a stock symbol to retrive the latest quote: ")
        else:
            print("No valid infomation found. Please check your validity of input\n\n\n")
            print("Enter anther stock symbol to continue..... \n")
            print("Enter 'quit' to quit ") 
            var = input("Please Enter a stock symbol to retrive the latest quote: ")
    print("Thank you for using the stock retriever.")
    
if __name__ == '__main__':
    main()





    
    






