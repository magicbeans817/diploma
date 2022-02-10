# -*- coding: utf-8 -*-


seznam_kw = [["Keywords"], ["Audi"], ["BMW"], ["Mercedes"], 
             ["Ford"], ["Fiat"], ["Vauxhall"]]

import csv 
#data = [['1'], ['3'], ['5'],['7']] 
file = open('keyword_list.csv', 'w+', newline ='') 
with file:     
    write = csv.writer(file) 
    write.writerows(seznam_kw) 
 

print("H")

from pytrends.request import TrendReq
import pandas as pd
import time
startTime = time.time()
pytrend = TrendReq(hl='en-GB', tz=360)

colnames = ["keywords"]
df = pd.read_csv("keyword_list.csv", names=colnames)
df2 = df["keywords"].values.tolist()
print(df)
print(df2)
df2.remove("Keywords")

dataset = []

for x in range(0,len(df2)):
     keywords = [df2[x]]
     pytrend.build_payload(
     kw_list=keywords,
     cat=0,
     timeframe='2020-04-01 2020-05-01',
     geo='GB')
     data = pytrend.interest_over_time()
     if not data.empty:
          data = data.drop(labels=['isPartial'],axis='columns')
          dataset.append(data)

result = pd.concat(dataset, axis=1)
result.to_csv('search_trends.csv')

executionTime = (time.time() - startTime)
print('Execution time in sec.: ' + str(executionTime))



# import numpy as np


prazdnej_list = []
data_matrix = pd.DataFrame(prazdnej_list)

for x in range(0, len(dataset)):
    keyword = pd.DataFrame(dataset[x])
    data_matrix = pd.concat([data_matrix, keyword], axis = 1)
    
print(data_matrix)
    
   
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    