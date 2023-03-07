# -*- coding: utf-8 -*-

# Make a list of your keywords
seznam_kw = [["Keywords"], ["inflace"], ["inflation"], ["prices"], ["cena elektriny"], ["cena plynu"], # tady prepsat keywords
             ["cena zlata"],  ["cena nafty"], ["cena benzinu"], ["cena nemovitosti"],["cena bydlleni"],
             ["cena"], ["cena ropy"], ["nakup zlata"]
             ]

#This chunk of code prints the list of keywords as csv to working directory
import csv 
#data = [['1'], ['3'], ['5'],['7']] 
file = open('keyword_list.csv', 'w+', newline ='') 
with file:     
    write = csv.writer(file) 
    write.writerows(seznam_kw) 
 

#Chunk of code, co jsem ukradl
from pytrends.request import TrendReq
import pandas as pd
import time
startTime = time.time()
pytrend = TrendReq(hl='cs-CZ', tz=360)  # tady je jazyk - en-GB je birtska anglictina, en-US americka

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
     timeframe=  '2004-01-01 2022-04-30', # casovy interval sberu, format je rok-mesic-den
     geo='CZ') # geopoliticka oblast, GB je Velka Britanie, US Spojene Staty
     data = pytrend.interest_over_time()
     if not data.empty:
          data = data.drop(labels=['isPartial'],axis='columns')
          dataset.append(data)

result = pd.concat(dataset, axis=1)
result.to_csv('search_trends.csv')

executionTime = (time.time() - startTime)
print('Execution time in sec.: ' + str(executionTime))



#From downloaded data a classical dataframe is created
prazdnej_list = []
data_matrix = pd.DataFrame(prazdnej_list)

for x in range(0, len(dataset)):
    keyword = pd.DataFrame(dataset[x])
    data_matrix = pd.concat([data_matrix, keyword], axis = 1)
    
print(data_matrix)
    
   
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    