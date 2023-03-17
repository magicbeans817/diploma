# -*- coding: utf-8 -*-

# Make a list of your keywords
seznam_kw = [["Keywords"], ["inflace"], ["inflation"], ["prices"], ["cena elektriny"], ["cena plynu"], # tady prepsat keywords
             ["cena zlata"],  ["cena nafty"], ["cena benzinu"], ["cena nemovitosti"],["cena bydleni"],
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
import pandas as pd
import time
startTime = time.time()


from pytrends.request import TrendReq as UTrendReq
GET_METHOD='get'

import requests

headers = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/110.0',
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8',
    'Accept-Language': 'en-US,en;q=0.5',
    # 'Accept-Encoding': 'gzip, deflate, br',
    'Alt-Used': 'trends.google.com',
    'Connection': 'keep-alive',
    'Cookie': 'AEC=ARSKqsK5L18XuHgUF1F74E40Ghm-sNozU1jLxpcMayyHO4BiKnQRxV718A; CONSENT=PENDING+973; SOCS=CAISHAgCEhJnd3NfMjAyMzAxMjQtMF9SQzIaAmNzIAEaBgiA7taeBg; NID=511=fm-9uN9aCT1Le5fMAsVQ5kIOf793_u2-djs9V2EL1bh8ILzYBaywNfygAAWGmrVb6Mtp3fJjLc35xtM1UDD8kdlDqafRRqGDSt-gnl2hz8-P5fzHMlcYNhsiPy1VzOiAfBjjmlzVrh4tdN4K6YkuAlnGc_I9RSs-cDsL0zCljnpCCtXjKydisVPdUncHxjxMBxjLCrUywRcHP-72QFIzq8ElmcG0iSuRLCC1BVY; SID=UQjmaDiDkwKLiDV8ZVxYjZ-kOVSeZ0QFkFeN3KjVRlR7GJkfW8VSzbkw2YPLYdK5u-GwhQ.; __Secure-1PSID=UQjmaDiDkwKLiDV8ZVxYjZ-kOVSeZ0QFkFeN3KjVRlR7GJkflRzM61gLRFL36WDZB1F0pg.; __Secure-3PSID=UQjmaDiDkwKLiDV8ZVxYjZ-kOVSeZ0QFkFeN3KjVRlR7GJkf1SgyjtFFd-Cxr3bSVNVEiA.; HSID=ACjwFnBjriv90GLFc; SSID=AR8ajqxV-i9O8_Y9O; APISID=V-HFjTMY3J8EGI2x/AouoWz1hc_r6cAifA; SAPISID=9M3iAyXh7epujisP/AmV0FcJf_1bFqExhR; __Secure-1PAPISID=9M3iAyXh7epujisP/AmV0FcJf_1bFqExhR; __Secure-3PAPISID=9M3iAyXh7epujisP/AmV0FcJf_1bFqExhR; SIDCC=AFvIBn_jLmqF53NJZ7mHsZzLo79FuxXrB3TLj0FpC6hmn2PVQ1AK1zgdvkMzsDK8gOo66osT; __Secure-1PSIDCC=AFvIBn8zb1ckucFUWEYo4Af5rpp1SaOz2Lzw_aHTU1p4GNk1ojD74Fs9xBkt_Jy43rrA-lRn; __Secure-3PSIDCC=AFvIBn9X1upEZqX7VSd5ghn6IJ54UEetK2b3VR82te272Q-fboddE8U8Uk_bPpFZCFMpYe-d1g; SEARCH_SAMESITE=CgQIupcB',
    'Upgrade-Insecure-Requests': '1',
    'Sec-Fetch-Dest': 'document',
    'Sec-Fetch-Mode': 'navigate',
    'Sec-Fetch-Site': 'cross-site',
    # Requests doesn't support trailers
    # 'TE': 'trailers',
}


class TrendReq(UTrendReq):
    def _get_data(self, url, method=GET_METHOD, trim_chars=0, **kwargs):
        return super()._get_data(url, method=GET_METHOD, trim_chars=trim_chars, headers=headers, **kwargs)


pytrend = TrendReq(hl='cz-CZ', tz=360)  # tady je jazyk - en-GB je birtska anglictina, en-US americka

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
     timeframe=  '2004-01-01 2023-02-28', # casovy interval sberu, format je rok-mesic-den
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
   
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    