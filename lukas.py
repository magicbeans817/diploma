# -*- coding: utf-8 -*-
"""
Created on Thu Apr 27 15:24:00 2023

@author: jsuch
"""


import statistics as stats

hodnoty = [1,7,37,9,14,21,100]


if 9 > 8: print("matematika funguje")



soucet_hodnot = 0
for hodnota in hodnoty:
    soucet_hodnot = soucet_hodnot + hodnota

print(soucet_hodnot)

print(len(hodnoty))


print(soucet_hodnot / len(hodnoty))

print(stats.mean(hodnoty))








