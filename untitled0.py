# -*- coding: utf-8 -*-
"""
Created on Sat Mar 11 23:10:52 2023

@author: N
"""

import pandas as pd

result0 = {'inflace': [54, 48, 58, 75, 100, 98, 87, 87, 84, 63, 54, 51, 45, 43, 47, 42, 44, 41, 44, 41, 42, 38, 37, 40, 44, 38],
        'pivo': [48, 45, 58, 67, 77, 78, 63, 70, 62, 53, 54, 53, 44, 44, 45, 45, 49, 48, 53, 46, 50, 45, 46, 53, 50, 45],
        'alkoholismus': [19, 20, 24, 19, 17, 21, 19, 23, 20, 14, 17, 18, 15, 16, 17, 15, 19, 18, 18, 17, 18, 16, 17, 18, 18, 16]}
index = pd.date_range(start='2020-01-05', end='2020-06-28', freq='W')
result = pd.DataFrame(result0, index=index)

print(result)

import matplotlib.pyplot as plt

# Plot the search interest data as histograms
plt.hist(result["inflace"], bins=20, alpha=0.5, label="inflace")
plt.hist(result["pivo"], bins=20, alpha=0.5, label="pivo")
plt.hist(result["alkoholismus"], bins=20, alpha=0.5, label="alkoholismus")

# Add axis labels and title
plt.xlabel("Search interest")
plt.ylabel("Frequency")
plt.title("Search interest in selected keywords (January-June 2020)")

# Add a legend and show the plot
plt.legend()
plt.show()

import matplotlib.pyplot as plt

# Plot the search interest data as a curve graph
plt.plot(result.index, result["inflace"], label="inflace")
plt.plot(result.index, result["pivo"], label="pivo")
plt.plot(result.index, result["alkoholismus"], label="alkoholismus")

# Add axis labels and title
plt.xlabel("Time")
plt.ylabel("Search interest")
plt.title("Search interest in selected keywords (January-June 2020)")

# Add a legend and show the plot
plt.legend()
plt.show()