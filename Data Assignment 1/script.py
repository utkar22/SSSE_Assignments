import pandas as pd
import numpy as np
import math

df = pd.read_csv('gw_level_jharkhand.csv')
df2 = df.loc[df["Level (m)"]!="-"]

#print(df2.to_string())

min_lat = float(min(df2["Latitude"]))
max_lat = float(max(df2["Latitude"]))
min_lon = float(min(df2["Longtitude"]))
max_lon = float(max(df2["Longtitude"]))

lat_diff = max_lat - min_lat
lon_diff = max_lon - min_lon

matrix = []

for i in range(10):
    curr = []
    for j in range(10):
        curr.append(None)
    matrix.append(curr)
        

for index, row in df2.iterrows():
    lat = float(row["Latitude"])
    lon = float(row["Longtitude"])
    val = float(row["Level (m)"])

    i = math.floor(((lat-min_lat)/lat_diff)*10) - 1
    j = math.floor(((lon-min_lon)/lon_diff)*10) - 1


    if (matrix[i][j] == None):
        matrix[i][j] = []
        matrix[i][j].append(val)
    else:
        matrix[i][j].append(val)

new_matrix = []

for i in range(10):
    curr = []
    for j in range(10):
        curr.append(None)
    new_matrix.append(curr)
    

declustered_cells = []


for i in range(len(matrix)):
    for j in range(len(matrix[0])):
        if matrix[i][j]!=None:
            val = sum(matrix[i][j])/len(matrix[i][j])
            declustered_cells.append(val)
            new_matrix[i][j] = val

def find_sd(arr, mean):
    n = len(arr)

    var = 0

    for i in arr:
        var+=(i-mean)**2

    var = var/n

    sd = math.sqrt(var)

    return sd

print("Rows with outliers: ")

for i in range(10):
    curr = []
    for j in range(10):
        if matrix[i][j]!=None:
            curr = matrix[i][j]

            if (len(curr) > 1):
                curr.sort()
                total = len(curr)

                mean = sum(curr)/total

                if (total%2==1):
                    med = curr[total//2]
                else:
                    med = (curr[total//2] + curr[total//2 - 1])/2

                sd = find_sd(curr, mean)

                u = (mean - med)*math.sqrt(total)/(0.7555*sd)

                if abs(u)>3:
                    print(i,j)



        



