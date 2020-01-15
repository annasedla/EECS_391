import numpy as np

import csv
import random
import importlib
from sys import platform as sys_pf
if sys_pf == 'darwin':
    import matplotlib
    matplotlib.use("TkAgg")
import matplotlib.pyplot as plt
import matplotlib.cm as cm

# put data in a list of lists
file = open('irisdata.csv', mode='r')
reader = csv.DictReader(file)
file.close
key = []
list_of_iris = []
# storing old means
old = [];

line = 0
for row in reader:
  if line == 0:
    for x in row:
      key.append(x)
  else:
    iris = []
    for x in row:
      if x != "species":
        iris.append(float(row[x]))
      else:
        iris.append(row[x])
    list_of_iris.append(iris)
  line += 1

# make means (two are three)
k = 3
list_of_means = []

for i in range(k):
  list_of_means.append(list_of_iris[random.randint(0,len(list_of_iris))])

def difference(p1, p2):
    d = [0,0,"",0,0]
    for i in range(len(p1)):
        if not isinstance(p1[i], str):
            d[i] = p1[i] - p2[i]
    return d

def sum(p1, p2):
    s = [0,0,"",0,0]
    for i in range(len(p1)):
        if not isinstance(p1[i], str):
            if not isinstance(p2[i], str):
                s[i] = p1[i] + p2[i]
    return s

def print_means(list_of_means):
  for mean in list_of_means:
    for value in mean:
      if not isinstance(value, str):
        print( round(value,3))
    print("")
  print("\n")

def if_equals(p1, p2):
  for i in range(len(p1)):
    if not isinstance(p1[i], str):
      if p1 != p2:
        return False
  return True

def distance(p1, p2):
    d = 0
    for i in range(len(p1)):
        if not isinstance(p1[i], str):
            if not isinstance(p2[i], str):
                d += abs(p1[i] - p2[i])
    return d

def closest_mean(iris, list_of_means):
    closest = 0
    for i in range(1,len(list_of_means)):
        if distance(list_of_means[i],iris) < distance(list_of_means[closest],iris):
            closest = i
    return closest

def divide(p1, n):
  d = [0,0,"",0,0]
  for i in range(len(p1)):
    if not isinstance(p1[i], str):
      d[i] = p1[i] / n
  return d

# moves means to the new positions within the group
def move_the_means(list_of_means,data):
  for i in range(0,len(list_of_means)):
    mean = [0,0,"",0,0]
    n_irises = 0
    for iris in data:
      if closest_mean(iris,list_of_means) == i:
        mean = sum(mean, iris)
        n_irises += 1
    list_of_means[i] = divide(mean,n_irises)

# keep adjusting means until their positions have been settled
while not if_equals(list_of_means,old):
  old = list(list_of_means)
  move_the_means(list_of_means,list_of_iris)
  x = map(lambda x: x[0], list_of_iris)
  y = map(lambda x: x[3], list_of_iris)
  plt.scatter(x,y)
  x = map(lambda x: x[0], list_of_means)
  y = map(lambda x: x[3], list_of_means)
  plt.scatter(x,y,color="red")
  plt.show()
