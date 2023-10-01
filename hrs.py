import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
## 2021: 630 hours
## 2022: 453 hours

data21 = {'jan21': 72,
        'feb21': 80,
        'march21': 50,
        'apr21': 51,
        'may21': 52,
        'june21': 27,
        'jul21': 50,
        'aug21': 26,
        'sep21': 40,
        'oct21': 84,
        'nov21': 73,
        'dec21': 25
        }
data22 = {'jan22': 50,
          'feb22': 50,
          'march22': 29,
          'apr22': 48,
          'may22': 69,
          'june22': 51,
          'jul22': 27,
          'aug22': 34,
          'sep22': 35,
          'oct22': 31,
          'nov22': 15,
          'dec22': 14}

data23 = {'jan23': 33,
          'feb23': 59,
          'march23': 28,
          'apr23': 21,
          'may23': 24,
          'june23': 35,
          'july23': 82,
          'aug23': 93,
          'sept23': 80}

hours_per_year = {'2021': 630, '2022': 453}

months21 = list(data21.keys())
hours21 = list(data21.values())

months22 = list(data22.keys())
hours22 = list(data22.values())

months23 = list(data23.keys())
hours23 = list(data23.values())


monthsTotal = months21 + months22 + months23
hoursTotal = hours21 + hours22 + hours23

fig = plt.figure(figsize = (10, 5))

def print_data21():
    plt.bar(months21, hours21, color='blue', width=0.4)
    plt.xlabel("months")
    plt.ylabel("hours studied")
    plt.title("Hours spent stdying")
    plt.show()

def print_data22():
    plt.bar(months22, hours22, color='blue', width=0.4)
    plt.xlabel("months")
    plt.ylabel("hours studied")
    plt.title("Hours spent stdying")
    plt.show()

def print_data23():
    plt.bar(months23, hours23, color='blue', width=0.4)
    plt.xlabel("months")
    plt.ylabel("hours studied")
    plt.title("Hours spent stdying")
    plt.show()

def print_dataTotal():
    plt.bar(monthsTotal, hoursTotal, color='blue', width=0.4)
    plt.xlabel("months")
    plt.ylabel("hours studied")
    plt.title("Hours spent stdying")
    plt.show()

###------------------------

def print_stat_data21():
    s = pd.Series(hours21)
    stat_data = dict(s.describe())
    return stat_data

def print_stat_data22():
    s = pd.Series(hours22)
    stat_data = dict(s.describe())
    return stat_data

def print_stat_data23():
    s = pd.Series(hours23)
    stat_data = dict(s.describe())
    return stat_data

def print_stat_dataTotal():
    s = pd.Series(hoursTotal)
    stat_data = dict(s.describe())
    return stat_data

##-> {'count': 28.0, 'mean': 43.714285714285715, 'std': 19.428610332983144, 'min': 14.0, '25%': 27.75, '50%': 44.0, '75%': 51.25, 'max': 84.0}

## {'count': 32.0, 'mean': 45.5625, 'std': 21.583950877535628, 'min': 14.0, '25%': 27.75, '50%': 44.0, '75%': 53.75, 'max': 93.0}
"""
>>> print_stat_data21()
{'count': 12.0, 'mean': 52.5, 'std': 20.952326839756964, 'min': 25.0, '25%': 36.75, '50%': 50.5, '75%': 72.25, 'max': 84.0}
>>> print_stat_data22()
{'count': 12.0, 'mean': 37.75, 'std': 16.23198301894358, 'min': 14.0, '25%': 28.5, '50%': 34.5, '75%': 50.0, 'max': 69.0}
print_stat_data23()
{'count': 9.0, 'mean': 50.55555555555556, 'std': 28.20953345551425, 'min': 21.0, '25%': 28.0, '50%': 35.0, '75%': 80.0, 'max': 93.0}
"""
