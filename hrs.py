import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

data = {'jan21': 72,
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
        'dec21': 25,
        'jan22': 50,
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
        'dec22': 14,
        'jan23': 33,
        'feb23': 59,
        'march23': 28,
        'apr23': 21}

months = list(data.keys())
hours = list(data.values())

fig = plt.figure(figsize = (10, 5))

def print_data():
    plt.bar(months, hours, color='blue', width=0.4)
    plt.xlabel("months")
    plt.ylabel("hours studied")
    plt.title("Hours spent stdying")
    plt.show()
###------------------------

def print_stat_data():
    s = pd.Series(hours)
    stat_data = dict(s.describe())
    return stat_data
