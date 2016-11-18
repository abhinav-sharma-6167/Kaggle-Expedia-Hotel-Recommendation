# -*- coding: utf-8 -*-
"""
Created on Fri May 27 14:12:45 2016

@author: Abhinav
"""

import datetime
from heapq import nlargest
from operator import itemgetter



def prepare_arrays_match():
#Reading the data and assigning somehow to arr    
    f = open("train.csv", "r")
    #reading the first line only    
    f.readline()
    #defining 4 objects of class dictionary
    best_hotels_od_ulc = dict()
    best_hotels_search_dest = dict()
    best_hotels_country = dict()
    popular_hotel_cluster = dict()
    total = 0

    # Calc counts
    while 1:
        line = f.readline().strip()
        total += 1
        
        if total % 2000000 == 0:
            print('Read {} lines...'.format(total))

        if line == '':
            break
        #Reading of the code completed!
        #what is arr assigned to?!        
        arr = line.split(",")
        user_location_city = arr[5]
        orig_destination_distance = arr[6]
        is_mobile = arr[8]
        srch_destination_id = arr[16]
        hotel_country = arr[21]
        hotel_market = arr[22]
        is_booking = float(arr[18])
        hotel_cluster = arr[23]
        book_year=int(arr[0][:4])
    
    
    
    
    
    
    
    
#if already occured in best hotel till now, subsetted by tuple then increment..else peace..basically best hotel wrt city and orig distance

def gen_submission(best_hotels_country, best_hotels_search_dest, best_hotels_od_ulc, popular_hotel_cluster):
    

best_hotels_country, best_hotels_od_ulc, best_hotels_search_dest, popular_hotel_cluster = prepare_arrays_match()
gen_submission(best_hotels_country, best_hotels_search_dest, best_hotels_od_ulc, popular_hotel_cluster)