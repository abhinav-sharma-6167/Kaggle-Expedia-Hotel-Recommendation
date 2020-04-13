import datetime
from heapq import nlargest
from operator import itemgetter

#defining a function that reads the train line by line
def prepare_arrays_match():
    f = open("train.csv", "r")
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
        
# print (is_mobile)
#the first loop
        if user_location_city != '' and orig_destination_distance != '':
            s1 = (user_location_city, orig_destination_distance)

            if s1 in best_hotels_od_ulc:
                if hotel_cluster in best_hotels_od_ulc[s1]:
                    best_hotels_od_ulc[s1][hotel_cluster] += 1
                else:
                    best_hotels_od_ulc[s1][hotel_cluster] = 1
            else:
                best_hotels_od_ulc[s1] = dict()
                best_hotels_od_ulc[s1][hotel_cluster] = 1
#if already occured in best hotel till now, subsetted by tuple then increment..else peace..basically best hotel wrt city and orig distance
        
        
        
        
        if srch_destination_id != '' and hotel_country != '' and hotel_market != '' and book_year==2014 and is_mobile==1:
            s2 = (srch_destination_id,hotel_country,hotel_market,is_mobile)
            if s2 in best_hotels_search_dest:
                if hotel_cluster in best_hotels_search_dest[s2]:
                    best_hotels_search_dest[s2][hotel_cluster] += is_booking*1 + (1-is_booking)*0.1544
                else:
                    best_hotels_search_dest[s2][hotel_cluster] = is_booking*1 + (1-is_booking)*0.1544
            else:
                best_hotels_search_dest[s2] = dict()
                best_hotels_search_dest[s2][hotel_cluster] = is_booking*1 + (1-is_booking)*0.1544
         
         
         
         
         
         
        if srch_destination_id != '' and hotel_country != '' and hotel_market != '' and book_year==2014:
            s3 = (srch_destination_id,hotel_country,hotel_market)
            if s3 in best_hotels_search_dest:
                if hotel_cluster in best_hotels_search_dest[s3]:
                    best_hotels_search_dest[s3][hotel_cluster] += is_booking*1 + (1-is_booking)*0.1544
                else:
                    best_hotels_search_dest[s3][hotel_cluster] = is_booking*1 + (1-is_booking)*0.1544
            else:
                best_hotels_search_dest[s3] = dict()
                best_hotels_search_dest[s3][hotel_cluster] = is_booking*1 + (1-is_booking)*0.1544






        if hotel_country != '':
            s4 = (hotel_country)
            if s4 in best_hotels_country:
                if hotel_cluster in best_hotels_country[s4]:
                    best_hotels_country[s4][hotel_cluster] += is_booking*1 + (1-is_booking)*0.20
                else:
                    best_hotels_country[s4][hotel_cluster] = is_booking*1 + (1-is_booking)*0.20
            else:
                best_hotels_country[s4] = dict()
                best_hotels_country[s4][hotel_cluster] = is_booking*1 + (1-is_booking)*0.20






        if hotel_cluster in popular_hotel_cluster:
            popular_hotel_cluster[hotel_cluster] += 1
        else:
            popular_hotel_cluster[hotel_cluster] = 1




    f.close()
    return best_hotels_country, best_hotels_od_ulc, best_hotels_search_dest, popular_hotel_cluster










def gen_submission(best_hotels_country, best_hotels_search_dest, best_hotels_od_ulc, popular_hotel_cluster):
    now = datetime.datetime.now()
    path = 'submission_' + str(now.strftime("%Y-%m-%d-%H-%M")) + '.csv'
    out = open(path, "w")
    f = open("test.csv", "r")
    f.readline()
    total = 0
    out.write("id,hotel_cluster\n")
    topclasters = nlargest(5, sorted(popular_hotel_cluster.items()), key=itemgetter(1))




    while 1:
        line = f.readline().strip()
        total += 1

        if total % 100000 == 0:
            print('Write {} lines...'.format(total))
            #okays so this is to read the 3gb ka data
        if line == '':
            break






        arr = line.split(",")
        id = arr[0]
        user_location_city = arr[6]
        orig_destination_distance = arr[7]
        is_mobile = arr[9]
        srch_destination_id = arr[17]
        hotel_country = arr[20]
        hotel_market = arr[21]

        out.write(str(id) + ',')
        filled = []







        s1 = (user_location_city, orig_destination_distance)
        if s1 in best_hotels_od_ulc:
            d = best_hotels_od_ulc[s1]
            topitems = nlargest(5, sorted(d.items()), key=itemgetter(1))
            for i in range(len(topitems)):
                if topitems[i][0] in filled:
                    continue
                if len(filled) == 5:
                    break
                out.write(' ' + topitems[i][0])
                filled.append(topitems[i][0])







        s2 = (srch_destination_id,hotel_country,hotel_market,is_mobile)
        if s2 in best_hotels_search_dest:
            d = best_hotels_search_dest[s2]
            topitems = nlargest(5, d.items(), key=itemgetter(1))
            for i in range(len(topitems)):
                if topitems[i][0] in filled:
                    continue
                if len(filled) == 5:
                    break
                out.write(' ' + topitems[i][0])
                filled.append(topitems[i][0])
                






        s3 = (srch_destination_id,hotel_country,hotel_market)
        if s3 in best_hotels_search_dest:
            d = best_hotels_search_dest[s3]
            topitems = nlargest(5, d.items(), key=itemgetter(1))
            for i in range(len(topitems)):
                if topitems[i][0] in filled:
                    continue
                if len(filled) == 5:
                    break
                out.write(' ' + topitems[i][0])
                filled.append(topitems[i][0])






        s4 = (hotel_country)
        if s4 in best_hotels_country:
            d = best_hotels_country[s4]
            topitems = nlargest(5, d.items(), key=itemgetter(1))
            for i in range(len(topitems)):
                if topitems[i][0] in filled:
                    continue
                if len(filled) == 5:
                    break
                out.write(' ' + topitems[i][0])
                filled.append(topitems[i][0])





                
        for i in range(len(topclasters)):
            if topclasters[i][0] in filled:
                continue
            if len(filled) == 5:
                break
            out.write(' ' + topclasters[i][0])
            filled.append(topclasters[i][0])

        out.write("\n")
    out.close()









best_hotels_country, best_hotels_od_ulc, best_hotels_search_dest, popular_hotel_cluster = prepare_arrays_match()
gen_submission(best_hotels_country, best_hotels_search_dest, best_hotels_od_ulc, popular_hotel_cluster)
