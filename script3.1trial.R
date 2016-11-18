#1st

# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.

library(data.table)
expedia_train <- fread('../input/train.csv', header=TRUE)
expedia_test <- fread('../input/test.csv', header=TRUE)

dest_id_hotel_cluster_count <- expedia_train[,length(is_booking),by=list(srch_destination_id, hotel_cluster)]

top_five <- function(hc,v1){
  hc_sorted <- hc[order(v1,decreasing=TRUE)]
  n <- min(5,length(hc_sorted))
  paste(hc_sorted[1:n],collapse=" ")
}

dest_top_five <- dest_id_hotel_cluster_count[,top_five(hotel_cluster,V1),by=srch_destination_id]

dd <- merge(expedia_test,dest_top_five, by="srch_destination_id",all.x=TRUE)[order(id),list(id,V1)]

setnames(dd,c("id","hotel_cluster"))

write.csv(dd, file='submission_1.csv', row.names=FALSE)
















#2nd
library(readr)

train <- read_csv('../input/train.csv')
test <- read_csv('../input/test.csv')
# dests <- read_csv('../input/destinations.csv')
# ss <- read_csv('../input/sample_submission.csv')

train <- train[sample(1:nrow(train), nrow(train)*0.25),]  # Script ran too long; working on 25% random sample

CompareSets <- function(test, train) {
  
  comprows <- ifelse(nrow(train) < nrow(test), nrow(train), nrow(test))
  fields <- intersect(names(train), names(test))
  fields <- setdiff(fields, 'Id')
  # fields <- setdiff(fields, names(which(sapply(train, class) =='factor')))
  
  tt_compare <- data.frame(NULL)
  for (name in sort(fields)) {
    if (class(train[[name]]) %in% c('numeric', 'integer')) {
      plot(density(na.omit(train[1:comprows,name])), col=rgb(1,0,0,0.5), main=name)
      lines(density(na.omit(test[1:comprows,name])), col=rgb(0,0,1,0.5))
      tt_compare <- rbind(tt_compare, 
                          cbind(name, ks.test(train[,name], test[,name])$stati))
    } else if(length(unique(train[,name])) < 50 && class(train[[name]]) == 'factor') {
      plot(train[,name], col=rgb(1,0,0,0.5), main=name)
      par(new=TRUE)
      plot(test[,name], col=rgb(0,0,1,0.5))
    }
  }
  tt_compare$V2 <- as.numeric(as.character(tt_compare$V2))
  return(tt_compare)
}

par( mfcol=c(3,3) )
tt_compare <- CompareSets(test, train)
print("ks-test values between train and test: higher numbers are less similar:")
tt_compare[order(tt_compare$V2),]







#3rd
## R version of most popular local hotels
library(data.table)
expedia_train <- fread('../input/train.csv', header=TRUE)
expedia_test <- fread('../input/test.csv', header=TRUE)

dest_id_hotel_cluster_count <- expedia_train[,length(is_booking),by=list(srch_destination_id, hotel_cluster)]

top_five <- function(hc,v1){
  hc_sorted <- hc[order(v1,decreasing=TRUE)]
  n <- min(5,length(hc_sorted))
  paste(hc_sorted[1:n],collapse=" ")
}

dest_top_five <- dest_id_hotel_cluster_count[,top_five(hotel_cluster,V1),by=srch_destination_id]

dd <- merge(expedia_test,dest_top_five, by="srch_destination_id",all.x=TRUE)[order(id),list(id,V1)]

setnames(dd,c("id","hotel_cluster"))

write.csv(dd, file='submission_1.csv', row.names=FALSE)






#4th
## R version of most popular local hotels
library(data.table)
expedia_train <- fread('../input/train.csv', header=TRUE, select= c("is_booking","orig_destination_distance","hotel_cluster","srch_destination_id"))
expedia_test <- fread('../input/test.csv', header=TRUE)

sum_and_count <- function(x){
  sum(x)*0.8456 + length(x) *(1-0.8456)
}

dest_id_hotel_cluster_count <- expedia_train[,sum_and_count(is_booking),by=list(orig_destination_distance, hotel_cluster)]
dest_id_hotel_cluster_count1 <- expedia_train[,sum_and_count(is_booking),by=list(srch_destination_id, hotel_cluster)]


top_five <- function(hc,v1){
  hc_sorted <- hc[order(v1,decreasing=TRUE)]
  n <- min(5,length(hc_sorted))
  paste(hc_sorted[1:n],collapse=" ")
}

dest_top_five <- dest_id_hotel_cluster_count[,top_five(hotel_cluster,V1),by=orig_destination_distance]
dest_top_five1 <- dest_id_hotel_cluster_count1[,top_five(hotel_cluster,V1),by=srch_destination_id]

dd <- merge(expedia_test,dest_top_five, by="orig_destination_distance",all.x=TRUE)[order(id),list(id,V1)]

dd1 <- merge(expedia_test,dest_top_five1, by="srch_destination_id",all.x=TRUE)[order(id),list(id,V1)]

dd$V1[is.na(dd$V1)] <- dd1$V1[is.na(dd$V1)] 

setnames(dd,c("id","hotel_cluster"))

write.csv(dd, file='submission_combo_merge.csv', row.names=FALSE)





#5th
library(data.table)
library(magrittr)
library(fastcluster)
library(parallel)
library(amap)
library(ggplot2)

n.cores <- detectCores()

cat("Loading destination data...")
dest <- fread("../input/destinations.csv", sep = ",") %>% as.matrix()
dest <- dest[sample(1:nrow(dest), 30000), ] # Not enough memory
cat("DONE\n")

gc()

cat("Dimensionality reduction...")
pca <- prcomp(dest, center = TRUE, scale. = TRUE) 
pc <- pca$x[, 1:2]
cat("DONE (take two main principal components)\n")

gc()

cat("Calculating distance matrix...")
distance.matrix <- Dist(pc, method = "euclidean", nbproc = n.cores - 1)
cat("DONE\n")

gc()

cat("Creating cluster tree...")
cluster.tree <- hclust(distance.matrix)
cat("DONE (see plot)\n")
rm(distance.matrix)
gc()

cat("Creating clusters...")
clusters <- cutree(cluster.tree, k = 15) # Chose the number of clusters
cat("DONE\n")
rm(cluster.tree)
gc()

data.plot <- data.table(PC1 = pc[,1], PC2 = pc[,2], cluster = as.factor(clusters))
ggplot(data = data.plot) + 
  geom_point(mapping = aes(x = PC1, y = PC2, color = cluster)) +
  xlab("Principal Component 1") +
  ylab("Principal Component 2") + 
  ggtitle("Destination clusters") +
  theme_bw()

#upar wale se clustering ka graph banana h














#6th
## Most Popular Local Hotels with Most Popular Global Fallbacks
library(readr)
library(dplyr)
library(data.table)

expedia_train <- read_csv('../input/train.csv')
expedia_test <- read_csv('../input/test.csv')

dest_id_hotel_cluster_count <- expedia_train %>%                      
  group_by(srch_destination_id, hotel_cluster) %>%
  summarize(count = n()) %>%
  data.table

overall_rank <- expedia_train %>% group_by(hotel_cluster) %>% summarize(count = n()) %>% arrange(desc(count)) %>% select(hotel_cluster)

get_top_five <- function(cluster, count) {
  hotel_cluster_sorted <- cluster[order(count, decreasing=TRUE)]                  # First order by top local hotel clusters
  hotel_cluster_sorted <- c(hotel_cluster_sorted, overall_rank$hotel_cluster)     # Append top global hotel clusters to address cases where local clusters have fewer than 5 values
  paste(hotel_cluster_sorted[1:5], collapse=" ")                                  # Get top 5 values (first local then global if local < 5)
}

dest_top_five <- dest_id_hotel_cluster_count[,get_top_five(hotel_cluster,count),by=srch_destination_id]

out <- merge(expedia_test, dest_top_five, by="srch_destination_id", all.x=TRUE)
out <- select(out, id, V1)

setnames(out, c("id", "hotel_cluster"))

write.csv(out, file='submission_2.csv', row.names=FALSE)








#7th
# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

#library(ggplot2) # Data visualization
#library(readr) # CSV file I/O, e.g. the read_csv function
library("randomForest")
library(dplyr)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.

#reading data
destinations <- read.csv("../input/destinations.csv")

col_Types_tr<-c("character",rep("factor",5),"numeric",rep("numeric",3),"factor","character","character",rep("numeric",3),rep("factor",2),"numeric","numeric",rep("factor",4))
col_Types_te<-c("numeric","character",rep("factor",5),"numeric",rep("numeric",3),"factor","character","character",rep("numeric",3),rep("factor",2),rep("factor",3))
#as train is very large, we'll read just a random part of it
train0 <- read.table("../input/train.csv",header=T,sep=",",nrows=1,colClasses=col_Types_tr)

train <- data.frame(c(0))

i <- 0
while(length(train[,1])<500000 && i<10)
{
  rm(train)
  train <- read.table("../input/train.csv",header=F,sep=",",skip=max(1,as.integer(runif(1,min=0,max = 500000))),nrows=500000,stringsAsFactors=F,colClasses=col_Types_tr)
  i <- i+1
}

#If selection of 10000000 lines from train, we simply take the first 1000000 ones
if(length(train[,1]<500000))
{
  train <- read.table("../input/train.csv",header=T,sep=",",nrows=500000,stringsAsFactors=F,colClasses=col_Types_tr)
} else {
  train <- rbind(train0,train)
}

str(train)
head(train,5)

#we must read all data in test so that we can submit !
test <- read.table("../input/test.csv",header=T,sep=",",stringsAsFactors=F,colClasses=col_Types_te)
str(test)
head(test,5)

# #we do a PCA analysis to reduce number of variable in destinations
# prin_comp <- prcomp(subset(destinations,select=-srch_destination_id), scale. = T)
# 
# #we take just the first PCA's
# n_com<-10
# destinations <-data.frame("srch_destination_id"=destinations$srch_destination_id,prin_comp$x[,c(1:n_com)])
# 
# #adding columns from destinations
# train <- left_join(train,destinations,by="srch_destination_id")
# test <- left_join(test,destinations,by="srch_destination_id")
# 
# #correcting NA's
# #correct orig_destination_distance
# train[is.na(train$orig_destination_distance),"orig_destination_distance"]<-0
# test[is.na(test$orig_destination_distance),"orig_destination_distance"]<-0
# 
# 
# #correct NA's in PCA's for train
# for(i in 1:n_com)
# {
#     train[is.na(train[,paste("PC",i,sep="")]),paste("PC",i,sep="")]<-mean(train[,paste("PC",i,sep="")],na.rm=T)
#     test[is.na(test[,paste("PC",i,sep="")]),paste("PC",i,sep="")]<-mean(test[,paste("PC",i,sep="")],na.rm=T)
# }
# 
# 
# #adding is_booking and cnt to test data
# test <-data.frame(test,"is_booking"=as.integer(rep(1,length(test[,1]))))
# test <-data.frame(test,"cnt"=as.integer(rep(0,length(test[,1]))))
# 
# rm(destinations)
# rm(prin_comp)
# 
# #str(train)
# #str(test)
# #head(train,n=20)
# #tail(train,n=20)
# 
# #creating user history
# hist_train<-summarise(group_by(train, user_id), m_adults_cnt = mean(srch_adults_cnt),m_children_cnt = mean(srch_children_cnt),m_rm_cnt = mean(srch_rm_cnt))
# hist_test<-summarise(group_by(test, user_id), m_adults_cnt = mean(srch_adults_cnt),m_children_cnt = mean(srch_children_cnt),m_rm_cnt = mean(srch_rm_cnt))
# 
# train <- left_join(train,hist_train,by="user_id")
# test <- left_join(test,hist_test,by="user_id")
# 
# #str(hist_train)
# head(hist_train)
# head(hist_test)
# 
# #str(hist_test)
# 
# #model
# model <- randomForest(factor(hotel_cluster) ~ . -date_time-site_name-user_id-srch_ci-srch_co-srch_destination_id-srch_destination_type_id-cnt , data=train, importance=FALSE,proximity=FALSE,ntree=200,nodesize=10,maxnodes=20)
# print(model)
# 
# #prediction
# predictions<-predict(model,test,type="class")
# 
# 
# #submission
# write.csv(data.frame("id"=test$id, "hotel_cluster"=predictions),"prediction.csv",quote=F,row.names=F)
# 





#9th


# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(data.table)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.

#sapply(c("data.table", "dplyr"), require, character.only = TRUE)

path <- file.path("../input", "train.csv")

train <- fread(path)
#dest <- fread("destinations.csv")

hist(train$site_name)



#normal sa histogram banana








#11
library(data.table)
train <- fread('../input/train.csv',nrows=25000)


hist(train[,hotel_cluster],breaks=100)

library(ggplot2)
ggplot(data = train) + 
  geom_point(mapping = aes(x =hotel_country , y =hotel_market , color = as.factor(hotel_cluster))) +
  xlab("hotel_country ") +
  ylab("hotel_market") + 
  ggtitle("cluster") +
  theme_bw()


library(ggplot2)
ggplot(data = train) + 
  geom_point(mapping = aes(x =hotel_country , y =srch_destination_type_id , color = as.factor(hotel_cluster))) +
  xlab("hotel_country ") +
  ylab("hotel_market") + 
  ggtitle("cluster") +
  theme_bw()




#thoda visualization of the work























#Python mein ab



#1
# coding: utf-8
__author__ = 'ZFTurbo: https://kaggle.com/zfturbo'

import datetime
from heapq import nlargest
from operator import itemgetter
from collections import defaultdict


def run_solution():
  print('Preparing arrays...')
f = open("../input/train.csv", "r")
f.readline()
best_hotels_od_ulc = defaultdict(lambda: defaultdict(int))
best_hotels_search_dest = defaultdict(lambda: defaultdict(int))
best_hotels_search_dest1 = defaultdict(lambda: defaultdict(int))
best_hotel_country = defaultdict(lambda: defaultdict(int))
popular_hotel_cluster = defaultdict(int)
total = 0

# Calc counts
while 1:
  line = f.readline().strip()
total += 1

if total % 10000000 == 0:
  print('Read {} lines...'.format(total))

if line == '':
  break

arr = line.split(",")
book_year = int(arr[0][:4])
user_location_city = arr[5]
orig_destination_distance = arr[6]
srch_destination_id = arr[16]
is_booking = int(arr[18])
hotel_country = arr[21]
hotel_market = arr[22]
hotel_cluster = arr[23]

append_1 = 3 + 17*is_booking
append_2 = 1 + 5*is_booking

if user_location_city != '' and orig_destination_distance != '':
  best_hotels_od_ulc[(user_location_city, orig_destination_distance)][hotel_cluster] += 1

if srch_destination_id != '' and hotel_country != '' and hotel_market != '' and book_year == 2014:
  best_hotels_search_dest[(srch_destination_id, hotel_country, hotel_market)][hotel_cluster] += append_1

if srch_destination_id != '':
  best_hotels_search_dest1[srch_destination_id][hotel_cluster] += append_1

if hotel_country != '':
  best_hotel_country[hotel_country][hotel_cluster] += append_2

popular_hotel_cluster[hotel_cluster] += 1

f.close()

print('Generate submission...')
now = datetime.datetime.now()
path = 'submission_' + str(now.strftime("%Y-%m-%d-%H-%M")) + '.csv'
out = open(path, "w")
f = open("../input/test.csv", "r")
f.readline()
total = 0
out.write("id,hotel_cluster\n")
topclasters = nlargest(5, sorted(popular_hotel_cluster.items()), key=itemgetter(1))

while 1:
  line = f.readline().strip()
total += 1

if total % 1000000 == 0:
  print('Write {} lines...'.format(total))

if line == '':
  break

arr = line.split(",")
id = arr[0]
user_location_city = arr[6]
orig_destination_distance = arr[7]
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

s2 = (srch_destination_id, hotel_country, hotel_market)
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
elif srch_destination_id in best_hotels_search_dest1:
  d = best_hotels_search_dest1[srch_destination_id]
topitems = nlargest(5, d.items(), key=itemgetter(1))
for i in range(len(topitems)):
  if topitems[i][0] in filled:
  continue
if len(filled) == 5:
  break
out.write(' ' + topitems[i][0])
filled.append(topitems[i][0])

if hotel_country in best_hotel_country:
  d = best_hotel_country[hotel_country]
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
print('Completed!')

run_solution()











#2

{"cells":[
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "# This Python 3 environment comes with many helpful analytics libraries installed\n# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python\n# For example, here's several helpful packages to load in \n\nimport numpy as np # linear algebra\nimport pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)\n\n# Input data files are available in the \"../input/\" directory.\n# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory\n\nfrom subprocess import check_output\nprint(check_output([\"ls\", \"../input\"]).decode(\"utf8\"))\n\n# Any results you write to the current directory are saved as output."
  },
  {
    "cell_type": "markdown",
    "metadata": {},
    "source": "This notebook shows a \"most popular local hotel\" benchmark implemented with pandas.\n\n### Read the train data\n\nRead in the train data using only the necessary columns. \nSpecifying dtypes helps reduce memory requirements. \n\nThe file is read in chunks of 1 million rows each. In each chunk we count the number of rows and number of bookings for every destination-hotel cluster combination."
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "train = pd.read_csv('../input/train.csv',\n                    dtype={'is_booking':bool,'srch_destination_id':np.int32, 'hotel_cluster':np.int32},\n                    usecols=['srch_destination_id','is_booking','hotel_cluster'],\n                    chunksize=1000000)\naggs = []\nprint('-'*38)\nfor chunk in train:\n    agg = chunk.groupby(['srch_destination_id',\n                         'hotel_cluster'])['is_booking'].agg(['sum','count'])\n    agg.reset_index(inplace=True)\n    aggs.append(agg)\n    print('.',end='')\nprint('')\naggs = pd.concat(aggs, axis=0)\naggs.head()"
  },
  {
    "cell_type": "markdown",
    "metadata": {},
    "source": "Next we aggregate again to compute the total number of bookings over all chunks. \n\nCompute the number of clicks by subtracting the number of bookings from total row counts.\n\nCompute the 'relevance' of a hotel cluster with a weighted sum of bookings and clicks."
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "CLICK_WEIGHT = 0.05\nagg = aggs.groupby(['srch_destination_id','hotel_cluster']).sum().reset_index()\nagg['count'] -= agg['sum']\nagg = agg.rename(columns={'sum':'bookings','count':'clicks'})\nagg['relevance'] = agg['bookings'] + CLICK_WEIGHT * agg['clicks']\nagg.head()"
  },
  {
    "cell_type": "markdown",
    "metadata": {},
    "source": "### Find most popular hotel clusters by destination\n\nDefine a function to get most popular hotels for a destination group.\n\nPrevious version used nlargest() Series method to get indices of largest elements. \nBut as @benjamin points out [in his fork](https://www.kaggle.com/benjaminabel/expedia-hotel-recommendations/pandas-version-of-most-popular-hotels/comments) the method is rather slow. \nI have updated this notebook with a version that runs faster."
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "def most_popular(group, n_max=5):\n    relevance = group['relevance'].values\n    hotel_cluster = group['hotel_cluster'].values\n    most_popular = hotel_cluster[np.argsort(relevance)[::-1]][:n_max]\n    return np.array_str(most_popular)[1:-1] # remove square brackets"
  },
  {
    "cell_type": "markdown",
    "metadata": {},
    "source": "Get most popular hotel clusters for all destinations."
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "most_pop = agg.groupby(['srch_destination_id']).apply(most_popular)\nmost_pop = pd.DataFrame(most_pop).rename(columns={0:'hotel_cluster'})\nmost_pop.head()"
  },
  {
    "cell_type": "markdown",
    "metadata": {},
    "source": "### Predict for test data\nRead in the test data and merge most popular hotel clusters."
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "test = pd.read_csv('../input/test.csv',\n                    dtype={'srch_destination_id':np.int32},\n                    usecols=['srch_destination_id'],)"
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "test = test.merge(most_pop, how='left',left_on='srch_destination_id',right_index=True)\ntest.head()"
  },
  {
    "cell_type": "markdown",
    "metadata": {},
    "source": "Check hotel_cluster column in test for null values."
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "test.hotel_cluster.isnull().sum()"
  },
  {
    "cell_type": "markdown",
    "metadata": {},
    "source": "Looks like there's about 14k new destinations in test. Let's fill nas with hotel clusters that are most popular overall."
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "most_pop_all = agg.groupby('hotel_cluster')['relevance'].sum().nlargest(5).index\nmost_pop_all = np.array_str(most_pop_all)[1:-1]\nmost_pop_all"
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "test.hotel_cluster.fillna(most_pop_all,inplace=True)"
  },
  {
    "cell_type": "markdown",
    "metadata": {},
    "source": "Save the submission."
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "test.hotel_cluster.to_csv('predicted_with_pandas.csv',header=True, index_label='id')"
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": ""
  }
  ],"metadata":{"kernelspec":{"display_name":"Python 3","language":"python","name":"python3"}}, "nbformat": 4, "nbformat_minor": 0}











#3

{"cells":[
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "# This Python 3 environment comes with many helpful analytics libraries installed\n# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python\n# For example, here's several helpful packages to load in \n%matplotlib inline\nimport numpy as np # linear algebra\nimport pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)\n\n# Input data files are available in the \"../input/\" directory.\n# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory\n\nfrom subprocess import check_output\nprint(check_output([\"ls\", \"../input\"]).decode(\"utf8\"))\n\n# Any results you write to the current directory are saved as output."
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "# Get first 10000 rows and print some info about columns\ntrain = pd.read_csv(\"../input/train.csv\", parse_dates=['srch_ci', 'srch_co'], nrows=10000)\ntrain.info()"
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "import seaborn as sns\nimport matplotlib.pyplot as plt\n# preferred continent destinations\nsns.countplot(x='hotel_continent', data=train)"
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "# most of people booking are from continent 3 I guess is one of the rich continent?\nsns.countplot(x='posa_continent', data=train)"
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "# putting the two above together\nsns.countplot(x='hotel_continent', hue='posa_continent', data=train)"
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "# how many people by continent are booking from mobile\nsns.countplot(x='posa_continent', hue='is_mobile', data = train)"
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "# Difference between user and destination country\nsns.distplot(train['user_location_country'], label=\"User country\")\nsns.distplot(train['hotel_country'], label=\"Hotel country\")\nplt.legend()"
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "import numpy as np\n# get number of booked nights as difference between check in and check out\nhotel_nights = train['srch_co'] - train['srch_ci'] \nhotel_nights = (hotel_nights / np.timedelta64(1, 'D')).astype(float) # convert to float to avoid NA problems\ntrain['hotel_nights'] = hotel_nights\nplt.figure(figsize=(11, 9))\nax = sns.boxplot(x='hotel_continent', y='hotel_nights', data=train)\nlim = ax.set(ylim=(0, 15))"
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "plt.figure(figsize=(11, 9))\nsns.countplot(x=\"hotel_nights\", data=train)"
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "# distribution of the total number of people per cluster\nsrc_total_cnt = train.srch_adults_cnt + train.srch_children_cnt\ntrain['src_total_cnt'] = src_total_cnt\nax = sns.kdeplot(train['hotel_cluster'], train['src_total_cnt'], cmap=\"Purples_d\")\nlim = ax.set(ylim=(0.5, 4.5))"
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "# plot all columns countplots\nimport numpy as np\nrows = train.columns.size//3 - 1\nfig, axes = plt.subplots(nrows=rows, ncols=3, figsize=(12,18))\nfig.tight_layout()\ni = 0\nj = 0\nfor col in train.columns:\n    if j >= 3:\n        j = 0\n        i += 1\n    # avoid to plot by date    \n    if train[col].dtype == np.int64:\n        sns.countplot(x=col, data=train, ax=axes[i][j])\n        j += 1"
  }
  ],"metadata":{"kernelspec":{"display_name":"Python 3","language":"python","name":"python3"}}, "nbformat": 4, "nbformat_minor": 0}










#4


{"cells":[
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "# This Python 3 environment comes with many helpful analytics libraries installed\n# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python\n# For example, here's several helpful packages to load in \n\nimport numpy as np # linear algebra\nimport pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)\n\nfrom subprocess import check_output\n\nimport ml_metrics as metrics\n\n"
  },
  {
    "cell_type": "markdown",
    "metadata": {},
    "source": "##I am going to show how order matters in MAP@K when there is only 1 answer. \n\n##This experiment is done by calculating AP@K, which gives 1 value. MAP@K is the average of AP@K. \n"
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "actual = [1]\n\npredicted = [1,2,3,4,5]\n\nprint('Answer=',actual,'predicted=',predicted)\nprint('AP@5 =',metrics.apk(actual,predicted,5) )\n\npredicted = [2,1,3,4,5]\nprint('Answer=',actual,'predicted=',predicted)\nprint('AP@5 =',metrics.apk(actual,predicted,5) )\n\npredicted = [3,2,1,4,5]\nprint('Answer=',actual,'predicted=',predicted)\nprint('AP@5 =',metrics.apk(actual,predicted,5) )\n\npredicted = [4,2,3,1,5]\nprint('Answer=',actual,'predicted=',predicted)\nprint('AP@5 =',metrics.apk(actual,predicted,5) )\n\npredicted = [4,2,3,5,1]\nprint('Answer=',actual,'predicted=',predicted)\nprint('AP@5 =',metrics.apk(actual,predicted,5) )"
  },
  {
    "cell_type": "markdown",
    "metadata": {},
    "source": "As you see from the above example, the \"earlier\" you predict the correct answer 1, the higher your score. \n\n### Next is an example of how MAP@K is calculated with a list of hotels\nI took the list of predictions from the previous AP@K example, and calculated the MAP@K. You can see the MAP@K is the average of AP@K\n"
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "metrics.mapk([[1],[1],[1],[1],[1]],[[1,2,3,4,5],[2,1,3,4,5],[3,2,1,4,5],[4,2,3,1,5],[4,2,3,5,1]], 5)"
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": ""
  }
  ],"metadata":{"kernelspec":{"display_name":"Python 3","language":"python","name":"python3"}}, "nbformat": 4, "nbformat_minor": 0}





#5


{"cells":[
  {
    "cell_type": "markdown",
    "metadata": {},
    "source": "#Latent Destination Features\nLet's have a look at the latent search region features.\nIt won't help you to boost your score immediately although you might gain a few ideas how to apply dimensionality reduction.\n\nI just wanted to play with seaborn a bit.\n"
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "%matplotlib inline\nimport numpy as np\nimport pandas as pd\nimport matplotlib.pyplot as plt\nimport seaborn as sns\nsns.set_style('whitegrid')\nsns.set(color_codes=True)"
  },
  {
    "cell_type": "markdown",
    "metadata": {},
    "source": "Destinations.csv has 62K rows. Let's keep only the frequent search destinations. \nRemoving 50K records we could still keep 97% of the test bookings."
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "destination_features = pd.read_csv(\"../input/destinations.csv\")\nprint(destination_features.shape)\ntest_destinations = pd.read_csv(\"../input/test.csv\", usecols=['srch_destination_id'])\nsrch_destinations, count = np.unique(test_destinations, return_counts=True)\nfig, ax = plt.subplots(ncols=2, sharex=True)\nax[0].semilogy(sorted(count))\nax[1].plot(1.0 * np.array(sorted(count)).cumsum()/count.sum())\nax[0].set_xticks(range(0, len(srch_destinations), 10000))\nax[1].set_ylabel('Cumulative sum')\nax[0].set_ylabel('Search destination counts in test set (log scale)')\nfrequent_destinations = srch_destinations[count >= 10]\nprint (1. * count[count >= 10].sum() / count.sum())"
  },
  {
    "cell_type": "markdown",
    "metadata": {},
    "source": "We show the correlations among the 149 latent features."
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "frequent_destinations = srch_destinations[count >= 10]\nfrequent_destination_features = destination_features[destination_features['srch_destination_id'].isin(frequent_destinations)]\nfrequent_destination_features = frequent_destination_features.drop('srch_destination_id', axis=1)\nprint(frequent_destination_features.shape)\ncorrelations = frequent_destination_features.corr()\nf = plt.figure()\nax = sns.heatmap(correlations)\nax.set_xticks([])\nax.set_yticks([])\nplt.title('Tartan or correlation matrix')\nf.savefig('tartan.png', dpi=300)\nplt.show()"
  },
  {
    "cell_type": "markdown",
    "metadata": {},
    "source": "It looks like a nice tartan! It is easy to see that we have many strong correlations and the column order seems to be randomized.\n"
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "fig=plt.figure()\nsns.distplot(correlations.values.reshape(correlations.size), bins=50, color='g')\nplt.title('Correlation values')\nplt.show()\nfig.savefig('CorrelationHist')"
  },
  {
    "cell_type": "markdown",
    "metadata": {},
    "source": "Using hierarchical clustering we try to reorder the features."
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "g = sns.clustermap(correlations)\ng.ax_heatmap.set_xticks([])\ng.ax_heatmap.set_yticks([])\ng.savefig('clustermap.png', dpi=300)"
  },
  {
    "cell_type": "markdown",
    "metadata": {},
    "source": "Use dendogram_col.reordered_ind to get the index of the original columns."
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "print(g.dendrogram_col.reordered_ind)"
  },
  {
    "cell_type": "markdown",
    "metadata": {},
    "source": "Select a few features from the beginning and check their distributions."
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "a = [8, 102, 120, 127, 74]\nto_plot = frequent_destination_features[frequent_destination_features.columns[a]].sample(1000)\ng = sns.PairGrid(to_plot, size=3)\ng.map_upper(plt.scatter, s=5, alpha=0.3)\ng.map_lower(sns.kdeplot, cmap=\"Blues_d\")\ng.map_diag(sns.kdeplot, legend=False, shade=True)\nplt.suptitle('A few features')\ng.savefig('cluster_1.png', dpi=300)  \n\n"
  },
  {
    "cell_type": "markdown",
    "metadata": {},
    "source": "Select a few correlated features from the middle and check their distributions."
  },
  {
    "cell_type": "code",
    "execution_count": null,
    "metadata": {
      "collapsed": false
    },
    "outputs": [],
    "source": "b = [89, 69, 115, 105, 71]\ndef green_kde_hack(x, color, **kwargs):\n    sns.kdeplot(x, color='g', **kwargs)\nto_plot = frequent_destination_features[frequent_destination_features.columns[b]].sample(1000)\ng = sns.PairGrid(to_plot, size=3)\ng.map_upper(plt.scatter, s=5, alpha=0.3, color='g')\ng.map_lower(sns.kdeplot, cmap=\"Greens_d\")\ng.map_diag(green_kde_hack, legend=False, shade=True)\nplt.suptitle('A few correlated features')\ng.savefig('cluster_2.png', dpi=300)"
  }
  ],"metadata":{"kernelspec":{"display_name":"Python 3","language":"python","name":"python3"}}, "nbformat": 4, "nbformat_minor": 0}






#6




# coding: utf-8
__author__ = 'ZFTurbo: https://kaggle.com/zfturbo'

import datetime
from heapq import nlargest
from operator import itemgetter
from collections import defaultdict


def prepare_arrays_match():
  print('Preparing arrays...')
f = open("../input/train.csv", "r")
f.readline()
best_hotels_od_ulc = defaultdict(lambda: defaultdict(int))
best_hotels_search_dest = defaultdict(lambda: defaultdict(int))
popular_hotel_cluster = defaultdict(int)
total = 0

# Calc counts
while 1:
  line = f.readline().strip()
total += 1

if total % 10000000 == 0:
  print('Read {} lines...'.format(total))

if line == '':
  break

arr = line.split(",")
user_location_city = arr[5]
orig_destination_distance = arr[6]
srch_destination_id = arr[16]
is_booking = int(arr[18])
hotel_cluster = arr[23]

append_1 = 85*is_booking + 15
append_2 = 85*is_booking + 15
append_3 = 85*is_booking + 15

if user_location_city != '' and orig_destination_distance != '':
  best_hotels_od_ulc[(user_location_city, orig_destination_distance)][hotel_cluster] += append_1

if srch_destination_id != '':
  best_hotels_search_dest[srch_destination_id][hotel_cluster] += append_2

popular_hotel_cluster[hotel_cluster] += append_3

f.close()
return best_hotels_od_ulc, best_hotels_search_dest, popular_hotel_cluster


def gen_submission(best_hotels_search_dest, best_hotels_od_ulc, popular_hotel_cluster):
  print('Generate submission...')
now = datetime.datetime.now()
path = 'submission_' + str(now.strftime("%Y-%m-%d-%H-%M")) + '.csv'
out = open(path, "w")
f = open("../input/test.csv", "r")
f.readline()
total = 0
out.write("id,hotel_cluster\n")
topclasters = nlargest(5, sorted(popular_hotel_cluster.items()), key=itemgetter(1))

while 1:
  line = f.readline().strip()
total += 1

if total % 1000000 == 0:
  print('Write {} lines...'.format(total))

if line == '':
  break

arr = line.split(",")
id = arr[0]
user_location_city = arr[6]
orig_destination_distance = arr[7]
srch_destination_id = arr[17]

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

if srch_destination_id in best_hotels_search_dest:
  d = best_hotels_search_dest[srch_destination_id]
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


best_hotels_od_ulc, best_hotels_search_dest, popular_hotel_cluster = prepare_arrays_match()
gen_submission(best_hotels_search_dest, best_hotels_od_ulc, popular_hotel_cluster)









#7





import pandas as pd
import numpy as np
import random
import ml_metrics as metrics
from sklearn.decomposition import PCA
from sklearn import cross_validation
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.cross_validation import KFold
from itertools import chain
import operator
from subprocess import check_output
print(check_output(["ls", "../input"]).decode("utf8"))

destinations = pd.read_csv("../input/destinations.csv")

train = pd.read_csv('../input/train.csv',
                    usecols=["date_time", "user_location_country", "user_location_region", "user_location_city",
                             "user_id", "is_booking", "orig_destination_distance",
                             "hotel_cluster", "srch_ci", "srch_co", "srch_destination_id", 
                             "hotel_continent", "hotel_country", "hotel_market"],
                    dtype={'date_time':np.str_, 'user_location_country':np.int8, 
                      'user_location_region':np.int8, 'user_location_city':np.int8, 
                      'user_id':np.int32, 'is_booking':np.int8,
                      "orig_destination_distance":np.float64,
                      "hotel_cluster":np.int8,
                      'srch_ci':np.str_, 'srch_co':np.str_,
                      "srch_destination_id":np.int32,
                      "hotel_continent":np.int8,
                      "hotel_country":np.int8,
                      "hotel_market":np.int8}
                    #,nrows=3000000
)

test = pd.read_csv('../input/test.csv',
                   usecols=["id", "date_time", "user_location_country", "user_location_region", "user_location_city",
                            "user_id", "orig_destination_distance",
                            "srch_ci", "srch_co", "srch_destination_id",
                            "hotel_continent", "hotel_country", "hotel_market"],
                   dtype={'id':np.int32, 'date_time':np.str_, 'user_location_country':np.int8, 
                     'user_location_region':np.int8, 'user_location_city':np.int8, 
                     'user_id':np.int32, 
                     "orig_destination_distance":np.float64, 'srch_ci':np.str_, 'srch_co':np.str_,
                     "srch_destination_id":np.int32,
                     "hotel_continent":np.int8,
                     "hotel_country":np.int8,
                     "hotel_market":np.int8})	
train.shape
test.shape
train.head(5)
test.head(5)

train["hotel_cluster"].value_counts()
test_ids = set(test.user_id.unique())
train_ids = set(train.user_id.unique())
intersection_count = len(test_ids & train_ids)
intersection_count == len(test_ids)

train["date_time"] = pd.to_datetime(train["date_time"])
train["year"] = train["date_time"].dt.year
train["month"] = train["date_time"].dt.month

unique_users = train.user_id.unique()

sel_user_ids = [unique_users[i] for i in sorted(random.sample(range(len(unique_users)), 10000)) ]
sel_train = train[train.user_id.isin(sel_user_ids)]

t1 = sel_train[((sel_train.year == 2012) | (sel_train.year == 2013))]
t2 = sel_train[(sel_train.year == 2014)]

t2 = t2[t2.is_booking == True]

most_common_clusters = list(train.hotel_cluster.value_counts().head().index)
predictions = [most_common_clusters for i in range(t2.shape[0])]
target = [[l] for l in t2["hotel_cluster"]]
metrics.mapk(target, predictions, k=5)
train.corr()["hotel_cluster"]

def make_key(items):
  return "_".join([str(i) for i in items])

match_cols = ["srch_destination_id"]
cluster_cols = match_cols + ['hotel_cluster']
groups = t1.groupby(cluster_cols)
top_clusters = {}
for name, group in groups:
  clicks = len(group.is_booking[group.is_booking == False])
bookings = len(group.is_booking[group.is_booking == True])

score = bookings + .15 * clicks

clus_name = make_key(name[:len(match_cols)])
if clus_name not in top_clusters:
  top_clusters[clus_name] = {}
top_clusters[clus_name][name[-1]] = score

cluster_dict = {}
for n in top_clusters:
  tc = top_clusters[n]
top = [l[0] for l in sorted(tc.items(), key=operator.itemgetter(1), reverse=True)[:5]]
cluster_dict[n] = top

preds = []
for index, row in t2.iterrows():
  key = make_key([row[m] for m in match_cols])
if key in cluster_dict:
  preds.append(cluster_dict[key])
else:
  preds.append([])

metrics.mapk([[l] for l in t2["hotel_cluster"]], preds, k=5)
match_cols = ['user_location_country', 'user_location_region', 'user_location_city', 'hotel_market', 'orig_destination_distance']

groups = t1.groupby(match_cols)

def generate_exact_matches(row, match_cols):
  index = tuple([row[t] for t in match_cols])
try:
  group = groups.get_group(index)
except Exception:
  return []
clus = list(set(group.hotel_cluster))
return clus

exact_matches = []
for i in range(t2.shape[0]):
  exact_matches.append(generate_exact_matches(t2.iloc[i], match_cols))

def f5(seq, idfun=None): 
  if idfun is None:
  def idfun(x): return x
seen = {}
result = []
for item in seq:
  marker = idfun(item)
if marker in seen: continue
seen[marker] = 1
result.append(item)
return result

full_preds = [f5(exact_matches[p] + preds[p] + most_common_clusters)[:5] for p in range(len(preds))]
metrics.mapk([[l] for l in t2["hotel_cluster"]], full_preds, k=5)

write_p = [" ".join([str(l) for l in p]) for p in full_preds]
write_frame = ["{0},{1}".format(t2["srch_destination_id"].iloc[i], write_p[i]) for i in range(len(full_preds))]
write_frame = ["id,hotel_cluster"] + write_frame
with open("predictions.csv", "w+") as f:
  f.write("\n".join(write_frame))








