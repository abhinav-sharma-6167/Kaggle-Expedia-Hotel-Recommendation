#Setting directory
setwd("D:/Learning/Kaggle/Expedia")


#Libraries used
library(randomForest)
library(data.table)
library(ff)
library(bigmemory)
library(ggplot2)
library(plyr)


#Setting the seed in case for random sampling later
set.seed(100)


#freading the data into data table
train <- fread("train.csv")
test <- fread("test.csv")
dest <- fread("destinations.csv")












sum_and_count <- function(x){
  sum(x)*0.8456 + length(x) *(1-0.8456)
}

dest_id_hotel_cluster_count <- train[,sum_and_count(is_booking),by=list(orig_destination_distance, hotel_cluster)]
dest_id_hotel_cluster_count1 <- train[,sum_and_count(is_booking),by=list(srch_destination_id, hotel_cluster)]


top_five <- function(hc,v1){
  hc_sorted <- hc[order(v1,decreasing=TRUE)]
  n <- min(5,length(hc_sorted))
  paste(hc_sorted[1:n],collapse=" ")
}

dest_top_five <- dest_id_hotel_cluster_count[,top_five(hotel_cluster,V1),by=orig_destination_distance]
dest_top_five1 <- dest_id_hotel_cluster_count1[,top_five(hotel_cluster,V1),by=srch_destination_id]

dd <- merge(test,dest_top_five, by="orig_destination_distance",all.x=TRUE)[order(id),list(id,V1)]

dd1 <- merge(test,dest_top_five1, by="srch_destination_id",all.x=TRUE)[order(id),list(id,V1)]

dd$V1[is.na(dd$V1)] <- dd1$V1[is.na(dd$V1)] 

setnames(dd,c("id","hotel_cluster"))

write.csv(dd, file='submission_combo_merge.csv', row.names=FALSE)




