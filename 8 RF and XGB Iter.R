###############################_____##__Author : Abhinav Sharma__##_____################################
########################################BenchMark Solution##############################################




#Setting directory
setwd("D:/Learning/Kaggle/Expedia")


#Libraries used
library(randomForest)
library(data.table)
library(ff)
library(bigmemory)
library(ggplot2)
library(plyr)
library(dplyr)
library(xgboost)


#Setting the seed in case for random sampling later
set.seed(100)


#freading the data into data table
train <- fread("train.csv")
test <- fread("test.csv")
dest <- fread("destinations.csv")



dest_data<-copy(dest)
test_data<-copy(test)
#train_data<-train_data[train_data$is_booking==1]
train_data1<-copy(train)



#train_data1$is_booking[train_data1$is_booking==0]<-0.3
x<-unclass(strptime(train_data1$date_time, format = "%Y-%m-%d"))



#subsetting stuff
xx<-x$year
train_data1$year <-xx
train_data1 <- train_data1[train_data1$year==113]


train_data1$mon24 <-x$mon
train_data1$mon24 <-train_data1$mon24+1
train_data1$quad <- as.integer((train_data1$mon24)/3)
train_data1$quad[train_data1$quad==4]<-as.integer(0)
#THIS PCA THING IS JUST EXPERIMENTATION
#a.ir <- dest_id_hotel_cluster_count[,1:4,with=FALSE]
#a.pca <-prcomp(a.ir,center = T,scale. = T)
train_data1$is_booking[train_data1$is_booking==0] <- 0.22
dest_id_hotel_cluster_count <- train_data1[,sum(is_booking),by=list(srch_destination_id,hotel_market,hotel_country,quad, hotel_cluster)]
#dest_id_hotel_cluster_count1 <- train_data[,length(is_booking),by=list(srch_destination_id,hotel_market, hotel_cluster)]
dest_id_hotel_cluster_count1 <- train_data1[,sum(is_booking),by=list(hotel_market,hotel_country,quad, hotel_cluster)]
dest_id_hotel_cluster_count2 <- train_data1[,sum(is_booking),by=list(hotel_country,quad, hotel_cluster)]

top_five <- function(hc,v1){
  hc_sorted <- hc[order(v1,decreasing=TRUE)]
  n <- min(5,length(hc_sorted))
  paste(hc_sorted[1:n],collapse=" ")
}
test_data1<-copy(test_data)
dest_top_five <- dest_id_hotel_cluster_count[,top_five(hotel_cluster,V1),by=list(srch_destination_id,hotel_market,hotel_country,quad)]
#dest_top_five1 <- dest_id_hotel_cluster_count1[,top_five(hotel_cluster,V1),by=list(srch_destination_id)]
#dest_top_five<-dest_top_five[order(srch_destination_id,hotel_market,quad)]
dest_top_five1 <- dest_id_hotel_cluster_count1[,top_five(hotel_cluster,V1),by=list(hotel_market,hotel_country,quad)]
#dest_top_five1<-dest_top_five1[order(hotel_market,quad)]
dest_top_five2 <- dest_id_hotel_cluster_count2[,top_five(hotel_cluster,V1),by=list(hotel_country,quad)]

dest_top_five$freq3 <- dest_top_five$V1
dest_top_five[,V1 := NULL]
dest_top_five1$freq2 <- dest_top_five1$V1
dest_top_five1[,V1 := NULL]
dest_top_five$setkey1<- paste0(dest_top_five$quad,dest_top_five$hotel_market,dest_top_five$hotel_country,dest_top_five$srch_destination_id)
dest_top_five1$setkey2<- paste0(dest_top_five1$quad,dest_top_five1$hotel_market,dest_top_five1$hotel_country)
dest_top_five2$setkey3<- paste0(dest_top_five2$quad,dest_top_five2$hotel_country)


y<-unclass(strptime(test_data$date_time, format = "%Y-%m-%d"))
test_data$mon24 <-y$mon
test_data$mon24 <-test_data$mon24+1
test_data$quad <- as.integer((test_data$mon24)/3)
test_data$quad[test_data$quad==4]<-as.integer(0)
test_data$setkey1 <- paste0(test_data$quad,test_data$hotel_market,test_data$hotel_country,test_data$srch_destination_id)
test_data1$setkey2<- paste0(test_data1$quad,test_data1$hotel_market,test_data1$hotel_country)
test_data$setkey3 <- paste0(test_data$quad,test_data$hotel_country)
#test_data1<-copy(test_data)
dd<-merge(test_data,dest_top_five, by="setkey1",all.x=TRUE)
dd1<-merge(test_data1,dest_top_five1, by="setkey2",all.x=TRUE,allow.cartesian = TRUE)
dd5<-merge(test_data,dest_top_five2, by="setkey3",all.x=TRUE)


#filling up is.na values
dd[,matchingIndex:=match(dd$id,dd1$id)]
dd[is.na(freq3),freq3:=dd1$freq2[dd$matchingIndex[is.na(dd$freq3)]]]
dd[,matchingIndex := NULL]

dd[,matchingIndex:=match(dd$id,dd5$id)]
dd[is.na(freq3),freq3:=dd5$V1[dd$matchingIndex[is.na(dd$freq3)]]]
dd[,matchingIndex := NULL]

dd2<-copy(dd)

#trying extraction from orig distance
dd2[,matchingIndex:=match(dd2$orig_destination_distance,train_data1$orig_destination_distance)]
dd2[(dd$orig_destination_distance %in% train_data1$orig_destination_distance),freq3:=train_data1$hotel_cluster[dd2$matchingIndex[which(dd2$orig_destination_distance %in% train_data1$orig_destination_distance)]]]
dd2[,matchingIndex := NULL]


#not using city for analysis currently
 # dd2[,matchingIndex:=match(dd2$user_location_city,train_data1$user_location_city)]
 # dd2[(dd$user_location_city %in% train_data1$user_location_city),freq3:=train_data1$hotel_cluster[dd2$matchingIndex[which(dd2$user_location_city %in% train_data1$user_location_city)]]]
 # dd2[,matchingIndex := NULL]
 #   

repeats <- c("41")
dd2[,matchingIndex:=match(dd2$id,dd$id)]
dd2[(dd2$freq3 %in% repeats),freq3 :=dd$freq3[dd2$matchingIndex[which(dd2$freq3 %in% repeats)]]]
dd2[,matchingIndex := NULL]

# 
# 
# dd<-dd[order(id),list(id,freq3)]
# dd2<-dd2[order(id),list(id,freq2)]
dd3<-copy(dd2)
dd3<-dd3[order(setkey1),list(srch_destination_id.x,hotel_market.x,hotel_country.x,quad.x,freq3,setkey1)]




setnames(dd3,c("srch_destination_id","hotel_market","hotel_country","quadrant","hotel_cluster","setkey"))
dd3<-unique(dd3)
write.csv(dd3, file='gen_var_best_i_guess.csv', row.names=FALSE)



# 
# dd$id<-1:nrow(test_data)
# k=nrow(test_data)
# v=c(25,46,83,18,70,21,42,59,98,5,65,64,48,41,91)
# p=c(0.014085131,0.014176635,0.014179130,0.014475173,0.014482818,0.014602807,0.014642971,0.015139012,0.015640388,0.016463742,0.017811383,0.018707951,0.020016648,0.020513326,0.027706713)
# set.seed(125)
# v1=sample(v,5*nrow(test_data),replace = TRUE,prob = p)
# 
# while(k)
#   {
# dd$hotel_cluster <- paste(as.character(v[5*k-5:5]))
#   k=k-1
#   }
# setnames(dd,c("id","hotel_cluster"))
# write.csv(dd, file='for fun.csv', row.names=FALSE)
# 





library(splitstackshape)


#dd <- merge(test,dest_top_five, by="srch_destination_id",all.x=TRUE)[order(id),list(id,V1)]
