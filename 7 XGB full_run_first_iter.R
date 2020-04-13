#Setting directory
setwd("D:/Learning/Kaggle/Expedia")

#Libraries used
library(randomForest)
library(data.table)
library(ff)
library(bigmemory)
library(ggplot2)
library(plyr)
library(xgboost)
library(mgcv)

#Setting the seed in case for random sampling later
gc()
set.seed(100)


#freading the data into data table
train <- fread("train.csv")
test <- fread("test.csv")
dest <- fread("destinations.csv")

train_data<-train
dest_data<-dest
test_data<-test

#n <- length(x)
#sort(x,partial=n-1)[n-1]
# important site names in both 2,11,24,37,34,8,13,23,28,17,30,18,25,26,33,22,,,,only in test 50,51



#not sampling


# train_user_id<-as.factor(unique(train_data$user_id))
# train_user_id_sample <- (unclass(train_user_id[sample(1:length(train_user_id),30000,replace = F)]))
# 
# 
# new_train <- subset(train_data, subset = user_id %in% train_user_id_sample)
# new_train<-new_train[order(user_id)]
 

#new_train_booking<-new_train[new_train$is_booking==1]



#new_train_booking<-new_train
new_train_booking<-new_train_booking[,srno :=as.factor(1:nrow(new_train_booking))]

new_train_booking<-new_train_booking[,srno :=as.factor(1:nrow(new_train_booking))]
x<-unclass(strptime(new_train_booking$date_time, format = "%Y-%m-%d"))
new_train_booking$to_group <-365*(x$year-113)+x$yday

#.SD means as if S ubset of the D ata table..so subset all non NA values by hotel market and calculate their mean
#very important lines of code for feature engineering....to be understood..properly!
hotel_market_avgdist <- new_train_booking[!is.na(orig_destination_distance),lapply(.SD,mean),by=c("hotel_market"),
                                  .SDcols=c("orig_destination_distance")]
hotel_country_avgdist <- new_train_booking[!is.na(orig_destination_distance),lapply(.SD,mean),by=c("hotel_country"),
                                   .SDcols=c("orig_destination_distance")]
new_train_booking[,matchingIndex:=match(new_train_booking$hotel_market,hotel_market_avgdist$hotel_market)]
new_train_booking[is.na(orig_destination_distance),orig_destination_distance:=hotel_market_avgdist$orig_destination_distance[new_train_booking$matchingIndex[is.na(new_train_booking$orig_destination_distance)]]]


#in new train booking create another column such that it holds a match from hotel_country in data to correspondinghotel country and its averaged distance  
new_train_booking[,matchingIndex:=match(new_train_booking$hotel_country,hotel_country_avgdist$hotel_country)]
new_train_booking[is.na(orig_destination_distance),orig_destination_distance:=hotel_country_avgdist$orig_destination_distance[new_train_booking$matchingIndex[is.na(new_train_booking$orig_destination_distance)]]]



new_train_booking<-new_train_booking[order(to_group)]

# for (i in 1:nrow(new_train_booking)) {
#   
# 
# agg_by_searchID <- table(new_train_booking$srch_destination_id[1:i],new_train_booking$hotel_cluster)
# max_col <- apply(agg_by_searchID,1,FUN = which.max)
# agg_by_searchID2 <- as.data.table(data.frame(srch_destination_id = rownames(agg_by_searchID),Cluster = colnames(agg_by_searchID)[max_col]))
# 
# }
agg_by_searchID <- table(train_data$srch_destination_id,train_data$hotel_cluster)
max_col <- apply(agg_by_searchID,1,FUN = which.max)
agg_by_searchID2 <- as.data.table(data.frame(srch_destination_id = rownames(agg_by_searchID),Cluster = colnames(agg_by_searchID)[max_col]))



agg_by_searchID2$srch_destination_id<-as.integer(agg_by_searchID2$srch_destination_id)
setkey(new_train_booking,srch_destination_id)
setkey(agg_by_searchID2,srch_destination_id)
new_train_booking<-merge(new_train_booking,agg_by_searchID2,all.x=TRUE)

new_train_booking<-new_train_booking[order(srno)]



agg_by_hotel_market <- table(train_data$hotel_market,train_data$hotel_cluster)
max_col1 <- apply(agg_by_hotel_market,1,FUN = which.max)
agg_by_hotel_market2 <- as.data.table(data.frame(hotel_market = rownames(agg_by_hotel_market),Cluster = colnames(agg_by_hotel_market)[max_col1]))






#Date time variables
y<-unclass(strptime(new_train_booking$srch_ci, format = "%Y-%m-%d"))
z<-unclass(strptime(new_train_booking$srch_co, format = "%Y-%m-%d"))
new_train_booking<-new_train_booking[,arr_day :=as.factor(y$wday)]
t2<-12*(x$year-113)+x$mon
new_train_booking<-new_train_booking[,mon24 :=as.factor(t2)]
y$yday1<-365*(y$year-113)+y$yday
z$yday1<-365*(z$year-113)+z$yday
y1<-z$yday1-y$yday1
y1<-as.data.table(y1)
new_train_booking<-cbind(new_train_booking,y1)
new_train_booking<-new_train_booking[,stay_days :=as.factor(y1)]
new_train_booking[,y1:=NULL]








 
# new_train_booking1<-copy(new_train_booking)
# for (i in 1:nrow(new_train_booking1)) {
#   for (j in 1:nrow(hotel_market_avgdist)) {
#     
#   
# 
# new_train_booking1$orig_destination_distance[which(is.na(new_train_booking1$orig_destination_distance))] <- hotel_market_avgdist$orig_destination_distance[hotel_market_avgdist$hotel_market[j] == new_train_booking1$hotel_market[i]]
# }}




#new_train_booking<-new_train_booking[,hotel_market_avgdist :=hotel_market_avgdist]
#new_train_booking<-new_train_booking[,hotel_country_avgdist :=hotel_country_avgdist]


 
training<-subset(new_train_booking,subset = new_train_booking$to_group>=365)
validation <-subset(new_train_booking,subset = new_train_booking$to_group<546)



train_target_t<-training$hotel_cluster
training1 <- copy(training)
training1 <-training1[,colnames(training1)[-c(7,21:23,28:30)]:= NULL,with=F]

training2 = data.matrix(as.data.frame(training1))

train.xg = xgb.DMatrix(training2, label=train_target_t, missing=NA)





validation1 <- copy(validation)
validation1<-validation1[validation1$is_booking==1]
train_target_v<-validation1$hotel_cluster
validation1 <-validation1[,colnames(validation1)[-c(7,21:23,28:30)]:= NULL,with=F]


validation2 = data.matrix(as.data.frame(validation1))

test.xg = xgb.DMatrix(validation2, label=train_target_v, missing=NA)

watchlist <- list(test=test.xg, train=train.xg)


library(Metrics)
map5 <- function(preds, dtrain)
  #function over prediction and xgboost object
{
  labels <- as.list(getinfo(dtrain,"label"))
  #why are we getting labels??okays for mapk..but what are these labels..cant see inside test.xg
  num.class = 100
  #so we need to use the entire probability table for preds 
  pred <- matrix(preds, nrow = num.class)
  top <- t(apply(pred,2,  function(y) order(y)[num.class:(num.class-4)]-1))
  #t is for transpose, apply for loop rowwise, function to get best 5 choices
  top1 <- split(top, 1:NROW(top))
  
  map <- mapk(5, labels, top1)
  return(list(metric = "map5", value = map))
}


param <- list(max_depth = 10,
              eta = 0.008,
              silent = 1,
              objective="multi:softprob",
              num_class=100,
              eval_metric=map5,
              # subsample = 0.75,
              min_child_weight = 1000,
              colsample_bytree = 1,
              base_score =0
)
set.seed(123)

start_time <- Sys.time()

model_xgb2 <- xgb.train(param, train.xg, nthread = 16, nround = 250, watchlist,
                        early.stop.round=10,maximize = T)

end_time <- Sys.time()
time_taken <- end_time - start_time

library(gbm)
start_time <- Sys.time()
model_gbm <- gbm.fit(training1, train_target_t, train.fraction=0.8,
                 n.trees=250,
                 interaction.depth=6, verbose=F, shrinkage=0.008,
                 n.minobsinnode = 100)
save(model_gbm, file="./model_gbm.Rdata")

end_time <- Sys.time()
time_taken <- end_time - start_time




prediction1<-predict(model_xgb2,validation2, missing=NA)




probability_table<-as.data.table((matrix(prediction1,nrow = 97210,byrow = T)))
#top5 <- t(apply(probability_table, 1, function(y) order(y)[num.class:(num.class-4)]-1))

#to find top 5


#calculation of the error metric
err <- map5(probability_table,test.xg)
err
