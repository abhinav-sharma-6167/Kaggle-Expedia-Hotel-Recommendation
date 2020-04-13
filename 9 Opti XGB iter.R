#Model with parameters from benchmark


#Libraries
library(data.table)
library(ggplot2)
library(plyr)
library(xgboost)

#Seed & Read
set.seed(100)
train <- fread("train.csv")
test <- fread("test.csv")
dd<-fread("data_subset_gen_var_best_i_guess.csv")
dd_2<-fread("gen_var_best_i_guess.csv")

#Playing with time 
x<-unclass(strptime(train$date_time, format = "%Y-%m-%d"))
train$year<-x$year

#Subsetting the 2014 data
train<-train[train$year==114]#now train has 26.48M rows
x<-unclass(strptime(train$date_time, format = "%Y-%m-%d"))

#Generating quadrants
train$mon<-x$mon
train$mon <-train$mon+1
train$quad <- as.integer((train$mon)/3)
train$quad[train$quad==4]<-as.integer(0)

#Setkey and merge
train$setkey <- paste0(train$quad,train$hotel_market,train$hotel_country,train$srch_destination_id)
train_freq<-merge(train,dd, by="setkey",all.x=TRUE)

train_freq<-rename(train_freq, c("srch_destination_id.x"="srch_destination_id", "hotel_market.x"="hotel_market" , "hotel_country.x"="hotel_country" , "hotel_cluster.y"="hotel_cluster"))
train_freq<-rename(train_freq, c("quad"="quad1"))
train_freq$quad<-train_freq$quad1
train_freq[,quad1:=NULL]


train_freq1<-merge(train,dd, by="setkey",all.x=TRUE)
target_variable<-train_freq1$hotel_cluster.x
rm(train_freq1)



#Preparing variables for xgboost
a<-strsplit(train_freq$hotel_cluster," ")
train_freq$first <- sapply(a, "[", 1)

train_freq$second <- sapply(a, "[", 2)

train_freq$third <- sapply(a, "[", 3)

train_freq$forth <- sapply(a, "[", 4)

train_freq$fifth <- sapply(a, "[", 5)

#train_freq<-readRDS("train_freq.rds")
train_freq[,hotel_cluster :=NULL]
train_freq <- rename(train_freq,c("hotel_cluster.x"="hotel_cluster"))

train_freq[,year :=NULL]
train_freq[,mon24 :=NULL]



#Date time variables
y<-unclass(strptime(train_freq$srch_ci, format = "%Y-%m-%d"))
z<-unclass(strptime(train_freq$srch_co, format = "%Y-%m-%d"))
train_freq<-train_freq[,arr_day :=as.factor(y$wday)]

y$yday1<-365*(y$year-113)+y$yday
z$yday1<-365*(z$year-113)+z$yday
y1<-z$yday1-y$yday1
y1<-as.data.table(y1)
train_freq<-cbind(train_freq,y1)
train_freq<-train_freq[,stay_days :=as.factor(y1)]
train_freq[,y1:=NULL]
train_freq[,srch_ci :=NULL]
train_freq[,srch_co :=NULL]

#filling up NA in orig dist
hotel_market_avgdist <- train_freq[!is.na(orig_destination_distance),lapply(.SD,mean),by=c("hotel_market"),
                                         .SDcols=c("orig_destination_distance")]
hotel_country_avgdist <- train_freq[!is.na(orig_destination_distance),lapply(.SD,mean),by=c("hotel_country"),
                                           .SDcols=c("orig_destination_distance")]
train_freq[,matchingIndex:=match(train_freq$hotel_market,hotel_market_avgdist$hotel_market)]
train_freq[is.na(orig_destination_distance),orig_destination_distance:=hotel_market_avgdist$orig_destination_distance[train_freq$matchingIndex[is.na(train_freq$orig_destination_distance)]]]


train_freq[,matchingIndex:=match(train_freq$hotel_country,hotel_country_avgdist$hotel_country)]
train_freq[is.na(orig_destination_distance),orig_destination_distance:=hotel_country_avgdist$orig_destination_distance[train_freq$matchingIndex[is.na(train_freq$orig_destination_distance)]]]

train_freq[,matchingIndex :=NULL]




#Preparing similar for test data
x1<-unclass(strptime(test$date_time, format = "%Y-%m-%d"))

test$mon<-x1$mon
test$mon <-test$mon+1
test$quad <- as.integer((test$mon)/3)
test$quad[test$quad==4]<-as.integer(0)



#Setkey and merge
test$setkey <- paste0(test$quad,test$hotel_market,test$hotel_country,test$srch_destination_id)
test_freq<-merge(test,dd_2, by="setkey",all.x=TRUE)

test_freq$mon <- x1$mon
test_freq[,mon24:=NULL]


#Date time variables
y<-unclass(strptime(test_freq$srch_ci, format = "%Y-%m-%d"))
z<-unclass(strptime(test_freq$srch_co, format = "%Y-%m-%d"))
test_freq<-test_freq[,arr_day :=as.factor(y$wday)]

y$yday1<-365*(y$year-113)+y$yday
z$yday1<-365*(z$year-113)+z$yday
y1<-z$yday1-y$yday1
y1<-as.data.table(y1)
test_freq<-cbind(test_freq,y1)
test_freq<-test_freq[,stay_days :=as.factor(y1)]
test_freq[,y1:=NULL]
test_freq[,srch_ci :=NULL]
test_freq[,srch_co :=NULL]



test_freq<-rename(test_freq, c("srch_destination_id.x"="srch_destination_id", "hotel_market.x"="hotel_market" , "hotel_country.x"="hotel_country" ))



#Preparing variables for xgboost
a<-strsplit(test_freq$hotel_cluster," ")
test_freq$first <- sapply(a, "[", 1)

test_freq$second <- sapply(a, "[", 2)

test_freq$third <- sapply(a, "[", 3)

test_freq$forth <- sapply(a, "[", 4)

test_freq$fifth <- sapply(a, "[", 5)





#filling up NA in orig dist
hotel_market_avgdist <- test_freq[!is.na(orig_destination_distance),lapply(.SD,mean),by=c("hotel_market"),
                                   .SDcols=c("orig_destination_distance")]
hotel_country_avgdist <- test_freq[!is.na(orig_destination_distance),lapply(.SD,mean),by=c("hotel_country"),
                                    .SDcols=c("orig_destination_distance")]
test_freq[,matchingIndex:=match(test_freq$hotel_market,hotel_market_avgdist$hotel_market)]
test_freq[is.na(orig_destination_distance),orig_destination_distance:=hotel_market_avgdist$orig_destination_distance[test_freq$matchingIndex[is.na(test_freq$orig_destination_distance)]]]


test_freq[,matchingIndex:=match(test_freq$hotel_country,hotel_country_avgdist$hotel_country)]
test_freq[is.na(orig_destination_distance),orig_destination_distance:=hotel_country_avgdist$orig_destination_distance[test_freq$matchingIndex[is.na(test_freq$orig_destination_distance)]]]

test_freq[,matchingIndex :=NULL]









#xgboosting

target <-train_freq$hotel_cluster








train_final <- copy(train_freq)
train_final <-train_final[,colnames(train_final)[-c(3:8,10:15,17,20:22,25,24,31,32,26:30)]:= NULL,with=F]
train_final[,is_booking:=NULL]
train_final[,cnt:=NULL]

train_matrix = data.matrix(as.data.frame(train_final))

train.xg = xgb.DMatrix(train_matrix, label=target, missing=NA)











test_final <- copy(test_freq)
test_final <-test_final[,colnames(test_final)[-c(4:9,11:16,18:21,23,22,28:32,26,27)]:= NULL,with=F]




















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
              min_child_weight = 800,
              colsample_bytree = 1,
              base_score =0
)
set.seed(123)

start_time <- Sys.time()

model <- xgb.train(params =  param, data =  train.xg, label = target, nthread = 16, nround = 250)
end_time <- Sys.time()
time_taken <- end_time - start_time


prediction1<-predict(model_xgb2,validation2, missing=NA)




probability_table<-as.data.table((matrix(prediction1,nrow = 97210,byrow = T)))
#top5 <- t(apply(probability_table, 1, function(y) order(y)[num.class:(num.class-4)]-1))

#to find top 5


#calculation of the error metric
err <- map5(probability_table,test.xg)
err
