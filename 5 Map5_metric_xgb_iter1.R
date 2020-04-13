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


#Setting the seed in case for random sampling later
set.seed(100)


#freading the data into data table
train <- fread("train.csv")
test <- fread("test.csv")
dest <- fread("destinations.csv")


#just a experimenting sample
train_sample<-train[1:2000,]
train_data<-train
dest_data<-dest
test_data<-test

#Sampling things
train_user_id<-as.factor(unique(train_data$user_id))
train_user_id_sample <- (unclass(train_user_id[sample(1:length(train_user_id),100000,replace = F)]))

#i<-1:length(train_user_id_sample)
#j<-1:length(train_data)


# new_train<-train_data[1,]
# for(i in 1:length(train_user_id_sample)) {
#   for(j in 1:length(train_data)){
#   if(as.integer(train_user_id_sample)[i] == train_data$user_id[j]) new_train <-rbind(new<-train,train_data[train_data$user_id==j])
#   }
# }

#dont just loop around..sunsetting cooler
new_train <- subset(train_data, subset = user_id %in% train_user_id_sample)
new_train<-new_train[order(user_id)]



new_train_booking<-new_train[new_train$is_booking==1]

x<-unclass(strptime(new_train_booking$date_time, format = "%Y-%m-%d %H:%M:%S"))
new_train_booking$to_group <-365*(x$year-113)+x$yday

#Making Features mon and mon24
new_train_booking<-new_train_booking[,mon :=as.factor(x$mon)]
t1<-as.integer((x$yday)/7)
new_train_booking<-new_train_booking[,week :=as.factor(t1)]
t2<-12*(x$year-113)+x$mon
new_train_booking<-new_train_booking[,mon24 :=as.factor(t2)]
new_train_booking<-new_train_booking[,hour :=as.factor(x$hour)]
new_train_booking<-new_train_booking[,wday :=as.factor(x$wday)]



new_train_booking<-new_train_booking[,srno :=as.factor(1:nrow(new_train_booking))]


k<-as.data.frame(table(new_train_booking$mon,new_train_booking$hotel_cluster))
k<-as.data.table(k)
new_train_booking$setkey1 <- 1000*(as.numeric(new_train_booking$mon)-1)+as.numeric(new_train_booking$hotel_cluster)
k$setkey1 <-1000*(as.numeric(k$Var1)-1)+as.numeric(k$Var2)-1
setkey(new_train_booking,setkey1)
setkey(k,setkey1)
new_train_booking<-merge(new_train_booking,k)

new_train_booking[,Var1:=NULL]
new_train_booking[,Var2:=NULL]
new_train_booking[,setkey1:=NULL]

new_train_booking<-new_train_booking[order(srno)]




#for mon24

k<-as.data.frame(table(new_train_booking$mon24,new_train_booking$hotel_cluster))
k<-as.data.table(k)
new_train_booking$setkey1 <- 1000*(as.numeric(new_train_booking$mon24)-1)+as.numeric(new_train_booking$hotel_cluster)
k$setkey1 <-1000*(as.numeric(k$Var1)-1)+as.numeric(k$Var2)-1
setkey(new_train_booking,setkey1)
setkey(k,setkey1)
k$frequency <- k$Freq
k[,Freq:=NULL]
new_train_booking<-merge(new_train_booking,k)

new_train_booking[,Var1:=NULL]
new_train_booking[,Var2:=NULL]
new_train_booking[,setkey1:=NULL]

new_train_booking<-new_train_booking[order(srno)]





#Creating ci co data
y<-unclass(strptime(new_train_booking$srch_ci, format = "%Y-%m-%d"))
z<-unclass(strptime(new_train_booking$srch_co, format = "%Y-%m-%d"))

y$yday1<-365*(y$year-113)+y$yday
z$yday1<-365*(z$year-113)+z$yday
y1<-z$yday1-y$yday1
y1<-as.data.table(y1)
new_train_booking<-cbind(new_train_booking,y1)


#Taking care of ci>c,subset = as.numeric(y1)>=0)
new_train_booking<-new_train_booking[,arr_mon :=as.factor(y$mon)]
new_train_booking<-new_train_booking[,dep_day :=as.factor(z$wday)]
new_train_booking<-subset(new_train_booking,subset = new_train_booking$y1>=0)

new_train_booking<-new_train_booking[,stay_days :=as.factor(y1)]
new_train_booking[,y1:=NULL]





training<-subset(new_train_booking,subset = new_train_booking$to_group<546)
validation <-subset(new_train_booking,subset = new_train_booking$to_group>=546)
training<-training[training$is_booking==1]
validation<-validation[validation$is_booking==1]
#not in common user_ids present

#Model making on available data using xgboost cause it can handle it..u know itne sare factors

#for xgboost constraints are: all should be numeric vectors









train_target_t<-training$hotel_cluster
training1 <- copy(training)
training1 <-training1[,colnames(training1)[c(1,12,13,24,25,28,29,30,32)]:= NULL,with=F]


#new_train_booking1<-new_train_booking1[,date_time:= NULL]


training2 = data.matrix(as.data.frame(training1))
# for(i in 1:ncol(new_train_booking2)){
#   new_train_booking2[,i] <- as.numeric(new_train_booking2[,i])
# }

train.xg = xgb.DMatrix(training2, label=train_target_t, missing=NaN)













train_target_v<-validation$hotel_cluster
validation1 <- copy(validation)
validation1 <-validation1[,colnames(validation1)[c(1,12,13,24,25,28,29,30,32)]:= NULL,with=F]


#new_train_booking1<-new_train_booking1[,date_time:= NULL]


validation2 = data.matrix(as.data.frame(validation1))
# for(i in 1:ncol(new_train_booking2)){
#   new_train_booking2[,i] <- as.numeric(new_train_booking2[,i])
# }

test.xg = xgb.DMatrix(validation2, label=train_target_v, missing=NaN)

watchlist <- list(test=test.xg, train=train.xg)

param <- list(max_depth = 10,
              eta = 0.008,
              silent = 1,
              objective="multi:softprob",
              num_class=100,
              eval_metric="ndcg",
              # subsample = 0.75,
              min_child_weight = 1000,
              colsample_bytree = 1,
              base_score =0)
set.seed(123)
model_xgb <- xgb.train(param, train.xg, nthread = 8, nround = 1000, watchlist,
                       early.stop.round=10)


prediction<-predict(model_xgb,validation2, missing=NaN)





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

model_xgb2 <- xgb.train(param, train.xg, nthread = 8, nround = 1000, watchlist,
                        early.stop.round=10,maximize = T)

end_time <- Sys.time()
time_taken <- end_time - start_time


prediction1<-predict(model_xgb1,validation2, missing=NaN)




probability_table<-as.data.table((matrix(prediction1,nrow = 97210,byrow = T)))
top5 <- t(apply(probability_table, 1, function(y) order(y)[num.class:(num.class-4)]-1))

#to find top 5


#calculation of the error metric
err <- map5(probability_table,test.xg)






# 
# #Making Submission File
# submit1 <- data.frame(user_id = validation2[,7], cluster = prediction)
# 
# #write the csv firstdtree
# write.csv(submit, file = "myfirst.csv", row.names = FALSE)





# for params, it would be better if you can read the documentation once

# objective and eval_metric will change



# any(!(validation$user_id %in% training$user_id))
# length(which((validation$user_id %in% training$user_id)==FALSE))
# length(which((unique(validation$user_id) %in% unique(training$user_id))==FALSE))




#Clearing 19% of the data, subsetting, b is subsetted data table




#Random sampling of the data, c is sampled data table
c<-b[sample(1:1000000)]

#Merging the data tables by search desti id, c is subsetted, sampled, merged data table
#setkey(c,srch_destination_id)
#setkey(d,srch_destination_id)

#c<-merge(c,d)



# Feature engineering using time and date stuff
# x<-unclass(strptime(c$date_time, format = "%Y-%m-%d %H:%M:%S"))
# y<-unclass(strptime(c$srch_ci, format = "%Y-%m-%d"))
# z<-unclass(strptime(c$srch_co, format = "%Y-%m-%d"))
# y<-z$yday-y$yday
# 
# Taking care of -360 type values
# i<-1:length(y)
# for(i in 1:length(y)) {
#   if(y[i]<0)y[i]<-y[i]+365
# }
# 
# 
# #adding month,hour,ndays as factor column to c
# c<-c[,year :=as.factor(x$year)]
# c<-c[,mon :=as.factor(x$mon)]
# t<-as.integer(4*x$mon) + as.integer(x$mday/7)
# c<-c[,week :=as.factor(t)]
# t1<-365*(x$year-113)+x$yday
# c<-c[,yday :=as.factor(t1)]
# 
# t2<-12*(x$year-113)+x$mon
# c<-c[,mon24 :=as.factor(t2)]
# c<-c[,hour :=as.factor(x$hour)]
# c<-c[,wday :=as.factor(x$wday)]
# c<-c[,ndays :=as.factor(y)]
# c<-c[,srno :=as.factor(1:nrow(c))]
# 


#Feature engineering using user position continents, countries and stuff
#let continent remain continent
#creating a function for top 25

#top_25 <- function(m,v1){
#  m_sorted <- m[order(v1,decreasing=TRUE)]
#  n <- min(25,length(m_sorted))
#  paste(m_sorted[1:n],collapse=" ")
#}

#Finding top 25 countries
#cap <- top_25(c,c$user_location_country)


#Have to subset according to hotel cluster and obtain number of bookings by month

#Finding freq per month per cluster, merging it into c, removing remaining unrequired columns


# 
# k<-as.data.frame(table(c$mon,c$hotel_cluster))
# k<-as.data.table(k)
# c$setkey1 <- 1000*(as.numeric(c$mon)-1)+as.numeric(c$hotel_cluster)
# k$setkey1 <-1000*(as.numeric(k$Var1)-1)+as.numeric(k$Var2)-1
# setkey(c,setkey1)
# setkey(k,setkey1)
# c<-merge(c,k)
# 
# c[,Var1:=NULL]
# c[,Var2:=NULL]
# c[,setkey1:=NULL]
# c<-c[order(srno)]
# 
# 
# 
# #Finding freq per week per cluster, merging it into c, removing remaining unrequired columns
# k<-as.data.frame(table(c$week,c$hotel_cluster))
# k<-as.data.table(k)
# c$setkey1 <- 1000*(as.numeric(c$week)-1)+as.numeric(c$hotel_cluster)
# k$setkey1 <-1000*(as.numeric(k$Var1)-1)+as.numeric(k$Var2)-1
# setkey(c,setkey1)
# setkey(k,setkey1)
# c<-merge(c,k)
# 
# c[,Var1:=NULL]
# c[,Var2:=NULL]
# c[,setkey1:=NULL]
# c<-c[order(srno)]
# 
# 
# 
# 
# 
# 
# 
# 
# #Finding freq per indiv month per cluster, merging it into c, removing remaining unrequired columns
# k<-as.data.frame(table(c$mon24,c$hotel_cluster))
# k<-as.data.table(k)
# k$frequency <- k$Freq
# k[,Freq:=NULL]
# c$setkey1 <- 1000*(as.numeric(c$mon24)-1)+as.numeric(c$hotel_cluster)
# k$setkey1 <-1000*(as.numeric(k$Var1)-1)+as.numeric(k$Var2)-1
# setkey(c,setkey1)
# setkey(k,setkey1)
# c<-merge(c,k)
# 
# #c[,Freq.x:=NULL]
# #c[,Freq.y:=NULL]
# c[,Var1:=NULL]
# c[,Var2:=NULL]
# c[,setkey1:=NULL]
# c<-c[order(srno)]
# 
# 
# 
# 
# 
# 
# #Finding freq per weekkaday per cluster, merging it into c, removing remaining unrequired columns
# k<-as.data.frame(table(c$hour,c$hotel_cluster))
# k<-as.data.table(k)
# k$frequency2 <- k$Freq
# k[,Freq:=NULL]
# c$setkey1 <- 1000*(as.numeric(c$hour)-1)+as.numeric(c$hotel_cluster)
# k$setkey1 <-1000*(as.numeric(k$Var1)-1)+as.numeric(k$Var2)-1
# setkey(c,setkey1)
# setkey(k,setkey1)
# c<-merge(c,k)
# 
# #c[,Freq.x:=NULL]
# #c[,Freq.y:=NULL]
# c[,Var1:=NULL]
# c[,Var2:=NULL]
# c[,setkey1:=NULL]
# c<-c[order(srno)]
# 
# 
# 
# 
# #Below are just analysis k liye plots!!
# 
# 
# #The 100 plots to understand seasonal changes, by month plots, just to study
# j<-1:length(unique(c$hotel_cluster))
# for(j in 1:length(unique(c$hotel_cluster))){
#   c1 <- subset(c, subset=hotel_cluster==j, select = c(26,30))
#   #c2<-as.data.frame(unique(c1))
#   c3 <- c1[,.N,by=mon]
# 
#   
# 
# ggplot(c3, aes(x = mon, y = N)) + geom_point()
# ggsave(file=paste0(j,".png"))
# 
#   
# }
# 
# 
# 
# #The 100 plots to understand seasonal changes, by date plots, just to study
# j<-1:length(unique(c$hotel_cluster))
# for(j in 1:length(unique(c$hotel_cluster))){
#   c1 <- subset(c, subset=hotel_cluster==j, select = c(33,35))
#   #c2<-as.data.frame(unique(c1))
#   c3 <- c1[,.N,by=mon24]
#   
#   
#   
#   ggplot(c3, aes(x = mon24, y = N)) + geom_point()
#   ggsave(file=paste0(j,".png"))
#   
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #The 100 plots to understand weekly changes, by date plots, just to study
# j<-1:length(unique(c$hotel_cluster))
# for(j in 1:length(unique(c$hotel_cluster))){
#   c1 <- subset(c, subset=hotel_cluster==j, select = c(27,38))
#   #c2<-as.data.frame(unique(c1))
#   c3 <- c1[,.N,by=hour]
#   
#   
#   
#   ggplot(c3, aes(x = hour, y = N)) + geom_point()
#   ggsave(file=paste0(j,".png"))
#   
#   
# }
# 









#qplot(x=df[,1],data=df,colour=factor(clus),geom="density",xlab=colnames(df)[1]) + ggtitle(paste0("Cluster Distribution for ", colnames(df)[1],"_partials_",varName,sep=""))






























#######################Please Ignore#####################################
#irrelevant comments
#train <- as.data.frame(fread("train.csv"))

#test <- as.data.frame(fread("test.csv"))

#model01 <- randomForest(as.factor(hotel_cluster) ~ site_name + posa_continent + user_location_country + user_location_region +
#user_location_city + orig_destination_distance + is_mobile + is_package + channel + srch_adults_cnt + srch_children_cnt + srch_rm_cnt +srch_destination_id +is_booking + cnt + hotel_continent + hotel_country + hotel_market, data=train, method="class")






