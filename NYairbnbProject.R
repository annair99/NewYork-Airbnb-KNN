######GET RID OF PRICE COLUMN

NYC_DATA= na.omit(AB_NYC_2019)##No NA values
NYC_DATA$neighbourhood<-as.factor(NYC_DATA$neighbourhood)
NYC_DATA_CLEAN= NYC_DATA[,-c(1:4,6,13)]
View(NYC_DATA_CLEAN)
#adding the target variable
NYC_TARGET<-NYC_DATA_CLEAN
NYC_TARGET$target = NA

#Creating the target class
for (i in 1:nrow(NYC_TARGET)){
  if(NYC_TARGET$price[i]<80){
    NYC_TARGET$target[i]="Low"
  }else if(NYC_TARGET$price[i]<150){
    NYC_TARGET$target[i]="Medium"
  }else{
    NYC_TARGET$target[i]="High"
  }
}
NYC_TARGET$target=as.factor(NYC_TARGET$target)

summary(NYC_TARGET$target)

##Data visualization
library(ggplot2)
ggplot(data = NYC_DATA_CLEAN,aes(x=room_type,y=price))+geom_boxplot(outlier.shape=NA)+ylim(0,300)
ggplot(data = NYC_DATA_CLEAN,aes(x=neighbourhood_group,y=price))+geom_boxplot(outlier.shape=NA)+ylim(0,300)

#one hot encoding categorical
library(data.table)
install.packages("mltools")
library(mltools)
S_NYC<-one_hot(as.data.table(NYC_TARGET), cols = c("room_type","neighbourhood_group"))

##Standardizing

library(standardize)
library("dplyr") 
S_NYC1<-S_NYC%>%mutate_if(is.numeric, function (x) as.vector(scale(x)))
S_NYC1<-S_NYC1[,-11]#get rid of original price var

#split into LOW, MED AND HIGH

set.seed(1)
LOWPRICE = S_NYC1[which(S_NYC1$target =="Low"),]

set.seed(1)
MEDPRICE = S_NYC1[which(S_NYC1$target =="Medium"),]

set.seed(1)
HIGHPRICE = S_NYC1[which(S_NYC1$target =="High"),]

###Compare histograms visually and conduct t-test

par(mfrow = c(3,3))
hist(LOWPRICE$availability_365, main = "Low Price" , col = "lightblue", xlab = "availability_365")
hist(MEDPRICE$availability_365, main = "Medium Price" , col = "lightblue", xlab = "availability_365")
hist(HIGHPRICE$availability_365, main = "High Price" , col = "lightblue", xlab = "availability_365")

t.test(LOWPRICE$availability_365,MEDPRICE$availability_365)
t.test(LOWPRICE$availability_365,HIGHPRICE$availability_365)
t.test(MEDPRICE$availability_365,HIGHPRICE$availability_365)

par(mfrow = c(1,3))
hist(LOWPRICE$reviews_per_month, main = "Low Price" , col = "lightblue", xlab = "reviews per month", xlim = c(0,35))
hist(MEDPRICE$reviews_per_month, main = "Medium Price" , col = "lightblue", xlab = "reviews per month", xlim = c(0,35))
hist(HIGHPRICE$reviews_per_month, main = "High Price" , col = "lightblue", xlab = "reviews per month",xlim =c(0,35))

t.test(LOWPRICE$reviews_per_month,MEDPRICE$reviews_per_month)
t.test(LOWPRICE$reviews_per_month,HIGHPRICE$reviews_per_month)
t.test(MEDPRICE$reviews_per_month,HIGHPRICE$reviews_per_month)

par(mfrow = c(1,3))
hist(LOWPRICE$number_of_reviews, main = "Low Price" , col = "lightblue", xlab = "number_of_reviews")
hist(MEDPRICE$number_of_reviews, main = "Medium Price" , col = "lightblue", xlab = "number_of_reviews")
hist(HIGHPRICE$number_of_reviews, main = "High Price" , col = "lightblue", xlab = "number_of_reviews")

t.test(LOWPRICE$number_of_reviews,MEDPRICE$number_of_reviews)
t.test(LOWPRICE$number_of_reviews,HIGHPRICE$number_of_reviews)
t.test(MEDPRICE$number_of_reviews,HIGHPRICE$number_of_reviews)


par(mfrow = c(3,5))
hist(LOWPRICE$minimum_nights, main = "Low Price" , col = "lightblue", xlab = "min nights")
hist(MEDPRICE$minimum_nights, main = "Medium Price" , col = "lightblue", xlab = "min nights")
hist(HIGHPRICE$minimum_nights, main = "High Price" , col = "lightblue", xlab = "min nights")

par(mfrow = c(3,3))
hist(LOWPRICE$neighbourhood_group_Manhattan, main = "Low Price" , col = "lightblue", xlab = "manhattan", yim=c(0,8000))
hist(MEDPRICE$neighbourhood_group_Manhattan, main = "Medium Price" , col = "lightblue", xlab = "manhattan", yim=c(0,8000))
hist(HIGHPRICE$neighbourhood_group_Manhattan, main = "High Price" , col = "lightblue", xlab = "manhattan", yim=c(0,8000))

hist(LOWPRICE$neighbourhood_group_Bronx, main = "Low Price" , col = "red", xlab = "Bronx")
hist(MEDPRICE$neighbourhood_group_Bronx, main = "Medium Price" , col = "red", xlab = "Bronx")
hist(HIGHPRICE$neighbourhood_group_Bronx, main = "High Price" , col = "red", xlab = "Bronx")

t.test(LOWPRICE$neighbourhood_group_Bronx,MEDPRICE$neighbourhood_group_Bronx)
t.test(LOWPRICE$neighbourhood_group_Bronx,HIGHPRICE$neighbourhood_group_Bronx)
t.test(MEDPRICE$neighbourhood_group_Bronx,HIGHPRICE$neighbourhood_group_Bronx)

t.test(LOWPRICE$`neighbourhood_group_Staten Island`,MEDPRICE$`neighbourhood_group_Staten Island`)
t.test(LOWPRICE$`neighbourhood_group_Staten Island`,HIGHPRICE$`neighbourhood_group_Staten Island`)
t.test(MEDPRICE$`neighbourhood_group_Staten Island`,HIGHPRICE$`neighbourhood_group_Staten Island`)

t.test(LOWPRICE$neighbourhood_group_Queens,MEDPRICE$neighbourhood_group_Queens)
t.test(LOWPRICE$neighbourhood_group_Queens,HIGHPRICE$neighbourhood_group_Queens)
t.test(MEDPRICE$neighbourhood_group_Queens,HIGHPRICE$neighbourhood_group_Queens)

hist(LOWPRICE$neighbourhood_group_Brooklyn, main = "Low Price" , col = "black", xlab = "brooklyn", ylim = c(0,8000))
hist(MEDPRICE$neighbourhood_group_Brooklyn, main = "Medium Price" , col = "black", xlab = "brooklyn", yim=c(0,8000))
hist(HIGHPRICE$neighbourhood_group_Brooklyn, main = "High Price" , col = "black", xlab = "brooklyn", yim=c(0,8000))

hist(LOWPRICE$`neighbourhood_group_Staten Island`, main = "Low Price" , col = "yellow", xlab = "staten")
hist(MEDPRICE$`neighbourhood_group_Staten Island`, main = "Medium Price" , col = "yellow", xlab = "staten")
hist(HIGHPRICE$`neighbourhood_group_Staten Island`, main = "High Price" , col = "yellow", xlab = "staten")

hist(LOWPRICE$neighbourhood_group_Queens, main = "Low Price" , col = "purple", xlab = "queens", yim=c(0,10000))
hist(MEDPRICE$neighbourhood_group_Queens, main = "Medium Price" , col = "purple", xlab = "queens", yim=c(0,10000))
hist(HIGHPRICE$neighbourhood_group_Queens, main = "High Price" , col = "purple", xlab = "queens", yim=c(0,10000))

##Room type
par(mfrow = c(3,3))
hist(LOWPRICE$`room_type_Entire home/apt`, main = "Low Price" , col = "purple", xlab = "entire home")
hist(MEDPRICE$`room_type_Entire home/apt`, main = "Medium Price" , col = "purple", xlab = "entire home")
hist(HIGHPRICE$`room_type_Entire home/apt`, main = "High Price" , col = "purple", xlab = "entire home")

hist(LOWPRICE$`room_type_Private room`, main = "Low Price" , col = "purple", xlab = "priv room")
hist(MEDPRICE$`room_type_Private room`, main = "Medium Price" , col = "purple", xlab = "priv room")
hist(HIGHPRICE$`room_type_Private room`, main = "High Price" , col = "purple", xlab = "priv room")

hist(LOWPRICE$`room_type_Shared room`, main = "Low Price" , col = "purple", xlab = "shared")
hist(MEDPRICE$`room_type_Shared room`, main = "Medium Price" , col = "purple", xlab = "shared")
hist(HIGHPRICE$`room_type_Shared room`, main = "High Price" , col = "purple", xlab = "shared")

t.test(LOWPRICE$`room_type_Shared room`,MEDPRICE$`room_type_Shared room`)
t.test(LOWPRICE$`room_type_Shared room`,HIGHPRICE$`room_type_Shared room`)
t.test(MEDPRICE$`room_type_Shared room`,HIGHPRICE$`room_type_Shared room`)

t.test(LOWPRICE$`room_type_Entire home/apt`,MEDPRICE$`room_type_Entire home/apt`)
t.test(LOWPRICE$`room_type_Entire home/apt`,HIGHPRICE$`room_type_Entire home/apt`)
t.test(MEDPRICE$`room_type_Entire home/apt`,HIGHPRICE$`room_type_Entire home/apt`)

t.test(LOWPRICE$`room_type_Private room`,MEDPRICE$`room_type_Private room`)
t.test(LOWPRICE$`room_type_Private room`,HIGHPRICE$`room_type_Private room`)
t.test(MEDPRICE$`room_type_Private room`,HIGHPRICE$`room_type_Private room`)

par(mfrow = c(1,3))
hist(LOWPRICE$latitude, main = "Low Price" , col = "lightblue", xlab = "latitude")
hist(MEDPRICE$latitude, main = "Medium Price" , col = "lightblue", xlab = "latitude")
hist(HIGHPRICE$latitude, main = "High Price" , col = "lightblue", xlab = "latitude")

t.test(LOWPRICE$latitude,MEDPRICE$latitude)
t.test(LOWPRICE$latitude,HIGHPRICE$latitude)
t.test(MEDPRICE$latitude,HIGHPRICE$latitude)

par(mfrow = c(1,3))
hist(LOWPRICE$longitude, main = "Low Price" , col = "lightblue", xlab = "longitude")
hist(MEDPRICE$longitude, main = "Medium Price" , col = "lightblue", xlab = "longitude")
hist(HIGHPRICE$longitude, main = "High Price" , col = "lightblue", xlab = "longitude")

##TRAIN/TEST SET: LOW
n<-nrow(LOWPRICE)
train<-sample(1:n, 0.8*n)
trainset_LOW <- LOWPRICE[train,]
testset_LOW <- LOWPRICE[-train,]

##TRAIN/TEST SET: MEDIUM
n<-nrow(MEDPRICE)
train<-sample(1:n, 0.8*n)
trainset_MED <- MEDPRICE[train,]
testset_MED <- MEDPRICE[-train,]

##TRAIN/TEST SET: HIGH
n<-nrow(HIGHPRICE)
train<-sample(1:n, 0.8*n)
trainset_HIGH <- HIGHPRICE[train,]
testset_HIGH <- HIGHPRICE[-train,]
#combined train set
TRAIN_SET = rbind(trainset_LOW,trainset_MED,trainset_HIGH)
TEST_SET = rbind(testset_LOW,testset_MED,testset_HIGH)

library(class)
TRAIN_predictor <- TRAIN_SET[,-c(16)]
TRAIN_label <- TRAIN_SET[, "target"]
TEST_predictor <- TEST_SET[,-c(16)]
TEST_label <- TEST_SET[,"target"]

TRAIN_predictor=as.data.frame(TRAIN_predictor)
TEST_predictor=as.data.frame(TEST_predictor)
TRAIN_label=as.data.frame(TRAIN_label)
TEST_label=as.data.frame(TEST_label)

K.set = c(5,10,15:35,50,80,101)
knn.train.accuracy <- numeric(length(K.set))
traincl = TRAIN_label[,1, drop = TRUE]
#train set knn
for (j in 1:length(K.set)){
  set.seed(1)
  knn.pred <- knn(train=TRAIN_predictor,
                  test=TRAIN_predictor,
                  cl=traincl,
                  k=K.set[j])
  knn.train.accuracy[j] <- mean(knn.pred == traincl)
}
knn.train.accuracy

#test set knn
knn.test.accuracy <- numeric(length(K.set))
cl = TRAIN_label[,1, drop = TRUE]
testcl = TEST_label[,1, drop = TRUE]
for (j in 1:length(K.set)){
  set.seed(1)
  knn.pred <- knn(train= TRAIN_predictor,
                  test=TEST_predictor,
                  cl= cl,
                  k=K.set[j])
  knn.test.accuracy[j] <- mean(knn.pred == testcl)
  
}
knn.test.accuracy

#plot for multiple k vs train and test
plot(K.set, knn.train.accuracy, type="o", col="blue", pch="o", lty=1, ylim=range( c(knn.train.accuracy,knn.test.accuracy) ) ) 
points(K.set, knn.test.accuracy, col="red", pch="*") 
lines(K.set, knn.test.accuracy, col="red",lty=2) 

##knn using bestk
knn.predtrain <- knn(train= data.frame(TRAIN_SET[,-c(16)]),
                    test=TRAIN_SET[,-c(16)],
                    cl= traincl,
                    k=21)

knn.predtest <- knn(train= TRAIN_SET[,-16],
                test=TEST_SET[,-16],
                cl= traincl,
                k=21)

#confusion matrix
conftrain<-table(data.frame(knn.predtrain,traincl))
conftest<-table(data.frame(knn.predtest,testcl))
library(DescTools)
#confidence intervals
Conf(conftrain)
Conf(conftest)
mean(knn.predtrain == traincl)
mean(knn.predtest == testcl)

#####STEP 4
#finding the weights for each feature using ks.test p value
#feature 1 neighbourhoodgroupbronx
ks.test(LOWPRICE$neighbourhood_group_Bronx,MEDPRICE$neighbourhood_group_Bronx)
#low vs medium PV12 value= 0.0001286
ks.test(LOWPRICE$neighbourhood_group_Bronx,HIGHPRICE$neighbourhood_group_Bronx)
#low vs high PV13, P VALUE = 0
ks.test(MEDPRICE$neighbourhood_group_Bronx,HIGHPRICE$neighbourhood_group_Bronx)
#medium vs high pv23 -> 0.2522
W1 = ((1-0.0001286)+(1-0)+(1-0.2522))/3

#feature 2 neighbourhoodgroupbrooklyn
ks.test(LOWPRICE$neighbourhood_group_Brooklyn,MEDPRICE$neighbourhood_group_Brooklyn)
#low vs medium PV12 value= 0
ks.test(LOWPRICE$neighbourhood_group_Brooklyn,HIGHPRICE$neighbourhood_group_Brooklyn)
#low vs high PV13, P VALUE = 0
ks.test(MEDPRICE$neighbourhood_group_Brooklyn,HIGHPRICE$neighbourhood_group_Brooklyn)
#medium vs high pv23 -> 0
W2 = ((1-0)+(1-0)+(1-0))/3

#feature 3 neighbourhoodgroupmanhattan
ks.test(LOWPRICE$neighbourhood_group_Manhattan,MEDPRICE$neighbourhood_group_Manhattan)
#low vs medium PV12 value= 0
ks.test(LOWPRICE$neighbourhood_group_Manhattan,HIGHPRICE$neighbourhood_group_Manhattan)
#low vs high PV13, P VALUE = 0
ks.test(MEDPRICE$neighbourhood_group_Manhattan,HIGHPRICE$neighbourhood_group_Manhattan)
#medium vs high pv23 -> 0
W3 = ((1-0)+(1-0)+(1-0))/3

#feature 4 neighbourhoodgroupqueens
ks.test(LOWPRICE$neighbourhood_group_Queens,MEDPRICE$neighbourhood_group_Queens)#low vs medium PV12 value= 0
ks.test(LOWPRICE$neighbourhood_group_Queens,HIGHPRICE$neighbourhood_group_Queens)#low vs high PV13, P VALUE = 0
ks.test(MEDPRICE$neighbourhood_group_Queens,HIGHPRICE$neighbourhood_group_Queens)#medium vs high pv23 -> 0
W4 = ((1-0)+(1-0)+(1-0))/3

#feature 5 neighbourhoodgroupstaten
ks.test(LOWPRICE$`neighbourhood_group_Staten Island`,MEDPRICE$`neighbourhood_group_Staten Island`)
#low vs medium PV12 value= 0.9195
ks.test(LOWPRICE$`neighbourhood_group_Staten Island`,HIGHPRICE$`neighbourhood_group_Staten Island`)
#low vs high PV13, P VALUE = 0.442
ks.test(MEDPRICE$`neighbourhood_group_Staten Island`,HIGHPRICE$`neighbourhood_group_Staten Island`)
#medium vs high pv23 -> 0.9999
W5 = ((1-.9195)+(1-.442)+(1-.9999))/3

#feature 6 latitude
ks.test(LOWPRICE$latitude,MEDPRICE$latitude)
#low vs medium PV12 value= 0
ks.test(LOWPRICE$latitude,HIGHPRICE$latitude)
#low vs high PV13, P VALUE = 0
ks.test(MEDPRICE$latitude,HIGHPRICE$latitude)
#medium vs high pv23 -> 0
W6 = ((1-0)+(1-0)+(1-0))/3

#feature 7 longitude
ks.test(LOWPRICE$longitude,MEDPRICE$longitude)#low vs medium PV12 value= 0
ks.test(LOWPRICE$longitude,HIGHPRICE$longitude)#low vs high PV13, P VALUE = 0
ks.test(MEDPRICE$longitude,HIGHPRICE$longitude)#medium vs high pv23 -> 0
W7 = ((1-0)+(1-0)+(1-0))/3

#feature 8 roomtype entire home
ks.test(LOWPRICE$`room_type_Entire home/apt`,MEDPRICE$`room_type_Entire home/apt`)
#low vs medium PV12 value= 0
ks.test(LOWPRICE$`room_type_Entire home/apt`,HIGHPRICE$`room_type_Entire home/apt`)
#low vs high PV13, P VALUE = 0
ks.test(MEDPRICE$`room_type_Entire home/apt`,HIGHPRICE$`room_type_Entire home/apt`)
#medium vs high pv23 -> 0
W8 = ((1-0)+(1-0)+(1-0))/3

#feature 9 roomtype priv
ks.test(LOWPRICE$`room_type_Private room`,MEDPRICE$`room_type_Private room`)
#low vs medium PV12 value= 0
ks.test(LOWPRICE$`room_type_Private room`,HIGHPRICE$`room_type_Private room`)
#low vs high PV13, P VALUE = 0
ks.test(MEDPRICE$`room_type_Private room`,HIGHPRICE$`room_type_Private room`)
#medium vs high pv23 -> 0
W9 = ((1-0)+(1-0)+(1-0))/3

#feature 10 roomtype shared
ks.test(LOWPRICE$`room_type_Shared room`,MEDPRICE$`room_type_Shared room`)
#low vs medium PV12 value= 0
ks.test(LOWPRICE$`room_type_Shared room`,HIGHPRICE$`room_type_Shared room`)
#low vs high PV13, P VALUE = 0
ks.test(MEDPRICE$`room_type_Shared room`,HIGHPRICE$`room_type_Shared room`)
#medium vs high pv23 -> 0.9985
W10 = ((1-0)+(1-0)+(1-.9985))/3

#feature 11 min nights
ks.test(LOWPRICE$minimum_nights,MEDPRICE$minimum_nights)#low vs medium PV12 value= 0
ks.test(LOWPRICE$minimum_nights,HIGHPRICE$minimum_nights)#low vs high PV13, P VALUE = 0
ks.test(MEDPRICE$minimum_nights,HIGHPRICE$minimum_nights)#medium vs high pv23 ->0
W11 = ((1-0)+(1-0)+(1-0))/3

#feature 12 number_of_reviews
ks.test(LOWPRICE$number_of_reviews,MEDPRICE$number_of_reviews)#low vs medium PV12 value= 0
ks.test(LOWPRICE$number_of_reviews,HIGHPRICE$number_of_reviews)#low vs high PV13, P VALUE = 0
ks.test(MEDPRICE$number_of_reviews,HIGHPRICE$number_of_reviews)#medium vs high pv23 -> 0
W12 = ((1-0)+(1-0)+(1-0))/3

#feature 13 reviews per month
ks.test(LOWPRICE$reviews_per_month,MEDPRICE$reviews_per_month)#low vs medium PV12 value= 0.007786
ks.test(LOWPRICE$reviews_per_month,HIGHPRICE$reviews_per_month)#low vs high PV13, P VALUE = 0
ks.test(MEDPRICE$reviews_per_month,HIGHPRICE$reviews_per_month)#medium vs high pv23 -> 0
W13 = ((1-0.007786)+(1-0)+(1-0))/3

#feature 14 calculated host listing count
ks.test(LOWPRICE$calculated_host_listings_count,MEDPRICE$calculated_host_listings_count)#low vs medium PV12 value= 0
ks.test(LOWPRICE$calculated_host_listings_count,HIGHPRICE$calculated_host_listings_count)#low vs high PV13, P VALUE = 0
ks.test(MEDPRICE$calculated_host_listings_count,HIGHPRICE$calculated_host_listings_count)#medium vs high pv23 -> 0
W14 = ((1-0)+(1-0)+(1-0))/3

#feature 15 availability 365
ks.test(LOWPRICE$availability_365,MEDPRICE$availability_365)#low vs medium PV12 value= 0
ks.test(LOWPRICE$availability_365,HIGHPRICE$availability_365)#low vs high PV13, P VALUE = 0
ks.test(MEDPRICE$availability_365,HIGHPRICE$availability_365)#medium vs high pv23 ->0
W15 = ((1-0)+(1-0)+(1-0))/3

#add the weights to original scaled data
SNYC_weighted = rbind(data.frame(W1*S_NYC1[,1],W2*S_NYC1[,2],W3*S_NYC1[,3],
                    W4*S_NYC1[,4],W5*S_NYC1[,5],W6*S_NYC1[,6],
                    W7*S_NYC1[,7],W8*S_NYC1[,8],W9*S_NYC1[,9],
                    W10*S_NYC1[,10],W11*S_NYC1[,11],W12*S_NYC1[,12],
                    W13*S_NYC1[,13],W14*S_NYC1[,14],W15*S_NYC1[,15],S_NYC1[,16]))

#rescale the weighted data
S_NYCWS<-SNYC_weighted%>%mutate_if(is.numeric, function (x) as.vector(scale(x)))

#split into train and test set
LOWPRICEw = S_NYCWS[which(S_NYCWS$target =="Low"),]
n<-nrow(LOWPRICEw)
train<-sample(1:n, 0.8*n)
trainset_LOW_w <- LOWPRICEw[train,]
testset_LOW_w <- LOWPRICEw[-train,]

##TRAIN/TEST SET: MEDIUM
MEDPRICEw = S_NYCWS[which(S_NYCWS$target =="Medium"),]
n<-nrow(MEDPRICEw)
train<-sample(1:n, 0.8*n)
trainset_MED_w <- MEDPRICEw[train,]
testset_MED_w <- MEDPRICEw[-train,]

##TRAIN/TEST SET: HIGH
HIGHPRICEw = S_NYCWS[which(S_NYCWS$target =="High"),]
n<-nrow(HIGHPRICEw)
train<-sample(1:n, 0.8*n)
trainset_HIGH_w <- HIGHPRICE[train,]
testset_HIGH_w <- HIGHPRICE[-train,]

#Train set weighted and test set weighted 
TRAIN_SETWS = rbind(trainset_LOW_w,trainset_MED_w,trainset_HIGH_w)
TEST_SETWS = rbind(testset_LOW_w,testset_MED_w,testset_HIGH_w)
#split into labels and predictors
TRAIN_predictor <- TRAIN_SETWS[,-c(16)]
TRAIN_label <- TRAIN_SETWS[, "target"]
TEST_predictor <- TEST_SETWS[,-c(16)]
TEST_label <- TEST_SETWS[,"target"]
TRAIN_predictor=as.data.frame(TRAIN_predictor)
TEST_predictor=as.data.frame(TEST_predictor)
TRAIN_label=as.data.frame(TRAIN_label)
TEST_label=as.data.frame(TEST_label)
cl = TRAIN_label[,1, drop = TRUE]
testcl = TEST_label[,1, drop = TRUE]

library(class)
#run the knn with best k on weighted standardized dataset
knn.predtrainWS <- knn(train= TRAIN_predictor,
                     test=TRAIN_predictor,
                     cl= cl,
                     k=21)

knn.predtestWS<- knn(train= TRAIN_predictor,
                    test=TEST_predictor ,
                    cl= cl,
                    k=21)
conftrainWS<-table(data.frame(knn.predtrainWS,cl))
conftestWS<-table(data.frame(knn.predtestWS,testcl))
mean(knn.predtrainWS == cl)
mean(knn.predtestWS == testcl)

#confidence intervals
library(DescTools)
Conf(conftrainWS)
Conf(conftestWS)
#weighted knn multiple k values to obtain the plot
K.set = c(5,10,15,20:25,30,40,50,80,100)
knn.train.accuracy <- numeric(length(K.set))
traincl = TRAIN_label[,1, drop = TRUE]
for (j in 1:length(K.set)){
  set.seed(1)
  knn.predWS <- knn(train=TRAIN_predictor,
                  test=TRAIN_predictor,
                  cl=traincl,
                  k=K.set[j])
  knn.train.accuracy[j] <- mean(knn.predWS == traincl)
}
knn.train.accuracy
#test set multiple values
knn.test.accuracy <- numeric(length(K.set))

cl = TRAIN_label[,1, drop = TRUE]
testcl = TEST_label[,1, drop = TRUE]

for (j in 1:length(K.set)){
  set.seed(1)
  knn.predWStest <- knn(train= TRAIN_predictor,
                  test=TEST_predictor,
                  cl= cl,
                  k=K.set[j])
  knn.test.accuracy[j] <- mean(knn.predWStest == testcl)
  
}
knn.test.accuracy

#plot the K values versus the train and test set accuracy
plot(K.set, knn.train.accuracy, type="o", col="blue", pch="o", lty=1, ylim=range( c(knn.train.accuracy,knn.test.accuracy) ) ) 
points(K.set, knn.test.accuracy, col="red", pch="*") 
lines(K.set, knn.test.accuracy, col="red",lty=2) 
###investigating K>80 since the training set performs better than the testing set
K.set = c(80:90,100,105:110)
knn.train.accuracy <- numeric(length(K.set))
traincl = TRAIN_label[,1, drop = TRUE]
for (j in 1:length(K.set)){
  set.seed(1)
  knn.predWS <- knn(train=TRAIN_predictor,
                    test=TRAIN_predictor,
                    cl=traincl,
                    k=K.set[j])
  knn.train.accuracy[j] <- mean(knn.predWS == traincl)
}
knn.train.accuracy
#test
knn.test.accuracy <- numeric(length(K.set))

cl = TRAIN_label[,1, drop = TRUE]
testcl = TEST_label[,1, drop = TRUE]

for (j in 1:length(K.set)){
  set.seed(1)
  knn.predWStest <- knn(train= TRAIN_predictor,
                        test=TEST_predictor,
                        cl= cl,
                        k=K.set[j])
  knn.test.accuracy[j] <- mean(knn.predWStest == testcl)
  
}
knn.test.accuracy
#plot with 80<k<110(weighted knn)
plot(K.set, knn.train.accuracy, type="o", col="blue", pch="o", lty=1, ylim=range( c(knn.train.accuracy,knn.test.accuracy) ) ) 
points(K.set, knn.test.accuracy, col="red", pch="*") 
lines(K.set, knn.test.accuracy, col="red",lty=2) 

##Individual investigation
#make a train test set with only low price and high price

TRAINLOWHIGHp=as.data.frame(rbind(trainset_LOW[,-16],trainset_HIGH[,-16]))
TESTLOWHIGHp=as.data.frame(rbind(testset_LOW[,-16],testset_HIGH[,-16]))
TRAINLOWHIGHlab=as.data.frame(rbind(trainset_LOW[,16],trainset_HIGH[,16]))
TESTLOWHIGHlab=as.data.frame(rbind(testset_LOW[,16],testset_HIGH[,16]))
cl1 = TRAINLOWHIGHlab[,1, drop = TRUE]
testcl1 = TESTLOWHIGHlab[,1, drop = TRUE]

knn.predtrain <- knn(train= TRAINLOWHIGHp,
                     test=TRAINLOWHIGHp,
                     cl= cl1,
                     k=21)

knn.predtest <- knn(train= TRAINLOWHIGHp,
                    test=TESTLOWHIGHp,
                    cl= cl1,
                    k=21)

conftrain1<-table(data.frame(knn.predtrain,cl1))
conftest1<-table(data.frame(knn.predtest,testcl1))
mean(knn.predtrain == cl1)
mean(knn.predtest == testcl1)  #91%


#################Med vs high

TRAINMEDHIGHp=as.data.frame(rbind(trainset_MED[,-16],trainset_HIGH[,-16]))
TESTMEDHIGHp=as.data.frame(rbind(testset_MED[,-16],testset_HIGH[,-16]))

TRAINMEDHIGHlab=as.data.frame(rbind(trainset_MED[,16],trainset_HIGH[,16]))
TESTMEDHIGHlab=as.data.frame(rbind(testset_MED[,16],testset_HIGH[,16]))



cl2 = TRAINMEDHIGHlab[,1, drop = TRUE]
testcl2 = TESTMEDHIGHlab[,1, drop = TRUE]


knn.predtrain2 <- knn(train= TRAINMEDHIGHp,
                      test=TRAINMEDHIGHp,
                      cl= cl2,
                      k=21)

knn.predtest2 <- knn(train= TRAINMEDHIGHp,
                     test=TESTMEDHIGHp,
                     cl= cl2,
                     k=21)

conftrain2<-table(data.frame(knn.predtrain2,cl2))
conftest2<-table(data.frame(knn.predtest2,testcl2))
mean(knn.predtrain2 == cl2)
mean(knn.predtest2 == testcl2)

###
#################LOW vs Med

TRAINLOWMEDp=as.data.frame(rbind(trainset_LOW[,-16],trainset_MED[,-16]))
TESTLOWMEDp=as.data.frame(rbind(testset_LOW[,-16],testset_MED[,-16]))

TRAINLOWMEDlab=as.data.frame(rbind(trainset_LOW[,16],trainset_MED[,16]))
TESTLOWMEDlab=as.data.frame(rbind(testset_LOW[,16],testset_MED[,16]))


cl3 = TRAINLOWMEDlab[,1, drop = TRUE]
testcl3 = TESTLOWMEDlab[,1, drop = TRUE]


knn.predtrain3 <- knn(train= TRAINLOWMEDp,
                      test=TRAINLOWMEDp,
                      cl= cl3,
                      k=21)

knn.predtest3 <- knn(train= TRAINLOWMEDp,
                     test=TESTLOWMEDp,
                     cl= cl3,
                     k=21)

conftrain3<-table(data.frame(knn.predtrain3,cl3))
conftest3<-table(data.frame(knn.predtest3,testcl3))
mean(knn.predtrain3 == cl3)
mean(knn.predtest3 == testcl3)

