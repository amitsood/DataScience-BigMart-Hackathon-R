setwd("~/Documents/Analytics/AnalyticsVidhya/bigmart")

train= read.csv("Train.csv", stringsAsFactors = FALSE, header = TRUE)
test = read.csv("Test.csv", stringsAsFactors = FALSE, header = TRUE)
test$Item_Outlet_Sales=0
train$which='train'
test$which='test'

alldata = rbind(train,test)
str(alldata)

summary(alldata$Item_Weight)
hist(alldata$Item_Weight, breaks=4, col="red")


#Item_Identifier
alldata$Item_Identifier

#Item_Weight
summary(alldata$Item_Weight)


#alldata$Item_Weight[is.na(alldata$Item_Weight)] = 12.60


library(plyr)
library(dplyr)
itemizedWeightsByIdentifier <- as.data.frame( ddply(na.omit(alldata), 
                                                    ~Item_Identifier, 
                                                    summarise, 
                                                    mean=mean(Item_Weight), 
                                                    sd=sd(Item_Weight)))


# we can now use these values to fill in the missing weight values:
alldata$Item_Weight <- ifelse(is.na(alldata$Item_Weight), 
                              itemizedWeightsByIdentifier$mean[
                                match(alldata$Item_Identifier, itemizedWeightsByIdentifier$Item_Identifier)], alldata$Item_Weight)


#Item_Fat_Content
table(alldata$Item_Fat_Content, alldata$Item_Type)


alldata$Item_Fat_Content[which(alldata$Item_Fat_Content == 'LF')] ="Low Fat"
alldata$Item_Fat_Content[which(alldata$Item_Fat_Content == 'low fat')] ="Low Fat"
alldata$Item_Fat_Content[which(alldata$Item_Fat_Content == 'Low Fat')] ="Low Fat"
alldata$Item_Fat_Content[which(alldata$Item_Fat_Content == 'reg')] ="Regular"


#Item_Visibility
summary(alldata$Item_Visibility)
alldata$Item_Visibility[(alldata$Item_Visibility == 0 )]=NA 
ZeroItem_Visibility=alldata$Item_Identifier[is.na(alldata$Item_Visibility)]

#checking mean item weight by each item type
itemvisibility= aggregate( alldata$Item_Visibility ~ alldata$Item_Identifier, alldata, mean )

for(index in 1:879){ 
  valu=ZeroItem_Visibility[index]
  alldata$Item_Visibility[(is.na(alldata$Item_Visibility) & alldata$Item_Identifier ==valu)]=itemvisibility$`alldata$Item_Visibility`[ itemvisibility$`alldata$Item_Identifier` ==valu]
}

#alldata$Item_Visibility=log(alldata$Item_Visibility)


#str(alldata)
#Item_Type

table(alldata$Item_Type)
#alldata$Item_Type[which(alldata$Item_Type == 'Seafood')] ="Meat"
#alldata$Item_Type[which(alldata$Item_Type == 'Breads')] ="Breakfast"
#alldata$Item_Type[which(alldata$Item_Type == 'Canned')] ="Frozen Foods"
#alldata$Item_Type[which(alldata$Item_Type == 'Starchy Foods')] ="Breakfast"


#Item_MRP
summary(alldata$Item_MRP)
hist(alldata$Item_MRP)

#Outlet_Identifier
summary(alldata$Outlet_Identifier)

#Outlet_Establishment_Year
summary(alldata$Outlet_Establishment_Year)
alldata$Outlet_Establishment_Year= 2016-alldata$Outlet_Establishment_Year
str(alldata)

#Outlet_Size
table(alldata$Outlet_Size)
table(alldata$Outlet_Size, alldata$Outlet_Type)

length(alldata$Outlet_Size[alldata$Outlet_Size == "" & alldata$Outlet_Type =="Grocery Store"])
alldata$Outlet_Size[alldata$Outlet_Size == "" & alldata$Outlet_Type =="Grocery Store"] = "Small"

table(alldata$Outlet_Size, alldata$Outlet_Location_Type)
alldata$Outlet_Size[alldata$Outlet_Size == "" ] = "Small"




#FACTOR
alldata$Item_Fat_Content = as.factor(alldata$Item_Fat_Content)
alldata$Outlet_Size= as.factor(alldata$Outlet_Size)
alldata$Outlet_Type= as.factor(alldata$Outlet_Type)
alldata$which= as.factor(alldata$which)
alldata$Item_Type = as.factor(alldata$Item_Type)
alldata$Outlet_Identifier = as.factor(alldata$Outlet_Identifier)
alldata$Outlet_Location_Type = as.factor(alldata$Outlet_Location_Type)


alldata$Volume="low"
alldata$Volume[which(alldata$Outlet_Type == 'Grocery')] ="low"
alldata$Volume[which(alldata$Outlet_Type == 'Supermarket Type1')] ="mid"
alldata$Volume[which(alldata$Outlet_Type == 'Supermarket Type2')] ="mid"
alldata$Volume[which(alldata$Outlet_Type == 'Supermarket Type3')] ="high"

alldata$Volume= as.factor(alldata$Volume)

a=data.frame(alldata$Item_Weight, bin=cut(alldata$Item_Weight, c(1,10,15,30), labels =c("low", "middle", "high"),  include.lowest=TRUE))
alldata$Item_Weight = a$bin

summary(alldata$Outlet_Establishment_Year)
#b=data.frame(alldata$Item_Visibility, bin=cut(alldata$Item_Visibility, c(0,.1,.2,.4), labels =c("low", "middle", "high"),  include.lowest=TRUE))
#alldata$Item_Visibility = b$bin

#hist(alldata$Outlet_Establishment_Year, breaks=7, col="red")
#b=data.frame(alldata$Outlet_Establishment_Year, bin=cut(alldata$Outlet_Establishment_Year, c(1,5,10,15,20,25,30,35), labels =c("1", "2", "3","4","5","6","7"),  include.lowest=TRUE))
#alldata$Outlet_Establishment_Year = b$bin



alldata$Item_Weight = as.factor(alldata$Item_Weight)
#alldata$Outlet_Establishment_Year = as.factor(alldata$Outlet_Establishment_Year)

table(is.na(alldata))
colSums(is.na(alldata))
library(MASS)
library(car)
#str(alldata)
#scatterplotMatrix(alldata[,c(2,4,6,8,13)])



require("ggplot2")
# Basic scatterplot
p1 <- ggplot(alldata, aes(x = alldata$Item_Identifier, y = alldata$Item_Outlet_Sales))
p1 + geom_point()
p2 <- p1 + geom_point(color="red")            #set one color for all points
p2
p3 <- p1 + geom_point(aes(color = alldata$Item_Fat_Content))        #set color scale by a continuous variable
p3


ggplot(alldata, aes(x = alldata$Outlet_Location_Type, y = alldata$Item_Outlet_Sales)) + geom_line(size=2, aes(color=factor(alldata$Outlet_Location_Type)))

train = alldata[which(alldata$which=='train'),]
test = alldata[which(alldata$which=='test'),]

train= train[,c(-13)]
test= test[,c(-13)]



dump=train$Item_Outlet_Sales

train$Items_Sold = train$Item_Outlet_Sales / train$Item_MRP
train$Items_Sold = round(train$Items_Sold)
train$Item_Outlet_Sales = NULL

######Baseline model

# Determine mean of the output column:
#Item_Outlet_Sales = mean(train$Item_Outlet_Sales)
#Item_Identifier = test$Item_Identifier
#Outlet_Identifier= test$Outlet_Identifier
#submission = data.frame(Item_Identifier,Outlet_Identifier,Item_Outlet_Sales )
#write.csv(submission, file = "submission1.csv" )


#Regression Model1
#names(train)
#model.full = lm(train$Item_Outlet_Sales ~ . -Item_Identifier
#                , data = train) 
#summary(model.full)

#predicted=predict(model.full, newdata = train)
#RMSE of 1019.095
#sqrt(mean((predicted-train$Item_Outlet_Sales)^2 , na.rm = TRUE ))



#install.packages("MASS")
#library(MASS)
#step=stepAIC(model.full, direction="both")
#model.stepwise = lm(train$Item_Outlet_Sales ~ Item_Fat_Content+Item_MRP+Outlet_Identifier
#                    , data = train) 
#summary(model.stepwise)

#library(car)
#durbinWatsonTest(model.stepwise)


# Tree based modelling

#library(rpart)
#fit <- rpart(train$Item_Outlet_Sales ~ ., data = train,method="anova")
#summary(fit)
#predicted=predict(fit,train)

#sqrt( mean( (predicted-train$Item_Outlet_Sales)^2 , na.rm = TRUE ) )
#summary(test)
#Item_Outlet_Sales= predict(fit,test)
#Item_Identifier = test$Item_Identifier
#Outlet_Identifier= test$Outlet_Identifier

#submission = data.frame(Item_Identifier,Outlet_Identifier,Item_Outlet_Sales)
#write.csv(submission, file = "submissiona.csv" )



#RandomForest
#install.packages("randomForest")
library(randomForest)


fit <- randomForest(train$Items_Sold ~ .-Item_Identifier, data = train,ntree=300)
predicted=predict(fit,train)
predicted= round(predicted*train$Item_MRP)
train$Item_Outlet_Sales=dump
#RMSE
sqrt( mean( (predicted-train$Item_Outlet_Sales)^2 , na.rm = TRUE ) )


test$Items_Sold=0

Item_Outlet_Sales= predict(fit,test)
Item_Outlet_Sales=round(Item_Outlet_Sales* test$Item_MRP)
Item_Identifier = test$Item_Identifier
Outlet_Identifier= test$Outlet_Identifier

submission = data.frame(Item_Identifier,Outlet_Identifier,Item_Outlet_Sales)
write.csv(submission, file = "PredictedSales.csv", row.names = F )
