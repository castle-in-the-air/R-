library('randomForest')
library('mice')


train<-read.csv('D:/study R/Kaggle/train.csv', header =TRUE, sep=",")
test<-read.csv('D:/study R/Kaggle/test.csv', header =TRUE, sep=",")
dim(train)
dim(test)
colnames (train)
colnames (test)

full<-bind_rows(train,test) # combine two csv files

str(full) # read the structure of "full"
head(full)
tail(full)
summary(full)
#Survived: Survived(1) or died(0), factor
#Pclass: cabin's class:1,2,3.
#        unique(full$Pclass)——>3 1 2
#        factor(unique(full$Pclass))——> levels:1 2 3
#        summary(as.factor(full$Pclass)) ——> no missing value
#Name: which(is.na(full$Name))——> no missing value
#Sex: factor(full$Sex)——> 2 levels:famel,male.   no missing value
#Age: ——> 263 missing value
#SibSp: the number of siblings and spouses
#       summary(as.factor(full$SibSp))——> no missing value
#Parch: the number of parents and children
#       summary(as.factor(full$Parch))——> no missing value
#Ticket: which(is.na(full$Name))——> no missing value
#Fare: ——> 1 missing value
#Cabin: summary(as.factor(full$Cabin))——> 1014 missing value
#Embarked: The port of embarkation. C=Cherbourg, Q=Queentown, S=Southampton.
#          factor(unique(full$Embarked))——> levels: c Q S
#          unique(full$Embarked)——> "S" "C" "Q" "" ——> 2 missing value

#1
full$Survived<-as.factor(full$Survived)
#2
full$Pclass<-as.factor(full$Pclass)
#3
full$Sex<-as.factor(full$Sex)
#4
full$SibSp<-as.numeric(full$SibSp)
#5
full$Parch<-as.numeric(full$Parch)

#6: fare, numeric, 1missing value.
full$Fare<-as.numeric(full$Fare)
which(is.na(full$Fare)) #1044
#Step 1: find the position of missing value
full$Fare[1044]  #NA

full[which(is.na(full$Fare)),c("Pclass","Embarked")]
#     Pclass Embarked
#1044      3        S
#Step 2: find the Pclass and Embarked of the missing value

fare_3S<-full[which(full$Pclass=="3" & full$Embarked=="S"),"Fare"]
#Step 3: find all the passengers' fare with Pclass=3, Embarked=S.

mean(fare_3S) #NA
mean(fare_3S,na.rm = TRUE) #14.43542
median(fare_3S,na.rm=TRUE) #8.05
hist(fare_3S) # after hist function, choose median value.
full$Fare[1044]<-median(fare_3S,na.rm=TRUE)


#7 Embarked
unique(full$Embarked)
which(full$Embarked=="") #62 830
#step 1: find the position of missing value

full[which(full$Embarked==""),c("Pclass","Fare")]
#      Pclass Fare
#62       1   80
#830      1   80
#step 2: find the Pclass and Fare value of 62 and 830, both Pclass=1,Fare=80.

median(full[which(full$Pclass=="1" & full$Embarked=="S"),"Fare"]) #52
median(full[which(full$Pclass=="1" & full$Embarked=="C"),"Fare"]) #76.7292
median(full[which(full$Pclass=="1" & full$Embarked=="Q"),"Fare"]) #90
# step 3: calculate the median value of Pclass=1,Embarked = S,C,Q respectivly,
# when embarked=C,median fare=76, the nearest to 80, choose "C" input the missing value

full$Embarked[c(62,830)]<-"C"
full$Embarked<-as.factor(full$Embarked)


#8 Cabin: 1014 missing value.
#         the percentage of missing data: 1014/1309=77.46% ——> drop this feature.
full$Cabin<-NULL
#drops<-c("Cabin")

#9 Name
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Title)
# Capt          Col          Don         Dona           Dr     Jonkheer 
#1            4            1            1            8            1 
#Lady        Major       Master         Miss         Mlle          Mme 
#1            2           61          260            2            1 
#Mr          Mrs           Ms          Rev          Sir the Countess 
#757          197            2            8            1            1

# Reassign rare titles
officer <- c('Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev')
royalty <- c('Dona', 'Lady', 'the Countess','Sir', 'Jonkheer')
# Reassign mlle, ms, and mme, and rare
full$Title[full$Title == 'Mlle']<- 'Miss' 
full$Title[full$Title == 'Ms']<- 'Miss'
full$Title[full$Title == 'Mme']<- 'Mrs' 
full$Title[full$Title %in% royalty] <- 'Royalty'
full$Title[full$Title %in% officer] <- 'Officer'

table(full$Title)
# Master    Miss      Mr     Mrs    Officer  Royalty 
#   61      264      757     198       24       5 

full$Title<-as.factor(full$Title)


#10 Age, 263 missing value
# 263/1309=20.1%
md.pattern(full)
tempFull<-mice(full[,!names(full) %in% c('PassengerID','Ticket','Survived','Name')],method="pmm")
summary(tempFull)
tempFull$imp$Age
completeFull<-complete(tempFull)

par(mfrow=c(1,2))
hist(full$Age,freq = F,main='Age: Original Data',col='lightGreen',ylim = c(0,0.04))
hist(full$Age,freq = F,main='Age: Mice Output',col='lightBlue',ylim = c(0,0.04))
par(mfrow=c(1,1))
full$Age=completeFull$Age

#11 Ticket
table(full$Ticket)
head(full$Ticket)
ticket_freq<-as.data.frame(table(full$Ticket))
colnames(ticket_freq)<-c("Ticket","Ticket_Frequency")
head(ticket_freq)
full<-merge(full,ticket_freq,by="Ticket")
head(full)

full_final<-full[with(full,order(PassengerId)),]
head(full_final)

####Modle
trainData<-full_final[1:891, ]
testData<-full_final[892:1309, ]
head(trainData)
head(testData)

## randomForest Model
set.seed(666)
rf_fit<-randomForest(factor(Survived) ~ Pclass + Sex + Age +SibSp + Parch + Fare + Embarked + Title + Ticket_Frequency, data = trainData,importance=TRUE)
print(rf_fit)


##feature importance
varImpPlot(rf_fit)
rf_fit_imp<-importance(rf_fit)
print(rf_fit_imp)

## predict on the test data
test_pred<-predict(rf_fit,testData)

plot(rf_fit, ylim=c(0,0.36))
legend('topright',colnames(rf_fit$err.rate), col=1:3, fill=1:3)


write.csv(data.frame(PassengerID = testData$PassengerId,Survived = test_pred), file='D:/study R/Kaggle/pre.csv',row.names = F)
