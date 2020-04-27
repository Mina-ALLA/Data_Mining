

######## Part 1-CLeaning Data: Acquire Data ######
##

#extraction of data
mydata=data.frame(read.csv("c:/Users/Marwa/Downloads/Data_mining_INPT_Patie1/employee_retention_data.csv"))
#let's take a look at our data
View(mydata)
# let's uniforme colomn names to lower
new_names <- tolower(names(mydata))
names(mydata)=new_names
#Verificate the type of each column
str(mydata)
class(mydata$employee_id)
class(mydata$company_id)
class(mydata$dept)
class(mydata$seniority)
class(mydata$salary)
class(mydata$join_date)
class(mydata$quit_date)
class(mydata$churn)
class(mydata$duration_jours)
#Transforme join_date and quit_date to dates, churn into caracter and salary into float

mydata$join_date=as.Date(mydata$join_date)
mydata$quit_date=as.Date(mydata$quit_date)
mydata$churn=as.character(mydata$churn)
mydata$salary=as.double(mydata$salary)
len=length(mydata$employee_id)
#Eliminate missing values in salary column
install.packages('tidyr')
library(tidyr)
mydata %>% drop_na(salary)


##
############ Part2-Describe data to Analyze ########
##

# basic EDA
dim(AchatClients)
#Column number 
ncol(mydata)
#row number
nrow(mydata)


summary(mydata)

##Suppose, for each company, that the workforce starts at zero on 01/23/2011.
#What is the estimate of the workforce, for each company, each day, from 24/01/2011 to 13/12/2015ee)
    
    #create the data frame that we will use (initially empty)
estimate_employee <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("comp_id", "join_daate", "count_employee")
colnames(estimate_employee) <- x
View(estimate_employee)
    #define the columns's data types
estimate_employee$comp_id=as.integer(estimate_employee$comp_id)
estimate_employee$join_daate=as.Date(estimate_employee$join_daate)
estimate_employee$count_employee=as.integer(estimate_employee$count_employee)

    #create date vectors from "2011-01-24" to "2015-12-13"
dates=seq(as.Date("2011-01-24"), as.Date("2015-12-13"),"day")
View(dates)

len_dates=length(dates)

    #browse our database and extract the number of employees in each company
    #each day and add it to our database estimate_employee

for (j in 1:len_dates) {
  for (id in 1:12) {
    s=0
    for (i in 1:len) {
      
      if(mydata$company_id[i]==id){
        if(mydata$join_date[i]<=dates[j]){
          if(mydata$churn[i]=='No'){
            s=s+1
          }
          else{
            if(mydata$quit_date[i]>dates[j]){
              s=s+1
            }
          }
        }
      }
      
    }
    comp_id=as.integer(c(id))
    join_daate=as.Date(c(dates[j]))
    count_employee=as.integer(c(s))
    df=data.frame(comp_id, join_daate, count_employee)
    estimate_employee=rbind.data.frame(estimate_employee, df)
    
    
  }
}


#now we want to add two columns to our data frame:
#the first contains the number of employees leaving each company each day
#the second clone contains the number of new employees of each company each day

    
  # we start by creating two new data frames containing a column each
len_est=length(estimate_employee$comp_id)
a <- data.frame(matrix(ncol = 1, nrow = 0))
x <- c("count_new_hire")
colnames(a) <- x

b <- data.frame(matrix(ncol = 1, nrow = 0))
x <- c("count_churn")
colnames(b) <- x

  #the 12 companies have no new employees leaving so the first 12 lines will contain 0

for (i in 1:12) {
  

  count_new_hire=as.integer(c(0))
  df=data.frame(count_new_hire)
  a=rbind.data.frame(a, df)

  count_churn=as.integer(c(0))
  df=data.frame(count_churn)
  b=rbind.data.frame(b, df)
}

  #we browse the data frame "estimate employee" to fill in the two new data frames

for (i in 1:(len_est-12)) {
  n=0
  ch=0
  if((estimate_employee$count_employee[i+12]-estimate_employee$count_employee[i])<0){
    ch=-(estimate_employee$count_employee[i+12]-estimate_employee$count_employee[i])
  }
  else{
    n=estimate_employee$count_employee[i+12]-estimate_employee$count_employee[i]
  }
  
  count_new_hire=as.integer(c(n))
  df=data.frame(count_new_hire)
  a=rbind.data.frame(a, df)
  
  count_churn=as.integer(c(ch))
  df=data.frame(count_churn)
  b=rbind.data.frame(b, df)
}


    #==>
estimate_employee=cbind.data.frame(estimate_employee, a)
estimate_employee=cbind.data.frame(estimate_employee, b)



#Plot the churn / salary distribution in a boxplot

boxplot(mydata$salary~mydata$churn, names=c("No","yes"),col=c("blue","red"),main="Wages distribution ")

#track new employees and employees who left their companies by day

plot(estimate_employee$count_new_hire)

plot(estimate_employee$count_churn)




##
############## Part3- Classification: ############

# Create a decision tree to classify contributors according to churn (Yes or No)
#with as input attributes seniority, salary

  
  #Installing libraries
install.packages('rpart')
install.packages('caret')
install.packages('rpart.plot')
install.packages('rattle')

  #Loading libraries
library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)
library(rattle)

  #data splicing

set.seed(12345)
train <- sample(1:nrow(mydata),size = ceiling(0.80*nrow(mydata)),replace = FALSE)
  # training set
mydata_train <- mydata[train,]
  # test set
mydata_test <- mydata[-train,]

  # building the classification tree with rpart
tree <- rpart(churn~seniority+salary,
              data=mydata_train,
              method = "class")
  # Visualize the decision tree with rpart.plot

plot(tree, main="prediction")
text(tree, use.n=T)
printcp(tree)

#===>we see that the variable seniority has been eliminated. it is not as significant
#as the salary variable for the classification of the churn variable

print(tree)

  # Visualize the decision tree with rpart.plot
rpart.plot(tree, nn=TRUE)

  #Testing the model
predi <- predict(object=tree,mydata_test[-1],type="class")
mc<-table(mydata_test$churn,predi)
mc
  #let s see the accuracy of our model
confusionMatrix(mc)

summary(mc)



####### KNN

mydata.subset=mydata[c("churn","seniority","salary")]
str(mydata.subset)

#Normalization
  #ceate the normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

  #apply this function to the salary and seniority columns of our
  #database and save it to a new database mydata.subset.n

mydata.subset.n <- as.data.frame(lapply(mydata.subset[,2:3], normalize))
  #splitting the data set into training and testing data set
set.seed(99345)
  #random selection of 70% data.
dat.d <- sample(1:nrow(mydata.subset.n),size=nrow(mydata.subset.n)*0.7,replace = FALSE) 

train.mydata <- mydata.subset.n[dat.d,] # 70% training data
test.mydata <- mydata.subset.n[-dat.d,] # remaining 30% test data

  #Creating seperate dataframe for 'churn' feature which is our target.
train.mydata_labels <- mydata.subset[dat.d,1]
test.mydata_labels <-mydata.subset[-dat.d,1]

### creating our model
library(class)
  #Find the number of observation in our training data set
NROW(train.mydata_labels) 
  
###===>
#So we have 17,291 observations in our training dataset.
#The square root of 17291 is around 131.49, therefore we will create two models.
# One with a value of "K" of 131 and the other model with a value of "K" of 132.

knn.131 <- knn(train=train.mydata, test=test.mydata, cl=train.mydata_labels, k=13)
knn.132 <- knn(train=train.mydata, test=test.mydata, cl=train.mydata_labels, k=132)


  #Calculate the proportion of correct classification for k = 131, 132
ACC.131 <- 100 * sum(test.mydata_labels == knn.131)/NROW(test.mydata_labels)
ACC.132<- 100 * sum(test.mydata_labels == knn.132)/NROW(test.mydata_labels)
ACC.131
ACC.132

  # Check prediction against actual value in tabular form for k=132
table(knn.132 ,test.mydata_labels)


### k value Optimization

 #We perform several knn models with k values ranging from 1 to 133 and we
 #choose the value of k which gives the maximum of the occurance

i=1
k.optm=1
for (i in 1:133){
   knn.mod <- knn(train=train.mydata, test=test.mydata, cl=train.mydata_labels, k=i)
   k.optm[i] <- 100 * sum(test.mydata_labels == knn.mod)/NROW(test.mydata_labels)
   k=i
   cat(k,'=',k.optm[i],'
  ')
}
  #Accuracy plot
plot(k.optm, type="l", xlab="K- Value",ylab="Accuracy level")

      ###===> The "k" best value is  127

knn.127 <- knn(train=train.mydata, test=test.mydata, cl=train.mydata_labels, k=127)
ACC.127<- 100 * sum(test.mydata_labels == knn.127)/NROW(test.mydata_labels)
ACC.127



##
####### #Part4-Prediction:########
##

#perform a linear regression to predict the duration
#in days based on the input attributes seniority, salary

duration_reg=lm(duration_jours ~ seniority + salary , data=mydata)

##see the coefficients and the characteristics of our model
coef(duration_reg)
summary(duration_reg)

#the estimated values for our data
q=fitted(duration_reg)
View(q)


#Predict duration in day for employee x salary = 9800 and seniority = 25
predict(duration_reg, newdata=data.frame(seniority=25,salary=9800), se.fit=TRUE, interval = "prediction", level = 0.99)

    #====> the estimated value: 527.12 days
