#--------------------------#
#  Ex 1 RTI Application    #
#                          #
#                          #
#     Sarah Mohamed        #
#--------------------------#


file.dir <- "C:/RTI/"
input.file.csv <- "records_flat.csv"

Records <- read.csv(file=paste(file.dir,input.file.csv,sep=""), header=TRUE,row.names=1)

#Checking for duplicates in ID category, no duplicates found.
#This concludes that assumption of independence holds for logistic regression in analysis
#and there is no duplicate entry data cleaning to be performed.
duplicated(Records$id)

#Exploring histograms for each variable to determine if there are any
#extremely skewed variables or outliers
library(ggplot2)

hist(Records$age)
hist(Records$education_num)
hist(Records$capital_gain)#92% zeros, skewed
hist(Records$capital_loss)#95% zeros, skewed
hist(Records$hours_week)
hist(Records$over_50k) #24% make over 50K (1), 76% make less than or equal to 50k (0)

ggp <- ggplot(data.frame(Records),aes(x=Workclass))
ggp + stat_count(fill="lightgreen")
#69% work in Private sector

ggp <- ggplot(data.frame(Records),aes(x=Education_level))
ggp + stat_count(fill="lightgreen")
#about half HS grad or some college

ggp <- ggplot(data.frame(Records),aes(x=Marital_Status))
ggp + stat_count(fill="lightgreen")
#about half married

ggp <- ggplot(data.frame(Records),aes(x=Occupation))
ggp + stat_count(fill="lightgreen")

ggp <- ggplot(data.frame(Records),aes(x=Relationship))
ggp + stat_count(fill="lightgreen")
#about half are husband

ggp <- ggplot(data.frame(Records),aes(x=Race))
ggp + stat_count(fill="lightgreen")
#Mostly White (41762/48841 = 86%)

ggp <- ggplot(data.frame(Records),aes(x=Sex))
ggp + stat_count(fill="lightgreen")
#roughly twice as many male as female

ggp <- ggplot(data.frame(Records),aes(x=Country))
ggp + stat_count(fill="lightgreen")


# Looking for missing values or values with "?".
# There are no missing values and variables with the "?" value 
# (Workclass, occupation, country) are not significant enough to exclude variables.
# Instead, the missing values will be their own category in the analysis.
sapply(Records,function(x) sum(is.na(x)))
sapply(Records,function(x) NROW(which(x == "?")))


#Making continuous variables into categorical (binned) variables.
#This is done so that all variables can be considered for the model without
#being cornered if continuous variables follow the assumption of linearity of the logit.
Records$age_bin <- as.factor(cut(Records$age,  breaks = seq(15, 90, by = 15)))
Records$edu_num_bin <- as.factor(cut(Records$education_num, breaks = seq(1, 16, by = 4)))
Records$capital_gain_bin <- as.factor(cut(Records$capital_gain, 2))
Records$capital_loss_bin <- as.factor(cut(Records$capital_loss, 2))
Records$hours_week_bin <- as.factor(cut(Records$hours_week, breaks = seq(0, 100, by = 20)))

#Looking at tables to make sure there would be no quasi-complete separation in model.
attach(Records)
names(Records)
table(Workclass, over_50k) #will combine without-pay and never-worked categories 
                            #to fix quasi-complete separation since the categories
                            #are similar in meaning and frequency
table(Education_level, over_50k) #no quasi-complete separation
table(Marital_Status, over_50k) #no quasi-complete separation
table(Occupation, over_50k) #no quasi-complete separation
table(Relationship, over_50k) #no quasi-complete separation
table(Race, over_50k) #no quasi-complete separation
table(Sex, over_50k) #no quasi-complete separation
table(Country, over_50k) #will take out observation of Holand-Netherlands to fix
                          #quasi-complete separation
table(age_bin, over_50k) #no quasi-complete separation
table(edu_num_bin, over_50k) #no qusi-complete separation
table(capital_gain_bin, over_50k) #variable will not be used due to quasi-complete separation
table(capital_loss_bin, over_50k) #no quasi-complete separation
table(hours_week_bin, over_50k) #no quasi-complete separation

#Fixing Quasi-Complete separation issues
Records$Workclass[Records$Workclass == 'Never-worked'] <- 'Without-pay'
droplevels(Records$Workclass)
table(Records$Workclass, Records$over_50k)

Records[Records$Country == "Holand-Netherlands", ]
Records <- Records[-19610, ]
droplevels(Records$Country)
table(Records$Country, Records$over_50k)


#Splitting Data into Training, Validation, Testing
#Randomizing values
Records_Rand <- Records[sample(nrow(Records), 48841, replace=FALSE), ]
#Training set, 70%, 34189 obs
48842*.70
Train <- Records_Rand[1:34189, ]
#Validation set, 15%, 7326
(48842*.15)+34189
Valid <- Records_Rand[34190:41515, ]
#Testing set, 15%, 7326
Test <- Records_Rand[41516:48842, ]

#Running model on Training Set and looking at summary for variable selection.
#I will be using an alpha of 0.001 based on the sample size of this data set.

Income <- glm (over_50k ~ age_bin + Workclass + Education_level + edu_num_bin +
                 Marital_Status + Occupation + Relationship + Race + Sex + Country
               + capital_loss_bin  + hours_week_bin, 
               family = binomial(link="logit"), data=Train)

summary(Income)

step(Income)

#After looking at outcome of step function and alpha levels, the following variables are
#chosen for the model.

Income_1 <- glm (over_50k ~ age_bin + Education_level + 
                 Marital_Status  + Relationship + Sex 
               + hours_week_bin, 
               family = binomial(link="logit"), data=Train)

summary(Income_1)


#Plotting ROC curve 
library(pROC)
Model.ROC <- roc(Income_1$y, Income_1$fitted)
plot(Model.ROC)

#Finding a cutoff point for Income_1 model
Class.Table <- cbind(Model.ROC$thresholds, Model.ROC$sensitivities, Model.ROC$specificities, Model.ROC$sensitivities + Model.ROC$specificities )
colnames(Class.Table) <- c("Probability", "Sensitivity", "Specificity", "Youden")
Class.Table[which(Class.Table[ ,4]== max(Class.Table[ ,4])), ]

#Training Misclassification Rate
prob=predict(Income_1, Train, type=c("response"))
Train$prob=prob
Train$PredTrain <- ifelse(Train$prob <= max(Class.Table[ ,4]), 0, 1)
nrow(Train[which(Train$over_50k + Train$PredTrain == 1), ])/ nrow(Train)

#Scoring Validation Set and calculating Misclassification Rate
prob=predict(Income_1, Valid, type=c("response"))
Valid$prob=prob
Valid$PredValid <- ifelse(Valid$prob <= max(Class.Table[ ,4]), 0, 1)
nrow(Valid[which(Valid$over_50k + Valid$PredValid == 1), ])/ nrow(Valid)

#Scoring Testing set and calculating Misclassification Rate
prob=predict(Income_1, Test, type=c("response"))
Test$prob=prob
Test$PredTest <- ifelse(Test$prob <= max(Class.Table[ ,4]), 0, 1)
nrow(Test[which(Test$over_50k + Test$PredTest == 1), ])/ nrow(Test)

#Plotting Model Performance
Act_1 <- subset(Test, Test$over_50k == 1)
hist (Act_1$prob)

Act_0 <- subset(Test, Test$over_50k == 0)
hist (Act_0$prob)

#Visualizing the Data
Records$over_50kF <- ifelse(Records$over_50k == 1, "Yes", "No")
Records$over_50kF <- as.factor(Records$over_50kF)


p1 <- ggplot(data.frame(Records),aes(x=age_bin, fill = over_50kF))
p1 <- p1   + geom_bar() + ggtitle ("Age") +
  ylab("Count") + labs(fill = ">50k Salary")
p1 <- p1  + theme(text = element_text(size = 15), axis.title.x=element_blank())

p2 <- ggplot(data.frame(Records),aes(x=Workclass, fill = over_50kF))
p2 <- p2  + geom_bar() + ggtitle ("Workclass") +
  xlab("Workclass") + ylab("Count") + labs(fill = ">50k Salary")

Records$Education_level2 <- factor(Records$Education_level, levels = 
                              c("Preschool", "1st-4th","5th-6th", "7th-8th",
                                "9th", "10th", "11th", "12th", "HS-grad",
                                "Some-college", "Assoc-voc", "Assoc-acdm",
                                "Bachelors", "Prof-school", "Masters",
                                "Doctorate"))
p3 <- ggplot(data.frame(Records),aes(x=Education_level2, fill = over_50kF))
p3 <- p3  + geom_bar() + ggtitle ("Education Level") +
  ylab("Count") + labs(fill = ">50k Salary")
p3 <- p3  + theme(text = element_text(size = 15), axis.title.x=element_blank())

Records$Marital_Status2 <- factor(Records$Marital_Status, levels = 
                                     c("Never-married", "Married-civ-spouse", "Married-AF-spouse",
                                       "Married-spouse-absent", "Separated",  "Divorced",
                                       "Widowed" ))
p4 <- ggplot(data.frame(Records),aes(x=Marital_Status2, fill = over_50kF))
p4 <- p4  + geom_bar() + ggtitle ("Marital Status") +
  ylab("Count") + labs(fill = ">50k Salary")
p4 <- p4  + theme(text = element_text(size = 15), axis.title.x=element_blank())

p5 <- ggplot(data.frame(Records),aes(x=Occupation, fill = over_50kF))
p5 <- p5   + geom_bar() + ggtitle ("Occupation") +
  ylab("Count") + labs(fill = ">50k Salary")

Records$Relationship2 <- factor(Records$Relationship, levels = 
                                    c("Husband", "Wife" ,  "Unmarried",
                                      "Own-child", "Not-in-family", 
                                      "Other-relative"))
p6 <- ggplot(data.frame(Records),aes(x=Relationship2, fill = over_50kF))
p6 <- p6   + geom_bar() + ggtitle ("Relationship") +
   ylab("Count") + labs(fill = ">50k Salary")
p6 <- p6  + theme(text = element_text(size = 15), axis.title.x=element_blank())

p7 <- ggplot(data.frame(Records),aes(x=Sex, fill = over_50kF))
p7 <- p7   + geom_bar() + ggtitle ("Sex") +
   ylab("Count") + labs(fill = ">50k Salary")
p7 <- p7  + theme(text = element_text(size = 15), axis.title.x=element_blank())

p8 <- ggplot(data.frame(Records),aes(x=hours_week_bin, fill = over_50kF))
p8 <- p8   + geom_bar() + ggtitle ("Work Hours per Week") +
   ylab("Count") + labs(fill = ">50k Salary")
p8 <- p8  + theme(text = element_text(size = 15), axis.title.x=element_blank())

library(gridExtra)
grid.arrange( p1, p7, p4, ncol=1, nrow=3)
grid.arrange( p6, p3, p8, ncol=1, nrow=3)

