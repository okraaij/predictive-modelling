#######################################################################
#   
#   University of Edinburgh Business School
#   MSc Business Analytics
#   Predictive Analytics and Modelling of Data
#
#   Classification assignment
#
#   Please read attached 'Report' file for the analysis and more information
#
#   Copyright© of this project is with the authors
#
#######################################################################

# Load libraries

library(zoo);
library(taRifx);
library(lubridate);
library(xts);
library(tidyquant);
library(tseries);

library(fpp);
library(dplyr);
library(corrplot);
library(car);

library(AppliedPredictiveModeling);
library(MASS);
library(pls);
library(gdata);
library(TTR);
library(vars);
library(lmtest);
library(tidyverse);
library(caTools);
library(e1071);
library(caret);
library(readxl);

#######################################################################
#
#                              Simulating the data
#
#######################################################################


# This create the same data set each run
set.seed(315) 

# Specific number of observations made (based on the probabilites of the actual data e.g. 0.2922 of customers went to CW in 
# data extracted from Hitwise. This number changes for each retailer.
n.size <- 3463 

Apple <- tibble(
  IncomeGroup = sample(c('<£10000', '£10000-£14999', '£15000-£19999', '£20000-£29999', '£30000-£39999', '£40000-£49999', 
                         '£50000-£59999', '£60000-£69999', '£70000-£99999', '£100000+'), size = n.size, 
                       prob = c(0.267358023016478, 0.166194131073246, 0.141290714023054, 0.28156566021802, 0.13610511785171, 
                                0.0777689781918101, 0.0342956194999574, 0.015213557947529, 0.0106148679281936, 0.0025308109528019), replace = TRUE),
  
  age = sample(c('18-24','25-34','35-44','45-54','55+'), size = n.size, prob = c(0.195881146137745, 0.219780171455125, 
                                                                                 0.208349955576096, 0.180076056482289, 0.195912670350532), replace = TRUE),
  
  sex = sample (c('Male', 'Female'), size = n.size, prob = c(0.550849101063351, 0.449150898934995), replace = TRUE),
  
  region = sample(c('Yorkshire', 'West-Midlands', 'Northern-Ireland', 'North-East', 'South-West', 'Scotland',
                    'London', 'Wales', 'North West', 'South East', 'East', 'East Midlands'), size = n.size, 
                  prob = c(0.0762547636805164, 0.0993039602889081, 0.0229535109373697, 0.0301930700243128, 0.067562563644706, 
                           0.0659140861010843, 0.15760606959337, 0.0432299523082554, 0.126846600199788, 0.146388586744844, 0.0879064056803375, 
                           0.075840430798295), replace = TRUE),
  employment = sample(c('Permanent-Full-time-Employment', 'Student-Or-Internship', 'Permanent-Part-time-Employment', 'Retired', 
                        'Unpaid-Employment', 'House-Wife-Or', 'Self-employed-.-Freelance', 'Temporary Work', 'Unable-to-Work', 'Without-Work'), 
                      size = n.size, prob = c(0.3630944905414, 0.0775197293922285, 0.104163334390019, 0.174273392996494, 0.0225034075774653, 
                                              0.0694197819797995, 0.0709460000406448, 0.00669374717325238, 0.0386049946782103, 0.0727811212322728), replace = TRUE)
)

# Export file to temp

library(writexl)
tempfile <- write_xlsx(Apple)


#######################################################################
#
#                              Naive Bayes
#
#######################################################################

# Obtain the time
ptm <- proc.time()

# Read the data
S8 <- read_excel("Simulated_Data_10K_Final_S8.xlsx")
View(S8)
summary(S8)

# Convert the columns into factors
S8[] <- lapply(S8, factor)

# Set Seed so that same sample can be reproduced in future also
set.seed(123)

# Now Selecting 70% of data as sample from total 'n' rows of the data  
sample = sample.split(S8$Sex, SplitRatio = .7)
train = subset(S8, sample == TRUE)
test  = subset(S8, sample == FALSE)

# Train the Naive Bayes Classifier
nB_model <- naiveBayes(Retailer ~ ., data = train)
nB_model

# Predict from texting data
pred <- predict(nB_model, test, type="class")
tab <- table(pred)

# Obtain confusion matrix
confusionMatrix(pred, test$Retailer)

# Change the data into numerical
changenumber <- ifelse(pred=="Carphone Warehouse",1,0)
change <- ifelse(test$Retailer=="Carphone Warehouse",1,0)

# Plot ROC curve
pr.naive <- prediction(changenumber, change)
prf.naive <- performance(pr.naive, measure="tpr", x.measure = "fpr")
plot(prf.naive)
abline(a=0,b=1, lwd=2, lty=2,col="blue")

# Calculate AUC value
auc.naive <- performance(pr.naive, measure="auc")
auc.naive <- auc.naive@y.values[[1]]
auc.naive


#######################################################################
#
#                              Decision Trees
#
#######################################################################


#                   Decision Trees were implemented in Weka


#######################################################################
#
#                         Support Vector Machines
#
#######################################################################

###########

#Code used to see impact of cost and epsilon

###########

# Load excel data
ip7 <- read_excel("Simulated_Data_10K_Final_S8.xlsx")

ip7[] <- lapply(ip7, factor);
set.seed(123)
sample = sample.split(ip7$Age, SplitRatio = .7);
trainingdata = subset(ip7, sample == TRUE);
testingdata = subset(ip7, sample == FALSE);

# Train Support Vector Machine
mymodel <- svm(Retailer~., data = trainingdata , kernel = "radial");
summary(mymodel)

# Confusion matrix and misclassification error
pred <- predict(mymodel, testingdata);
tab <- table(predicted = pred, actual = testingdata$Retailer);
misclassificaton <- 1 - sum(diag(tab)/sum(tab));
tab
misclassificaton

# Tuning/hyperparameter optimisation for the SVM

# set the random seet to 123, so the results are reproduceable 
set.seed(123); 

# Use the tune function to tune the hyperparameters of the svm
tmodel <- tune(svm, Retailer~., data=trainingdata, ranges = list(epsilon=seq(0,1,0.1), cost = (1:4)));
plot(tmodel);
summary(tmodel);


###########
                             
#Code used to see impact of gamma
                             
###########

ip7 <- read_excel("Simulated_Data_10K_Final_S8.xlsx");
ip7[] <- lapply(ip7, factor);
set.seed(123);

# Split test/train
sample = sample.split(ip7$Age, SplitRatio = 0.7);
trainingdata = subset(ip7, sample == TRUE);
testingdata = subset(ip7, sample == FALSE);
                             
# Train Support Vector Machine
mymodel <- svm(Retailer~., data = trainingdata , kernel = "radial");
summary(mymodel);
                             
# Confusion matrix and misclassification error
pred <- predict(mymodel, testingdata);
tab <- table(predicted = pred, actual = testingdata$Retailer);
misclassificaton <- 1 - sum(diag(tab)/sum(tab));
tab;
misclassificaton;
                             
# Tuning/hyperparameter optimisation for the SVM

# Set the random seet to 123, so the results are reproduceable 
set.seed(123); 

# Use the tune function to tune the hyperparameters of the SVM
tmodel <- tune(svm, Retailer~., data=trainingdata, ranges = list(gamma=seq(0,5,1)));
plot(tmodel);
summary(tmodel);

# End of file
                             