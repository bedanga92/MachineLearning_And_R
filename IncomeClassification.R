library(ggplot2)
library(Amelia)
library(leaflet)
library(rworldmap)
library(ggmap)
library(reshape2)
library(tidyr)
library(purrr)
library(dplyr)
library(dummies)
library(randomForest)
library(e1071)
library(pROC)
library(caret)
#Importing the data set for Income Classification
income_data = read.csv("income_evaluation.csv")

View(income_data)


#Checking the structure of the variables of the data set

str(income_data)
summary(income_data)
#unique(income_data$income)

#Column names in the dataframe
names(income_data)

#Removing starting and trailing Whitespaces
income_data <- data.frame(lapply(income_data, trimws), stringsAsFactors = TRUE)

#Checking for Null values
sapply(income_data,function(x) sum(is.na(x)))
missmap(income_data, main="Missing Values vs Observed")

# We have observed that there are some values in the data set that has a value ?
# now replacing the value ? with NA

income_data[income_data == "?"] <- NA
income_data$occupation = as.character(income_data$occupation)
income_data$occupation = as.factor(income_data$occupation)
income_data$workclass = as.character(income_data$workclass)
income_data$workclass = as.factor(income_data$workclass)
income_data$native.country = as.character(income_data$native.country)
income_data$native.country = as.factor(income_data$native.country)

sapply(income_data,function(x) sum(is.na(x)))
missmap(income_data, main="Missing Values vs Observed")
#Dropping all NA rows
income_data <- income_data %>% drop_na()
sapply(income_data,function(x) sum(is.na(x)))
missmap(income_data, main="Missing Values vs Observed")

#Function to filter data by some category
get_filtered_data <- function(data,filter,param){
  
  data_filter <- data %>%
    select (names(data)) %>%
    filter(filter == param)
  return (data_filter)
}

income_gtr50K <- get_filtered_data(income_data,income_data$income,">50K")

income_less50K <- get_filtered_data(income_data,income_data$income,"<=50K")
  

#Function to plot the filtered data with some category

plot_filtered_data<- function(data,filter,pricategory,category){
  plot <- ggplot(data,aes(x=category,fill=pricategory))+ geom_bar(position = "dodge")+
    ggtitle(filter)+
    geom_text(aes(label=..count..,vjust = -0.2),stat='count',position = position_dodge(width = 1))
    
  }

#Plotting the income levels
plot_income <- plot_filtered_data(income_data,"Income Distribution",income_data$income,income_data$income)
plot_income

#plotting the Marital Status
plot_marital_status <- plot_filtered_data(income_data,"Marital Status",income_data$income,
                                          income_data$marital.status)
plot_marital_status

#Plotting the relationship
plot_relationship_status <- plot_filtered_data(income_data,"Relationships",income_data$income,
                                          income_data$relationship)
plot_relationship_status

#Plotting the occupations

plot_occupation <- plot_filtered_data(income_data,"Occupations",income_data$income,
                                               income_data$occupation)
plot_occupation

#Plotting native.country,education,race
plot_native_country <- plot_filtered_data(income_data,"Native Country",income_data$income,
                                      income_data$native.country) + coord_flip()
plot_native_country
#Plotting education
plot_education <- plot_filtered_data(income_data,"Education",income_data$income,
                                      income_data$education)
plot_education
#Plotting race
plot_race <- plot_filtered_data(income_data,"Race",income_data$income,
                                      income_data$race)
plot_race

#Creating dummy variabls for the categorical variables.

#native Country

income_data_new <- dummy.data.frame(income_data, name = c('native.country'), sep='.')
income_data_new <- income_data_new[,c("age","workclass","fnlwgt",
                                      "education",
                                      "education.num",
                                      "marital.status",
                                      "occupation",
                                      "relationship",
                                      "race",
                                      "sex",
                                      "capital.gain",
                                      "capital.loss",
                                      "hours.per.week",
                                      "native.country.Cambodia",
                                      "native.country.Canada",                    
                                       "native.country.China",                     
                                      "native.country.Columbia",                  
                                       "native.country.Cuba",                      
                                       "native.country.Dominican-Republic",        
                                       "native.country.Ecuador",                   
                                       "native.country.El-Salvador",               
                                       "native.country.England",                   
                                       "native.country.France",                    
                                       "native.country.Germany",                   
                                       "native.country.Greece",                    
                                       "native.country.Guatemala",                 
                                       "native.country.Haiti",                     
                                       "native.country.Holand-Netherlands",        
                                       "native.country.Honduras",                  
                                       "native.country.Hong",                      
                                      "native.country.Hungary",                   
                                       "native.country.India",                     
                                      "native.country.Iran",                      
                                       "native.country.Ireland",                   
                                      "native.country.Italy",                     
                                       "native.country.Jamaica",                   
                                      "native.country.Japan",                     
                                       "native.country.Laos",                      
                                       "native.country.Mexico",                    
                                       "native.country.Nicaragua",                 
                                       "native.country.Outlying-US(Guam-USVI-etc)",
                                       "native.country.Peru",                      
                                       "native.country.Philippines",               
                                       "native.country.Poland",                    
                                       "native.country.Portugal",                  
                                       "native.country.Puerto-Rico",               
                                       "native.country.Scotland",                  
                                       "native.country.South",                     
                                      "native.country.Taiwan",                    
                                       "native.country.Thailand",                  
                                       "native.country.Trinadad&Tobago",
                                      "native.country.Vietnam",
                                      "native.country.Yugoslavia","native.country.United-States","income")]
income_data_new$native.country.Other <- rowSums( income_data_new[,14:53] )
income_data_new <- income_data_new[ -c(14:53)]

#Workclass column

income_data_new_copy  <- income_data_new
income_data_new_copy <- dummy.data.frame(income_data_new, name = c('workclass'), sep='.')
income_data_new <- income_data_new_copy

# Race Column

income_data_new_copy <- dummy.data.frame(income_data_new, name = c('race'), sep='.')
income_data_new_copy <- income_data_new_copy[,c("age",
                                      "workclass.Federal-gov",
                                      "workclass.Local-gov",
                                      "workclass.Private",
                                      "workclass.Self-emp-inc",
                                      "workclass.Self-emp-not-inc",
                                      "workclass.State-gov",
                                      "workclass.Without-pay",
                                      "fnlwgt",
                                      "education",
                                      "education.num",
                                      "marital.status",
                                      "occupation",
                                      "relationship",
                                      "race.Amer-Indian-Eskimo",
                                      "race.Asian-Pac-Islander",
                                      "race.Other",
                                      "race.Black",
                                      "race.White",
                                      "sex",
                                      "capital.gain",
                                      "capital.loss",
                                      "hours.per.week",
                                      "native.country.United-States",
                                      "native.country.Other",
                                      "income")]
income_data_new_copy$race.Other <- rowSums( income_data_new_copy[,15:16])
income_data_new_copy <- income_data_new_copy[ -c(15:16)]
income_data_new <- income_data_new_copy
#Education Column
income_data_new_copy <- dummy.data.frame(income_data_new, name = c('education'), sep='.')
income_data_new_copy <- income_data_new_copy[,c(1,2,3,4,5,6,7,8,9,23,13,14,15,16,10,11,12,24,17,18,19,25,20,22,21,26,27,28,29,30,31,32,33,34,35,36,37,38,39)]
income_data_new_copy$pre_prof_school <- rowSums( income_data_new_copy[,10:18])
income_data_new_copy <- income_data_new_copy[ -c(10:18)]
income_data_new_copy$assosiate_edu <- rowSums( income_data_new_copy[,10:11])
income_data_new_copy <- income_data_new_copy[ -c(10:11)]
income_data_new_copy$masters_doc <- rowSums( income_data_new_copy[,12:13])
income_data_new_copy <- income_data_new_copy[ -c(12:13)]
income_data_new_copy$college <- rowSums( income_data_new_copy[,10:11])
income_data_new_copy <- income_data_new_copy[ -c(10:11)]
income_data_new_copy <- income_data_new_copy[ -c(10)]
income_data_new <- income_data_new_copy

#Marital Status

income_data_new_copy <- dummy.data.frame(income_data_new, name = c('marital.status'), sep='.')

income_data_new_copy <- income_data_new_copy[,c(1,2,3,4,5,6,7,8,9,10,11,14,16,17,12,13,15,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32)]
income_data_new_copy$Divorced <- rowSums( income_data_new_copy[,11:14])
income_data_new_copy <- income_data_new_copy[ -c(11:14)]
income_data_new_copy$Married <- rowSums( income_data_new_copy[,11:12])
income_data_new_copy <- income_data_new_copy[ -c(11:12)]
income_data_new_copy$Never_Married <- income_data_new_copy$`marital.status.Never-married`
income_data_new_copy <- income_data_new_copy[ -c(11)]
income_data_new <- income_data_new_copy

#Occupation 
income_data_new_copy <- dummy.data.frame(income_data_new, name = c('occupation'), sep='.')
income_data_new_copy <- income_data_new_copy[,c(1,2,3,4,5,6,7,8,9,10,11,12,15,16,18,19,21,13,17,23,24,14,20,22,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41)]
income_data_new_copy$Group_1_ocp <- income_data_new_copy$`occupation.Adm-clerical`
income_data_new_copy <- income_data_new_copy[ -c(11)]
income_data_new_copy$Group_2_ocp <- rowSums( income_data_new_copy[,11:16])
income_data_new_copy <- income_data_new_copy[ -c(11:16)]
income_data_new_copy$Group_3_ocp <- rowSums( income_data_new_copy[,11:14])
income_data_new_copy <- income_data_new_copy[ -c(11:14)]
income_data_new_copy$Group_4_ocp <- rowSums( income_data_new_copy[,11:12])
income_data_new_copy <- income_data_new_copy[ -c(11:12)]
income_data_new_copy$Group_5_ocp <- income_data_new_copy$occupation.Sales
income_data_new_copy <- income_data_new_copy[ -c(11)]
income_data_new <- income_data_new_copy
# Relationship

#Dropping Relationship

income_data_new_copy <- income_data_new_copy[ -c(11)]
income_data_new <- income_data_new_copy

#Sex 
income_data_new_copy <- income_data_new

income_data_new_copy$sex = factor(income_data_new_copy$sex,
                                      levels = c('Male', 'Female'),
                                      labels = c(1,0))

income_data_new <- income_data_new_copy
#Income

income_data_new_copy <- income_data_new

income_data_new_copy$income = factor(income_data_new_copy$income,
                                  levels = c('<=50K', '>50K'),
                                  labels = c(0,1))

income_data_new <- income_data_new_copy

#Reordering
income_data_new <- income_data_new[,c(1,9,10,15,16,17,2,3,4,5,6,7,8,11,12,13,14,18,19,20,21,22,23,24,25,26,27,28,29,30,31)]

#Transforming to continous variables
income_data_new$age <- as.numeric(income_data_new$age)
income_data_new$fnlwgt <- as.numeric(income_data_new$fnlwgt)
income_data_new$education.num <- as.numeric(income_data_new$education.num)
income_data_new$capital.gain <- as.numeric(income_data_new$capital.gain)
income_data_new$capital.loss <- as.numeric(income_data_new$capital.loss)
income_data_new$hours.per.week <- as.numeric(income_data_new$hours.per.week)
#Splitting the data to test and train
library(caTools)
set.seed(123)
split <- sample.split(income_data_new$income, SplitRatio = 0.8)
training_set = subset(income_data_new, split == TRUE)
test_set = subset(income_data_new, split == FALSE)
#Feature Scaling

training_set[,1:6] = scale(training_set[,1:6])
test_set[,1:6] = scale(test_set[,1:6])

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Applying Random Forest<<<<<<<<<<<<<<<<<<<<<<<<<<<


#Randon Forest
set.seed(123)
rf_classifier = randomForest(x = training_set[-20],
                          y = training_set$income,
                          ntree =500,
                          mtry = 6,
                          importance = TRUE)

#Summary and model
print(rf_classifier)
summary(rf_classifier)

#Importance Plot
rfImp <-varImpPlot(rf_classifier)
rfImp

#Confusion Matrix
predValid <- predict(rf_classifier, test_set[-20], type = "class")
confusionMatrix(table(predValid,test_set$income))
#AUC and ROC
predTestwithProb = predict(rf_classifier, test_set[-20], type= 'prob')
auc <- auc(test_set$income,predTestwithProb[,2])
auc

plt_rf<- plot(roc(test_set$income,predTestwithProb[,2]))
plt_rf

##############################################################
##>>>>>>>>>>>>>>>> SVM <<<<<<<<<<<<<<<<<<<<<<<<<<<##
svm_classifier = svm(formula = income ~ .,
                     data = training_set,
                     type = 'C-classification',
                     kernel = 'polynomial',gamma = 0.03)

#Summary and model
print(svm_classifier)
summary(svm_classifier)

#Confusion Matrix
y_pred = predict(svm_classifier, newdata = test_set[-20], type='class')
confusionMatrix(table(y_pred,test_set$income))

#AUC and ROC
predTestwithProb = predict(svm_classifier, test_set[-20], type= 'prob')
predTestwithProb = as.numeric(predTestwithProb)
auc <- auc(test_set$income,predTestwithProb)
auc
plt2<-plot(roc(test_set$income,predTestwithProb))
plt2
