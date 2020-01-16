library(ggplot2)
library(Amelia)
library(leaflet)
library(rworldmap)
library(ggmap)
library(reshape2)
library(tidyr)
library(purrr)
library(dplyr)
library(caTools)
library(caret)
library(rpart.plot)
library(corrgram)
library(corrplot)
#Importing the data

#Filling all the blank values with NA
stroke_data = read.csv("Stroke-Data.csv",na.strings = c("","NA"))

# Variable summary in the dataset

str(stroke_data)

summary(stroke_data)

#Column names in the dataframe
names(stroke_data)

sapply(stroke_data,function(x) sum(is.na(x)))


missmap(stroke_data, main="Missing Values vs Observed")


#Imputations

#bmi
stroke_data$bmi <- as.numeric(stroke_data$bmi)
stroke_data$bmi = ifelse(is.na(stroke_data$bmi),
                         ave(stroke_data$bmi, FUN = function(x) mean(x, na.rm = TRUE)),
                         stroke_data$bmi)

#smoking_status
stroke_data$smoking_status = factor(stroke_data$smoking_status,
                                    levels = c('never smoked', 'formerly smoked','smokes'),
                                    labels = c(0,1,2))
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
stroke_data$smoking_status = ifelse(is.na(stroke_data$smoking_status),
                                    ave(stroke_data$smoking_status, FUN = function(x) Mode(x, na.rm = TRUE)),
                                    stroke_data$smoking_status)  



#Checking for Null values
sapply(stroke_data,function(x) sum(is.na(x)))
missmap(stroke_data, main="Missing Values vs Observed")
#Changing to categorical variables
stroke_data$hypertension = as.factor(stroke_data$hypertension)
stroke_data$heart_disease = as.factor(stroke_data$heart_disease)
stroke_data$stroke = as.factor(stroke_data$stroke)
stroke_data$smoking_status = as.factor(stroke_data$smoking_status)
str(stroke_data)



# Bar graph shows the distribution of Number of patients having stroke or not

stroke_data_cat_plot <- ggplot(stroke_data,aes(x=stroke,fill=stroke))+ geom_bar()+
  ggtitle("Number of patients having stroke or not")+
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2))
stroke_data_cat_plot

# Selecting the group of data with stroke = 0
stroke_data_cat_0 <- stroke_data %>%
  select (names(stroke_data)) %>%
  filter(stroke == 0)

# Selecting the group of data with stroke = 1

stroke_data_cat_1 <- stroke_data %>%
  select (names(stroke_data)) %>%
  filter(stroke == 1)

#Visulalizing the ever_married with stroke value = 0
stroke_data_cat_0_plot <- ggplot(stroke_data_cat_0,aes(x=ever_married,fill=ever_married))+ geom_bar()+
  ggtitle("stroke = No")+
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2))

stroke_data_cat_0_plot

#Visulalizing the ever_married with stroke value = 1
stroke_data_cat_1_plot <- ggplot(stroke_data_cat_1,aes(x=ever_married,fill=ever_married))+ geom_bar()+
  ggtitle("stroke = Yes")+
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2))

stroke_data_cat_1_plot
  


#Visulalizing the work_type with stroke value = 0
stroke_data_cat_0_plot <- ggplot(stroke_data_cat_0,aes(x=work_type,fill=work_type))+ geom_bar()+
  ggtitle("stroke = No")+
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2))

stroke_data_cat_0_plot

#Visulalizing the work_type with stroke value = 1
stroke_data_cat_1_plot <- ggplot(stroke_data_cat_1,aes(x=work_type,fill=work_type))+ geom_bar()+
  ggtitle("stroke = Yes")+
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2))

stroke_data_cat_1_plot

#Visulalizing the smoking_status with stroke value = 0
stroke_data_cat_0_plot <- ggplot(stroke_data_cat_0,aes(x=smoking_status,fill=smoking_status))+ geom_bar()+
  ggtitle("stroke = No")+
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2))

stroke_data_cat_0_plot

#Visulalizing the smoking_status with stroke value = 1
stroke_data_cat_1_plot <- ggplot(stroke_data_cat_1,aes(x=smoking_status,fill=smoking_status))+ geom_bar()+
  ggtitle("stroke = Yes")+
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2))

stroke_data_cat_1_plot

#Visulalizing the Residence_type with stroke value = 0
stroke_data_cat_0_plot <- ggplot(stroke_data_cat_0,aes(x=Residence_type,fill=Residence_type))+ geom_bar()+
  ggtitle("stroke = No")+
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2))

stroke_data_cat_0_plot

#Visulalizing the Residence_type with stroke value = 1
stroke_data_cat_1_plot <- ggplot(stroke_data_cat_1,aes(x=Residence_type,fill=Residence_type))+ geom_bar()+
  ggtitle("stroke = Yes")+
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2))

stroke_data_cat_1_plot


#Visulalizing the hypertension with stroke value = 0
stroke_data_cat_0_plot <- ggplot(stroke_data_cat_0,aes(x=hypertension,fill=hypertension))+ geom_bar()+
  ggtitle("stroke = No")+
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2))

stroke_data_cat_0_plot

#Visulalizing the hypertension with stroke value = 1
stroke_data_cat_1_plot <- ggplot(stroke_data_cat_1,aes(x=hypertension,fill=hypertension))+ geom_bar()+
  ggtitle("stroke = Yes")+
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2))

stroke_data_cat_1_plot



#Visulalizing the heart_disease with stroke value = 0
stroke_data_cat_0_plot <- ggplot(stroke_data_cat_0,aes(x=heart_disease,fill=heart_disease))+ geom_bar()+
  ggtitle("stroke = No")+
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2))

stroke_data_cat_0_plot

#Visulalizing the heart_disease with stroke value = 1
stroke_data_cat_1_plot <- ggplot(stroke_data_cat_1,aes(x=heart_disease,fill=heart_disease))+ geom_bar()+
  ggtitle("stroke = Yes")+
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2))

stroke_data_cat_1_plot



str(stroke_data)







#stroke_data$stroke <- as.factor(stroke_data$stroke)

#<<<<<<<<<<<<<<<<< Splitting Data >>>>>>>>>>>>>>>>>#

set.seed(123)
split <- sample.split(stroke_data$stroke, SplitRatio = 0.7)
training_set <- subset(stroke_data, split == TRUE)
test_set <- subset(stroke_data, split == FALSE)

#Decision Tree

library(rpart)

DecisionTree_classifier = rpart(formula = stroke ~ .,method='class',
                                data = training_set,
                                control =rpart.control(minsplit =7,minbucket=10, cp=0))

print(DecisionTree_classifier)
summary(DecisionTree_classifier)
predTest = predict(DecisionTree_classifier, test_set[-12], type= 'class')
confusionMatrix(table(predTest, test_set$stroke))

rpart.plot(DecisionTree_classifier, box.palette="RdBu", nn=TRUE)



pdf('newrplot.pdf')
rpart.plot(DecisionTree_classifier, box.palette="RdBu", nn=TRUE)
dev.off()




