library(ggplot2)
library(Amelia)
library(leaflet)
library(rworldmap)
library(ggmap)
library(reshape2)
library(tidyr)
library(purrr)
library(dummies)
library(caTools)
library(scorer)
# Importing the data
housing_data= read.csv("housing.csv")
#housing_data = as.data.frame(read.csv("housing.csv"))
# Variable summary in the dataset
str(housing_data)
summary(housing_data)
#Column names in the dataframe
names(housing_data)
#Plotting the houses in a map to visualise the density of the houses
#Checking for NA values in the dataset

sapply(housing_data,function(x) sum(is.na(x)))

missmap(housing_data, main="Missing Values vs Observed")


#number of rows in the dataset
nrow(housing_data)

# Handelling missing values in total_bedrooms column in the dataset
housing_data$total_bedrooms = ifelse(is.na(housing_data$total_bedrooms),
                     ave(housing_data$total_bedrooms, FUN = function(x) mean(x, na.rm = TRUE)),
                     housing_data$total_bedrooms)

sapply(housing_data,function(x) sum(is.na(x)))

missmap(housing_data, main="Missing Values vs Observed")

#Finding the unique factor values in the data column ocean_proximity

unique(Filter(is.factor,housing_data))
# NEAR BAY,<1H OCEAN,INLAND,NEAR OCEAN,ISLAND

housing_ocean_proximity <- ggplot(housing_data,aes(x=ocean_proximity,fill=ocean_proximity))+ geom_bar()+
  ggtitle("Ocean Proximity")+
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2))
housing_ocean_proximity


#Changing the ocean_proximity to 
housing_data$ocean_proximity = factor(housing_data$ocean_proximity,
                        levels = c('NEAR BAY', '<1H OCEAN', 'INLAND','NEAR OCEAN','ISLAND'),
                       labels = c(0,1,2,3,4))


#View(housing_data)
housing_data$ocean_proximity = as.numeric(housing_data$ocean_proximity)

housing_data$ocean_proximity
#Correlation Matrix
corrMat <- round(cor(housing_data),2)
melted_corrmat <- melt(corrMat)
head(melted_corrmat)

ggplot(data= melted_corrmat, aes(x=Var1,y=Var2, fill= value))+
  geom_tile()



get_upper_triangle<- function(correlationMatrix){
  correlationMatrix[lower.tri(correlationMatrix)] <- NA
  return(correlationMatrix)
}


upper_triangular_matrix <- get_upper_triangle(corrMat)
upper_triangular_matrix
melted_corrmat <- melt(upper_triangular_matrix, na.rm = TRUE)


corrHeatMap <- ggplot(data=melted_corrmat, aes(Var2,Var1, fill=value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "#c40310", mid = "grey",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,size = 12, hjust = 1))+
  coord_fixed()


corrHeatMap+
geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# We see multi-collinearity demonstrated by households with poulation, total_bedrooms, total_rooms

#Therefore removing the the variable "households"

dropColName <- c("households")

housing_data <- housing_data[,!(names(housing_data) %in% dropColName)]

#Taking average of total_rooms and total_bedrooms with population

housing_data['average_rooms'] = housing_data['total_rooms']/housing_data['population']
housing_data['average_bedrooms'] = housing_data['total_bedrooms']/housing_data['population']

dropColName <- c("total_rooms","total_bedrooms")
housing_data <- housing_data[,!(names(housing_data) %in% dropColName)]
  

  
corrMat <- round(cor(housing_data),2)
melted_corrmat <- melt(corrMat)
head(melted_corrmat)

ggplot(data= melted_corrmat, aes(x=Var1,y=Var2, fill= value))+
  geom_tile()



get_upper_triangle<- function(correlationMatrix){
  correlationMatrix[lower.tri(correlationMatrix)] <- NA
  return(correlationMatrix)
}


upper_triangular_matrix <- get_upper_triangle(corrMat)
upper_triangular_matrix
melted_corrmat <- melt(upper_triangular_matrix, na.rm = TRUE)


corrHeatMap <- ggplot(data=melted_corrmat, aes(Var2,Var1, fill=value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "#c40310", mid = "grey",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,size = 12, hjust = 1))+
  coord_fixed()


corrHeatMap+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))



#Visualizing all the variables in the dataframe by plotting a histogram

housing_data %>%
  keep(is.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


#Checking for outliers in housing median age 

loop.vector <- length(housing_data)
for (i in 1:loop.vector)
{
  gplot<-ggplot(housing_data, aes(x = "", y=housing_data[,i])) + 
    geom_boxplot(fill="maroon")+ ggtitle(colnames(housing_data[i])) + coord_flip()
  print(gplot)
}
ggplot(housing_data, aes(x = "", y=housing_median_age)) + 
  geom_boxplot(fill="maroon") + coord_flip()

#Checking for outliers in median housing value 
ggplot(housing_data, aes(x = "", y=median_house_value)) + 
  geom_boxplot(fill="maroon") + ggtitle("median_house_value") + coord_flip()

# Removing Outliers in median housing value
housing_data = housing_data[housing_data$median_house_value < 420001,]


#>>>>><<<<< Applying Linear Regression >>>>><<<<<

#Reordering
housing_data <- housing_data[,c(1,2,3,4,5,6,8,9,7)]
#housing_data[-9] = scale(housing_data[-9])
# Splitting the data into train and test.

set.seed(123)
split <- sample.split(housing_data$median_house_value, SplitRatio = 0.8)
training_set <- subset(housing_data, split == TRUE)
test_set <- subset(housing_data, split == FALSE)
#>>>>>>>>>>>>>>> Feature Scaling <<<<<<<<<<<<<<<<#

training_set[-9] = scale(training_set[-9])
test_set[-9] = scale(test_set[-9])



#Applying Linear Model
linearModel <- lm(median_house_value ~., data = training_set)


#Interpret the model

summary(linearModel)

#Residuals

res <- residuals(linearModel)
res<- as.data.frame(res)

ggplot(res,aes(res)) + geom_histogram(fill='blue',alpha=0.5)


par(mfrow = c(2,2))
plot(linearModel)
#Predictions

medianHouseValue_Predictions <- predict(linearModel,test_set)
results <- cbind(medianHouseValue_Predictions,test_set$median_house_value)
colnames(results) <- c('predicted', 'actual')
results
results <- as.data.frame(results)


#Adding id column in the result dataframe
library(dplyr)
results <- results %>% mutate(id = row_number())

ggplot(results, aes(id)) + 
  geom_line(aes(y = results$actual, colour = "actual")) + 
  geom_line(aes(y = results$predicted, colour = "predicted"),alpha = 0.5)



options(repr.plot.width=8, repr.plot.height=4)
my_data = as.data.frame(cbind(predicted = medianHouseValue_Predictions,
                              observed = results$actual))

ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Linear Regression ')  +
  xlab("Predecited Power Output ") + ylab("Observed Power Output") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))

#Evaluation
#Mean Square Error

mean_squared_error(results$actual,results$predicted)

r2_score(results$actual,results$predicted)
rmse <- (mean_squared_error(results$actual,results$predicted))^0.5
rmse
mean_absolute_error(results$actual,results$predicted)
mean_absolute_percent_error(results$actual,results$predicted)

#XGboost


library(xgboost)
library(caret)


split <- createDataPartition(y = housing_data$median_house_value, 
                                    p = 0.8, list = FALSE)

training_set <- housing_data[split,]
testing_set <- housing_data[-split,]

#Scaling
training_set[-9] = scale(training_set[-9])
testing_set[-9] = scale(testing_set[-9])

#median_house_value # 6





dtrain <- xgb.DMatrix(data = as.matrix(training_set[-6]), label = training_set$median_house_value)
dtest <- xgb.DMatrix(data = as.matrix(test_set[-6]), label = test_set$median_house_value)
watchlist <- list(train=dtrain, test=dtest)
bst <- xgb.train(data=dtrain, booster = "gblinear", max.depth=5, nthread = 2, nrounds=100, 
                 watchlist=watchlist, eval.metric = "logloss", eval.metric = "rmse")
print(bst)
#Summary
xgb.dump(bst, with_stats = T)
#Residuals
res <- residuals(linearModel)
res<- as.data.frame(res)

ggplot(res,aes(res)) + geom_histogram(fill='blue',alpha=0.5)

#Evaluation
importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)



predicted = predict(bst, dtest)


results_xgb <- cbind(predicted,test_set$median_house_value)
colnames(results_xgb) <- c('predicted', 'actual')

results_xgb <- as.data.frame(results_xgb)
results_xgb <- results_xgb %>% mutate(id = row_number())
mean_squared_error(results_xgb$actual,results_xgb$predicted)
rmse<- (mean_squared_error(results_xgb$actual,results_xgb$predicted))^0.5
rmse
r2_score(results_xgb$actual,results_xgb$predicted)
mean_absolute_error(results_xgb$actual,results_xgb$predicted)
mean_absolute_percent_error(results_xgb$actual,results_xgb$predicted)


#Actual vs Predicted graph
ggplot(results_xgb, aes(id)) + 
  geom_line(aes(y = results_xgb$actual, colour = "actual")) + 
  geom_line(aes(y = results_xgb$predicted, colour = "predicted"),alpha = 0.5)
# Plot predictions vs test data 
options(repr.plot.width=8, repr.plot.height=4)
my_data = as.data.frame(cbind(predicted = predicted,
                              observed = test_set$median_house_value))

ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Linear Regression ') + ggtitle("Extreme Gradient Boosting: Prediction vs Test Data") +
  xlab("Predecited Power Output ") + ylab("Observed Power Output") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))


