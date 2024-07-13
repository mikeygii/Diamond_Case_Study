#Loading Possible Libraries
library(tidyverse)
#library(carat)
library(caret)
library(leaps)
library(glmnet)
library(leaps)
library(ggplot2)
library(earth)
library(mgcv)
library(class)
library(readr)
library(xgboost)
library(Ckmeans.1d.dp)
library(pdp)
library(ROCR)
library(randomForest)
library(plotROC)
library(sqldf)
library(readr)

#Setting the Seed so that when I use random functions it will be the same random pull each time so my numbers don't change
set.seed(123)
#______________________________________________________________________________#
#loading in the data 
diamond <- read_csv("ENOVA/training.csv")
diamond <- data.frame(diamond)

colnames(diamond)
sapply(diamond, typeof)

#______________________________________________________________________________#

#                             DATA MANAGEMENT

#______________________________________________________________________________#

#______________________________________________________________________________#
#Turning Measurements into 3 separate columns so I can work with Continuous Data
#Making sure the new values are read in as doubles/numeric
diamond <- separate_wider_delim(diamond, cols = Measurements, delim = "x", names = c("Length", "Width", "Depth2"))
diamond$Length <- as.double(diamond$Length)
diamond$Depth <- as.double(diamond$Depth)
diamond$Depth2 <- as.double(diamond$Depth2)
diamond$Width <- as.double(diamond$Width)
#______________________________________________________________________________#



#______________________________________________________________________________#
#Checking for Missing values for each column
MV <-NULL
for (i in 1:21){
  MV[i] <- sum(is.na(diamond[i]))
  print(names(diamond)[i])
  print(MV[i])
}
#MISSING VALUES
#CUT 3922 CHARACTER
#DEPTH 1440 DOUBLE
#POLISH 899 CHARACTER
#SYMMETRY 899 CHARACTER
#TABLE 2531 DOUBLE

#Creating functions to deal with missing values
#Imputes Median
impute_median <- function(x){
  ind_na <- is.na(x)
  x[ind_na] <- median(x[!ind_na])
  as.numeric(x)
}

#Imputes 'Missing' for character columns
impute_m <- function(x){
  ind_na <- is.na(x)
  x[ind_na] <- 'Missing'
  factor(x)
}
#______________________________________________________________________________#

#Using SQL to manage data and create new variables I feel more comfortable to do it this way
#Creating a new Depth % column with Actual values and Estimated Values where there were NAs 
#Creating a Depth % flag for where the rows that initially had NAs
#Grouping Colors together based on research found from diamond experts groups were based on their input
#Grouping Clarity together based on research found from diamond experts groups were based on their input

diamond <- sqldf("select *
                   , Case when Depth is NULL or Depth = 0.0 then Round((Depth2/Width)*100,1) else Depth end as Depth_imputed
                   , Case when Depth is NULL or Depth = 0.0 then 1 else 0 end as Depth_imputed_flag
                   , CASE WHEN Color in ('D', 'E', 'F') or Color like 'F%' then 'Colorless'
                          WHEN Color in ('G', 'H', 'I', 'J') or Color like 'G%' then 'NearColorless'
                          WHEN Color in ('K', 'L', 'M') or Color like 'L%' then 'FaintColor'
                          Else 'ColoredDiamond' end as Color_Adj
                   , CASE 
                          when Clarity in ('I1', 'I2', 'I3', 'N') then 'Included'
                          when Clarity in ('IF', 'FL') or Clarity like 'None' then 'Internally Flawless'
                          when Clarity in ('SI1', 'SI2') then 'Slightly Included'
                          when Clarity in ('VS1', 'VS2') then 'Very Slightly Included'
                          when Clarity in ('VVS1', 'VVS2') then 'Very Very Slightly Included'
                          else 'Other' end as Clarity_Adj
                          
           from diamond")

#Trying to Table input median value for Table grouped by shape but each same had the same median Table value
#so Table overall Median was inputted instead of median by Shape

max(diamond$Table, na.rm = TRUE)
min(diamond$Table, na.rm = TRUE)

diamond %>%
  group_by(Shape)%>%
  summarise(median(Table, na.rm = TRUE))

diamond[, "TABLE_MISSING_FLAG"] <-  as.numeric(is.na(diamond$Table))

diamond <- diamond %>%
  mutate(Table = impute_median(diamond$Table))

#______________#
#Fixing Shape 
#______________#
#values needed capitalized as some had all caps
unique(diamond$Shape)
n_distinct(diamond$Shape)
diamond$Shape <- toupper(diamond$Shape)
#______________#
#Fixing and Imputing Missing values for Symmetry 
#______________#
#one value was misspelled
unique(diamond$Symmetry)
diamond %>%
  filter(Symmetry == "Execllent") %>%
  nrow()

diamond <- diamond %>%
  mutate(Symmetry = replace(Symmetry,Symmetry == "Execllent", "Excellent"))


#Imputing Missing
diamond <- diamond %>%
  mutate(Symmetry = impute_m(diamond$Symmetry))
#Values needed capitalized as some had all caps while others did not
diamond$Symmetry <- toupper(diamond$Symmetry)

#______________#
#Imputing Cut
#______________#
diamond <- diamond %>%
  mutate(Cut = impute_m(diamond$Cut))
#______________#
#Imputing Polish
#______________#
diamond <- diamond %>%
  mutate(Polish = impute_m(diamond$Polish))

#______________________________________________________________________________#
#Making sure that the columns were either numeric or factors so modeling would be easier
diamond <- diamond %>%
  mutate(LogPrice = as.numeric(LogPrice),
         Price = as.numeric(Price),
         Carats = as.numeric(Carats),
         Cert = as.factor(Cert),
         Clarity = as.factor(Clarity),
         Clarity_Adj = as.factor(Clarity_Adj),
         Color = as.factor(Color),
         Color_Adj = as.factor(Color_Adj),
         Cut = as.factor(Cut),
         Depth = as.numeric(Depth),
         Known_Conflict_Diamond = as.factor(Known_Conflict_Diamond),
         Length = as.numeric(Length),
         Width  = as.numeric(Width),
         Depth2 = as.numeric(Depth2),
         Polish = as.factor(Polish),
         Regions = as.factor(Regions),
         Shape = as.factor(Shape),
         Symmetry = as.factor(Symmetry),
         Table = as.numeric(Table),
         Vendor = as.factor(Vendor),
         Depth_imputed = as.numeric(Depth_imputed),
         Depth_imputed_flag = as.factor(Depth_imputed_flag),
         TABLE_MISSING_FLAG = as.factor(TABLE_MISSING_FLAG)
  )

#Splitting dataframe into training and Validation/Test sets by id column
train <- diamond %>% sample_frac(0.7)
testing <- anti_join(diamond, train, by = 'id')


#______________________________________________________________________________#

#                               MODEL CREATION

#______________________________________________________________________________#


#______________________________________________________________________________#
#Random Forest
#Initial Model with all columns in it
rf_diamond <- randomForest(Price ~ Carats + Clarity_Adj + Color_Adj + Cut + Known_Conflict_Diamond + Length + Width +
                           Depth2 + Polish + Regions + Shape + Symmetry + Table + Depth_imputed +
                           Depth_imputed_flag + TABLE_MISSING_FLAG
                         , data=train, ntree = 500,
                         importance = TRUE)
#Variable Importance Plot
#This is shows the most important variables to help the model predict Price
varImpPlot(rf_diamond,
           sort = TRUE,
           n.var = 10,
           main = "Look for Variables Below Random Variable"
)

# This plot shows how MSE decreases and smoothes out as the # of trees increases
plot(rf_diamond, main = "Number of Trees Compared to MSE")

#Creating another train dataset so the original isn't disturbed
train_select <- train %>% select(Price, Carats , Clarity_Adj , Color_Adj , Cut , Known_Conflict_Diamond , Length , Width ,
                          Depth2 , Polish , Regions , Shape , Symmetry , Table , Depth_imputed ,
                          Depth_imputed_flag , TABLE_MISSING_FLAG)
#Tuning Random Forest
#Finding the best Random Forest Parameters
tuneRF(x = train_select[,-1], y = train_select[,1],
       plot = TRUE, ntreeTry = 300, stepFactor = .5)

#Addinga column to the new data set with Random values from a normal distribution
train_select$random <- rnorm(5635)

#Creating a new model with random this way we can see if any columns have less importance than a random variable
rf_diamond_VS <- randomForest(Price ~ Carats + Clarity_Adj + Color_Adj + Cut + Known_Conflict_Diamond + Length + Width +
                              Depth2 + Polish + Regions + Shape + Symmetry + Table + Depth_imputed +
                              Depth_imputed_flag + TABLE_MISSING_FLAG + random
                            , data=train_select,
                            ntree = 500, mtry = 10, importance = TRUE)
#Variable Importance Plot2
varImpPlot(rf_diamond_VS,
           sort = TRUE,
           n.var = 15,
           main = "Look for Variables Below Random Variable"
)
importance((rf_diamond_VS))


#FINAL MODEL 
#Removed most variables as they were less important than the random variable created
rf_diamond2 <- randomForest(Price ~ Carats + Clarity_Adj + Color_Adj + Length + Width +
                            Depth2 + Symmetry, data=train,
                          ntree = 500, mtry = 7, importance = TRUE)
#Variable Importance same as the plot, but just the numbers
importance(rf_diamond2)

#predicting the values of the test set
predictions <- predict(rf_diamond2, newdata = testing, type = "response")



#_____________________________#
#MODEL EVALUATION
#_____________________________#

# Calculate the mean absolute error
MAE <- mean(abs(predictions - testing$Price))
print(paste("Mean Absolute Error:", MAE))

# Calculate the mean squared error
MSE <- mean((predictions - testing$Price)^2)
print(paste("Mean Squared Error:", MSE))

# Calculate R-squared value
SSR <- sum((predictions - mean(testing$Price))^2)
SST <- sum((testing$Price - mean(testing$Price))^2)
R_squared <- SSR/SST
print(paste("R-squared Value:", R_squared))

#Partial Plot to help interpret the relationship between carat and price
partialPlot(rf_diamond2, train,Carats)


View(data.frame(id = testing$id, actual = testing$Price, predicted = predictions, diff = testing$Price-predictions))
#______________________________________________________________________________#






#______________________________________________________________________________#
#XGBOOST
#Creating a matrix to use for the XGBOOST
train_x <- model.matrix(Price ~ Carats + Clarity_Adj + Color_Adj + Cut + Known_Conflict_Diamond + Length + Width +
                          Depth2 + Polish + Regions + Shape + Symmetry + Table + Depth_imputed +
                          Depth_imputed_flag + TABLE_MISSING_FLAG, data = train)[,-1]

#Traget Variable for model
train_y <- train$Price

#BUILDING INITIAL MODEL
set.seed(123)
xgb_insur <- xgboost(data =train_x, label= train_y, subsample =.5, nrounds = 100)

#CROSS VALIDATION MODEL 
set.seed(123)
xgb_insur2 <- xgb.cv(data =train_x, label= train_y, subsample =.5, nrounds = 100, nfold= 10)
summary(xgb_insur2)

#TUNING OUR MODEL 
tune_grid <- expand.grid(
  nrounds = 10, #CHOOSE THE NUMBER OF ROUNDS THAT IS BEST FOR YOUR MODEL
  eta = c(0.1, 0.15, 0.2, 0.25, 0.3),
  max_depth = c(1:10),
  gamma = c(0),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = c(0.25, 0.5, 0.75, 1)
)

set.seed(123)
xgb.insur.caret <- train(x = train_x, y = train_y,
                         method = "xgbTree",
                         tuneGrid = tune_grid,
                         trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                                  number = 10))
#Ploting our graphs to see how the tuning affected each model 
plot(xgb.insur.caret)

#You can determine the best tune from the plots above, but this is the simplest way to see the best parameters
xgb.insur.caret$bestTune

#MODEL AFTER TUNING
xgb_insur3 <- xgboost(data =train_x, label= train_y, subsample =1, nrounds = 10, eta = .3, max_depth = 4, objective = "reg:squarederror")

#Variable Importance Plot
xgb.importance(feature_names = colnames(train_x), model = xgb_insur3)
xgb.ggplot.importance(xgb.importance(feature_names = colnames(train_x), model = xgb_insur3))


#Random Variable if needed for Variable selection
#train$random <- rnorm(5635)

#VARIABLE SELECTION PROCESS
#Creating a new matrix with random variable
train_x_VS <- model.matrix(Price ~ Carats + Clarity_Adj + Color_Adj + Cut + Known_Conflict_Diamond + Length + Width +
                             Depth2 + Polish + Regions + Shape + Symmetry + Table + Depth_imputed +
                             Depth_imputed_flag + TABLE_MISSING_FLAG + random, data = train)[,-1]

#Creating the validation Model with the Parameters above
xgb_insur_VS <- xgboost(data =train_x_VS, label= train_y, subsample =1, nrounds = 10, eta = .3, max_depth = 4, objective = "reg:squarederror")

#Variable Importance Plot
xgb.ggplot.importance(xgb.importance(feature_names = colnames(train_x_VS), model = xgb_insur_VS))
xgb.importance(feature_names = colnames(train_x_VS), model = xgb_insur_VS)


#Final Model
#Creating the Model with the Parameters above
train_x_final <- model.matrix(Price ~ Carats + Clarity_Adj + Color_Adj + Length + Width +
                             Depth2, data = train)[,-1]

xgb_diamond_final <- xgboost(data =train_x_final, label= train_y, subsample =.1, nrounds = 10, eta = .3, max_depth = 4, objective = "reg:squarederror")

#Creating this test matrix to get the predictions
test_x <- model.matrix(Price ~ Carats + Clarity_Adj + Color_Adj + Length + Width +
                         Depth2, data = testing)[,-1]

#Predicting the values in test matrix so we can measure our model
predictions2 <- predict(xgb_diamond_final, newdata = test_x, type = "response")

#Making sure names were similar
#xgb_diamond_final[["feature_names"]]
#colnames(test_x)


#_____________________________#
#MODEL EVALUATION
#_____________________________#
# Calculate the mean absolute error
MAE2 <- mean(abs(predictions2 - testing$Price))
print(paste("Mean Absolute Error:", MAE2))

# Calculate the mean squared error
MSE2 <- mean((predictions2 - testing$Price)^2)
print(paste("Mean Squared Error:", MSE2))

# Calculate R-squared value
SSR2 <- sum((predictions2 - mean(testing$Price))^2)
SST2 <- sum((testing$Price - mean(testing$Price))^2)
R_squared2 <- SSR2/SST2
print(paste("R-squared Value:", R_squared2))



#Partial Plot to help interpret the relationship between carat and price
partial(xgb_diamond_final, pred.var = "Carats",
        plot = TRUE, rug = TRUE, alpha = 0.1,
        plot.engine = "lattice", train = train_x_final)





View(data.frame(actual = testing$Price, predicted = predictions2, diff = testing$Price-predictions2))


View(data.frame(id = testing$id, actual = testing$Price, RFpredicted = predictions, XGBpredicted = predictions2, RFdiff = testing$Price-predictions, XGBdiff = testing$Price-predictions2))
#______________________________________________________________________________#
#Comparing Models
print(paste("XGB Mean Absolute Error:", MAE2))
print(paste("RF Mean Absolute Error:", MAE))

# Calculate the mean squared error
print(paste("XGB Mean Squared Error:", MSE2))
print(paste("RF Mean Squared Error:", MSE))

# Calculate R-squared value
print(paste("XGB R-squared Value:", R_squared2))
print(paste("RF R-squared Value:", R_squared))



#______________________________________________________________________________#
#Random Forest for LogPrice
#Initial Model with all columns in it
rf_diamondLP <- randomForest(LogPrice ~ Carats + Clarity_Adj + Color_Adj + Cut + Known_Conflict_Diamond + Length + Width +
                             Depth2 + Polish + Regions + Shape + Symmetry + Table + Depth_imputed +
                             Depth_imputed_flag + TABLE_MISSING_FLAG
                           , data=train, ntree = 500,
                           importance = TRUE)

varImpPlot(rf_diamondLP,
           sort = TRUE,
           n.var = 10,
           main = "Look for Variables Below Random Variable"
)

plot(rf_diamondLP, main = "Number of Trees Compared to MSE")

train_selectLP <- train %>% select(LogPrice, Carats , Clarity_Adj , Color_Adj , Cut , Known_Conflict_Diamond , Length , Width ,
                                 Depth2 , Polish , Regions , Shape , Symmetry , Table , Depth_imputed ,
                                 Depth_imputed_flag , TABLE_MISSING_FLAG)
#Tuning Random Forest
tuneRF(x = train_selectLP[,-1], y = train_selectLP[,1],
       plot = TRUE, ntreeTry = 300, stepFactor = 0.5)

train_selectLP$random <- rnorm(5635)


rf_diamond_VSLP <- randomForest(LogPrice ~ Carats + Clarity_Adj + Color_Adj + Cut + Known_Conflict_Diamond + Length + Width +
                                Depth2 + Polish + Regions + Shape + Symmetry + Table + Depth_imputed +
                                Depth_imputed_flag + TABLE_MISSING_FLAG + random
                              , data=train_selectLP,
                              ntree = 300, mtry = 10, importance = TRUE)
varImpPlot(rf_diamond_VSLP,
           sort = TRUE,
           n.var = 11,
           main = "Look for Variables Below Random Variable"
)

#FINAL MODEL 
rf_diamond2LP <- randomForest(LogPrice ~ Carats + Clarity_Adj + Color_Adj + Length + Width +
                              Depth2, data=train,
                            ntree = 300, mtry = 6, importance = TRUE)
importance(rf_diamond2)
summary(rf_diamond2)


typeof(list(rf_diamond2$predicted))
sapply(testing, typeof)
sapply(train, typeof)
str(testing2)
str(train)
unique(train$Color_Adj)

#_____________________________#
#MODEL EVALUATION
#_____________________________#

predictions3 <- predict(rf_diamond2LP, newdata = testing, type = "response")

# Calculate the mean absolute error
MAE3 <- mean(abs(predictions3 - testing$LogPrice))
print(paste("Mean Absolute Error:", MAE3))

# Calculate the mean squared error
MSE3 <- mean((predictions3 - testing$LogPrice)^2)
print(paste("Mean Squared Error:", MSE3))

# Calculate R-squared value
SSR3 <- sum((predictions3 - mean(testing$LogPrice))^2)
SST3 <- sum((testing$LogPrice - mean(testing$LogPrice))^2)
R_squared3 <- SSR3/SST3
print(paste("R-squared Value:", R_squared3))

View(data.frame(actual = testing$LogPrice, predicted = predictions3, diff = testing$LogPrice-predictions3))

View(data.frame(actual = testing$Price, predicted = exp(predictions3), diff = testing$Price-exp(predictions3)))




predictions4 <- exp(predictions3)

#train$prob <- predict(rf_diamond2, type = "prob")[,2]
#plotROC(train$Price, train$prob)
#AUROC(train$Price, training$prob)

# Calculate the mean absolute error
MAE4 <- mean(abs(predictions4 - testing$Price))
print(paste("Mean Absolute Error:", MAE4))
# Calculate the mean squared error
MSE4 <- mean((predictions4 - testing$Price)^2)
print(paste("Mean Squared Error:", MSE4))

# Calculate R-squared value
SSR4 <- sum((predictions4 - mean(testing$Price))^2)
SST4 <- sum((testing$Price - mean(testing$Price))^2)
R_squared4 <- SSR4/SST4
print(paste("R-squared Value:", R_squared4))

print(paste("R-squared Value:", R_squared))




#______________________________________________________________________________#





#______________________________________________________________________________#
#Final Predictions 
#Bringing in Offers as the final data frame
offers <- read_csv("ENOVA/offers.csv")
offers <- data.frame(offers)

colnames(offers)
sapply(offers, typeof)



#______________________________________________________________________________#
#Turning Measurements into 3 separate columns so I can work with Continuous Data
#Making sure the new values are read in as doubles/numeric
offers$Measurements <- chartr('*', "x", offers$Measurements)
offers <- separate_wider_delim(offers, cols = Measurements, delim = "x", names = c("Length", "Width", "Depth2")) #, too_few = "debug")
offers$Length <- as.double(offers$Length)
offers$Depth <- as.double(offers$Depth)
offers$Depth2 <- as.double(offers$Depth2)
offers$Width <- as.double(offers$Width)
#______________________________________________________________________________#



#______________________________________________________________________________#
#Checking for Missing values for each column
MV2 <-NULL
for (i in 1:18){
  MV2[i] <- sum(is.na(offers[i]))
  print(names(offers)[i])
  print(MV2[i])
}
#MISSING VALUES
#CUT 3922 CHARACTER
#DEPTH 1440 DOUBLE
#POLISH 899 CHARACTER
#SYMMETRY 899 CHARACTER
#TABLE 2531 DOUBLE

#WHEN Clarity like 'FL' then 'Flawless'
#______________________________________________________________________________#
offers <- sqldf("select *
                   , Case when Depth is NULL or Depth = 0.0 then Round((Depth2/Width)*100,1) else Depth end as Depth_imputed
                   , Case when Depth is NULL or Depth = 0.0 then 1 else 0 end as Depth_imputed_flag
                   , CASE WHEN Color in ('D', 'E', 'F') or Color like 'F%' then 'Colorless'
                          WHEN Color in ('G', 'H', 'I', 'J') or Color like 'G%' then 'NearColorless'
                          WHEN Color in ('K', 'L', 'M') or Color like 'L%' then 'FaintColor'
                          Else 'ColoredDiamond' end as Color_Adj
                   , CASE when Clarity in ('I1', 'I2', 'I3', 'N') then 'Included'
                          when Clarity in ('IF', 'FL') or Clarity like 'None' then 'Internally Flawless'
                          when Clarity in ('SI1', 'SI2') then 'Slightly Included'
                          when Clarity in ('VS1', 'VS2') then 'Very Slightly Included'
                          when Clarity in ('VVS1', 'VVS2') then 'Very Very Slightly Included'
                          else 'Other' end as Clarity_Adj
                from offers")



offers <- offers %>%
  mutate(Symmetry = impute_m(offers$Symmetry))

offers$Symmetry <- toupper(offers$Symmetry)

#______________________________________________________________________________#
#Making sure that the columns were either numeric or factors so modeling would be easier
offers <- offers %>%
  mutate(Carats = as.numeric(Carats),
         Cert = as.factor(Cert),
         Clarity = as.factor(Clarity),
         Clarity_Adj = as.factor(Clarity_Adj),
         Color = as.factor(Color),
         Color_Adj = as.factor(Color_Adj),
         Cut = as.factor(Cut),
         Depth = as.numeric(Depth),
         Known_Conflict_Diamond = as.factor(Known_Conflict_Diamond),
         Length = as.numeric(Length),
         Width  = as.numeric(Width),
         Depth2 = as.numeric(Depth2),
         Polish = as.factor(Polish),
         Regions = as.factor(Regions),
         Shape = as.factor(Shape),
         Symmetry = as.factor(Symmetry),
         Table = as.numeric(Table),
         Vendor = as.factor(Vendor),
         Depth_imputed = as.numeric(Depth_imputed),
         Depth_imputed_flag = as.factor(Depth_imputed_flag),
  )

#Bringing only the columns used in the model
x_final <- offers %>% select(Carats, Clarity_Adj, Color_Adj, Length , Width ,
                            Depth2, Symmetry)

#Predicting the prce of the offers diamonds with the original random forest model
final_predictions <- predict(rf_diamond2, newdata = x_final, type = "response")

#Creating a data frame
offers_final <- data.frame(offers, predictions = final_predictions)

#______________________________________________________________________________#

diamond2 <- diamond %>%
  mutate(Retail = as.numeric(Retail),
         Carats = as.numeric(Carats),
         Cert = as.factor(Cert),
         Clarity = as.factor(Clarity),
         Clarity_Adj = as.factor(Clarity_Adj),
         Color = as.factor(Color),
         Color_Adj = as.factor(Color_Adj),
         Cut = as.factor(Cut),
         Depth = as.numeric(Depth),
         Known_Conflict_Diamond = as.factor(Known_Conflict_Diamond),
         Length = as.numeric(Length),
         Width  = as.numeric(Width),
         Depth2 = as.numeric(Depth2),
         Polish = as.factor(Polish),
         Regions = as.factor(Regions),
         Shape = as.factor(Shape),
         Symmetry = as.factor(Symmetry),
         Table = as.numeric(Table),
         Vendor = as.factor(Vendor),
         Depth_imputed = as.numeric(Depth_imputed),
         Depth_imputed_flag = as.factor(Depth_imputed_flag),
         TABLE_MISSING_FLAG = as.factor(TABLE_MISSING_FLAG)
  )

#Splitting dataframe into training and Validation/Test sets by id column
train2 <- diamond2 %>% sample_frac(0.7)
testing2 <- anti_join(diamond2, train2, by = 'id')
#______________________________________________________________________________#




#______________________________________________________________________________#
#Random Forest For Offers
#Initial Model with all columns in it
rf_diamond_o <- randomForest(Retail ~ Carats + Clarity_Adj + Color_Adj + Cut + Known_Conflict_Diamond + Length + Width +
                             Depth2 + Polish + Regions + Shape + Symmetry + Table + Depth_imputed +
                             Depth_imputed_flag + TABLE_MISSING_FLAG
                           , data=train2, ntree = 500,
                           importance = TRUE)
#Variable Importance Plot
#This is shows the most important variables to help the model predict Price
varImpPlot(rf_diamond_o,
           sort = TRUE,
           n.var = 10,
           main = "Look for Variables Below Random Variable"
)

# This plot shows how MSE decreases and smoothes out as the # of trees increases
plot(rf_diamond_o, main = "Number of Trees Compared to MSE")

#Creating another train2 dataset so the original isn't disturbed
train2_select <- train2 %>% select(Retail, Carats , Clarity_Adj , Color_Adj , Cut , Known_Conflict_Diamond , Length , Width ,
                                 Depth2 , Polish , Regions , Shape , Symmetry , Table , Depth_imputed ,
                                 Depth_imputed_flag , TABLE_MISSING_FLAG)
#Tuning Random Forest
#Finding the best Random Forest Parameters
tuneRF(x = train2_select[,-1], y = train2_select[,1],
       plot = TRUE, ntreeTry = 300, stepFactor = .5)

#Adding a column to the new data set with Random values from a normal distribution
train2_select$random <- rnorm(5635)

#Creating a new model with random this way we can see if any columns have less importance than a random variable
rf_diamond_o_VS <- randomForest(Retail ~ Carats + Clarity_Adj + Color_Adj + Cut + Known_Conflict_Diamond + Length + Width +
                                Depth2 + Polish + Regions + Shape + Symmetry + Table + Depth_imputed +
                                Depth_imputed_flag + TABLE_MISSING_FLAG + random
                              , data=train2_select,
                              ntree = 500, mtry = 10, importance = TRUE)
#Variable Importance Plot2
varImpPlot(rf_diamond_o_VS,
           sort = TRUE,
           n.var = 15,
           main = "Look for Variables Below Random Variable"
)
importance((rf_diamond_o_VS))


#FINAL MODELS for Retail Price
#Removed most variables as they were less important than the random variable created
rf_diamond_o2 <- randomForest(Retail ~ Carats + Clarity_Adj + Color_Adj + Length + Width +
                              Depth2 + Symmetry + Known_Conflict_Diamond +Regions, data=train2,
                            ntree = 500, mtry = 7, importance = TRUE)


rf_diamond_o3 <- randomForest(Retail ~ Carats + Clarity_Adj + Color_Adj + Length + Width +
                                Depth2 + Symmetry + Known_Conflict_Diamond +Regions, data=train2,
                              ntree = 500, mtry = 7, importance = TRUE)
#Variable Importance same as the plot, but just the numbers
importance(rf_diamond_o2)

predictions5 <- predict(rf_diamond_o2, newdata = testing2, type = "response")

MAE5 <- mean(abs(predictions5 - testing$Retail))
print(paste("Mean Absolute Error:", MAE5))
# Calculate the mean squared error
MSE5 <- mean((predictions5 - testing$Retail)^2)
print(paste("Mean Squared Error:", MSE5))

# Calculate R-squared value
SSR5 <- sum((predictions5 - mean(testing$Retail))^2)
SST5 <- sum((testing$Retail - mean(testing$Retail))^2)
R_squared5 <- SSR5/SST5
print(paste("R-squared Value:", R_squared5))

print(paste("R-squared Value:", R_squared))

#predicting the values of the test set
x_final2 <- offers %>% select(Carats, Clarity_Adj, Color_Adj, Length , Width ,
                             Depth2, Symmetry, Known_Conflict_Diamond, Regions)

predictionso <- predict(rf_diamond_o2, newdata = x_final2, type = "response")

offers_final <- data.frame(offers, predictions = final_predictions, retail_pred = predictionso, gain = predictionso-final_predictions)

write_xlsx(offers_final, "C:\\Users\\mikey\\OneDrive\\Documents\\ENOVA\\offers_final.xlsx")
write_xlsx(diamond, "C:\\Users\\mikey\\OneDrive\\Documents\\ENOVA\\diamond.xlsx")
#______________________________________________________________________________#





#______________________________________________________________________________#
#Short answer Problems Analysis
#Problem 1

#Median Price by Vendor
diamond %>%
  group_by(Vendor)%>%
  summarise(median(Price, na.rm = TRUE))

#Median Carat by Vendor
diamond %>%
  group_by(Vendor)%>%
  summarise(median(Carats, na.rm = TRUE))

#Mean Price by Vendor
diamond %>%
  group_by(Vendor)%>%
  summarise(mean(Price, na.rm = TRUE))

res_aov <- aov(Price ~ Vendor, data = diamond)
summary(res_aov)


par(mfrow = c(1, 2)) # combine plots

# histogram
hist(res_aov$residuals)

# QQ-plot
qqPlot(res_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)


crosstab(Color_Adj, Vendor, data = diamond)
crosstab(Clarity_Adj, Vendor, data = diamond)
crosstab(Shape, Vendor, data = diamond)




#PROBLEM 2
cor(diamond$Carats, diamond$Price)

#Median Price by Carat Weight Class
diamond %>%
  group_by(round(Carats,digits = 0))%>%
  summarise(median(Price, na.rm = TRUE))

#Mean Price by Carat Weight Class 
diamond %>%
  group_by(round(Carats,digits = 0))%>%
  summarise(mean(Price, na.rm = TRUE))

#Plot showing the relationship between Carats and Price
ggplot(diamond, aes(x = Carats, y = Price))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)




#Extremely Helpful Charts to help me select which Diamonds to Purchase after looking at the biggest gain

View(diamond %>%
  group_by(Color_Adj,round(Carats,digits = 0))%>%
  summarise(median(Price, na.rm = TRUE)))

View(diamond %>%
       group_by(Clarity_Adj,round(Carats,digits = 0))%>%
       summarise(median(Price, na.rm = TRUE)))

diamond %>%
  group_by(round(Carats,digits = 0))%>%
  summarise(median(Price, na.rm = TRUE))

diamond %>%
  group_by(round(Depth2,digits = 0))%>%
  summarise(median(Price, na.rm = TRUE))

diamond %>%
  group_by(Regions)%>%
  summarise(median(Price, na.rm = TRUE))

diamond %>%
  group_by(Clarity_Adj)%>%
  summarise(median(Price, na.rm = TRUE))