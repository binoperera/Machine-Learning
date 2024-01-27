library(readxl)
#install.packages("forecast") 
library(forecast) 
library(neuralnet) 
library(caret) 
(MLmetrics)

#importing uow_load dataset
uow_load_data_set <-read_excel('d:/life/objective2/UoW_load.xlsx')


#Data pre-processing
names(uow_data)[2] <- 'seventh'
names(uow_data)[3] <- 'eighthh'
names(uow_data)[4] <- 'nineth'


dates <-factor(uow_load_data_set$Dates) #factoding data 
dates <-as.numeric(dates) #numbaric
dates


uow_df <- data.frame(dates, uow_data$'seventh', uow_data$'eightth', uow_data$'nineth')
uow_df

#normalization
uow_norm <- as.data.frame(lapply(uow_df, function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}))

names(uow_norm)[2] <- 'seventh'
names(uow_norm)[3] <- 'eightth'
names(uow_norm)[4] <- 'nineth'


#dividing data into training and testing data set
set.seed(123)
uow_norm_train <- uow_norm[1:430,]
uow_norm_test <- uow_norm[431:500,]

#________________________
#AR Approach
#________________________

#Generate Neural Network in AR
UoW_load_data_NNAR <- neuralnet(eleventh ~ dates + eleventh, hidden = c(3,2), data = uow_norm_train, linear.output = TRUE, threshold = 0.01)
plot(UoW_load_data_NNAR)

#model performance evaluation
UoW_load_modelPerform<- predict(UoW_load_data_NNAR, UoW_load_testData_normalization)
UoW_load_modelPerform

#Get trained data test dataset without normalization
UoW_load_trainData <- UoW_load_data[1:430,"nineth"]
UoW_load_testData <- UoW_load_data[431:500,"nineth"]

#find minimum and maximum values of the train data set
trained_min <- min(UoW_load_trainData)
trained_max <- max(UoW_load_testData)

#un-normalizing the data

unNormalized <- function(x, min, max) {
  return ((max - min)*x + min)
}

predUnnorm <- unNormalized(UoW_load_modelPerform, trained_min, trained_max)
predUnnorm

#Testing performance with RMSE
RMSE(exp(predUnnorm), UoW_load_testData$nineth)

#testing performance with MSE
MSE(exp(predUnnorm), UoW_load_testData$nineth)

#testing performance with MAPE
MAPE(exp(predUnnorm), UoW_load_testData$nineth)

#correlation between predicted and actual values
cor(predUnnorm, UoW_load_testData$nineth)

#generate nineth hour plot

par(mfrow = c(1,1))

plot(UoW_load_testData$nineth, UoW_load_predicted_unNormalized, col = "red", main = "Unnormalized Prediction Graph AR", pch = 18, cex = 0.7)
abline(0, 1, lwd = 2)
legend("bottomright", legend = "NN", pch = 18, col = "red", bty = "m")

UoW_load_finalResult <- cbind(UoW_load_testData, UoW_load_predicted_unNormalized)
UoW_load_finalResult

plot(UoW_load_testData$eleventh, ylab = "Predicted vs Expected AR", type = "l", col = "yellow")
par(new = TRUE)

plot(UoW_load_predicted_unNormalized, ylab = "", yaxt = "m", type = "l", col = "red", main = "Predicted val vs Expected val AR")
legend("lopleft", c("Expected", "Predicted"), fill = c("yellow", "red")) #ERROR

#Calculate Accuracy
UowDataset_predicted = UoW_load_modelPerform * abs(diff(range(UoW_load_testData_normalization$eleventh))) + min(UoW_load_testData_normalization$eleventh)

UowDataset_actual = UoW_load_testData_normalization$eleventh * abs(diff(range(UoW_load_testData_normalization$eleventh))) + min(UoW_load_testData_normalization$eleventh)

predict_actual_comparison = data.frame(UowDataset_predicted, UowDataset_actual) #Remove

Data_deviation = ((UowDataset_actual - UowDataset_predicted) / UowDataset_actual)
Data_deviation
is.na(Data_deviation) <- sapply(Data_deviation, is.infinite)
Data_deviation

Data_deviation_OmitNA <- na.omit(Data_deviation)
Data_deviation_OmitNA

comparison = data.frame(UowDataset_predicted, UowDataset_actual, Data_deviation)
Accuracy_level = 1 - abs(mean(Data_deviation_OmitNA))
Accuracy_level

#________________________
#NARX Approach
#________________________

#Generate Neural Network in AR

#UoW_load_NNAR <- neuralnet(t_eleven ~ day + t_eleven,hidden = c(3,2), data = UoW_load_trainData_normalization, linear.output = TRUE, threshold = 0.01)
UoW_load_narx <- neuralnet(eleventh ~ dates + nineth + tenth + eleventh , hidden = c(3,2), data = UoW_load_trainData_normalization, linear.output = TRUE, threshold = 0.01)
plot(UoW_load_narx)

#model performance evaluation
UoW_load_RX_modelPerform <- predict(UoW_load_narx, UoW_load_testData_normalization)
UoW_load_RX_modelPerform

UoW_load_predicted_NARXunNormalized <- unNormalized(UoW_load_RX_modelPerform, trained_min, trained_max)
UoW_load_predicted_NARXunNormalized

#testing performance with RMSE
RMSE(exp(UoW_load_predicted_NARXunNormalized), UoW_load_testData$eleventh)

#testing performance with MSE
MSE(exp(UoW_load_predicted_NARXunNormalized), UoW_load_testData$eleventh)

#testing performance with MAPE
MAPE(exp(UoW_load_predicted_NARXunNormalized), UoW_load_testData$eleventh)


#correlation between predicted and actual values
cor(UoW_load_predicted_NARXunNormalized, UoW_load_testData$eleventh)

#Generate 11th hour plot in NARX
par(mfrow = c(1,11))

plot(UoW_load_testData$eleventh, UoW_load_predicted_NARXunNormalized, col = "red", main = "Unnormalized Prediction Grpah", pch = 18, cex = 0.7)
abline(0, 1, lwd = 2)

UoW_load_finalNARX <- cbind(UoW_load_testData, UoW_load_predicted_NARXunNormalized)
UoW_load_finalNARX

plot(UoW_load_testData$t_eleven, ylab = '', yaxt = 'm', type = 'l', col = "red", main = 'Predict val vs Ecpected val')
legend("lopleft", c("Expected", "Predicted"), fill = c("red", "green")) #ERROR

#calculate accuracy
RX_predicted_values = UoW_load_RX_modelPerform * abs(diff(range(UoW_load_testData_normalization$eleventh))) + min(UoW_load_testData_normalization$eleventh)
RX_actual_values = UoW_load_testData_normalization$eleventh * abs(diff(range(UoW_load_testData_normalization$eleventh))) + min(UoW_load_testData_normalization$eleventh)

RX_comparison = data.frame(RX_predicted_values, RX_actual_values)

RX_deviation = ((RX_actual_values - RX_predicted_values) / RX_actual_values)
RX_deviation
is.na(RX_deviation) <- sapply(RX_deviation, is.infinite)
RX_deviation

RX_deviation_omitNA <- na.omit(RX_deviation)
RX_deviation_omitNA

RX_comparison = data.frame(RX_predicted_values, RX_actual_values, RX_deviation)
RX_accuray = 1 - abs(mean(RX_deviation_omitNA))
RX_accuray

