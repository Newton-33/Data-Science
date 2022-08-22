library(odbc)
library(dplyr)
library(factoextra)
library(Hmisc)
library(skimr)
library(corrplot)

con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "DESKTOP-MOH7EQS",
                 Database = "HomePrediction",
                 Port = 1433)
traindata <- tbl(con, 'train')
traindata <-collect(traindata)
View(traindata)
summary(traindata)
str(traindata)


testdata  <- tbl(con, 'test')
testdata <-collect(testdata)
View(testdata)
summary(testdata)

#Getting the no of na in individual coluns
is.na(traindata)
 colSums(is.na(traindata))

sum(is.na(traindata))
View(traindata%>%summarise_all(~sum(is.na(.))))
which(colSums(is.na(traindata))> 0)
names(which(colSums(is.na(traindata)) > 0))


#Transform columns to numeric



traindata <- traindata %>% mutate_at(c('Id','LotFrontage', 'MasVnrArea',
                                       'BsmtFinSF1','BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF',
                                       'MoSold', '_2ndFlrSF', 'LowQualFinSF', 'BsmtFullBath',
                                       'FullBath','BedroomAbvGr','BsmtHalfBath', 'KitchenAbvGr',
                                       'PoolArea','GarageCars', 'GarageArea', 'WoodDeckSF',
                                       'OpenPorchSF', 'EnclosedPorch','_3SsnPorch',
                                       'ScreenPorch', 'Fireplaces', 'MiscVal'), as.numeric)

testdata <- testdata %>% mutate_at(c('Id','LotFrontage', 'MasVnrArea',
                                       'BsmtFinSF1','BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF',
                                       'MoSold', '_2ndFlrSF', 'LowQualFinSF', 'BsmtFullBath',
                                       'FullBath','BedroomAbvGr','BsmtHalfBath', 'KitchenAbvGr',
                                       'PoolArea','GarageCars', 'GarageArea', 'WoodDeckSF',
                                       'OpenPorchSF', 'EnclosedPorch','_3SsnPorch',
                                       'ScreenPorch', 'Fireplaces', 'MiscVal'), as.numeric)

#Replace NA with median in traindata
str(traindata)
median(traindata$LotFrontage, na.rm=T)
median(traindata$MasVnrArea, na.rm=T)



traindata$LotFrontage[is.na(traindata$LotFrontage)] <-median(traindata$LotFrontage, na.rm = T)
traindata$MasVnrArea[is.na(traindata$MasVnrArea)] <-median(traindata$MasVnrArea, na.rm = T)

#Replace NA with median in testdata
testdata$LotFrontage[is.na(testdata$LotFrontage)] <-median(testdata$LotFrontage, na.rm = T)
testdata$MasVnrArea[is.na(testdata$MasVnrArea)] <-median(testdata$MasVnrArea, na.rm = T)




#Removing empty values in df
cleaned_traindata <- na.omit(traindata)
cleaned_testdata <- na.omit(testdata)

#Slicing relevant numeric columns

df_train <- select(cleaned_traindata, c(1,60,4,27,35,37,38,39,45,46,48,50,51,52,49,53,57,62,63,67,68,69,70,71,72,76,77))
lapply(df_train, class)
View(df_train)

df_test <- select(cleaned_testdata, c(1,60,4,27,35,37,38,39,45,46,48,50,51,52,49,53,57,62,63,67,68,69,70,71,72,76,77))
lapply(df_test, class)
View(df_test)



# Check for correlation
cor(df)
str(traindata)
corrplot(cor(df))



#PCA

dftrain_pca <- prcomp(df_train, scale=TRUE, center=TRUE, retx=T)
View(dftrain_pca)
summary(dftrain_pca)

dftest_pca <- prcomp(df_test, scale=TRUE, center=TRUE, retx=T)
View(dftest_pca)
summary(dftest_pca)


#multiple linear reg. uses all columns in the data 
reg1= lm(cleaned_traindata$SalePrice~., data=df_train)
summary(reg1)

reg2= lm(df_test$SalePrice~., data=df_test)
summary(reg2)

#prediction
dff <- data.frame(df_train)
Predicted_Sales_Price=predict(reg1,df_train)
summary(prediction1)
View(prediction1)


dff <- data.frame(df_train)
Predicted_Sales_Price=predict(reg1,df_test)
summary(prediction1)
View(prediction1)


write.csv(Predicted_Sales_Price, "C:\\Users\\HP\\Documents\\Price.csv", row.names = TRUE)
