# Insurance
predicting company insurance premium

# Install required packages

install.packages("xlsx")
install.packages("corrplot")
install.packages("randomForest")
require(xlsx)
require(ggplot2)
require(dplyr)
require(corrplot)
require(randomForest)

getwd()
setwd("C:/Users/LONPT12/Desktop/Training/Data Science Bootcamp 2/Kaggle/Insurance")

data_original <- read.xlsx("Task_2.xlsx","Sheet1")
head(data_original)
summary(data_original)

# options
options(scipen=999)

# remove NA
data_no_na <- data_original[-which(is.na(data_original$NewPolicyNumber)),]
summary(data_no_na)

data_no_na[is.na(data_no_na$NewPolicyNumber),]
sum(is.na(data_original$NewPolicyNumber))

data_original[which(is.na(data_original$NewPolicyNumber)),]

# ---------Explore variables

#Comission
hist(data_no_na$Commission)
data_no_na$Commission <- as.numeric(data_no_na$Commission)

#Sum_Insured
boxplot(data_no_na$Sum_Insured[data_no_na$Sum_Insured < 150000000])
summary(data_no_na$Sum_Insured)
data_no_na[data_no_na$Sum_Insured > 140000000,]
data_no_na$Sum_Insured[data_no_na$Sum_Insured == 150000000] <- median(data_no_na$Sum_Insured)
boxplot(data_no_na$Sum_Insured)

#Excess
data_no_na[data_no_na$Excess>0,]

#Turnover
summary(data_no_na$Turnover)
boxplot(data_no_na$Turnover)

#No Claim
summary(data_no_na$No_Claims)
data_no_na$No_Claims <- as.factor(data_no_na$No_Claims)
table(data_no_na$No_Claims)

plot(data_no_na$Premium, data_no_na$Policy_Premium, main="Scatterplot Example")
boxplot(data_no_na$Policy_Premium)

head(data_no_na[,c("Premium","Policy_Premium")])
head(data_no_na)
sum(data_no_na$Premium) > sum(data_no_na$Policy_Premium)

# policy length
data_no_na$pol_length <- as.numeric(data_no_na$EndDate - data_no_na$InceptionDate)

for (i in 1:nrow(data_no_na)) {
  if(data_no_na$pol_length[i]<360) {
    data_no_na$pol_length[i] <- "Less_Y"
  } else if (data_no_na$pol_length[i]>360 & data_no_na$pol_length[i]<400) {
    data_no_na$pol_length[i] <- "Y"
  } else {
    data_no_na$pol_length[i] <- "More_Y"
  }
}
table(data_no_na$pol_length)
data_no_na$pol_length <- as.factor(data_no_na$pol_length)

# check for correlation
cor_data <- data_no_na[,c("Sum_Insured","Excess","Turnover","Incurred")]
head(cor_data)
cor_m <- cor(cor_data)
corrplot(cor_m,method = "number",type = "upper")


# scale and create regression dataset
#reg_data <- data_no_na[,c("Premium","Sum_Insured","Turnover","No_Claims","SITUATION.","pol_length")]

scaledat <- data_no_na[,c("Sum_Insured","Turnover")]
scaledat_scaled <- scale(scaledat)
factordata <- data_no_na[,c("Premium","No_Claims","SITUATION.","pol_length")]
reg_data <- cbind(factordata,scaledat_scaled)

# split train and test set
set.seed(123)
smpl_size <- floor(0.75*nrow(reg_data))
smpl_ind <- sample(seq_len(nrow(reg_data)),size = smpl_size)
train <- reg_data[smpl_ind,]
test <- reg_data[-smpl_ind,]

# multivariate linear regression
regression_1 <- lm(Premium~.,data=train)
summary(regression_1)
plot(regression_1)
#1133 and 975

#find best model
install.packages("MASS")
library(MASS)
step <- stepAIC(regression_1,direction = "both")
step$anova

regression_2 <- lm(Premium~No_Claims,data=train)
summary(regression_2)
plot(regression_2)

# SVR regression
install.packages("e1071")
library(e1071)

svm_model <- svm(Premium~.,data = reg_data,type = "eps-regression")
summary(svm_model)

# Random Forest
rf_model <- randomForest(Premium~., data = reg_data, importance = TRUE, ntree = 2000, nodesize = 50)
varImpPlot(rf_model)

rf_pred <- predict(rf_model,test)
result <- data.frame(real_premium = test$Premium, predicted = rf_pred)
result
