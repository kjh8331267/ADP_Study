

######################
# 클래스 불균형
######################
# 1. caret::upSample
#업 샘플링:데이터가 적은 쪽을 많게 추출하는 방법
#다운 샘플링: 데이터가 많은 쪽을 적게 추출하는 방법
# install.packages("mlbench")
library(mlbench)
data("BreastCancer")
# install.packages("e1071")
library(caret)
library(rpart)
library(e1071)
x <- upSample(subset(BreastCancer, select=-Class), BreastCancer$Class)
table(BreastCancer$Class)
# benign malignant 
# 458       241
table(x$Class)
# benign malignant 
# 458       458 
## 비교
data <- subset(BreastCancer, select=-Id)
parts <- createDataPartition(data$Class, p=.8)
data.train <- data[parts$Resample1, ]
data.test <- data[-parts$Resample1, ]
m <- rpart(Class ~., data=data.train)
confusionMatrix(data.test$Class,predict(m, newdata=data.test, type="class"))
# 업샘플링
data.up.train <- upSample(subset(data.train, select=-Class),data.train$Class)
m <- rpart(Class ~., data=data.up.train)
confusionMatrix(data.test$Class,predict(m, newdata = data.test, type="class"))

# 2. DMwR::SMOTE
# 분류 개수가 적은 쪽의 데이터의 샘플을 취한 뒤
# 이 샘플의 k 최근접 이웃을 찾는다. 
# 그리고 현재 샘플과 이들 k개 이웃 간의 차를 구하고,
# 이 차이에 0 ~ 1 사이의 임의의 값을 곱하여 원래 샘플에 더한다. 
# 이렇게 만든 새로운 샘플을 훈련 데이터에 추가한다. 
# 결과적으로 SMOTE는 기존의 샘플을 주변의 이웃을 고려해 
# 약간씩 이동시킨 점들을 추가하는 방식으로 동작한다.
# install.packages("DMwR")
library(DMwR)
data(iris)
data <- iris[, c(1,2,5)]
data$Species <- factor(ifelse(data$Species == "setosa", "rare", "commen"))
table(data$Species)
newData <- SMOTE(Species ~., data, perc.over = 600, perc.under=100)
table(newData$Species)
# perc.over : 개수가 적은 분류로부터 얼마나 많은 데이터를 생성해낼지를 조정하는 변수
#              보통 100 이상으로 정하는 값으로 
#              적은 쪽의 데이터 한 개당 perc.over/100개의 추가 데이터 생성

# perc.under : 개수가 많은 분류의 데이터를 얼마나 적게 샘플링할지를 조정하는 변수
# 많은 쪽의 데이터 중 얼마만큼의 비율을 샘플링할 것인지를 정하는데
# 이 비율은 perc.over에 의해 추가로 생성된 데이터와 비례해서 정해진다. 
# 예를 들어, perc.over에 의해 200개의 추가 데이터가 적은 쪽 분류에 추가로 생성되었고, 
# perc.over가 100이라면 많은 쪽 분류에 속하는 데이터로부터도 200개의 데이터가 취해진다.

