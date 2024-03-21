data(iris)
head(iris)
table(iris$Species)
library(ggplot2)
library(reshape2)
df_raw <- iris
df_raw$no <- c(1:50, 1:50, 1:50)
#### 1. eda
df_melt <- melt(df_raw, id.vars = c("no", "Species"))
head(df_melt)
ggplot(df_melt, aes(x = no, y = value, colour = Species))+
  geom_point()+
  facet_grid(variable~., scales = "free_y")

df_tmp1 <- subset(df_melt, variable == "Sepal.Length")
ggplot(df_tmp1, aes(x = value, fill = Species))+
  geom_histogram(binwidth = 0.1, alpha = .5)

df_tmp1 <- subset(df_melt, variable == "Sepal.Width")
ggplot(df_tmp1, aes(x = value, fill = Species))+
  geom_histogram(binwidth = 0.1, alpha = .5)

df_tmp1 <- subset(df_melt, variable == "Petal.Width")
ggplot(df_tmp1, aes(x = value, fill = Species))+
  geom_histogram(binwidth = 0.1, alpha = .5)

df_tmp1 <- subset(df_melt, variable == "Petal.Length")
ggplot(df_tmp1, aes(x = value, fill = Species))+
  geom_histogram(binwidth = 0.1, alpha = .5)
#### 2. 파생변수 생성
df_raw$Petal.Length.ft <- ifelse(df_raw$Petal.Length <= 2.5, 1,
                                 ifelse((df_raw$Petal.Length > 2.5)&
                                          (df_raw$Petal.Length <= 4.8), 2,3))

df_raw$Petal.Width.ft <- ifelse(df_raw$Petal.Width <= 0.8, 1,
                                 ifelse((df_raw$Petal.Width > 0.8)&
                                          (df_raw$Petal.Width <= 1.7), 2,3))

df_raw$Petal.Length.ft <- as.factor(df_raw$Petal.Length.ft)
df_raw$Petal.Width.ft <- as.factor(df_raw$Petal.Width.ft)

#### 3. 독립변수 선별
#3.1 상관계수
df_data <- dplyr::select(df_raw, -no)

caret::findCorrelation(cor(df_data[,c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")]))

# 3번째 Petal.Length는 상관관계가 너무 높아 제거
df_data_fs1 <- dplyr::select(df_data, -Petal.Length)
# install.packages("FSelector")
library(rpart)
m <- rpart(Species~., data = df_data_fs1)
caret::varImp(m)

#### 4번. 종속변수를 이항으로 바꾸고 로지스틱회귀로 분석
# 데이터 전처리
df_lg_data <- df_data_fs1
df_lg_data$y <- ifelse(df_lg_data$Species=="setosa",1,0)
df_lg_data$y <- as.factor(df_lg_data$y)
df_lg_data <- dplyr::select(df_lg_data, -Species)


df_lg_data[df_lg_data$Petal.Length.ft %in% c("2","3"),"Petal.Length.ft"] <- "2"
df_lg_data[df_lg_data$Petal.Width.ft %in% c("2","3"),"Petal.Width.ft"] <- "2"

# 데이터 분할
data <- df_lg_data
y <- "y"
x <- c("Sepal.Length","Sepal.Width","Petal.Width","Petal.Length.ft")
train_index <- sample(nrow(data), floor(0.8*nrow(data)))
df_train <- data[train_index, c(x,y)]
df_test <- data[-train_index, c(x,y)]

# 모델 생성
lm <- glm(y~., data =df_train, family = "binomial")
summary(lm)
library(car)
vif(lm)
# fitting값 확인
f <- fitted(lm)
table(df_train$y, ifelse(f > 0.5, 1, 0))

# testset 예측
pred <- predict(lm, newdata = dplyr::select(df_test, -y), type = "response")
table(df_test$y, ifelse(as.vector(pred) > 0.5, 1, 0))


##### 5번. 종속변수를 다항인 상태에서 SVM포함한 3가지 알고리즘으로 모델을 돌리고 평가하기
colnames(df_data_fs1)
data <- df_data_fs1
y <- "Species"
x <- c("Sepal.Length","Sepal.Width","Petal.Width","Petal.Length.ft","Petal.Width.ft")
  
train_index <- sample(nrow(data), floor(0.8*nrow(data)))
df_train <- data[train_index, c(x,y)]
df_test <- data[-train_index, c(x,y)]
# 1 svm
library(e1071)
library(kernlab)
# m <- ksvm(Species~., data = df_train)
tune.out <-tune(svm, Species ~., data = df_train, kernel='linear', range=list(cost=c(0.001,0.01,0.1,1.5,10,50), gamma = 2^(-1:1))) #
summary(tune.out) # best cost = 1.5  #보면 1.5일때 에러율이 가장 작다.
tune.out$best.parameters
bestmod <-tune.out$best.model
bestmod
summary(bestmod)
# testset 예측
ypred <- predict(bestmod, dplyr::select(df_test, -y))
yreal <- df_test$Species
tab <- table(ypred, yreal)

caret::confusionMatrix(tab)
# Metrics::f1(yreal, ypred)
# Metrics::recall(yreal, ypred)

# 2 랜덤포레스트
# install.packages("randomForest")
library(randomForest)
head(df_train)
set.seed(10)
rf <- randomForest(Species ~., data = df_train)
plot(rf)
ntree_min <- which.min(rf$err.rate[,1])

rf_1 <- randomForest(Species ~., data = df_train, ntree = ntree_min)
rf_1
# testset 예측
ypred <- predict(rf_1, newdata = dplyr::select(df_test, -y), type = "response")
yreal <- df_test$Species
tab <- table(ypred, yreal)

caret::confusionMatrix(tab)

# xgboost
# install.packages("xgboost")
library(xgboost)

data <- df_data_fs1
y <- "Species"
x <- c("Sepal.Length","Sepal.Width","Petal.Width")
## factor형 안됨
train_index <- sample(nrow(data), floor(0.8*nrow(data)))
df_train <- data[train_index, c(x,y)]
df_test <- data[-train_index, c(x,y)]

mat_train_x <- as.matrix(dplyr::select(df_train, -y))
mat_train_y <- as.matrix(dplyr::select(df_train, y))
mat_test_x <- as.matrix(dplyr::select(df_test, -y))
mat_test_y <- as.matrix(dplyr::select(df_test, y))


cntrl <- caret::trainControl(method = "cv", number = 5, verboseIter = TRUE,
                      returnData = FALSE, returnResamp = "final")
grid <- expand.grid(nrounds = c(75, 100), colsample_bytree = 1,
                    min_child_weight = 1, eta = c(0.01,0.1,0.3),
                    gamma = c(0.5,0.25),
                    subsample = 0.5, max_depth = c(2, 3))

set.seed(1)
train.xgb = caret::train(x = mat_train_x,
                  y = mat_train_y,
                  trControl = cntrl,
                  tuneGrid = grid,
                  method = "xgbTree")

# testset 예측
ypred <- predict(train.xgb, newdata = mat_test_x)
yreal <- mat_test_y
tab <- table(ypred, yreal)

caret::confusionMatrix(tab)


##### 6. 군집분석을 돌려서 독립변수에 추가하고 5번의 최적 알고리즘 결과와 비교하기
# install.packages("NbClust")
# library(NbClust)

data <- df_data_fs1
y <- "Species"
x <- c("Sepal.Length","Sepal.Width","Petal.Width")
mat_data_x <- as.matrix(data[, x])
data_x <- data[, x]
data_y <- data[, y]

# 클러스터링
nc <- NbClust(mat_data_x, min.nc=2, max.nc=15, method="kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")


iris.kmeans <- kmeans(mat_data_x, centers = 3, iter.max = 10000)
# install.packages("fpc")
# library(fpc)
plotcluster(mat_data_x, iris.kmeans$cluster, color=TRUE, shade=TRUE)


data_x$cl <- as.vector(iris.kmeans$cluster)

data_cl <- cbind(data_x, data_y)
colnames(data_cl)[5] <- "Species"
train_index <- sample(nrow(data_cl), floor(0.8*nrow(data_cl)))
df_train <- data_cl[train_index, c(x,y,"cl")]
df_test <- data_cl[-train_index, c(x,y,"cl")]

# 1 svm
library(e1071)
library(kernlab)
# m <- ksvm(Species~., data = df_train)
tune.out <-tune(svm, Species ~., data = df_train, kernel='linear', range=list(cost=c(0.001,0.01,0.1,1.5,10,50), gamma = 2^(-1:1))) #
summary(tune.out) # best cost = 1.5  #보면 1.5일때 에러율이 가장 작다.
tune.out$best.parameters
bestmod <-tune.out$best.model
bestmod
summary(bestmod)
# testset 예측
ypred <- predict(bestmod, dplyr::select(df_test, -y))
yreal <- df_test$Species
tab <- table(ypred, yreal)

caret::confusionMatrix(tab)
