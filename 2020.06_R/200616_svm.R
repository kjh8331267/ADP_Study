######################
# svm
######################

# SVM : 패턴인식, 자료분석을 위한 학습모델. 이진분류
# SVM 패키지
# install.packages("e1071")
library(e1071)
set.seed(123)
x <-matrix(rnorm(20*2),ncol=2) # 정규분포를
x
y<- c(rep(-1,10),rep(1,10)) #산정도
y
x[y==1, ] <-x[y==1, ] +1
x
plot(x,col=(3-y)) #

dat <- data.frame(x=x,y=as.factor(y)) #이진분류 as.factor(y)
str(dat)

svm_fit <- svm(y ~ ., data=dat, kernel='linear',cost=10 , scale = F) #cost 마진 만든것에대한 비용(학습) cost값이 많으면 마진이 넓어짐
svm_fit
plot(svm_fit, dat)

attributes(svm_fit)
svm_fit$index # 벡터 확인하기
summary(svm_fit)

#cost 비용 변경하기
svm_fit <- svm(y ~ ., data=dat, kernel='linear',cost=0.1 , scale = F) #cost 값 0.1로
plot(svm_fit,dat)
svm_fit$index #값 : 14개

#best cost 찾기 - 교차검증 tune()
set.seed(123) #난수 발생
tune.out <-tune(svm, y ~., data = dat, kernel='linear', range=list(cost=c(0.001,0.01,0.1,1.5,10,50))) #
summary(tune.out) # best cost = 1.5  #보면 1.5일때 에러율이 가장 작다.

bestmod <-tune.out$best.model
bestmod
summary(bestmod)

#예측하기
xtest <- matrix(rnorm(20 * 2),ncol=2) #샘플링
ytest <- sample(c(-1,1),20,rep=T) #샘플링
xtest
ytest

xtest[ytest==1, ] <-xtest[ytest==1, ] +1
testda <- data.frame(x=xtest, y=as.factor(ytest))
testda


ypred <- predict(bestmod,testda)
table(예측값=ypred, 실제값=testda$y)
(5+10)/nrow(testda)


#iris data로 실습해보기
model <- svm(Species ~., data=iris) #Species에 의해서 분류를함
model # Support Vectors 51개 만듬

pred <- predict(model, iris[,-5])
pred
table(pred,iris$Species) #교차분할표
(50 + 48 + 48) / nrow(iris)

