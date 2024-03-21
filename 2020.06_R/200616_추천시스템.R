######################
# 추천시스템 알고리즘
######################
# install.packages("recommenderlab") #설치되지 않았으면 설치
library(recommenderlab)
data(Jester5k)
str(Jester5k)
image(Jester5k[1:6,1:10])
# 평점메트릭스에서 데이터프레임으로 변경
df<-as(Jester5k, "data.frame")
head(df)
# user item rating
# 1     u2841   j1   7.91
# 3315  u2841   j2   9.17
# 6963  u2841   j3   5.34
# 10301 u2841   j4   8.16
# 13443 u2841   j5  -8.74
# 18441 u2841   j6   7.14
# 데이터프레임에서 평점메트릭스으로 변경
# ratings_matrix <- as(df, "realRatingMatrix")

#### 모델생성 (popular 방식)

split.matrix<-sample(nrow(Jester5k), size=1000, replace=FALSE)
training<-Jester5k[split.matrix, ]
test<-Jester5k[-split.matrix, ]

r <- Recommender(training, method = "POPULAR")
r
# UBCF = User-Based Collaborative Filterings
# IBCF = Item-Based Collaborative Filterings
# Principal Component Analysis

#### 예측
# popular 방식 => 아이템의 “순위”가 반환
recom <- predict(r, Jester5k[1001:1002], n=20)
recom
as(recom, "list")
# ratings 방식 => 아이템 별 “예측 점수" 반환
recom <- predict(r, Jester5k[1001:1002], type="ratings")
recom
as(recom, "list")

#### 평가
# 평가안 
eval_scheme <- evaluationScheme(training, method="split",
                                train=0.9, given=15)
# 평가안 출력
eval_scheme

## UBCF를 사용한 모델 훈련
# 모델 훈련
training_recommender <- Recommender(getData(eval_scheme, "train"), "UBCF")
# 테스트 데이터상의 예측
test_rating <- predict(training_recommender, getData(eval_scheme, "known"),
                       type="ratings")
# 오류
error <- calcPredictionAccuracy(test_rating, getData(eval_scheme,"unknown"))

error
## IBCF를 사용한 모델 훈련
# IBCF를 사용한 트레이닝 모델
training_recommender_2 <- Recommender(getData(eval_scheme, "train"),"IBCF")
# 테스트 데이터 세트상의 예측
test_rating_2 <-
  predict(training_recommender_2, getData(eval_scheme, "known"), type="ratings")

## UBCF 와 IBCF와의 비교
error_compare <- 
  rbind(calcPredictionAccuracy(test_rating, getData(eval_scheme, "unknown")),
        calcPredictionAccuracy(test_rating_2, getData(eval_scheme,"unknown")))
rownames(error_compare) <- c("User Based CF", "Item Based CF")
error_compare
head(error_compare)

