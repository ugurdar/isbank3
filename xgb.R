library(xgboost)
library(MLmetrics)

predtable <-  matrix(, nrow = dim(testlj)[1], ncol = 5)
predtesting <- NULL
k=0
for(i in 1:5){
set.seed(1234+k)
birler <- trainsamet[trainlj$target==1,]
sifirlar <- sample_n(trainlj[trainlj$target==0,],2440 )
xgtrain <- rbind(birler,sifirlar)
xgtrain <- xgtrain 

set.seed(1234)
indeks <- createDataPartition(xgtrain$target, p=0.8,list=FALSE)
training <- xgtrain[indeks,]
testing <- xgtrain[-indeks,]

train_x <- training %>% dplyr::select(-target)
train_y <- training$target

test_x <- testing %>% dplyr::select(-target)
test_y <- testing$target

dtrain <- xgb.DMatrix(data = as.matrix(train_x), label = train_y)
dtest <- xgb.DMatrix(data = as.matrix(test_x), label = test_y)
watchlist <- list(train=dtrain, test=dtest)

a=0
b=0
c=0.81
while(a<c){
b=b+1
xgb <- xgb.train(data = dtrain
          , params = list(objective = "binary:logistic"
                          , eta =sample(c(0.001,0.003,0.01,0.02,0.03,0.05,
                                          0.06,0.07,0.08,0.09,0.1,0.12,0.13,0.15,
                                          0.2,0.3,0.4),1)
                          , max.depth = sample(1:14,1)
                          , min_child_weight = sample(seq(10,120,by=10),1)
                          , subsample = 1
                          , colsample_bytree = 1
                          , nthread = sample(5:15,1)
                          , eval_metric = "auc"
                          , booster = "gbtree"
          )
          , watchlist = list(test = dtest)
          , nrounds = 5000
          , early_stopping_rounds = 200
          , print_every_n = 100
)
a = xgb$best_score
if(b == 100){c = c-0.01}
}


bst <-
  xgb.train(
    data = dtrain,
    max.depth = xgb$params$max_depth,
    eta = xgb$params$eta,
    nthread = xgb$params$nthread,
    nrounds = xgb$best_iteration,
    early_stopping_rounds = 100,
    min_child_weight = xgb$params$min_child_weight,
    watchlist = watchlist,
    eval_metric = "auc",
    objective = "binary:logistic",
    booster = "gbtree"
  )


xgb_predict_train <- predict(bst, newdata = as.matrix(test_x))
predtesting[i] = AUC(xgb_predict_train, test_y)
print(predtesting)

predtable = predict(bst, newdata = as.matrix(testsamet))
k = k + 5
}
