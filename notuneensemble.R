predtable <-  matrix(, nrow = dim(testlj)[1], ncol = 2)
predtable1 <-  matrix(, nrow = dim(testlj)[1], ncol = 2)
predtable2 <-  matrix(, nrow = dim(testlj)[1], ncol = 2)
predtable3 <-  matrix(, nrow = dim(testlj)[1], ncol = 2)
predtesting <- NULL;predtesting1 <- NULL;predtesting2 <- NULL;predtesting3 <- NULL
predtesting4 <- NULL
tt <- trainlj[which(trainlj$total > 50),]
k=0
for(i in 1:2){
set.seed(1234+k)
birler <- trainlj[trainlj$target==1,]
sifirlar <- sample_n(trainlj[trainlj$target==0,],dim(birler)[1] )
xgtrain <- rbind(birler,sifirlar)
xgtrain <- xgtrain 
set.seed(1234+k)
k=k+1

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
#subsample

bst <- xgb.train(data=dtrain, max.depth=5, eta=0.3,nrounds=5000, 
                 early_stopping_rounds =200,min_child_weight =110,
                 watchlist=watchlist, eval_metric = "auc",objective = "binary:logistic",
                 booster = "gbtree")
  

rf_classifier = randomForest(as.factor(target)~ ., data=training, mtry=4,nodesize=25, ntree=2000,  importance=TRUE)
tree_mod <- C5.0(x = train_x, y = as.factor(train_y),100)
tree_rule <- C5.0(x = train_x, y = as.factor(train_y),rules = TRUE)

predtable[,i] = predict(bst, newdata = as.matrix(testlj))
predtable1[,i] = predict(rf_classifier, newdata = as.matrix(testlj),type="prob")[,2]
predtable2[,i] = predict(tree_mod, newdata = as.matrix(testlj),type="prob")[,2]
predtable3[,i] = predict(tree_rule, newdata = as.matrix(testlj),type="prob")[,2]

  
xgb_predict_train <- predict(bst, newdata = as.matrix(test_x))
rf_predict_train <- predict(rf_classifier, newdata = test_x, type = "prob")[,2]
tree_mod_predict_train <- predict(tree_mod, newdata = test_x, type = "prob")[,2]
tree_rule_predict_train <- predict(tree_rule, newdata = test_x, type = "prob")[,2]

predtesting[i] = AUC(xgb_predict_train,test_y)
predtesting1[i] = AUC(rf_predict_train,test_y)
predtesting2[i] = AUC(tree_mod_predict_train,test_y)
predtesting3[i] = AUC(tree_rule_predict_train,test_y)
ensemble <- xgb_predict_train*0.45+
  as.numeric(as.character(tree_mod_predict_train))*0.2
  +as.numeric(as.character(rf_predict_train))*0.35
#  +as.numeric(as.character(tree_rule_predict_train))*0.10
ensemble <- ifelse(ensemble > 0.5,1,0)
predtesting4[i] = AUC(ensemble,test_y)


}
