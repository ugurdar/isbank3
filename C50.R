library(ranger)
library(MLmetrics)

predtable_tree <-  matrix(, nrow = dim(testlj)[1], ncol =100)
predtest_tree <- NULL
for(i in 1:100){
  set.seed(3126+i)
  birler <- trainlj[trainlj$target==1,]
  sifirlar <- sample_n(trainlj[trainlj$target==0,],2431  )
  xgtrain <- rbind(birler,sifirlar)
  xgtrain <- xgtrain 
  
  indeks <- createDataPartition(xgtrain$target, p=0.8,list=FALSE)
  training <- xgtrain[indeks,]
  testing <- xgtrain[-indeks,]
  
  train_x <- training %>% dplyr::select(-target)
  train_y <- training$target
  
  test_x <- testing %>% dplyr::select(-target)
  test_y <- testing$target
  
  
say <- NULL
st <- 0
for(k in 1:100){
  
  tree_mod <- C5.0(x = train_x, y = as.factor(train_y),k)
  tree_mod_predict_train <- predict(tree_mod, newdata = test_x, type = "prob")[,2]
  say[k] <- AUC(tree_mod_predict_train,test_y)
  if(k!=1){
    if(say[k]==say[k-1]){
      st = st +1
    }
  }
  if(st>4){
    break
  }
}
print(max(say))
predtest_tree[i] <- max(say)
tree_mod <- C5.0(x = train_x, y = as.factor(train_y),which(say==max(say)))
predtable_tree[,i] = predict(tree_mod, newdata = as.matrix(testlj),type="prob")[,2]
  
}
