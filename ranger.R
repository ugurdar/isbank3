

library(ranger)
predtable_rf <-  matrix(, nrow = dim(testlj)[1], ncol =10)
predtest_rf <- NULL
for(j in 1:10){
  set.seed(j*12345)
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
  
hyper_grid <- expand.grid(
  mtry       = seq(2, 16, by = 1),
  sampe_size = c(0.3,.55, .632, .70, .80),
  min.node.size = c(5,10,20,40,60,80),
  OOB_AUC   = 0
)


st=0
for(i in 1:nrow(hyper_grid)) {
  # train model
  model <- ranger(
    formula         = as.factor(target) ~ ., 
    data            = training, 
    num.trees       = 1000,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    probability     = TRUE,
    seed            = 1234
  )
  pred_ranger <- predict(model,test_x)
  # add OOB error to grid
  hyper_grid$OOB_AUC[i] <- AUC(y_pred = pred_ranger$predictions[,2],y_true = test_y)
  print(hyper_grid$OOB_AUC[i])
  if(hyper_grid$OOB_AUC[i] < 0.77){
    st = st + 1
  }
  if(st > 20){
    break
  }
}

rfb <- which.max(hyper_grid$OOB_AUC)

model <- ranger(
  formula         = as.factor(target) ~ ., 
  data            = training, 
  num.trees       = 1000,
  mtry            = hyper_grid[rfb,1],
  min.node.size   = hyper_grid[rfb,3],
  sample.fraction = hyper_grid[rfb,2],
  probability     = TRUE,
  seed            = 1234
)
pred_ranger1 <- predict(model,test_x)
predtest_rf[j] <- AUC(y_pred = pred_ranger1$predictions[,2],y_true = test_y)
predtable_rf[,j] <- predict(model,testlj)$predictions[,2]
}
