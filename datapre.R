library(readr)
library(tidyverse)
library(caret)
library(xgboost)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(microbenchmark)
library(MLmetrics)
library(ROSE)
library(data.table)
library(fastDummies)
library(randomForest)
library(C50)


train <- read.csv("train.csv",na.strings ="")
test <- read.csv("test.csv",na.strings ="")
monthly <- read.csv("monthly_expenditures.csv")
submission <- read.csv("sample_submission.csv")
monthly$islem_adedi <- ifelse(monthly$islem_adedi <0, monthly$islem_adedi*-1,monthly$islem_adedi)


sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

 #train[train$target == 1 & train$meslek_grubu  == "NA",]
train$meslek_grubu <- NULL
test$meslek_grubu <- NULL

# train[is.na(train$egitim),"egitim"] <- "7e6640bfe0"
# test[is.na(test$egitim),"egitim"] <- "7e6640bfe0"
# 
# train[is.na(train$is_durumu),"is_durumu"] <- "991c4998fb"
# test[is.na(test$is_durumu),"is_durumu"] <- "991c4998fb"


# train <- na.omit(train)
# test <- na.omit(test)

# # Meslek Grubu
meslek_train_levels <- levels(factor(train$meslek_grubu))
meslek_test_levels <- levels(factor(test$meslek_grubu))
all.equal(meslek_train_levels,meslek_test_levels)

# İş Durumu
is_train_levels <- levels(factor(train$is_durumu ))
is_test_levels <- levels(factor(test$is_durumu ))

# 3773727d6e olan silinecek
train <- train[-which(train$is_durumu=="3773727d6e") ,]
is_train_levels <- levels(factor(train$is_durumu ))
all.equal(is_train_levels,is_test_levels)

### Eğitim
egitim_train_levels <- levels(factor(train$egitim ))
egitim_test_levels <- levels(factor(test$egitim ))

# which(train$egitim=="2eb5ddd72c") 48736
train <- train[-which(train$egitim=="2eb5ddd72c"),]
egitim_train_levels <- levels(factor(train$egitim ))
all.equal(egitim_train_levels,egitim_test_levels)


# Egitim to numeric
train$egitim <- as.numeric(factor(train$egitim))
test$egitim <- as.numeric(factor(test$egitim))

# is_durumu to numeric
train$is_durumu <- as.numeric(factor(train$is_durumu))
test$is_durumu <- as.numeric(factor(test$is_durumu))

#meslek_grubu to numeric
train$meslek_grubu <- as.numeric(factor(train$meslek_grubu))
test$meslek_grubu <- as.numeric(factor(test$meslek_grubu))

train$tarih<- NULL
test$tarih <- NULL





monthly[monthly$tarih ==20190101,"tarih"] <-1
monthly[monthly$tarih ==20190201,"tarih"] <-2
monthly[monthly$tarih ==20190301,"tarih"] <-3
monthly[monthly$tarih ==20190401,"tarih"] <-4
monthly[monthly$tarih ==20190501,"tarih"] <-5
monthly[monthly$tarih ==20190601,"tarih"] <-6



aylikislem <- monthly %>% 
  group_by(musteri) %>%
  summarize(ortalamaislemadedi = mean(islem_adedi))

toplamislem <- monthly %>% 
  group_by(musteri) %>%
  summarize(toplamislemadedi = sum(islem_adedi))


top <- monthly %>% 
  group_by(musteri) %>%
  summarize(total = sum(aylik_toplam_tutar))

ort <- monthly %>% 
  group_by(musteri) %>%
  summarize(ort = mean(aylik_toplam_tutar))

sektortop <- monthly %>%
  group_by(musteri,sektor) %>%
  summarize(sektorsum = sum(aylik_toplam_tutar))

sektortop <- dcast(setDT(sektortop),musteri~sektor, value.var=c("sektorsum"))



joining <- left_join(top, sektortop,
                   by = c("musteri" = "musteri"))

joining <- left_join(joining,ort,
                     by = c("musteri"="musteri"))

joining <- left_join(joining,toplamislem,
                     by = c("musteri"="musteri"))
joining <- left_join(joining,aylikislem,
                     by = c("musteri"="musteri"))



joining[is.na(joining)] <- 0


trainlj <- left_join(train, joining,
                     by = c("musteri" = "musteri"))
testlj <- left_join(test, joining,
                    by = c("musteri" = "musteri"))


# trainlj$restoran5 <- read.csv("5restoran.csv")$x
# testlj$restoran5 <- read.csv("5restorantest.csv")$x
# names(trainlj$restoran5) <- "restoran5"
# names(testlj$restoran5) <- "restoran5"
# 
# trainlj$restoran6 <- read.csv("6restoran.csv")$x
# testlj$restoran6 <- read.csv("6restorantest.csv")$x
# names(trainlj$restoran6) <- "restoran6"
# names(testlj$restoran6) <- "restoran6"
# 
# 
# trainlj$giyimaksesuar2 <- read.csv("2giyimaksesuar.csv")$x
# testlj$giyimaksesuar2 <- read.csv("2giyimaksesuartest.csv")$x
# names(trainlj$giyimaksesuar2) <- "giyimaksesuar2"
# names(testlj$giyimaksesuar2) <- "giyimaksesuar2"

trainlj <- na.omit(trainlj)
testlj <- na.omit(testlj)
testtedekinalar <- which(is.na(testlj))

trainlj_musteri <- trainlj$musteri
trainlj$musteri <- NULL

testlj_musteri <- testlj$musteri
testlj$musteri <- NULL


# trainlj$egitim <- as.factor(trainlj$egitim)
# trainlj <- dummy_cols(trainlj,select_columns = "is_durumu")
# trainlj$is_durumu <- NULL
# trainlj <- dummy_cols(trainlj,"egitim")
# trainlj$egitim <- NULL


#testlj$egitim <- as.factor(testlj$egitim)
# testlj <- dummy_cols(testlj,select_columns = "is_durumu")
# testlj$is_durumu <- NULL
#testlj <- dummy_cols(testlj,"egitim")
#testlj$egitim <- NULL


nrm <- function(x){(x-min(x))/(max(x)-min(x)+0.000001)}
# trainlj$total <- NULL
# testlj$total <- NULL

# trainlj[,7:19] <- trainlj[,7:19]  /(trainlj$total+0.000001)
# testlj[,6:18]  <- testlj[,6:18]   /(testlj$total+0.000001)
# 
# 

trainlj$kidemimsin <- trainlj$kidem_suresi*trainlj$total
trainlj$kidemimsin  <- nrm(trainlj$kidemimsin)

testlj$kidemimsin <- testlj$kidem_suresi*testlj$total
testlj$kidemimsin  <- nrm(testlj$kidemimsin)





############################# 


trainlj$kidemimsin2 <- trainlj$kidemimsin/trainlj$yas
trainlj$kidemimsin2  <- nrm(trainlj$kidemimsin2)

testlj$kidemimsin2 <- testlj$kidemimsin/testlj$yas
testlj$kidemimsin2  <- nrm(testlj$kidemimsin2)



trainlj$restelkt <- trainlj$RESTORAN_CATER/(trainlj$ELKT_ESYA_BILG+0.00000001)
trainlj$restelkt  <- nrm(trainlj$restelkt)

testlj$restelkt <- testlj$RESTORAN_CATER/(testlj$ELKT_ESYA_BILG+0.00000001)
testlj$restelkt  <- nrm(testlj$restelkt)


trainlj$yasis <- trainlj$yas * (trainlj$is_durumu)
testlj$yasis <- testlj$yas * testlj$is_durumu



###################### Loglular###################### 
trainlj$kidemimsin <- log(trainlj$kidemimsin^(1/3)+1)
testlj$kidemimsin <- log(testlj$kidemimsin^(1/3)+1)

trainlj$ort <- log(trainlj$ort+1)
testlj$ort <- log(testlj$ort+1)

trainlj$toplamislemadedi <- log(trainlj$toplamislemadedi)
testlj$toplamislemadedi <- log(testlj$toplamislemadedi)
