setwd("C:/Users/gr/Desktop/turkiye-is-bankas-machine-learning-challenge-3")

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


train <- read.csv("train.csv",na.strings ="")
test <- read.csv("test.csv",na.strings ="")
monthly <- read.csv("monthly_expenditures.csv")
submission <- read.csv("sample_submission.csv")



sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

 #train[train$target == 1 & train$meslek_grubu          == "NA",]
train$meslek_grubu <- NULL
test$meslek_grubu <- NULL
train <- na.omit(train)
 test <- na.omit(test)
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

# meslek_grubu to numeric
# train$meslek_grubu <- as.numeric(factor(train$meslek_grubu))
# test$meslek_grubu <- as.numeric(factor(test$meslek_grubu))

train$tarih<- NULL
test$tarih <- NULL

monthly[monthly$tarih ==20190101,"tarih"] <-1
monthly[monthly$tarih ==20190201,"tarih"] <-2
monthly[monthly$tarih ==20190301,"tarih"] <-3
monthly[monthly$tarih ==20190401,"tarih"] <-4
monthly[monthly$tarih ==20190501,"tarih"] <-5
monthly[monthly$tarih ==20190601,"tarih"] <-6


ort <- monthly %>%
  group_by(musteri) %>%
  summarize(mean = mean(aylik_toplam_tutar))

# Müşterilerin aylık harcama ortalamaları
# ayliktoplam <- monthly %>%
#   group_by(musteri,tarih) %>%
#   summarize(mean = mean(aylik_toplam_tutar))
# ayliktoplam <- dcast(setDT(ayliktoplam),musteri~tarih, value.var=c("mean"))
# colnames(ayliktoplam) <- paste0(colnames(ayliktoplam),"_mean")

# Musterilerin aylik harcama ortalamaları
aylikort <- monthly %>%
  group_by(musteri,tarih) %>%
  summarize(ortalama = mean(aylik_toplam_tutar))
aylikort <- dcast(setDT(aylikort),musteri~tarih, value.var=c("ortalama"))
colnames(aylikort) <- paste0(colnames(aylikort),"_ort")

# Musterilerin aylik harcama standart sapmaları
ayliksd <- monthly %>%
  group_by(musteri,tarih) %>%
  summarize(sd = sd(aylik_toplam_tutar))
ayliksd <- dcast(setDT(ayliksd),musteri~tarih, value.var=c("sd"))
colnames(ayliksd) <- paste0(colnames(ayliksd),"_sd")

sektorort <- monthly %>%
  group_by(musteri,sektor) %>%
  summarize(sektorort = mean(aylik_toplam_tutar))
sektorort <- dcast(setDT(sektorort),musteri~sektor, value.var=c("sektorort"))
colnames(sektorort) <- paste0(colnames(sektorort),"_ort")
# müşterilerin aylık ve sektörel harcamaları
ayliksektor <- monthly %>%
  group_by(musteri,sektor,tarih) %>%
  summarize(ayliktop = sum(aylik_toplam_tutar))
ayliksektor <- dcast(setDT(ayliksektor),musteri~tarih+sektor, value.var=c("ayliktop"))
colnames(ayliksektor) <- paste0(colnames(ayliksektor),"_sektor")

join1 <- left_join(ort, aylikort,
                     by = c("musteri" = "musteri_ort"))

join2 <- left_join(join1, ayliksd,
                   by = c("musteri" = "musteri_sd"))

join3 <- left_join(join2, sektorort,
                   by = c("musteri" = "musteri_ort"))

join4 <- left_join(join3, ayliksektor,
                   by = c("musteri" = "musteri_sektor"))

join4[is.na(join4)] <- 0


trainlj <- left_join(train, join4,
                     by = c("musteri" = "musteri"))
testlj <- left_join(test, join4,
                    by = c("musteri" = "musteri"))

