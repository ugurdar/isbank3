top <- monthly %>% 
  group_by(musteri) %>%
  summarize(total = sum(aylik_toplam_tutar))

ort <- monthly %>%
  group_by(musteri) %>%
  summarize(mean = mean(aylik_toplam_tutar))

# Müşterilerin aylık harcama ortalamaları
# ayliktoplam <- monthly %>%
#   group_by(musteri,tarih) %>%
#   summarize(mean = mean(aylik_toplam_tutar))
# ayliktoplam <- dcast(setDT(ayliktoplam),musteri~tarih, value.var=c("mean"))
# colnames(ayliktoplam) <- paste0(colnames(ayliktoplam),"_mean")
sektortop <- monthly %>%
  group_by(musteri,sektor) %>%
  summarize(sektorsum = sum(aylik_toplam_tutar))
sektortop <- dcast(setDT(sektortop),musteri~sektor, value.var=c("sektorsum"))

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
join5 <- left_join(join4, top,
                   by = c("musteri" = "musteri"))

join5[is.na(join5)] <- 0

monthly_df1 <- data.frame(join4)

#write.csv(monthly_df,"monthly_df.csv",row.names = FALSE)
#write.csv(monthly_df1,"monthly_df1.csv",row.names = FALSE)

#######################################################################################################
aylikort <- monthly %>%
  group_by(musteri) %>%
  summarize(mean(aylik_toplam_tutar))

ayliksd <- monthly %>%
  group_by(musteri) %>%
  summarize(sd(aylik_toplam_tutar))

ayliksd[is.na(ayliksd)] <- 0

ayliktoplam <- data.frame(ayliktoplam,aylikort=aylikort$`mean(aylik_toplam_tutar)`,ayliksd=ayliksd$`sd(aylik_toplam_tutar)`)

summusteriaylik <- monthly %>%
  group_by(musteri) %>%
  summarize(sum(aylik_toplam_tutar))
  
  
trainlj <- left_join(train, join5,
                     by = c("musteri" = "musteri"))
testlj <- left_join(test, join5,
                    by = c("musteri" = "musteri"))
testmusteri <- testlj$musteri
