ggplot(data=train, aes(x=yas, group=as.factor(target), fill=as.factor(target))) +
  geom_density(adjust=1.5, alpha=.4) 

ggplot(data=train, aes(x=kidem_suresi , group=as.factor(target), fill=as.factor(target))) +
  geom_density(adjust=1.5, alpha=.4) 

ggplot(data=train, aes(x=as.factor(is_durumu  ), group=as.factor(target), fill=as.factor(target))) +
  geom_density(adjust=1.5, alpha=.4)

ggplot(data=train, aes(x=as.factor(egitim)   , group=as.factor(target), fill=as.factor(target))) +
  geom_density(adjust=1.5, alpha=.4)

ggplot(data=train, aes(x=yas  , group=as.factor(target), fill=as.factor(target))) +
  geom_density(adjust=1.5, alpha=.4)

ggplot(data=train, aes(x=total     , group=as.factor(meslek_grubu), fill=as.factor(meslek_grubu))) +
  geom_density(adjust=1.5, alpha=.4)

ggplot(data=train, aes(x=KUYUMCU       , group=as.factor(target), fill=as.factor(target))) +
  geom_density(adjust=1.5, alpha=.4)

ggplot(data=train,aes(x=ICKILI_YERLER1   ,group=as.factor(target)))+
         geom_boxplot()
