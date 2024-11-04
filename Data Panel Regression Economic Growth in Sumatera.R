library(readxl) 
library(plm)        
library(kableExtra) 
library(lmtest)     
library(tseries)
library(car)
library(tidyverse)
library(ggplot2)
library(pastecs)
library(psych)

skripsi <- read_excel("D:/SKRIPSIIIII/BARU LAGI BISMILLAH/ekspor1 - Copy.xlsx")
names(skripsi)
summary(skripsi)
describe(skripsi)
stat.desc(skripsi)

#Plot
ggplot(data = skripsi, aes(x = Tahun, y = Y , group = Provinsi, colour = Provinsi)) +
  theme_bw() +
  geom_line(size = 1) +
  geom_point(size = 2, shape = 15, fill = "red") + 
  labs(colour = "Provinsi", title = "PDRB", subtitle = "Tahun 2016-2022") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5))

common=plm(Y~X1+X2+X3,data=skripsi,model="pooling")
summary(common)
fem <- plm(Y~X1+X2+X3, index = c("Provinsi", "Tahun"), 
                  model = "within", data = skripsi)
summary(fem)
summary(fixef(fem, effect="individual"))

#uji chow
pooltest(common,fem)

fixed=plm(Y~X1+X2+X3,data=skripsi,model="within",index = c("Provinsi","Tahun"))
random=plm(Y~X1+X2+X3,data=skripsi,model="random",index = c("Provinsi","Tahun"))
summary(random)

#uji hausman
phtest(fixed,random)

#Uji Breusch Pagan
plmtest(common, effect = "individual", type = c("bp"))
#Efek Dua Arah
plmtest(random, effect="twoways", type="bp")
#Efek Individu/Cross Section
plmtest(random, effect="individual", type="bp")
#Efek Waktu/Time
plmtest(random, effect="time", type="bp")

#Uji asumsi tanpa transformasi
#uji normalitas 
rem=plm(Y~X1+X2+X3,data=skripsi,model="random"
       , effect= "individual", index = c("Provinsi","Tahun"))
summary(rem)
residrandom=rem$residuals
jarque.bera.test(residrandom)

#uji heteroskedastisitas
coeftest(rem,vcovHC)
bptest(rem)

#uji multikolinearitas
vif(rem)

#uji autokorelasi
pbgtest(rem)
pbgtest(rem,order=7)
pdwtest(rem)

#Uji asumsi klasik transformasi
#uji normalitas
log=plm(log(Y)~log(X1)+log(X2)+log(X3),data=skripsi,
       model="random",effect="individual",index = c("Provinsi","Tahun"), )
summary(log)
residrandom=log$residuals
jarque.bera.test(residrandom)

#uji heteroskedastisitas
coeftest(log,vcovHC)
bptest(log)

#uji multikolinearitas
vif(log)

#uji autokorelasi
pbgtest(log)
pdwtest(log)

#uji signifikansi
summary(log)

#Melihat seberapa besar pengaruh masing-masing cross section
ranef(log)
ranef(log, effect="individual")
