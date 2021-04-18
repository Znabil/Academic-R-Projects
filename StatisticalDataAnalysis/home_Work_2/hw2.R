#1
names(HW.diamonds) <- c("Caratage","Purity","Clarity","CertifInst","Price")
plot(HW.diamonds$Caratage,HW.diamonds$Price)
plot(HW.diamonds$Caratage,log(HW.diamonds$Price))


#2
HW.diamonds$Purity <- relevel(HW.diamonds$Purity,ref="D")
HW.diamonds$Clarity <- relevel(HW.diamonds$Clarity,ref="IF")
HW.diamonds$CertifInst <- relevel(HW.diamonds$CertifInst,ref="HRD")

lm1=lm(HW.diamonds$Price ~HW.diamonds$Caratage+HW.diamonds$Purity+HW.diamonds$Clarity+HW.diamonds$CertifInst,data=HW.diamonds)
summary(lm1)
library(car)
residualPlots(lm1)
outlierTest(lm1, cutoff=0.05)


#3a
  
HW.diamonds <- mutate(HW.diamonds,sizeCarat= ifelse( HW.diamonds$Caratage < 0.5,"small",
                                                       ifelse(HW.diamonds$Caratage >= 0.5 & HW.diamonds$Caratage <1,"medium","large")))
                                                     
HW.diamonds <-within(HW.diamonds,sizeCarat <- as.factor(sizeCarat))
HW.diamonds$sizeCarat <- relevel(HW.diamonds$sizeCarat,ref="small")

lm2=lm(HW.diamonds$Price ~ HW.diamonds$Caratage+HW.diamonds$Purity+HW.diamonds$Clarity+HW.diamonds$CertifInst+HW.diamonds$sizeCarat+ HW.diamonds$sizeCarat * HW.diamonds$Caratage,data = HW.diamonds)
summary(lm2)
#3b
lm3 =lm(HW.diamonds$Price ~ HW.diamonds$Caratage+HW.diamonds$Purity+HW.diamonds$Clarity+HW.diamonds$CertifInst+I(HW.diamonds$Caratage^2))
summary(lm3)
