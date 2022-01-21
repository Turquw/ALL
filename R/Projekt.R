#Scharakteryzuj 5 wybranych zmiennych: minimum, pierwszy kwartyl, medina, œrednia, trzeci kwartyl, maksimum, odchylenie standardowe, wybran¹ miarê asymetrii i koncentracji w ka¿dej z 6 grup: wina czerwone z Katanii, wina bia³e z Katanii, wina czerwone z Bolonii itd.
library(e1071)
x1<-subset(cos,cos$region=="Bolonia")
x1<-subset(x1,x1$color=="white")
BoloniaWMin<-mapply(min,x1[,c(3,4,5,6,7 )])
BoloniaWMax<-mapply(max,x1[,c(3,4,5,6,7 )])
BoloniaWMean<-mapply(mean,x1[,c(3,4,5,6,7 )])
BoloniaWMedian<-mapply(median,x1[,c(3,4,5,6,7 )])
BoloniaWSd<-mapply(sd,x1[,c(3,4,5,6,7 )])
BoloniaWQ1<-mapply(quantile,x1[,c(3,4,5,6,7 )],0,25,na.rm=TRUE)
BoloniaWQ3<-mapply(quantile,x1[,c(3,4,5,6,7 )],0,75,na.rm=TRUE)
BoloniaWPerson<-3*(BoloniaWMean-BoloniaWMedian)/BoloniaWSd
BoloniaWKurtiza<-mapply(kurtosis,x1[,c(3,4,5,6,7 )])

x2<-subset(cos,cos$region=="Bolonia")
x2<-subset(x2,x2$color=="red")
# TERAZ czerwone Z BOLONI
BoloniaRMin<-mapply(min,x2[,c(3,4,5,6,7 )])
BoloniaRMax<-mapply(max,x2[,c(3,4,5,6,7 )])
BoloniaRMean<-mapply(mean,x2[,c(3,4,5,6,7 )])
BoloniaRMedian<-mapply(median,x2[,c(3,4,5,6,7 )])
BoloniaRSd<-mapply(sd,x2[,c(3,4,5,6,7 )])
BoloniaRQ1<-mapply(quantile,x2[,c(3,4,5,6,7 )],0,25,na.rm=TRUE)
BoloniaRQ3<-mapply(quantile,x2[,c(3,4,5,6,7 )],0,75,na.rm=TRUE)
BoloniaRPerson<-3*(BoloniaRMean-BoloniaRMedian)/BoloniaRSd
BoloniaRKurtiza<-mapply(kurtosis,x2[,c(3,4,5,6,7 )])

#bia³e z Emilia Romana
x3<-subset(cos,cos$region=="Emilia Romana")
x3<-subset(x3,x3$color=="white")
Emilia_RomanaWMin<-mapply(min,x3[,c(3,4,5,6,7 )])
Emilia_RomanaWMax<-mapply(max,x3[,c(3,4,5,6,7 )])
Emilia_RomanaWMean<-mapply(mean,x3[,c(3,4,5,6,7 )])
Emilia_RomanaWMedian<-mapply(median,x3[,c(3,4,5,6,7 )])
Emilia_RomanaWSd<-mapply(sd,x3[,c(3,4,5,6,7 )])
Emilia_RomanaWQ1<-mapply(quantile,x3[,c(3,4,5,6,7 )],0,25,na.rm=TRUE)
Emilia_RomanaWQ3<-mapply(quantile,x3[,c(3,4,5,6,7 )],0,75,na.rm=TRUE)
Emilia_RomanaWPerson<-3*(Emilia_RomanaWMean-Emilia_RomanaWMedian)/Emilia_RomanaWSd
Emilia_RomanaWKurtiza<-mapply(kurtosis,x3[,c(3,4,5,6,7 )])

#czerwone
x4<-subset(cos,cos$region=="Emilia Romana")
x4<-subset(x4,x4$color=="red")
Emilia_RomanaRMin<-mapply(min,x4[,c(3,4,5,6,7 )])
Emilia_RomanaRMax<-mapply(max,x4[,c(3,4,5,6,7 )])
Emilia_RomanaRMean<-mapply(mean,x4[,c(3,4,5,6,7 )])
Emilia_RomanaRMedian<-mapply(median,x4[,c(3,4,5,6,7 )])
Emilia_RomanaRSd<-mapply(sd,x4[,c(3,4,5,6,7 )])
Emilia_RomanaRQ1<-mapply(quantile,x4[,c(3,4,5,6,7 )],0,25,na.rm=TRUE)
Emilia_RomanaRQ3<-mapply(quantile,x4[,c(3,4,5,6,7 )],0,75,na.rm=TRUE)
Emilia_RomanaRPerson<-3*(Emilia_RomanaRMean-Emilia_RomanaRMedian)/Emilia_RomanaRSd
Emilia_RomanaRKurtiza<-mapply(kurtosis,x4[,c(3,4,5,6,7 )])

#Catania
x5<-subset(cos,cos$region=="Catania")
x5<-subset(x5,x5$color=="white")
CataniaWMin<-mapply(min,x5[,c(3,4,5,6,7 )])
CataniaWMax<-mapply(max,x5[,c(3,4,5,6,7 )])
CataniaWMean<-mapply(mean,x5[,c(3,4,5,6,7 )])
CataniaWMedian<-mapply(median,x5[,c(3,4,5,6,7 )])
CataniaWSd<-mapply(sd,x5[,c(3,4,5,6,7 )])
CataniaWQ1<-mapply(quantile,x5[,c(3,4,5,6,7 )],0,25,na.rm=TRUE)
CataniaWQ3<-mapply(quantile,x5[,c(3,4,5,6,7 )],0,75,na.rm=TRUE)
CataniaWPerson<-3*(CataniaWMean-CataniaWMedian)/CataniaWSd
CataniaWKurtiza<-mapply(kurtosis,x5[,c(3,4,5,6,7 )])
#czerwone
x6<-subset(cos,cos$region=="Catania")
x6<-subset(x6,x6$color=="red")
CataniaRMin<-mapply(min,x6[,c(3,4,5,6,7 )])
CataniaRMax<-mapply(max,x6[,c(3,4,5,6,7 )])
CataniaRMean<-mapply(mean,x6[,c(3,4,5,6,7 )])
CataniaRMedian<-mapply(median,x6[,c(3,4,5,6,7 )])
CataniaRSd<-mapply(sd,x6[,c(3,4,5,6,7 )])
CataniaRQ1<-mapply(quantile,x6[,c(3,4,5,6,7 )],0,25,na.rm=TRUE)
CataniaRQ3<-mapply(quantile,x6[,c(3,4,5,6,7 )],0,75,na.rm=TRUE)
CataniaRPerson<-3*(CataniaRMean-CataniaRMedian)/CataniaRSd
CataniaRKurtiza<-mapply(kurtosis,x6[,c(3,4,5,6,7 )])


library(normtest)


#Zbuduj 95% przedzia³ ufnoœci dla œredniej i wariancji dla dwóch wybranych zmiennych osobno dla win z Bolonii, Katanii i Emilii Romany
x51<-subset(cos,cos$region=="Catania")

Q1.N        <- length(x51$`fixed acidity`)# rozmiar probki
Q1.mean     <- mapply(mean,x6[,c(3,4 )])  
Q1.sd       <- mapply(sd,x6[,c(3,4)])
Q1.z        <- qnorm(.975)   # wartosc Z95
Q1.mean - Q1.sd*Q1.z/sqrt(Q1.N)
Q1.mean + Q1.sd*Q1.z/sqrt(Q1.N)

#SprawdŸ czy poziom siarki istotnie zmieni³ siê po filtracji (zmienne sulphates i sulphates after filtering). Test wykonaj dla wszystkich win ogó³em jak i w podziale na kolor wina.
#ogólny test
testisto<-t.test(cos$sulphates,cos$`sulphates after filtering`)$p.value 
ifelse (testisto <0.05, 
       "Odrzucamy hipotezê zerow¹ na poziomie istotnoœci 0.05", 
       "Nie ma podstaw do odrzucenia hipotezy zerowej na poziomie istotnoœci 
0.05")
#bia³e wino
x12<-subset(cos,cos$color=="white")
testisto1<-t.test(x12$sulphates,x12$`sulphates after filtering`)$p.value 
ifelse (testisto1 <0.05, 
        "Odrzucamy hipotezê zerow¹ na poziomie istotnoœci 0.05", 
        "Nie ma podstaw do odrzucenia hipotezy zerowej na poziomie istotnoœci 
0.05")
#czerwone wina
x13<-subset(cos,cos$color=="red")
testisto12<-t.test(x13$sulphates,x13$`sulphates after filtering`)$p.value 
ifelse (testisto12 <0.05, 
        "Odrzucamy hipotezê zerow¹ na poziomie istotnoœci 0.05", 
        "Nie ma podstaw do odrzucenia hipotezy zerowej na poziomie istotnoœci 
0.05")

