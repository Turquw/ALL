#zad 1
#Scharakteryzuj 5 wybranych zmiennych: minimum, pierwszy kwartyl, medina, �rednia, trzeci kwartyl, maksimum, odchylenie standardowe, wybran� miar� asymetrii i koncentracji w ka�dej z 6 grup: wina czerwone z Katanii, wina bia�e z Katanii, wina czerwone z Bolonii itd.
library(e1071)
x1<-subset(cos,cos$region=="Bolonia")
x1<-subset(x1,x1$color=="white")
BoloniaWMin<-mapply(min,x1[,c(3,4,5,6,7 )])
BoloniaWMax<-mapply(max,x1[,c(3,4,5,6,7 )])
BoloniaWMean<-mapply(mean,x1[,c(3,4,5,6,7 )])#srednia
BoloniaWMedian<-mapply(median,x1[,c(3,4,5,6,7 )])
BoloniaWSd<-mapply(sd,x1[,c(3,4,5,6,7 )])#odchylenie standardowe
BoloniaWQ1<-mapply(quantile,x1[,c(3,4,5,6,7 )],0,25,na.rm=TRUE)#kwartyl pierwszy
BoloniaWQ3<-mapply(quantile,x1[,c(3,4,5,6,7 )],0,75,na.rm=TRUE)
BoloniaWPerson<-3*(BoloniaWMean-BoloniaWMedian)/BoloniaWSd#asymetria
BoloniaWKurtiza<-mapply(kurtosis,x1[,c(3,4,5,6,7 )])#koncentracja

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

#bia�e z Emilia Romana
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

#zad 6

#Zbuduj 95% przedzia� ufno�ci dla �redniej i wariancji dla dw�ch wybranych zmiennych osobno dla win z Bolonii, Katanii i Emilii Romany
x51<-subset(cos,cos$region=="Catania")
#�rednia
Q1.N        <- length(x51$`fixed acidity`)# rozmiar probki
Q1.mean     <- mapply(mean,x51[,c(3,4 )])  
Q1.sd       <- mapply(sd,x51[,c(3,4)])
Q1.z        <- qnorm(.975)   # wartosc Z95
Q1.mean - Q1.sd*Q1.z/sqrt(Q1.N)
Q1.mean + Q1.sd*Q1.z/sqrt(Q1.N)
#wariacja
d<-mapply(var,x51[,c(3,4)])
((2*Q1.N-1)*d)/((sqrt(2*Q1.N-5)+qnorm(1 -0.95/2)))^2
((2*Q1.N-1)*d)/((sqrt(2*Q1.N-5)-qnorm(1 -0.95/2)))^2
#koeljny region

x522<-subset(cos,cos$region=="Emilia Romana")
#�rednia
Q12.N        <- length(x522$`volatile acidity`)# rozmiar probki
Q12.mean     <- mapply(mean,x522[,c(3,4 )])  
Q12.sd       <- mapply(sd,x522[,c(3,4)])
Q12.z        <- qnorm(.975)   # wartosc Z95
Q12.mean - Q12.sd*Q12.z/sqrt(Q12.N)
Q12.mean + Q12.sd*Q12.z/sqrt(Q12.N)
#wariacja
d<-mapply(var,x522[,c(3,4)])
((2*Q12.N-1)*d)/((sqrt(2*Q12.N-5)+qnorm(1 -0.95/2)))^2
((2*Q12.N-1)*d)/((sqrt(2*Q12.N-5)-qnorm(1 -0.95/2)))^2


#koeljny region

x5222<-subset(cos,cos$region=="Bolonia")
#�rednia
Q122.N        <- length(x522$`fixed acidity`)# rozmiar probki
Q122.mean     <- mapply(mean,x522[,c(3,4 )])  
Q122.sd       <- mapply(sd,x522[,c(3,4)])
Q122.z        <- qnorm(.975)   # wartosc Z95
Q122.mean - Q122.sd*Q122.z/sqrt(Q122.N)
Q122.mean + Q122.sd*Q122.z/sqrt(Q122.N)
#wariacja
d<-mapply(var,x5222[,c(3,4)])
((2*Q122.N-1)*d)/((sqrt(2*Q122.N-5)+qnorm(1 -0.95/2)))^2
((2*Q122.N-1)*d)/((sqrt(2*Q122.N-5)-qnorm(1 -0.95/2)))^2






#zad 7
#Sprawd� czy poziom siarki istotnie zmieni� si� po filtracji (zmienne sulphates i sulphates after filtering). Test wykonaj dla wszystkich win og�em jak i w podziale na kolor wina.
#og�lny test
testisto<-t.test(cos$sulphates,cos$`sulphates after filtering`)$p.value 
ifelse (testisto <0.05, 
       "Odrzucamy hipotez� zerow� na poziomie istotno�ci 0.05", 
       "Nie ma podstaw do odrzucenia hipotezy zerowej na poziomie istotno�ci 
0.05")
#bia�e wino
x12<-subset(cos,cos$color=="white")
testisto1<-t.test(x12$sulphates,x12$`sulphates after filtering`)$p.value 
ifelse (testisto1 <0.05, 
        "Odrzucamy hipotez� zerow� na poziomie istotno�ci 0.05", 
        "Nie ma podstaw do odrzucenia hipotezy zerowej na poziomie istotno�ci 
0.05")
#czerwone wina
x13<-subset(cos,cos$color=="red")
testisto12<-t.test(x13$sulphates,x13$`sulphates after filtering`)$p.value 
ifelse (testisto12 <0.05, 
        "Odrzucamy hipotez� zerow� na poziomie istotno�ci 0.05", 
        "Nie ma podstaw do odrzucenia hipotezy zerowej na poziomie istotno�ci 
0.05")


#zad 8
#Wybierz 2 zmienne charakterystyk wina. Sprawd� czy �redni poziom ka�dej zmiennej r�ni si� pomi�dzy grup� win czerwonych a bia�ych
licz1<-x13[,c(3,4 )]
licz2<-x12[,c(3,4 )]
am.test<-t.test(licz1,licz2,paired=F)
am.test$p.value
#tak r�ni sie 
#zad 9
#Wybierz 2 zmienne charakterystyk wina. Sprawd� czy wariancja ka�dej zmiennej r�ni si� pomi�dzy grup� win czerwonych a bia�ych
d1<-mapply(var,x12[,c(3,4)])
d11<-mapply(var,x13[,c(3,4)])
am.test2<-t.test(d1,d11,paired=F)
am.test2$p.value
#tak r�ni sie
#zad 10
#Wybierz 2 zmienne charakterystyk wina. Sprawd� czy �redni poziom ka�dej zmiennej r�ni si� pomi�dzy regionami W�och

