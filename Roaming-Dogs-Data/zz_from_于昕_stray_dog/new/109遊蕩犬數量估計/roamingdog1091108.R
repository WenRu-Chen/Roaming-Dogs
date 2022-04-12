setwd("/Users/User/Desktop/NCKU/project/stray_dog/new/109遊蕩犬數量估計")
original_dta<-read.csv(file = "v2.csv", sep = ',', fileEncoding = "big5") #read.csv(file = "1125斜率p25revised.csv", sep = ',', fileEncoding = "big5")
investvillage<-read.csv(file = "investvillage.csv", sep = ',',fileEncoding = 'big5')
investtownship<-read.csv(file = "investtownship.csv", sep = ',', fileEncoding = "big5")

'%!in%' <- function(x,y)!('%in%'(x,y))
dta<-merge(investvillage,original_dta[,c(5,6,7,8)], by='fullname', all.x = TRUE)

#classifying the sample data by sampling method
SRSdta<-dta[which(dta$cluster==0 & dta$investigate=="Y"),]
Twostagedta<-dta[which(dta$cluster!=0 & dta$investigate=="Y"),]
Twostagedta$city_township<-paste(Twostagedta$city, Twostagedta$township, sep = '')

#----SRS----
#sample total of each city (SRS)
NbSRS<-aggregate(Nb~city, data = SRSdta, sum)
NrSRS<-aggregate(Nr~city, data = SRSdta, sum)
NtSRS<-aggregate(Nt~city, data = SRSdta, sum)

#各縣市第0層investigate/original village number and multiplier
cluster0vill.no<-aggregate(village_number~city, data = investtownship[which(investtownship$cluster==0),], sum)
cluster0invest.vill.no<-data.frame(table(SRSdta$city))
colnames(cluster0vill.no)<-c('city','vill.p.size')
colnames(cluster0invest.vill.no)<-c('city','vill.s.size')
multiplier.cluster0<-merge(cluster0vill.no,cluster0invest.vill.no, by='city')
##
multiplier.cluster0$multiplierSRS<-multiplier.cluster0$vill.p.size/multiplier.cluster0$vill.s.size

####Estimated total of each city in cluster 0
SRS.estimated.total<-data.frame('city'=NbSRS$city, 'NbSRS.e'=NbSRS$Nb*multiplier.cluster0$multiplierSRS, 'NrSRS.e'=NrSRS$Nr*multiplier.cluster0$multiplierSRS, 'NtSRS.e'=NtSRS$Nt*multiplier.cluster0$multiplierSRS)
####

#sample mean of each city
NbSRSmean<-aggregate(Nb~city, data = SRSdta, mean)
NrSRSmean<-aggregate(Nr~city, data = SRSdta, mean)
NtSRSmean<-aggregate(Nt~city, data = SRSdta, mean)
colnames(NbSRSmean)<-c('city','NbSRSmean')
colnames(NrSRSmean)<-c('city','NrSRSmean')
colnames(NtSRSmean)<-c('city','NtSRSmean')

#full SRS data with sample mean
SRSdtafull<-merge(merge(merge(SRSdta,NbSRSmean,by='city',all.x = TRUE),NrSRSmean,by='city',all.x = TRUE),NtSRSmean,by='city',all.x = TRUE)

SRS.square<-data.frame('city'=SRSdtafull$city,"nb.square"=(SRSdtafull$Nb-SRSdtafull$NbSRSmean)^2,"nr.square"=(SRSdtafull$Nr-SRSdtafull$NrSRSmean)^2,"nt.square"=(SRSdtafull$Nt-SRSdtafull$NtSRSmean)^2)
SRS.ss<-merge(merge(aggregate(nb.square~city,data = SRS.square,sum),aggregate(nr.square~city,data = SRS.square,sum),by='city'),aggregate(nt.square~city,data = SRS.square,sum),by='city')
mm.cluster0<-multiplier.cluster0$vill.p.size*(multiplier.cluster0$vill.p.size-multiplier.cluster0$vill.s.size)/(multiplier.cluster0$vill.s.size*(multiplier.cluster0$vill.s.size-1))
SRS.var<-data.frame('city'=SRS.ss$city, 'nb.var'=SRS.ss$nb.square*mm.cluster0, 'nr.var'=SRS.ss$nr.square*mm.cluster0, 'nt.var'=SRS.ss$nt.square*mm.cluster0)

###--------非0層的遊蕩犬數量估計(2-stage sampling)----------
#multiplier of the estimating process at the first stage 
Twostage.township<-investvillage[investvillage$township%in%Twostagedta$township,]
Twostage.township$city_township<-paste(Twostage.township$city,Twostage.township$township, sep='')
township.p.sample<-data.frame(table(Twostage.township$city_township))
township.s.sample<-data.frame(table(Twostage.township[which(Twostage.township$investigate=="Y"),]$city_township))
colnames(township.p.sample)<-c('city_township', 'township.p.sample')
colnames(township.s.sample)<-c('city_township', 'township.s.sample')
multiplier.township<-merge(township.p.sample,township.s.sample, by='city_township')
###
multiplier.township$multiplier2stage<-multiplier.township$township.p.sample/multiplier.township$township.s.sample

#sample total of each township (2stage)
Nb2stage<-aggregate(Nb~city_township, data = Twostagedta, sum)
Nr2stage<-aggregate(Nr~city_township, data = Twostagedta, sum)
Nt2stage<-aggregate(Nt~city_township, data = Twostagedta, sum)

#estimated total of each township in the second stage
Twostage.estimated.total1<-data.frame('city_township'=multiplier.township$city_township, 'Nb2stage.e'=Nb2stage$Nb*multiplier.township$multiplier2stage,
                                      'Nr2stage.e'=Nr2stage$Nr*multiplier.township$multiplier2stage, 'Nt2stage.e'=Nt2stage$Nt*multiplier.township$multiplier2stage)

#sample mean of each township
Nb2stagemean<-aggregate(Nb~city_township, data = Twostagedta, mean)
Nr2stagemean<-aggregate(Nr~city_township, data = Twostagedta, mean)
Nt2stagemean<-aggregate(Nt~city_township, data = Twostagedta, mean)
colnames(Nb2stagemean)<-c('city_township','Nb2stagemean')
colnames(Nr2stagemean)<-c('city_township','Nr2stagemean')
colnames(Nt2stagemean)<-c('city_township','Nt2stagemean')

#full SRS data with sample mean (township)
Twostagedtafull<-merge(merge(merge(Twostagedta,Nb2stagemean,by='city_township',all.x = TRUE),Nr2stagemean,by='city_township',all.x = TRUE),Nt2stagemean,by='city_township',all.x = TRUE)

Twostage.square.1<-data.frame('city_township'=Twostagedtafull$city_township,"nb.square"=(Twostagedtafull$Nb-Twostagedtafull$Nb2stagemean)^2,"nr.square"=(Twostagedtafull$Nr-Twostagedtafull$Nr2stagemean)^2,"nt.square"=(Twostagedtafull$Nt-Twostagedtafull$Nt2stagemean)^2)
Twostage.ss.1<-merge(merge(aggregate(nb.square~city_township,data = Twostage.square.1,sum),aggregate(nr.square~city_township,data = Twostage.square.1,sum),by='city_township'),aggregate(nt.square~city_township,data = Twostage.square.1,sum),by='city_township')
mm.cluster1<-multiplier.township$township.p.sample*(multiplier.township$township.p.sample-multiplier.township$township.s.sample)/(multiplier.township$township.s.sample*(multiplier.township$township.s.sample-1))

Twostage.square.2<-data.frame('city_township'=Twostage.ss.1$city, 'nb.square'=Twostage.ss.1$nb.square*mm.cluster1, 'nr.square'=Twostage.ss.1$nr.square*mm.cluster1, 'nt.square'=Twostage.ss.1$nt.square*mm.cluster1)
Twostage.square.2<-merge(Twostage.square.2,Twostagedtafull[,c(1,4,7)], by.x='city_township', no.dups = TRUE)
library(dplyr)
Twostage.square.2<-Twostage.square.2%>%distinct(city_township,.keep_all = TRUE)
Twostage.square.2$city_cluster<-paste(Twostage.square.2$city,Twostage.square.2$cluster,sep = '')
###------------------
#multiplier of the estimating process at the first stage 
Twostage.cluster<-investtownship[which(investtownship$cluster!=0),]
Twostage.cluster$city_township<-paste(Twostage.cluster$city,Twostage.cluster$township, sep='')
Twostage.cluster$city_cluster<-paste(Twostage.cluster$city,Twostage.cluster$cluster, sep='')
cluster.p.sample<-data.frame(table(Twostage.cluster$city_cluster))
cluster.s.sample<-data.frame(table(Twostage.cluster[which(Twostage.cluster$investigate_towship=="Y"),]$city_cluster))
colnames(cluster.p.sample)<-c('city_cluster', 'cluster.p.sample')
colnames(cluster.s.sample)<-c('city_cluster', 'cluster.s.sample')
multiplier.cluster<-merge(cluster.p.sample,cluster.s.sample, by='city_cluster')
###
multiplier.cluster$multiplier2stage<-multiplier.cluster$cluster.p.sample/multiplier.cluster$cluster.s.sample

Twostage.cluster.e<-merge(Twostage.estimated.total1,Twostage.cluster[,c(4,6,7)], by='city_township', all.x = TRUE)

#sample total of each cluster (2stage)
Nb2stage.e<-aggregate(Nb2stage.e~city_cluster, data = Twostage.cluster.e, sum)
Nr2stage.e<-aggregate(Nr2stage.e~city_cluster, data = Twostage.cluster.e, sum)
Nt2stage.e<-aggregate(Nt2stage.e~city_cluster, data = Twostage.cluster.e, sum)
 
Twostage.estimated.total2<-data.frame('city_cluster'=multiplier.cluster$city_cluster, 'Nb2stage.ee'=Nb2stage.e$Nb2stage.e*multiplier.cluster$multiplier2stage,
                                      'Nr2stage.ee'=Nr2stage.e$Nr2stage.e*multiplier.cluster$multiplier2stage, 'Nt2stage.ee'=Nt2stage.e$Nt2stage.e*multiplier.cluster$multiplier2stage)


Twostage.ss.2<-merge(merge(aggregate(nb.square~city_cluster,data = Twostage.square.2,sum),aggregate(nr.square~city_cluster,data = Twostage.square.2,sum),by='city_cluster'),aggregate(nt.square~city_cluster,data = Twostage.square.2,sum),by='city_cluster')

Twostage.var.2<-data.frame('city_cluster'=Twostage.ss.2$city_cluster, 'nb.var'=Twostage.ss.2$nb.square*multiplier.cluster$multiplier2stage, 'nr.var'=Twostage.ss.2$nr.square*multiplier.cluster$multiplier2stage, 'nt.var'=Twostage.ss.2$nt.square*multiplier.cluster$multiplier2stage)

mm.cluster2<-multiplier.cluster$cluster.p.sample*(multiplier.cluster$cluster.p.sample-multiplier.cluster$cluster.s.sample)/(multiplier.cluster$cluster.s.sample*(multiplier.cluster$cluster.s.sample-1))


#非0層第一階段var------------------
#鄉鎮估計
mu.u.nb<-aggregate(Nb2stage.e~city_cluster, data = Twostage.cluster.e, mean)
mu.u.nr<-aggregate(Nr2stage.e~city_cluster, data = Twostage.cluster.e, mean)
mu.u.nt<-aggregate(Nt2stage.e~city_cluster, data = Twostage.cluster.e, mean)
colnames(mu.u.nb)<-c('city_cluster','mu.u.nb')
colnames(mu.u.nr)<-c('city_cluster','mu.u.nr')
colnames(mu.u.nt)<-c('city_cluster','mu.u.nt')
Twostage.cluster.e.full<-merge(merge(merge(Twostage.cluster.e,mu.u.nb,by='city_cluster'),mu.u.nr,by='city_cluster'),mu.u.nt, by='city_cluster')

first.stage.square<-data.frame('city_cluster'=Twostage.cluster.e.full$city_cluster,
                               'nb.square'=(Twostage.cluster.e.full$Nb2stage.e-Twostage.cluster.e.full$mu.u.nb)^2,
                               'nr.square'=(Twostage.cluster.e.full$Nr2stage.e-Twostage.cluster.e.full$mu.u.nr)^2,
                               'nt.square'=(Twostage.cluster.e.full$Nt2stage.e-Twostage.cluster.e.full$mu.u.nt)^2)
first.stage.ss<-merge(merge(aggregate(nb.square~city_cluster, data = first.stage.square, sum),
                            aggregate(nr.square~city_cluster, data = first.stage.square, sum),by='city_cluster'),
                      aggregate(nt.square~city_cluster, data = first.stage.square, sum),by='city_cluster')
colnames(first.stage.ss)<-c('city_cluster','nb.ss','nr.ss','nt.ss')

mm.cluster<-multiplier.cluster$cluster.p.sample*(multiplier.cluster$cluster.p.sample-multiplier.cluster$cluster.s.sample)/(multiplier.cluster$cluster.s.sample*(multiplier.cluster$cluster.s.sample-1))
Twostage.var.1<-data.frame('city_cluster'=first.stage.ss$city_cluster, 'nb.var'=first.stage.ss$nb.ss*mm.cluster, 'nr.var'=first.stage.ss$nr.ss*mm.cluster, 'nt.var'=first.stage.ss$nt.ss*mm.cluster)

Twostage.var.1
Twostage.var.2
SRS.var

Twostage.var.1$city<-substr(Twostage.var.1$city_cluster,1, 3)
Twostage.var.2$city<-substr(Twostage.var.2$city_cluster,1, 3)

Twostage.var.1.complete<-cbind(aggregate(nb.var~city, data = Twostage.var.1, sum),
                               aggregate(nr.var~city, data = Twostage.var.1, sum)[,2],
                               aggregate(nt.var~city, data = Twostage.var.1, sum)[,2])
colnames(Twostage.var.1.complete)<-colnames(SRS.var)

Twostage.var.2.complete<-cbind(aggregate(nb.var~city, data = Twostage.var.2, sum),
                               aggregate(nr.var~city, data = Twostage.var.2, sum)[,2],
                               aggregate(nt.var~city, data = Twostage.var.2, sum)[,2])
colnames(Twostage.var.2.complete)<-colnames(SRS.var)

var.complete<-merge(merge(SRS.var, Twostage.var.1.complete, by='city', all = TRUE), Twostage.var.2.complete, by='city', all = TRUE)

###縣市variance (Nb, Nr, Nt)
Varianceee<-data.frame('city'=var.complete$city,'nb.var'=rowSums(var.complete[,c(2,5,8)], na.rm = TRUE),
                       'nr.var'=rowSums(var.complete[,c(3,6,9)], na.rm = TRUE), 'nt.var'=rowSums(var.complete[,c(4,7,10)], na.rm = TRUE))


##-------合併SRS/two-stage的估計資料-----
Twostage.estimated.total2$city<-substr(Twostage.estimated.total2$city_cluster,1,3)

SRS.estimated.total$city_cluster<-SRS.estimated.total$city
colnames(SRS.estimated.total)<-colnames(Twostage.estimated.total2)

#各層（都市化程度分層）estimation
full.estimated.dta<-rbind(SRS.estimated.total,Twostage.estimated.total2)

#縣市estimation (Nb, Nr, Nt)
city.Nb.total<-aggregate(Nb2stage.ee~city, data = full.estimated.dta, sum)
city.Nr.total<-aggregate(Nr2stage.ee~city, data = full.estimated.dta, sum)
city.Nt.total<-aggregate(Nt2stage.ee~city, data = full.estimated.dta, sum)
city.estimated.total<-merge(merge(city.Nb.total,city.Nr.total, by='city'),city.Nt.total, by = 'city')

#全國estimation (Nb, Nr, Nt)
sum(city.estimated.total$Nb2stage.ee)
sum(city.estimated.total$Nr2stage.ee)
sum(city.estimated.total$Nt2stage.ee)

write.csv(city.estimated.total,"C:/Users/User/Desktop/NCKU/project/stray_dog/new/109遊蕩犬數量估計/vv2.csv")
write.csv(Varianceee,"C:/Users/User/Desktop/NCKU/project/stray_dog/new/109遊蕩犬數量估計/variance.csv")



