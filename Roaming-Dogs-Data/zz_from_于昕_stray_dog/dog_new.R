#----2020 stray dog----
library(readr)
library(dplyr)
library(readxl)
dogs <- read_excel("C:/Users/User/Desktop/NCKU/project/stray_dog/dogs.xlsx")
colnames(dogs)[1:8] = c("name","city","township","village","day","y","x","m")

## 計算所需變數
# 加一行新的(xj==前(j-1)次被捕的總犬隻)
xj <- matrix(0,dim(dogs)[1],1)
dogs=cbind(dogs,xj)
# 算xj的值
for(i in 1:(dim(dogs)[1]/5)){
  dogs$xj[(5*i-3):(5*i)]=dogs$x[(5*i-4):(5*i-1)]
  dogs$xj[5*i-4]=0
}

# 計算Mj*xj
dogs$Mj_xj = (dogs$m + dogs$y)*dogs$xj

## Nb_hat
options(scipen = 999)
N_nb <- dogs %>% group_by(city,township,village) %>% summarise(Nb = round(sum(Mj_xj)/sum(m),digits = 4))

# 測試資料集
DOG <- dogs

## Nr_hat
# 重新排序斜率為正的資料
# 使用if判斷時,coefficients不能有NA值,因此須將NA用0代替
for (i in 1:(dim(dogs)[1]/5)) {
  if(lm(y[(5*i-4):(5*i)]~xj[(5*i-4):(5*i)] , data = dogs)$coefficients[2]>=0 || is.na(lm(y[(5*i-4):(5*i)]~xj[(5*i-4):(5*i)] , data = dogs)$coefficients[2]))
  {
    dogs$y[(5*i-4):(5*i)] <- sort(dogs$y[(5*i-4):(5*i)] , decreasing = T)
    dogs$x[((5*i-4))] <- dogs$y[((5*i-4))]
    for (j in 1:4) {
      dogs$x[((5*i-4)+j)] <- dogs$x[((5*i-4)+j-1)]+dogs$y[((5*i-4)+j)]
    }
    dogs$xj[(5*i-3):(5*i)]=dogs$x[((5*i-4)):(5*i-1)]
    dogs$xj[(5*i-4)]=0
    dogs$m[(5*i-4):(5*i)] = dogs$m[5*(i-1)+order(dogs$y[(5*i-4):(5*i)],decreasing = T)]
  }
}
dogs$Mj_xj = (dogs$m + dogs$y)*dogs$xj

## 計算x_bar,y_bar,cof1,cof2,N
options(scipen = 999)
N <- dogs %>% group_by(city,township,village) %>% summarise(y_bar=sum(y)/5,x_bar=sum(xj)/5,
                                                           cof1 = lm(y~xj)$coefficients[1],
                                                           cof2 = round(lm(y~xj)$coefficients[2],digits = 4),
                                                           Nb = round(sum(Mj_xj)/sum(m),digits = 4))
fullname <- matrix(paste0(N$city,N$township,N$village),dim(N)[1],1)
N=cbind(data.frame(N[,1:3]),data.frame(fullname),data.frame(N[,4:8]))
N$Nr=round(-(N$cof1/N$cof2),digits = 4)
N$Nb=N_nb$Nb

# 補上記錄人姓名
# xj <- matrix(0,dim(N)[1],1)
# for (i in 1:dim(N)[1]) {
#   xj[i,1] = dogs$name[5*i]
# }
# 
# library(tibble)
# N <- add_column(N, xj, .before = "city")
# colnames(N)[1] = "names"

# 處理Nr的缺失值
N$Nr[is.na(N$Nr)] <-0
N$Nr[is.infinite(N$Nr)] <- dogs$x[which(is.infinite(N$Nr))*5]
# 處理Nb的缺失值
N$Nb[is.na(N$Nb)&N$Nr==0] <-0
N$Nb[is.na(N$Nb)&N$Nr!=0] <-N$Nr[which(is.na(N$Nb)&N$Nr!=0)]
N$Nb[is.infinite(N$Nb)] <- dogs$x[which(is.infinite(N$Nb))*5]
# 總觀察數
for (i in 1:(dim(dogs)[1]/5)) {
  N$Nt[i] <- dogs$x[i*5]
}

#生成最終檔案
NN <- N[,-(5:8)]

write.csv(NN,"C:/Users/User/Desktop/NCKU/project/stray_dog/new/109遊蕩犬數量估計/v.csv")
write.csv(dogs,"C:/Users/User/Desktop/NCKU/project/stray_dog/new/v1_dogs.csv")

#----利用斜率挑選第6天需要加0的村里----
N1 <- N
c = which(N1$cof2 > -0.1)
for (i in 1:length(c)) {
  try <- dogs[(5*c[i]-4):(5*c[i]),]
  x <- c(0,0,0,0,0,0,0,0,0,0)
  try <- rbind(try,x)
  try$x[6] <- try$x[5]
  try$xj[6] <- try$x[5]
  N1$y_bar[c[i]] = sum(try$y)/6
  N1$x_bar[c[i]] = sum(try$xj)/6
  N1$cof1[c[i]] = lm(try$y~try$xj)$coefficients[1]
  N1$cof2[c[i]] = round(lm(try$y~try$xj)$coefficients[2],digits = 4)
  N1$Nr[c[i]] <- round(-(N1$cof1[c[i]]/N1$cof2[c[i]]),digits = 4)
}
#生成最終檔案
N1 <- N1[,-(5:8)]
write.csv(N1,"C:/Users/User/Desktop/NCKU/project/stray_dog/new/109遊蕩犬數量估計/v1.csv")

#----利用斜率挑選第6天需要加1的村里----
N2 <- N
c = which(N2$cof2 > -0.1)
for (i in 1:length(c)) {
  try <- dogs[(5*c[i]-4):(5*c[i]),]
  x <- c(0,0,0,0,0,1,0,0,0,0)
  try <- rbind(try,x)
  try$x[6] <- try$x[5]+1
  try$xj[6] <- try$x[5]
  try$Mj_xj[6] <- try$x[5]
  N2$y_bar[c[i]] = sum(try$y)/6
  N2$x_bar[c[i]] = sum(try$xj)/6
  N2$cof1[c[i]] = lm(try$y~try$xj)$coefficients[1]
  N2$cof2[c[i]] = round(lm(try$y~try$xj)$coefficients[2],digits = 4)
  N2$Nr[c[i]] <- round(-(N2$cof1[c[i]]/N2$cof2[c[i]]),digits = 4)
}
# 處理Nr的缺失值(因為第6天加1會導致有些村里六天都是1  所以要再修正一次)
N2$Nr[is.na(N2$Nr)] <-0
N2$Nr[is.infinite(N2$Nr)] <- dogs$x[which(is.infinite(N2$Nr))*5]

#生成最終檔案
N2 <- N2[,-(5:8)]
write.csv(N2,"C:/Users/User/Desktop/NCKU/project/stray_dog/new/109遊蕩犬數量估計/v2.csv")


#----調整Nr.Nb----
#挑選Nr>100且Nt<100的村里進行修正
N1 <- N
for (i in 1:length(which(N$Nr>100 & N$Nt<100))) {
  try <- dogs[(5*which(N$Nr>100 & N$Nt<100)[i]-4):(5*which(N$Nr>100 & N$Nt<100)[i]),]
  x <- c(0,0,0,0,0,0,0,0,0,0)
  try <- rbind(try, x)
  try$x[6] <- try$x[5]
  try$xj[6] <-try$x[5]
  # N1[which(N$Nr>100 & N$Nt<100)[i],5:8] <- c(sum(try$y)/6, sum(try$xj)/6, 
  #                                           lm(try$y~try$xj)$coefficients[1], 
  #                                           round(lm(try$y~try$xj)$coefficients[2],digits = 4))
  N1$y_bar[which(N$Nr>100 & N$Nt<100)[i]] = sum(try$y)/6
  N1$x_bar[which(N$Nr>100 & N$Nt<100)[i]] = sum(try$xj)/6
  N1$cof1[which(N$Nr>100 & N$Nt<100)[i]] = lm(try$y~try$xj)$coefficients[1]
  N1$cof2[which(N$Nr>100 & N$Nt<100)[i]] = round(lm(try$y~try$xj)$coefficients[2],digits = 4)
  N1$Nr[which(N$Nr>100 & N$Nt<100)[i]] <- 
    round(-(N1$cof1[which(N$Nr>100 & N$Nt<100)[i]]/N1$cof2[which(N$Nr>100 & N$Nt<100)[i]]),digits = 4)
}
#將Nb>100的改成Nt(實際觀察到的數量)
for (i in 1:dim(N1)[1]) {
  if(N1$Nb[i] > 100){
    N1$Nb[i] <- N1$Nt[i]
    }
}

write.csv(N1,"C:/Users/User/Desktop/NCKU/project/stray_dog/1125.csv")

#挑選Nr>100的村里進行修正
N2 <- N
for (i in 1:length(which(N$Nr>100 ))) {
  try <- dogs[(5*which(N$Nr>100 )[i]-4):(5*which(N$Nr>100 )[i]),]
  x <- c(0,0,0,0,0,0,0,0,0,0)
  try <- rbind(try, x)
  try$x[6] <- try$x[5]
  try$xj[6] <-try$x[5]
  # N2[which(N$Nr>100 )[i],5:8] <- c(sum(try$y)/6, sum(try$xj)/6, 
  #                                           lm(try$y~try$xj)$coefficients[1], 
  #                                           round(lm(try$y~try$xj)$coefficients[2],digits = 4))
  N2$y_bar[which(N$Nr>100 )[i]] = sum(try$y)/6
  N2$x_bar[which(N$Nr>100 )[i]] = sum(try$xj)/6
  N2$cof1[which(N$Nr>100 )[i]] = lm(try$y~try$xj)$coefficients[1]
  N2$cof2[which(N$Nr>100 )[i]] = round(lm(try$y~try$xj)$coefficients[2],digits = 4)
  N2$Nr[which(N$Nr>100 )[i]] <- 
    round(-(N2$cof1[which(N$Nr>100 )[i]]/N2$cof2[which(N$Nr>100 )[i]]),digits = 4)
}

#將Nb>100的改成Nt(實際觀察到的數量)
for (i in 1:dim(N2)[1]) {
  if(N2$Nb[i] > 100){
    N2$Nb[i] <- N2$Nt[i]
  }
}
write.csv(N2,"C:/Users/User/Desktop/NCKU/project/stray_dog/N2.csv")






###
i=178
try <- dogs[(5*c[i]-4):(5*c[i]),]
x <- c(0,0,0,0,0,1,0,0,0,0)
try <- rbind(try,x)
try$x[6] <- try$x[5]+1
try$xj[6] <- try$x[5]
try$Mj_xj[6] <- try$x[5]



###切資料
library(readxl)
library(dplyr)

dogs <- read_excel("C:/Users/User/Desktop/NCKU/project/stray_dog/dogs.xlsx")
dogs = dogs[,-1]
d = data.frame(table(dogs$縣市))

for(k in 1:dim(d)[1]){
  data = dogs[which(dogs$縣市 == as.character(d[k,1])),]
  write.csv(data,paste("C:/Users/User/Desktop/NCKU/project/stray_dog/city",paste(as.character(d[k,1]),".csv"),sep="/"))
}
