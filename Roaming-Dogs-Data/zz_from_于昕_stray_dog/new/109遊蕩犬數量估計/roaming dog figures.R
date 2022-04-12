#----------------------------------------------------------
fig.info<-data.frame('city'=c("宜蘭縣","花蓮縣","金門縣","南投縣","屏東縣","苗栗縣",
                              "桃園市","高雄市","基隆市","連江縣","雲林縣","新北市",
                              "新竹市","新竹縣","嘉義市","嘉義縣","彰化縣","臺中市",
                              "臺北市","臺東縣","臺南市","澎湖縣"),
                     'estimate'=c(4765,7113,289,8106,26033,4433,13659,21135,4648,0,9357,
                                  14341,965,5080,1248,22307,11321,7993,3851,4872,24205,
                                  1803),
                     'estimate107'=c(4667,8167,305,4243,10973,1685,9686,5992,
                                     4807,41,3542,12202,783,5659,3094,9097,
                                     16473,7029,4660,8485,22176,3008),
                     'CI-length'=c(1713,3321,194,2484,11726,2289,3949,5939,1426,
                                   0,5204,4412,395,960,721,8876,3974,3388,1404,
                                   1967,11658,1010),
                     'CI-length107'=c(2372.80643193767,4667.70761935441,257.563987047879,
                                      2534.25614109419,4467.56589010204,1505.47789212223,
                                      6293.46028214991,7601.19487293654,4365.6377287239,
                                      15.245222202382,2035.11878041239,5658.5973088103,
                                      285.789871042345,4768.24166246804,1316.91336858337,
                                      5758.3818874215,3583.54928445797,6086.43980757908,
                                      2318.28622029015,12721.2590260083,10208.5883240611,
                                      4121.40532367143),
                     'estimate93'=c(8430,4989,634,3730,8143,11302,16187,16928,
                                     5229,95,2819,35960,1688,4828,1191,2662,
                                     12141,24962,4946,4036,8246,311),
                     'estimate98'=c(4006,2084,442,1202,1783,6543,4020,9505,626,
                                     87,8646,5060,2304,5210,573,3579,7130,9174,
                                     3192,3130,5870,725),
                     'estimate103'=c(6626,4807,908,2819,4625,2435,8960,15220,
                                      404,87,9163,4295,1862,2318,3535,6811,7100,
                                      15028,2931,3229,24596,713)
)



fig.info$city<-factor(fig.info$city, levels =c("宜蘭縣","花蓮縣","金門縣","南投縣","屏東縣","苗栗縣",
                                               "桃園市","高雄市","基隆市","連江縣","雲林縣","新北市",
                                               "新竹市","新竹縣","嘉義市","嘉義縣","彰化縣","臺中市",
                                               "臺北市","臺東縣","臺南市","澎湖縣") )

fig.info2<-data.frame('city'=rep(fig.info$city,2),
                      'year'=rep(c('107','109'),each=nrow(fig.info)),
                      'estimate'=c(fig.info$estimate107,fig.info$estimate),
                      'CI-length'=c(fig.info$CI.length107,fig.info$CI.length))

year.compare<-data.frame('year'=c("93","98","103","107","109"),'estimate'=c(179457,84891,128473,146773,155869), 'CI.length'=c(NA,NA,NA,24592,17897))
year.compare$year<-factor(year.compare$year, levels = c("93","98","103","107","109"))
#----------------------------------------

library(ggplot2)
library(scales) # to access break formatting functions
# x and y axis are transformed and formatted
#----------------------------------------

fig.109<-ggplot(fig.info, aes(x=estimate, y=city)) + 
  scale_x_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=0.7) + 
  geom_errorbar(width=.1, aes(xmin=estimate-CI.length, xmax=estimate+CI.length), colour="navy") +
  labs(x="估計數量",y= "縣市")+
  theme(text=element_text(family="BiauKai", size=12))

which(fig.info$estimate107-fig.info$CI.length107<0)

fig.107<-ggplot(fig.info, aes(x=estimate107, y=city)) + 
  scale_x_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=0.7) + 
  geom_errorbar(data = fig.info[-c(8,20,22),],width=.1, aes(xmin=estimate107-CI.length107, xmax=estimate107+CI.length107), colour="red") +
  geom_errorbar(data = fig.info[c(8,20,22),],width=.1, aes(xmin=0, xmax=estimate107+CI.length107), colour="red") +
  labs(x="估計數量",y= "縣市")+
  theme(text=element_text(family="BiauKai", size=12))

#----------------------------------------

fig.info.t<-data.frame(t(fig.info)[-1,])
colnames(fig.info.t)<-fig.info$city
fig.info.t<-as.data.frame(lapply(fig.info.t, as.numeric))
fig.info.t$info<-c("109","107","CI109","CI107","93","98","103")
fig.info.t.a<-fig.info.t[c(5,6,7,2,1),]
fig.info.t.b<-fig.info.t[c(4,3),]

fig.info.t.a$info<-factor(fig.info.t.a$info, levels = c("93","98","103","107","109"))

fig.1<-ggplot(fig.info.t.a, aes(y=宜蘭縣, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[c(4,5),],width=.1, aes(ymin=宜蘭縣-fig.info.t.b$宜蘭縣, ymax=宜蘭縣+fig.info.t.b$宜蘭縣)) +
  labs(y="估計數量",x= "年份",title = "宜蘭縣")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.2<-ggplot(fig.info.t.a, aes(y=花蓮縣, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[c(4,5),], width=.1, aes(ymin=花蓮縣-fig.info.t.b$花蓮縣, ymax=花蓮縣+fig.info.t.b$花蓮縣)) +
  labs(y="估計數量",x= "年份",title = "花蓮縣")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.3<-ggplot(fig.info.t.a, aes(y=金門縣, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[c(4,5),], width=.1, aes(ymin=金門縣-fig.info.t.b$金門縣, ymax=金門縣+fig.info.t.b$金門縣)) +
  labs(y="估計數量",x= "年份",title="金門縣")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.4<-ggplot(fig.info.t.a, aes(y=南投縣, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[c(4,5),], width=.1, aes(ymin=南投縣-fig.info.t.b$南投縣, ymax=南投縣+fig.info.t.b$南投縣)) +
  labs(y="估計數量",x= "年份",title = "南投縣")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.5<-ggplot(fig.info.t.a, aes(y=屏東縣, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[c(4,5),], width=.1, aes(ymin=屏東縣-fig.info.t.b$屏東縣, ymax=屏東縣+fig.info.t.b$屏東縣)) +
  labs(y="估計數量",x= "年份",title = "屏東縣")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.6<-ggplot(fig.info.t.a, aes(y=苗栗縣, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[c(4,5),], width=.1, aes(ymin=苗栗縣-fig.info.t.b$苗栗縣, ymax=苗栗縣+fig.info.t.b$苗栗縣)) +
  labs(y="估計數量",x= "年份",title = "苗栗縣")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.7<-ggplot(fig.info.t.a, aes(y=桃園市, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[c(4,5),], width=.1, aes(ymin=桃園市-fig.info.t.b$桃園市, ymax=桃園市+fig.info.t.b$桃園市)) +
  labs(y="估計數量",x= "年份",title = "桃園市")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.8<-ggplot(fig.info.t.a, aes(y=高雄市, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[5,],width=.1, aes(ymin=高雄市-fig.info.t.b$高雄市[2], ymax=高雄市+fig.info.t.b$高雄市[2])) +
  geom_errorbar(data = fig.info.t.a[4,],width=.1, aes(ymin=高雄市-高雄市, ymax=高雄市+fig.info.t.b$高雄市[1])) +
  labs(y="估計數量",x= "年份",title="高雄市")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.9<-ggplot(fig.info.t.a, aes(y=基隆市, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[c(4,5),], width=.1, aes(ymin=基隆市-fig.info.t.b$基隆市, ymax=基隆市+fig.info.t.b$基隆市)) +
  labs(y="估計數量",x= "年份",title = "基隆市")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.10<-ggplot(fig.info.t.a, aes(y=連江縣, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[c(4,5),], width=.1, aes(ymin=連江縣-fig.info.t.b$連江縣, ymax=連江縣+fig.info.t.b$連江縣)) +
  labs(y="估計數量",x= "年份",title = "連江縣")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.11<-ggplot(fig.info.t.a, aes(y=雲林縣, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[c(4,5),], width=.1, aes(ymin=雲林縣-fig.info.t.b$雲林縣, ymax=雲林縣+fig.info.t.b$雲林縣)) +
  labs(y="估計數量",x= "年份",title = "雲林縣")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.12<-ggplot(fig.info.t.a, aes(y=新北市, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[c(4,5),], width=.1, aes(ymin=新北市-fig.info.t.b$新北市, ymax=新北市+fig.info.t.b$新北市)) +
  labs(y="估計數量",x= "年份",title = "新北市")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.13<-ggplot(fig.info.t.a, aes(y=新竹市, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[c(4,5),], width=.1, aes(ymin=新竹市-fig.info.t.b$新竹市, ymax=新竹市+fig.info.t.b$新竹市)) +
  labs(y="估計數量",x= "年份",title = "新竹市")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.14<-ggplot(fig.info.t.a, aes(y=新竹縣, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[c(4,5),], width=.1, aes(ymin=新竹縣-fig.info.t.b$新竹縣, ymax=新竹縣+fig.info.t.b$新竹縣)) +
  labs(y="估計數量",x= "年份",title = "新竹縣")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.15<-ggplot(fig.info.t.a, aes(y=嘉義市, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[c(4,5),], width=.1, aes(ymin=嘉義市-fig.info.t.b$嘉義市, ymax=嘉義市+fig.info.t.b$嘉義市)) +
  labs(y="估計數量",x= "年份",title = "嘉義市")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.16<-ggplot(fig.info.t.a, aes(y=嘉義縣, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[c(4,5),], width=.1, aes(ymin=嘉義縣-fig.info.t.b$嘉義縣, ymax=嘉義縣+fig.info.t.b$嘉義縣)) +
  labs(y="估計數量",x= "年份",title = "嘉義縣")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.17<-ggplot(fig.info.t.a, aes(y=彰化縣, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[c(4,5),], width=.1, aes(ymin=彰化縣-fig.info.t.b$彰化縣, ymax=彰化縣+fig.info.t.b$彰化縣)) +
  labs(y="估計數量",x= "年份", title = "彰化縣")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.18<-ggplot(fig.info.t.a, aes(y=臺中市, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[c(4,5),], width=.1, aes(ymin=臺中市-fig.info.t.b$臺中市, ymax=臺中市+fig.info.t.b$臺中市)) +
  labs(y="估計數量",x= "年份",title = "臺中市")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.19<-ggplot(fig.info.t.a, aes(y=臺北市, x=info)) + 
  scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[c(4,5),], width=.1, aes(ymin=臺北市-fig.info.t.b$臺北市, ymax=臺北市+fig.info.t.b$臺北市)) +
  labs(y="估計數量",x= "年份",title = "臺北市")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.20<-ggplot(fig.info.t.a, aes(y=臺東縣, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[5,],width=.1, aes(ymin=臺東縣-fig.info.t.b$臺東縣[2], ymax=臺東縣+fig.info.t.b$臺東縣[2])) +
  geom_errorbar(data = fig.info.t.a[4,],width=.1, aes(ymin=臺東縣-臺東縣, ymax=臺東縣+fig.info.t.b$臺東縣[1])) +
  labs(y="估計數量",x= "年份",title = "臺東縣")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.21<-ggplot(fig.info.t.a, aes(y=臺南市, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[c(4,5),], width=.1, aes(ymin=臺南市-fig.info.t.b$臺南市, ymax=臺南市+fig.info.t.b$臺南市)) +
  labs(y="估計數量",x= "年份", title = "臺南市")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))

fig.22<-ggplot(fig.info.t.a, aes(y=澎湖縣, x=info)) + 
  #scale_y_continuous(breaks=seq(0,34000,2000))+
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(data = fig.info.t.a[5,],width=.1, aes(ymin=澎湖縣-fig.info.t.b$澎湖縣[2], ymax=澎湖縣+fig.info.t.b$澎湖縣[2])) +
  geom_errorbar(data = fig.info.t.a[4,],width=.1, aes(ymin=澎湖縣-澎湖縣, ymax=澎湖縣+fig.info.t.b$澎湖縣[1])) +
  labs(y="估計數量",x= "年份", title = "澎湖縣")+
  theme(legend.position='none',text=element_text(family="BiauKai", size=12))
#----------------------------------------
gridExtra::grid.arrange(fig.1, fig.2, fig.3, fig.4)
gridExtra::grid.arrange(fig.5, fig.6, fig.7, fig.8)
gridExtra::grid.arrange(fig.9, fig.10, fig.11, fig.12)
gridExtra::grid.arrange(fig.13, fig.14,fig.15, fig.16)
gridExtra::grid.arrange(fig.17, fig.18,fig.19, fig.20)
gridExtra::grid.arrange(fig.21, fig.22)
#----------------------------------------

fig.year<-ggplot(year.compare, aes(y=estimate, x=year)) + 
  geom_point(alpha=0.7, size=1) + 
  geom_errorbar(width=.1, aes(ymin=estimate-CI.length, ymax=estimate+CI.length)) +
  labs(y="估計數量",x= "年度", title = '歷年全國遊蕩犬數量估計')+
  theme(text=element_text(family="BiauKai", size=12))

fig.107.109<-ggplot(year.compare[c(4,5),], aes(y=estimate, x=year)) + 
  geom_errorbar(width=.1, aes(ymin=estimate-CI.length, ymax=estimate+CI.length), colour=c('red', 'navy')) +
  geom_point(alpha=0.7, size=1) +
  labs(y="估計數量",x= "年度", title = '全國遊蕩犬數量估計')+
  theme(text=element_text(family="BiauKai", size=12))

