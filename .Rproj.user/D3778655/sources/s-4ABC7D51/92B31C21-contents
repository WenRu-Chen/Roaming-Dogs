# library the packages #####
library(GWmodel)      ### GW models
library(dplyr)
library(sp)           ## Data management
library(car)          ## vif
library(spdep)        ## Spatial autocorrelation
library(RColorBrewer) ## Visualization
library(classInt)     ## Class intervals
library(raster)       ## spatial data
library(grid)         # plot
library(gridExtra)    # Multiple plot
library(ggplot2)      # Multiple plot
library(gtable)
library(GGally)       # 相關係數圖矩陣（scatter plot matrix）
library(maptools)
library(MASS)
library(tmap)
getwd()

# Load data ####
path = 'Roaming-Dogs-Data\\'
df <- read.csv("Roaming-Dogs-Data/@Test_KS/Variable.csv", fileEncoding = 'utf-8')
Variable_KS<-shapefile("Roaming-Dogs-Data/@Test_KS/Variable.shp",warnPRJ = F)
area(Variable_KS)
# Xy ####
col_X = c("Market","Cluster",   
           "high_rat",   "mid_rat",    "low_rat",   
          "Hospital" ,  "Clinic", "M_F_RAT" ,    
           "P_DEN",      "DEPENDENCY", "YOUN_DEP",   "OLD_DEP",    "AGING_IDX",     
           "Ele" ,       "Ele_stu",    "Junior" ,   
           "Junior_stu", "Senior",     "Senior_stu", "TaxPayer" ,  "Income_tot", "Income_mea",
           "Income_med" ,"Income_Q1",  "Income_Q3",  "Income_sta", "Income_CV",  "Temple",    
           "Train.stat", "Train.crow" )
col_y = c('Nt')

col_X_count =c("Market","Hospital" ,  "Clinic",    
              "Ele" ,"Ele_stu","Junior" ,"Junior_stu", "Senior","Senior_stu", 
              "TaxPayer" ,  "Income_tot",  "Temple","Train.stat", "Train.crow" )

for (i in c(col_y,col_X_count)) {
  df[i] = df[i]/(df['Area']*10000)
  
}

# cor ####

corr = cor(df[c(col_y,col_X)])
col_income = c( "Income_mea","Income_med" ,"Income_Q1", 
                "Income_Q3",  "Income_sta", "Income_CV")
corr_income = sort(corr[col_income,'Nt'])
idx = abs(corr[,'Nt'])>.15
col_X_02 = names(corr[idx,'Nt']) %>% tail(-1)


# pdf plot ####


pdf_plot <- function(x){
  g = ggplot()+
    geom_histogram(aes(x = x, y = ..density..), 
                   fill = '#557C55', alpha = 0.8)+
    geom_density(aes(x = x, y = ..density..), 
                 color = '#062C30', size = 1)+
    theme_bw()
  
  return(g)
}

Nt_pdf = pdf_plot(df$Nt)+xlab('Nt')

# Poisson regression ####

Fit_Po <-glm(Nt~.,data=df[c(col_y,col_X_02)],family=poisson()) #建poisson 
summary(Fit_Po) #查看回归模型参数
pdf_plot(Fit_Po$residuals)+xlab('residuals')

# VIF
# https://www.statology.org/variance-inflation-factor-r/

idx = (vif(Fit_Po)<10); col_X_03 = names(vif(Fit_Po)[idx])

Fit_Po_02 <-glm(Nt~.,data=df[c(col_y,col_X_03)],family=poisson()) #建poisson

summary(Fit_Po_02) #查看回归模型参数
pdf_plot(Fit_Po_02$residuals)+xlab('residuals')


# NB ####
# https://blog.csdn.net/weixin_54000907/article/details/117915956
# 定義 theta 


Fit_NB = glm.nb(Nt~.,data=df[c(col_y,col_X_02)], link = log)

summary(Fit_NB)
pdf_plot(Fit_NB$residuals)+xlab('residuals')

# VIF
# https://www.statology.org/variance-inflation-factor-r/

idx = (vif(Fit_NB)<10); col_X_03 = names(vif(Fit_NB)[idx])

Fit_NB_02 = glm.nb(Nt~.,data=df[c(col_y,col_X_03)], link = log)
summary(Fit_NB_02) #查看回归模型参数
pdf_plot(Fit_NB_02$residuals)+xlab('residuals')



#---

theta = logtrans(Nt~.,data=df[c(col_y,col_X_02)])
theta = theta$x[theta$y == max(theta$y)]

Fit_NB_2 = glm(Nt~.,data=df[c(col_y,col_X_03)], family = negative.binomial(theta))
idx = (vif(Fit_NB_2)<10); col_X_03 = names(vif(Fit_NB_2)[idx])
summary(Fit_NB_2)
pdf_plot(Fit_NB_2$residuals)+xlab('residuals')

# GRPW ####
# https://zia207.github.io/geospatial-r-github.io/geographically-weighted-poisson-regression.html

Variable_KS<-shapefile("Roaming-Dogs-Data/@Test_KS/Variable.shp",warnPRJ = F)
class(Variable_KS)

formula.GWPR = Nt~.
DM<-gw.dist(dp.locat=coordinates(Variable_KS))

bw <- bw.ggwr(formula.GWPR,  
              data = Variable_KS[,c(col_X_03, col_y)],
              family = "poisson",
              approach = "AICc",
              kernel = "gaussian", 
              adaptive = TRUE,
              dMat = DM )

GWPR_model <- ggwr.basic(formula.GWPR, 
                         data =Variable_KS[,c(col_X_03, col_y)],
                         family = "poisson",
                         bw = bw, 
                         kernel = "gaussian", 
                         adaptive = TRUE,
                         dMat = DM)
summary(GWPR_model)
pdf_plot(GWPR_model$glms$residuals)+xlab('residuals')

## 探索性因子分析 ####
# https://zhuanlan.zhihu.com/p/37440281
library(psych)
corr = cor(df[c(col_y,col_X)])
fa.parallel(corr, n.obs = 58, fa = "both", n.iter = 100, main = "平行分析碎石图")
fa <- fa(corr, nfactors = 5, rotate = "none", fm = "pa")
