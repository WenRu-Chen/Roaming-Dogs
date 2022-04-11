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

# Function ####

# * Plot ####
pdf_plot <- function(x){
  g = ggplot()+
    geom_histogram(aes(x = x, y = ..density..), 
                   fill = '#557C55', alpha = 0.8)+
    geom_density(aes(x = x, y = ..density..), 
                 color = '#062C30', size = 1)+
    theme_bw()
  
  return(g)
}

# Load data ####
getwd()
path = 'Roaming-Dogs-Data\\'
Variable_KS_df <- read.csv(paste0(path, "@Test_KS\\Variable.csv" ), fileEncoding = 'utf-8')
Variable_KS_df["Clinic"][is.na(Variable_KS_df["Clinic"])] = 0
Variable_KS_shp<-shapefile(paste0(path, "@Test_KS\\Variable.shp" ),encoding = 'big5')
crs(Variable_KS_shp) <- CRS('+init=EPSG:3826') 

# *轉數值 ####
for(i in c(col_X, col_y)){
  
  Variable_KS_shp@data[i] = sapply(Variable_KS_df[i], function(x) as.numeric(x))
}

# Select X y ####

col_X = 
  c( "Cluster", # 分群
     "Market","Hospital" ,  "Temple",  "Ele" ,"Junior" ,"Senior", "Train.stat",  "Clinic", # 公共建設
     "high_rat",   "mid_rat","low_rat", "M_F_RAT" , "P_DEN", "YOUN_DEP","OLD_DEP","AGING_IDX", # 人口統計(教育程度、人口密度...)
     "Income_mea", "Income_med" ,"Income_Q1",  "Income_Q3",  "Income_sta") # 村里收入
col_y = c('Nt')

# *轉換成密度 (間/平方公里)

for(i in c('Market','Hospital',  "Temple",  "Ele" ,"Junior" ,"Senior", "Train.stat",  "Clinic")){
  
  i_new = paste0(i, "_den")
  print(i_new)
  
  Variable_KS_shp@data[i_new] = Variable_KS_shp@data[i]/Variable_KS_shp@data$Area_sqkm
  Variable_KS_df[i_new] = Variable_KS_df[i]/Variable_KS_df$Area_sqkm
}

# *收入range ####
Variable_KS_shp@data["Income_Q3-Q1"] = Variable_KS_shp@data["Income_Q3"]-Variable_KS_shp@data["Income_Q1"]
Variable_KS_df["Income_Q3-Q1"] = Variable_KS_df["Income_Q3"]-Variable_KS_df["Income_Q1"]

# *new X ####
col_X = 
  c( "Cluster", # 分群
     "Market_den","Hospital_den" ,  "Temple_den",  "Ele_den" ,"Junior_den" ,"Senior_den", "Train.stat_den",  "Clinic_den", # 公共建設
     "high_rat",   "mid_rat","low_rat", "M_F_RAT" , "P_DEN", "YOUN_DEP","OLD_DEP","AGING_IDX", # 人口統計(教育程度、人口密度...)
     "Income_mea","Income_sta")

# cor ####

corr = cor(Variable_KS_df[c(col_y,col_X)])
col_income = c( "Income_mea","Income_med" ,"Income_Q3-Q1",  "Income_sta", "Income_CV")
corr_income = cor(Variable_KS_df[c(col_y,col_income)])
idx = abs(corr[,'Nt'])>.15
col_X_02 = names(corr[idx,'Nt']) %>% tail(-1)


# GWPCA 處理共縣性問題 ####

DM<-gw.dist(dp.locat=data.matrix(((Variable_KS_df[c('X', "Y")]))))


bw.gwpca.basic <-  # 25
  bw.gwpca(Variable_KS_shp, vars = col_X_02, k =3, robust = FALSE, adaptive = TRUE, dMat = DM)

gwpca.basic <- gwpca(Variable_KS_shp,
                     vars = col_X_02, bw = bw.gwpca.basic, k = 3, robust = FALSE, adaptive = TRUE)


# bw.gwpca.robust <-
# bw.gwpca(Variable_KS_shp, vars = col_X_02, k = 3, robust = TRUE, adaptive = TRUE, dMat = DM )

