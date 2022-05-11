library(ggplot2) 
library(GGally)
library(ggcorrplot) 
library(zoo)
library(dplyr)
library(stringr)
library(skimr)
library(openxlsx)
library(forecast)    
library(ggfortify)  
library(lubridate)
library(xts)

df = read.csv('C:\\Users\\berke\\Downloads\\IE360files\\IE360_Spring22_HW2_data.csv')

head(df)

col_names = c('Quarter','UGS','RNUV','NLPG','PU','PG','NUGV','NDGV','GNPA','GNPC','GNP')
colnames(df) = col_names

df$Quarter = as.character(df$Quarter)
df$Quarter = str_replace_all(df$Quarter,'_',' ')
df$Quarter = as.Date(as.yearqtr(df$Quarter))

head(df)

df_tr = df[is.na(df$UGS) == FALSE,]
df_te = df[is.na(df$UGS) == TRUE,]

ggplot(df_tr,aes(x=Quarter,y=UGS)) + geom_line(color = 'black') + 
    geom_smooth(color='blue',linetype='solid',size=1,fill=NA) +
    labs(title = 'Quarterly Gasoline Sales(UGS) from 2000 to 2007', x = 'Quarters', y = 'Gasoline Sales (in 1000m^3)') +
    scale_x_date(date_breaks = '3 month', date_labels = '%Y %b', date_minor_breaks = '3 month' ) +
    theme(axis.text.x=element_text(angle=90, hjust=1.4, vjust = 1.4))

dfts <- ts(df_tr$UGS, freq=4, start=c(2000,1))
dfts_dec_additive<-decompose(dfts, type="additive")
plot(dfts_dec_additive)

acf(df_tr$UGS,lag.max=20)

df_tr$quarter = rep(1:4,7)
df_tr$trend = seq(1,28,1)
df_tr
df_te$quarter = rep(1:4)
df_te$trend = seq(29,32,1)
df_te
df_tr$quarter = as.factor(df_tr$quarter)
df_tr$trend = as.integer(df_tr$trend)
df_te$quarter = as.factor(df_te$quarter)
df_te$trend = as.integer(df_te$trend)

Y_t_1 = c()
for( i in 2:nrow(df_tr)){
    Y_t_1[i] = df_tr$UGS[i-1]
}
Y_t_4 = c()
for( i in 5:nrow(df_tr)){
    Y_t_4[i] = df_tr$UGS[i-4]
}

df_tr = cbind(df_tr,Y_t_1,Y_t_4)
df_tr
df_te

cor_df <- df_tr[!is.na(df_tr$UGS & df_tr$Y_t_1 & df_tr$Y_t_4),]
corr = cor(cor_df[,unlist(lapply(cor_df, is.numeric))])
ggcorrplot(corr,
           hc.order = TRUE,
           type='lower',
           lab=TRUE)

pairs(df_tr)

fit = lm(UGS ~.-Quarter, data = df_tr)
summary(fit)
checkresiduals(fit)

fit2 = lm(UGS ~.-Quarter-Y_t_4, data = df_tr)
summary(fit2)
checkresiduals(fit2)

fit3 = lm(UGS ~.-Quarter-Y_t_4-GNPA, data = df_tr)
summary(fit3)
checkresiduals(fit3)

fit4 = lm(UGS ~.-Quarter-Y_t_4-GNPA-PU, data = df_tr)
summary(fit4)
checkresiduals(fit4)

fit5 = lm(UGS ~.-Quarter-Y_t_4-GNPA-PU-trend, data = df_tr)
summary(fit5)
checkresiduals(fit5)

fit6 = lm(UGS ~.-Quarter-Y_t_4-GNPA-PU-trend-GNP, data = df_tr)
summary(fit6)
checkresiduals(fit6)

best_model = lm(UGS ~PU+NUGV+NDGV+quarter+Y_t_1+trend, data = df_tr)
summary(best_model)
checkresiduals(best_model)
plot(best_model)

df_te$Y_t_1[1] = NA
df_te$Y_t_1[1][1] = df_tr$UGS[28]
df_te


predictions = c()
for(i in 1:nrow(df_te)){
     predictions[i] = predict(best_model,newdata=df_te[i,])
    if(i+1 < 5){
     df_te$Y_t_1[i+1] = predictions[i]
        }
}

for(i in 1:length(predictions))
    {
    print(paste("Prediction for 2007 Q",i," : ", predictions[i]))
}
