library(dplyr)
library(MASS)
library(ggplot2)
library(zoo)
library(tidyverse)
library(magrittr)
library(Metrics)
library(plotly)
library(zeallot)
library(lubridate)
library(cowplot)
library(forecast)
library(ggpubr)
library(gridExtra)
library(lmerTest)
library(nlme)
library(tidyverse)
library(tsibble)
library(fable)

# global options
options(warn=-1)
options(repr.plot.width = 15, repr.plot.height = 8)
options(jupyter.plot_scale=1) 

theme_set(theme_gray(base_size=15))
#the title will go to the center
#hjust=1: right; 0.5: center; 0: left
theme_update(plot.title=element_text(color='#555555', size=18, face='bold', hjust=0.5),
             plot.subtitle=element_text(color='#999999', size=12, face='italic', hjust=0.5))
theme_update(plot.caption=element_text(color='#999999', size=10, face='italic',hjust=0))

train = read_csv("C:/Users/dell/Desktop/train.csv")
transactions = read_csv("C:/Users/dell/Desktop/transactions.csv")
stores = read_csv("C:/Users/dell/Desktop/stores.csv")
oil = read_csv("C:/Users/dell/Desktop/oil.csv")
submission = read_csv("C:/Users/dell/Desktop/sample_submission.csv")


train['date']<-as.Date(train$date, format='%Y-%m-%d')
oil['date']<-as.Date(oil$date, format='%Y-%m-%d')
transactions['date']<-as.Date(transactions$date, format='%Y-%m-%d')

# calculate the median oil price of each month
oil$year=year(oil$date)
oil$month=month(oil$date)

med_oil<-oil %>% group_by(year,month) %>%
  summarise(median_oil_yr_mt=median(dcoilwtico,na.rm=TRUE))

oil <- oil %>% left_join(med_oil, by=c('year','month')) %>% subset(select=-c(year,month))

# calculate the median transaction of each month for each store 
transactions$year=year(transactions$date)
transactions$month=month(transactions$date)

med_transactions<-transactions %>% group_by(store_nbr, year,month) %>%
  summarise(median_transaction_yr_mt=median(transactions,na.rm=TRUE))

transactions <- transactions %>% 
  left_join(med_transactions, by=c('store_nbr','year','month')) %>% 
  subset(select=-c(year,month))

raw<-train %>% 
  left_join(stores, by='store_nbr') %>%
  left_join(oil,by='date') %>%
  left_join(transactions,by=c('date','store_nbr')) 

# set factor variables to chacter for table merging
raw$state=as.character(raw$state)
raw$city=as.character(raw$city)
raw$family=as.character(raw$family)

#set  variable
df=raw

# set back to factors
df$state=as.factor(df$state)
df$city=as.factor(df$city)
df$family=as.factor(df$family)
df$store_nbr=as.factor(df$store_nbr)
df$cluster=as.factor(df$cluster)

#set date related variables
df['date']<-as.Date(df$date, format='%Y-%m-%d')

df$year=year(df$date)
df$month=month(df$date)
df$mday=mday(df$date)
df$wday=wday(df$date)
df$myear <- format(as.Date(df$date), "%Y-%m")
df$wyear=strftime(as.character(df$date), format = "%V")

# cleanse outliers. set (+/-)1.5IQR boundary for outliers. 
df_clean <- df %>% arrange("store_nbr", "family", "date")
a=df_clean %>% group_by(store_nbr, family) %>%
  summarise(min=min(sales, na.rm = TRUE), 
            Q1=quantile(sales, probs = 0.25, na.rm = TRUE),
            median=median(sales, na.rm = TRUE), 
            Q3=quantile(sales, probs = 0.75, na.rm = TRUE),
            max=max(sales, na.rm = TRUE),
            iqr_h=Q3+1.5*IQR(sales, na.rm = TRUE),
            iqr_l=ifelse(Q1-1.5*IQR(sales, na.rm = TRUE)<=0,0,Q1-1.5*IQR(sales, na.rm = TRUE))
  ) %>%
  ungroup()
df_clean <- df_clean %>% left_join(a, by=c('store_nbr'='store_nbr', 
                                           'family'='family'))

df_clean <- df_clean %>% 
  mutate(sales = ifelse(sales>iqr_h,NaN,sales))

# special take_care of O's , implanted mean of weekly sales
t<-df_clean %>% group_by(store_nbr,family,year,wyear) %>%
  summarise(mean7=mean(sales, na.rm = TRUE))

df_clean <- df_clean %>% left_join(t, by=c('store_nbr','family','year','wyear'))

df_clean <- df_clean %>% 
  mutate(sales = ifelse(mean7<0.0001 | is.na(mean7),iqr_l,sales))
mean_transactions=mean(df_clean$transactions, na.rm = TRUE)
mean_dcoilwtico=mean(df_clean$dcoilwtico, na.rm = TRUE)

t<-df_clean %>% group_by(store_nbr,family,year,month) %>%
  summarise(median_yr_mt=median(sales, na.rm=TRUE))

df_clean <- df_clean %>% left_join(t, by=c('store_nbr','family','year','month'))

# data cleansing on oil price(dcoilwtico) and transactions
df_clean <- df_clean %>% 
  mutate(sales = ifelse(is.na(sales)==TRUE,median_yr_mt,sales),         
         transactions = ifelse(is.na(transactions)==TRUE | transactions==0,
                               ifelse(is.na(median_transaction_yr_mt)==T | median_transaction_yr_mt==0, 
                                      mean_transactions,
                                      median_transaction_yr_mt) , transactions),
         dcoilwtico = ifelse(is.na(dcoilwtico)==TRUE | dcoilwtico==0,
                             ifelse(is.na(median_oil_yr_mt)==T | median_oil_yr_mt==0, 
                                    mean_dcoilwtico,
                                    median_oil_yr_mt) , dcoilwtico))
df1 <- df_clean %>% subset(select=c(id, date, store_nbr,family,sales,onpromotion,
                                    city,state,type,cluster,dcoilwtico,transactions,
                                    year,month,mday,wday,myear,wyear))
# add log scale 
df1 <- df1 %>% mutate(sales=ifelse(sales==0, 0.01, sales),
                      dcoilwtico=ifelse(dcoilwtico==0, 0.01, dcoilwtico),
                      onpromotion=ifelse(onpromotion==0, 0.01, onpromotion),
                      transactions=ifelse(transactions==0, 0.01, transactions))

df1 <- df1 %>% mutate(log.sales=log(sales), 
                      log.onpromotion=log(onpromotion), 
                      log.dcoilwtico=log(dcoilwtico), 
                      log.transactions=log(transactions))


smp_size <- floor(0.70 * nrow(df1))

# set the seed to make partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df1)), size = smp_size)

df2 = df1 %>% subset(select=c(id, type, cluster, family, store_nbr, city, state,
                              wday, year, log.transactions, log.onpromotion, log.sales, transactions, onpromotion, sales, date))

train <- df2[train_ind, ]
test <- df2[-train_ind, ]

# read in test dataset
df_test = read_csv("C:/Users/dell/Desktop/test.csv")
df_test['date']<-as.Date(df_test$date, format='%Y-%m-%d')

df_test$family=as.factor(df_test$family)
df_test$store_nbr=as.factor(df_test$store_nbr)

#Model
factors=c('cluster','family','wday','log.onpromotion','year','type')
lst.formula=NULL
lst.paramn=NULL
for (i in 1:length(factors)){
  a=combn(factors,i)   
  if (i==length(factors))
    lst.formula=rbind(paste(a, collapse = '+'),lst.formula)
  else
    for (j in 1:ncol(a)){
      # check models with 3 or more factors
      if (i+j<3){
        next
      }
      else{
        b=paste(a[,j], collapse = '+')
        lst.formula=rbind(b,lst.formula)  
        lst.paramn=rbind(i,lst.paramn)
      }
    }    
}

df.aic <- data.frame(formula=character(),
                     aic=double(),
                     paramn=integer(),
                     stringsAsFactors=FALSE)

for (i in 1:length(lst.formula))
{  
  Form=as.formula(paste('log.sales ~' , lst.formula[i], collapse=''))
  possibleError <- tryCatch(
    {
      lm <- gls(Form, data = train,na.action=na.exclude) 
      df.aic[i,1]=lst.formula[i]
      df.aic[i,2]=AIC(lm)
      df.aic[i,3]=lst.paramn[i]
    },
    error=function(e) e
  )
  if(inherits(possibleError, "error")) {
    df.aic[i,1]=lst.formula[i]
    df.aic[i,2]=NaN
    df.aic[i,3]=lst.paramn[i]
    next
  }    
}

df.aic=df.aic %>% arrange(aic)
head(df.aic,10)

rmsle_lmm<-function(true, predict){
  predicted_value = abs(predict) 
  actual_value =abs(true)
  
  return(rmsle(actual_value,predicted_value ))
}

# Set up the Test Dataset for Forecasting
stores$state=as.factor(stores$state)
stores$city=as.factor(stores$city)
stores$store_nbr=as.factor(stores$store_nbr)
stores$cluster=as.factor(stores$cluster)
stores$type=as.factor(stores$type)


df_t<-df_test %>% 
  left_join(stores, by='store_nbr')


df_t$year=year(df_t$date)
df_t$month=month(df_t$date)
df_t$wday=wday(df_t$date)

# add log scale 
df_t <- df_t %>% mutate(onpromotion=ifelse(onpromotion==0, 0.01, onpromotion))
df_t <- df_t %>% mutate(log.onpromotion=log(onpromotion))

names(df_t)

final_predict<-function(df_fit,df_predict, features,random='(1|store_nbr:cluster)',
                        output='submission.csv',optimizer='bobyqa'){
  formu=paste('log.sales ~' , features, "+", random, collapse='')
  Form=as.formula(formu)
  
  lm_s <- lmer(Form, data = df_fit, na.action=na.exclude,
                control = lmerControl(optimizer = optimizer)) 
  lmm.predict_s=predict(object=lm_s,newdata=df_predict,type='response')
  
  df_sub <- df_predict %>% cbind(lmm.predict_s) %>% 
    mutate(sales=round(exp(lmm.predict_s),1)) %>% subset(select=c(id, sales))
  write.csv(df_sub, output, row.names=FALSE)
  
  return(df_sub)
}
s1="cluster+family+wday+log.onpromotion+year"
s2="cluster+family+log.onpromotion+year"
p1=final_predict(features=s1,df_fit=train, df_predict=df_t,output='submission.csv')
head(p1)
# get the score of 1.46365

#TIME SERIES PREDICTION
df_sales<-train %>%
  subset(select=c(date, store_nbr, family, onpromotion, log.onpromotion, log.sales, sales, wday)) %>%
  group_by(date) %>%
  arrange(date)

sales_tbl<-as_tsibble(df_sales,index=date,key=c(store_nbr,family))
head(sales_tbl)




# mean daily sales dataset used to check the order of arima
sales_mean <- sales_tbl %>%
  summarise(mean=mean(sales))
head(sales_mean,5)

# check distribution
sales_tbl %>% filter( store_nbr %in% 30:41) %>%
  ggplot() +
  geom_density(aes(x=sales, fill=store_nbr), alpha=0.5)+
  facet_wrap(~ store_nbr) +
  #     xlim(range(l$x)) +
  labs(title = "Daily Sales Density Plot for Store 30 to 41 ",
       x = "Daily Sales",
       y = 'Density',
       caption='Kaggle--Store Price')

# check time series
plot(sales_mean,ylab='sales',
     main='sales data')
acf(sales_mean,
    main='autocorrelation function of mean daily sales data', na.action = na.pass)
#pacf cuts off after p lags
acf(sales_mean,type='partial',
    main='partial autocorrelation function of daily sales data', na.action = na.pass)
