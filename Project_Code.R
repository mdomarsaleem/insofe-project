rm(list=ls(all=TRUE))
setwd("~/INSOFE/Project")

library(mice)
library(rpart)
library(data.table)
library(caret)
library(rCharts)
library(plyr)

Year_wise <- function(data, variable=NULL){
  if(is.null(variable)){
    for(i in levels(as.factor(data$STAT_PROFILE_DATE_YEAR))){
      cat(i,table(is.na(agent[data$STAT_PROFILE_DATE_YEAR==i,])),'\n')
    }
  } else{
    for(i in levels(as.factor(data$STAT_PROFILE_DATE_YEAR))){
      cat(i,table(is.na(agent[data$STAT_PROFILE_DATE_YEAR==i,variable])),'\n')
    }
  }
}

column_wise <- function(data) apply(data,2,function(x) sum(is.na(x)))

agent <- read.csv('agency_final.csv',stringsAsFactors = F,header = T,na.strings = "99999")
agent <- agent[agent$STAT_PROFILE_DATE_YEAR!=2015,]

#Missing values wit respective to Growth_rate_3year
Year_wise(agent,'GROWTH_RATE_3YR')

#We have 2005,2006,2007 with all NA'sand If 2014 is not there 
agent <- agent[agent$STAT_PROFILE_DATE_YEAR>2007,]
# agent <- agent[!((is.na(agent$GROWTH_RATE_3YR))*(agent$STAT_PROFILE_DATE_YEAR ==2014)),]
# agent <- agent[!((is.na(agent$WRTN_PREM_AMT))*(agent$STAT_PROFILE_DATE_YEAR ==2014)),]
# agent <- agent[!((is.na(agent$POLY_INFORCE_QTY))*(agent$STAT_PROFILE_DATE_YEAR ==2014)),]

#Missing values wit respective to LOSS_RATIO
Year_wise(agent,'LOSS_RATIO')
table(agent$LOSS_RATIO==99998)   #Positive loss 
agent$LOSS_RATIO[agent$LOSS_RATIO==99998] <- 
  median(agent[agent$LOSS_RATIO>0 & agent$LOSS_RATIO<9000,'LOSS_RATIO'],na.rm=T)

table(agent$LOSS_RATIO==99997)  #Negative Loss
agent$LOSS_RATIO[agent$LOSS_RATIO==99997] <- median(agent[agent$LOSS_RATIO<0,'LOSS_RATIO'],na.rm=T)

#Imputing the 3 year growth and loss ratio growth AGENCY_APPOINTMENT_YEAR
new_agent <- which(agent$STAT_PROFILE_DATE_YEAR < agent$AGENCY_APPOINTMENT_YEAR+3)
agent[new_agent,'LOSS_RATIO_3YR'] <- 0
agent[new_agent,'GROWTH_RATE_3YR'] <- 0

#Missing values wit respective to LOSS_RATIO_3YR
Year_wise(agent,'LOSS_RATIO_3YR')

#Identify the variables with maximum NA's
app <- data.frame(column_wise(agent))

#Variables with least missing values
good_variables <- row.names.data.frame(app)[app<20000]
rm(app)

#good data set
good_data <- agent[,good_variables]

#Corelationplot
numeric <- colnames(good_data)[5:20]
correlation <- cor(na.omit(good_data[,numeric[2:9]]))
corrplot::corrplot(correlation)

#Feature Engineering
good_data$QTY_Growth <- (good_data$POLY_INFORCE_QTY- good_data$PREV_POLY_INFORCE_QTY)/abs(good_data$PREV_POLY_INFORCE_QTY+1)
good_data$AMOUNT_Growth <-  (good_data$WRTN_PREM_AMT- good_data$PREV_WRTN_PREM_AMT)/(abs(good_data$PREV_WRTN_PREM_AMT)+1)
good_data$Range <- good_data$MAX_AGE-good_data$MIN_AGE
good_data[is.na(good_data$Range),'Range'] <- 0

#Clearning the missing values 
good_data <-na.omit(good_data)

#Variable analysis for the growth rate
anova <- aov(GROWTH_RATE_3YR~.,data.frame(good_data))
summary(anova)

#Understading the analysis of Plicy Quantity variation for year
year <- melt(ddply(good_data,.(STAT_PROFILE_DATE_YEAR),summarize,
                   First_Quantile=quantile(POLY_INFORCE_QTY,probs = 0.25),
                   Mean=mean(POLY_INFORCE_QTY),
                   Median = median(POLY_INFORCE_QTY),
                   Third_Quantile=quantile(POLY_INFORCE_QTY,probs = 0.75)),id=1)

tfrPlot <- nPlot(
  value ~ STAT_PROFILE_DATE_YEAR,
  data = year,
  group = "variable",
  type = "lineChart")
tfrPlot$yAxis(axisLabel = "Policy Quantity", width = 62)
tfrPlot$xAxis(axisLabel = "Year")

tfrPlot
# 
# ggplot(year,aes(x=STAT_PROFILE_DATE_YEAR,y=value))+geom_line(aes(col=variable))+
#   ggtitle('Yearwise Insurances Active Quantity Statistics')+labs(x="Year",y="Insurances Active Quantity", color = "Statistics")+
#   theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0)) +
#   theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22)) 

#Understading the analysis of Plicy Quantity variation for year
year <- melt(ddply(good_data,.(STAT_PROFILE_DATE_YEAR),summarize,
                   First_Quantile=quantile(PRD_ERND_PREM_AMT,probs = 0.25),
                   Mean=mean(PRD_ERND_PREM_AMT),
                   Median = median(PRD_ERND_PREM_AMT),
                   Third_Quantile=quantile(PRD_ERND_PREM_AMT,probs = 0.75)),id=1)

tfrPlot <- nPlot(
  value ~ STAT_PROFILE_DATE_YEAR,
  data = year,
  group = "variable",
  type = "lineChart")
tfrPlot$yAxis(axisLabel = "Revenue amount", width = 62)
tfrPlot$xAxis(axisLabel = "Year")

tfrPlot

rm(year)
# 
# ggplot(year,aes(x=STAT_PROFILE_DATE_YEAR,y=value))+geom_line(aes(col=variable))+
#   ggtitle('Yearwise Premiun Amount Statistics')+labs(x="Year",y="Premium Amount", color = "Statistics")+
#   theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0)) +
#   theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22)) 

#Connverting the Vendor  Indicator to numeric
good_data$VENDOR_IND <- as.numeric(as.factor(good_data$VENDOR_IND))

#Converting the Long data frame and Wide data frame
d = good_data[,c('AGENCY_ID','PROD_ABBR','Range','PROD_LINE','STATE_ABBR','MONTHS','LOSS_RATIO_3YR','VENDOR','VENDOR_IND','MAX_AGE','MIN_AGE','GROWTH_RATE_3YR','STAT_PROFILE_DATE_YEAR','POLY_INFORCE_QTY','WRTN_PREM_AMT','QTY_Growth','AMOUNT_Growth')]
magent <- data.table::dcast(setDT(d), AGENCY_ID+PROD_LINE+VENDOR~STAT_PROFILE_DATE_YEAR,value.var = c('GROWTH_RATE_3YR','POLY_INFORCE_QTY','WRTN_PREM_AMT','QTY_Growth','AMOUNT_Growth','MONTHS','LOSS_RATIO_3YR','VENDOR_IND','Range'),fun=mean)
magent <- data.frame(magent)
rm(d)
column_wise(magent)

#2014 variables
new_values <- -1 * grep('2014',colnames(magent))
new_values<- new_values[-1]
  
# fit model
anova <- aov(GROWTH_RATE_3YR_mean_2014~.,magent[,new_values])
summary(anova)

#Imputation
total <- c(-1,-2,-3,new_values)
library(mice)
new <- complete(mice(magent[,total],m=5,maxit = 1))

#Create train dataframe for all data
train <- cbind(magent[,c(1,2,3)],new)
# rm(new)

#imputing all with 0 assuming that the NA's means that was 0
# train <- cbind(magent[,new_values])
train[is.na(train)] <- 0
# train$GROWTH_RATE_3YR_mean_2014 <- as.factor(ifelse(train$GROWTH_RATE_3YR_mean_2014>0.4,'top',ifelse(train$GROWTH_RATE_3YR_mean_2014< -0.5,'Low','No Change')))

#Linear Regression for growth_rate_3YR
linear <- lm(GROWTH_RATE_3YR_mean_2014~.+I(GROWTH_RATE_3YR_mean_2013**3) ,data=train)
summary(linear)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(linear, las = 1)
rm(opar)

#Getting collinearity
library(car)
vif(linear)
outlierTest(linear)
outliers = as.numeric(names(outlierTest(linear)[[1]]))

#interaction Between Quantity and the Amount
linear <- lm(GROWTH_RATE_3YR_mean_2014~. ,train[-outliers,])
summary(linear)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(linear, las = 1)

#Robust linear regreesoion
Robust <- rlm(GROWTH_RATE_3YR_mean_2014~. ,train[,])
summary(Robust)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(Robust, las = 1)

#Cross Validation
fitControl <- trainControl(method = "cv",
                           number = 9)

lm <- train(GROWTH_RATE_3YR_mean_2014~.
            ,data=train[,],
            method = 'lm',
            trControl = fitControl)
lm

rpart <- train(GROWTH_RATE_3YR_mean_2014~.
               ,data=train[,],
               method = 'rpart',
               tuneGrid = expand.grid(cp=c(0.0001,0.0002,0.0004,0.00001,0.00005)),
               trControl = fitControl)
rpart

RandomForest <- train(GROWTH_RATE_3YR_mean_2014~.
                      ,data=train,
                      method = 'rf',
                      tuneLength = 3,
                      ntree = 100,
                      do.trace = T,
                      trControl = fitControl)
RandomForest

pred_rf <- RandomForest$finalModel$predicted
pred_lm <- lm$finalModel$fitted.values
pred_rpart <- predict(rpart,train)

Fitness <- function(x){
  a = 0
  x1 = x[1]
  x2 = x[2]
  x3 = x[3]
  for(i in 1:5){
    index <- sample(1:length(pred_rf),1000)
    prediction = pred_lm[index]*x1 + pred_rpart[index]*x2 + pred_rf[index]*x3
    a <- a +Metrics::rmse(prediction,train[index,'GROWTH_RATE_3YR_mean_2014'])
  }
  return(abs(a/5))
}
  
optimisation <- stats::optim(c(0.2,0.3,0.5),Fitness, method = "Nelder-Mead")


Predictions <- optimisation$par[1]*pred_lm + optimisation$par[2]*pred_rpart +optimisation$par[3]*pred_rf

#Histogram Of the predictions
ggplot(data.frame(Predictions),aes(x=Predictions))+geom_histogram()
Top_Performers_index <- which(Predictions>0.6)
Under_Performers_index <- which(Predictions<= -0.75)

Top_Performers = magent[Top_Performers_index,c('AGENCY_ID','PROD_LINE')]
Under_Performers = magent[Under_Performers_index,c('AGENCY_ID','PROD_LINE')]

#Converting the Long data frame and Wide data frame
d = good_data[,c('AGENCY_ID','PROD_ABBR','PROD_LINE','STATE_ABBR','MONTHS','LOSS_RATIO_3YR','VENDOR','VENDOR_IND','MAX_AGE','MIN_AGE','GROWTH_RATE_3YR','STAT_PROFILE_DATE_YEAR','POLY_INFORCE_QTY','WRTN_PREM_AMT','QTY_Growth','AMOUNT_Growth')]
Product <- data.table::dcast(setDT(d), AGENCY_ID+PROD_ABBR+PROD_LINE+VENDOR~STAT_PROFILE_DATE_YEAR,value.var = c('POLY_INFORCE_QTY','WRTN_PREM_AMT','QTY_Growth','AMOUNT_Growth'),fun=mean)
Product <- data.frame(Product)
rm(d)

Top_agents <- Product[Product$AGENCY_ID %in% unique(Top_Performers$AGENCY_ID) & Product$PROD_LINE %in% unique(Top_Performers$PROD_LINE), ]
Under_Agents <- Product[Product$AGENCY_ID %in% unique(Under_Performers$AGENCY_ID) & Product$PROD_LINE %in% unique(Under_Performers$PROD_LINE), ]

Top_agents$Negatives <-apply(Top_agents[,c(-1,-2,-3,-4)],1,function(x) sum(as.numeric(x<0),na.rm=T))
Top_agents$Recent_Negatives <- apply(Top_agents[,grep('2013|2012|2011',colnames(Top_agents))],1,function(x) sum(as.numeric(x<0),na.rm=T))

Under_Agents$Negatives <- apply(Under_Agents[,c(-1,-2,-3,-4)],1,function(x) sum(as.numeric(x<0),na.rm=T))
Under_Agents$Recent_Negatives <- apply(Under_Agents[,grep('2013|2012|2011',colnames(Under_Agents))],1,function(x) sum(as.numeric(x<0),na.rm=T))

Agents_UnderPerform_by_Others <- table(Top_agents[which(Top_agents$Negatives>5),'AGENCY_ID'])
Agents_UnderPerform_recent_Others <-table(Top_agents[which(Top_agents$Recent_Negatives>3),'AGENCY_ID'])

Product_Underperformed_by_TopAgg <- table(Under_Agents[which(Under_Agents$Negatives>5),'PROD_ABBR'])
Product_Underperformed_recent_TopAgg <- table(Under_Agents[which(Under_Agents$Recent_Negatives>3),'PROD_ABBR'])


Metrics <- confusionMatrix(ifelse(train$GROWTH_RATE_3YR_mean_2014>0.6,'Top',ifelse(train$GROWTH_RATE_3YR_mean_2014< -0.5,'Low','NC')),ifelse(Predictions>0.6,'Top',ifelse(Predictions< -0.5,'Low','NC')))
