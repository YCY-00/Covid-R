library("dplyr")

#read Data
Data <-read.csv("CCIMonthlyData.csv", header = TRUE) 

#add CCI change and change month data usable
Data <- Data %>% 
  mutate(CCIChange = ifelse(iso_code == lag(iso_code), CCI - lag(CCI), 0)) %>%
  mutate(YearMonth = YearMonth%/%100 * 12 + YearMonth %% 100 - 1)

Data[is.na(Data)] <- 0

#remove some useless columns
Data <- Data[,-c(5,8,11,14,26,30)] #we're using average. so delete smoothed data
Data <- Data[,-c(11:18)] #only few countries have icu/hosp data
Data <- Data[,-c(17,20)] #remove composite policies (stringency/containment)
names(Data)

#grouping
prevent <- Data[, c(17, 18, 19, 30, 34)] #prevent spread of covid
connect <- Data[, c(22, 25, 23, 27, 31, 32, 21, 29, 34)] #reduce connect
income <- Data[, c(20, 28, 34)] #financial support

#linear regression: prevent
a=prevent
selectNames <- names(a)[c(1:length(a)-1)] 
formula = as.formula(paste('CCIChange~',paste(selectNames,collapse = '+')))
full.model <- lm(formula, data = a)
step.model <- step(direction = "backward", object = full.model)
summary(step.model)
par(mfrow=c(2,2))
plot(step.model)
par(mfrow=c(1,1))
plot(prevent$public_information_campaigns, prevent$CCIChange)

#linear regression: connect
a=connect
selectNames <- names(a)[c(1:length(a)-1)] 
formula = as.formula(paste('CCIChange~',paste(selectNames,collapse = '+')))
full.model <- lm(formula, data = a)
step.model <- step(direction = "backward", object = full.model)
summary(step.model)
par(mfrow=c(2,2))
#plot(step.model)
plot(connect$cancel_public_events, connect$CCIChange)
plot(connect$restriction_gatherings, connect$CCIChange)
plot(connect$workplace_closures, connect$CCIChange)
plot(connect$stay_home_requirements, connect$CCIChange)
par(mfrow=c(1,1))
plot(connect$school_closures, connect$CCIChange)

#cancel_public_events ¹¶Ä£ Á¤µµ
count_1=0
count_2=0
count_3=0
count_4=0
i=1
while(i<=length(connect$cancel_public_events)){
  if(connect$cancel_public_events[i]<=0.5){
    count_1=count_1+1
    i=i+1
  }
  else if(connect$cancel_public_events[i]<=1.0){
    count_2=count_2+1
    i=i+1
  }
  else if(connect$cancel_public_events[i]<=1.5){
    count_3=count_3+1
    i=i+1
  }
  else{
    count_4=count_4+1
    i=i+1
  }
}
max(count_1, count_2, count_3, count_4)/(count_1+count_2+count_3+count_4)

#restriction_gatherings ¹¶Ä£ Á¤µµ
count_1=0
count_2=0
count_3=0
count_4=0
i=1
while(i<=length(connect$restriction_gatherings)){
  if(connect$restriction_gatherings[i]<=1){
    count_1=count_1+1
    i=i+1
  }
  else if(connect$restriction_gatherings[i]<=2){
    count_2=count_2+1
    i=i+1
  }
  else if(connect$restriction_gatherings[i]<=3){
    count_3=count_3+1
    i=i+1
  }
  else{
    count_4=count_4+1
    i=i+1
  }
}
max(count_1, count_2, count_3, count_4)/(count_1+count_2+count_3+count_4)

#workplace_closures ¹¶Ä£ Á¤µµ
count_1=0
count_2=0
count_3=0
i=1
while(i<=length(connect$workplace_closures)){
  if(connect$workplace_closures[i]<=1){
    count_1=count_1+1
    i=i+1
  }
  else if(connect$workplace_closures[i]<=2){
    count_2=count_2+1
    i=i+1
  }
  else{
    count_3=count_3+1
    i=i+1
  }
}
max(count_1, count_2, count_3)/(count_1+count_2+count_3)

#stay_home_requirements ¹¶Ä£ Á¤µµ
count_1=0
count_2=0
count_3=0
i=1
while(i<=length(connect$stay_home_requirements)){
  if(connect$stay_home_requirements[i]<=1){
    count_1=count_1+1
    i=i+1
  }
  else if(connect$stay_home_requirements[i]<=2){
    count_2=count_2+1
    i=i+1
  }
  else{
    count_3=count_3+1
    i=i+1
  }
}
max(count_1, count_2, count_3)/(count_1+count_2+count_3)

#school_closures ¹¶Ä£ Á¤µµ
count_1=0
count_2=0
count_3=0
i=1
while(i<=length(connect$school_closures)){
  if(connect$school_closures[i]<=1){
    count_1=count_1+1
    i=i+1
  }
  else if(connect$school_closures[i]<=2){
    count_2=count_2+1
    i=i+1
  }
  else{
    count_3=count_3+1
    i=i+1
  }
}
max(count_1, count_2, count_3)/(count_1+count_2+count_3)

#linear regression: income
a=income
selectNames <- names(a)[c(1:length(a)-1)] 
formula = as.formula(paste('CCIChange~',paste(selectNames,collapse = '+')))
full.model <- lm(formula, data = a)
step.model <- step(direction = "backward", object = full.model)
summary(step.model)
par(mfrow=c(2,2))
plot(step.model)
par(mfrow=c(1,1))
