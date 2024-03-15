library("dplyr")

#read Data
Data <-read.csv("CCIMonthlyData.csv", header = TRUE) 

#add CCI change and change month data usable
Data <- Data %>% 
  mutate(CCIChange = ifelse(iso_code == lag(iso_code), CCI - lag(CCI), 0)) %>%
  mutate(YearMonth = YearMonth%/%100 * 12 + YearMonth %% 100 - 1)

Data[is.na(Data)] <- 0

#add CCI norm
CCINorm <- vector("numeric", length(Data$CCI))
for(i in 1:(length(Data$CCI)-1)){
  if(Data$iso_code[i] == Data$iso_code[i+1]){
    CCINorm[i+1] <- Data$CCIChange[i+1] + CCINorm[i]
  }else{
    CCINorm[i+1] <- 0
  }
}
rm("i")
Data$CCINorm <- CCINorm
rm("CCINorm")
Data[is.na(Data)] <- 0

#remove some useless columns
names(Data)
Data <- Data[,-c(5,8,11,14,26,30)] #we're using average. so delete smoothed data
Data <- Data[,-c(11:18)] #only few countries have icu/hosp data
Data <- Data[,-c(17,20)] #remove composite policies (stringency/containment)

#CCI change : linear regression
selectNames <- names(Data)[c(2:32)] 
formula = as.formula(paste('CCIChange~',paste(selectNames,collapse = '+')))
full.model <- lm(formula, data = Data)
step.model <- step(direction = "backward", object = full.model)
summary(step.model)
par(mfrow=c(2,2))
plot(step.model)
par(mfrow=c(1,1))

#CCI norm : linear regression
selectNames <- names(Data)[c(2:32)] 
formula = as.formula(paste('CCINorm~',paste(selectNames,collapse = '+')))
full.model <- lm(formula, data = Data)
step.model <- step(direction = "backward", object = full.model)
summary(step.model)
par(mfrow=c(2,2))
plot(step.model)
par(mfrow=c(1,1))
8