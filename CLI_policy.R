library("dplyr")

#monthly average case data
origin <- read.csv("Case/origin.csv", header = TRUE)
monthlyCaseData <- origin

#read policy Data
policy_list <- list(public_transport <- read.csv("Policy/public-transport-covid.csv", header = TRUE),
                    school_closures <- read.csv("Policy/school-closures-covid.csv", header = TRUE),
                    stay_at_home <- read.csv("Policy/stay-at-home-covid.csv", header = TRUE),
                    visitors_transit <- read.csv("Policy/visitors-transit-covid.csv", header = TRUE),
                    workplace_closures <- read.csv("Policy/workplace-closures-covid.csv", header = TRUE),
                    workplace_visitors <- read.csv("Policy/workplace-visitors-covid.csv", header = TRUE))

#monthly average
for (i in 1:length(policy_list)){
  policy_list[[i]]$Day <- as.Date(policy_list[[i]]$Day)
  policy_list[[i]]$YearMonth <- as.integer(format(policy_list[[i]]$Day,format="%Y%m"))
  policy_list[[i]] <- 
    policy_list[[i]]%>%
    group_by(Code, YearMonth) %>%
    summarise(across( -c("Day", "Entity") , mean))
}

#join
for (i in policy_list)
  monthlyCaseData <- inner_join(monthlyCaseData, i, by=c("iso_code" = "Code", "YearMonth"))

######################

#read economy data and monthly average
CLI <- read.csv("Economy/CLI.csv",  header = TRUE)
CLI$TIME <- as.Date(CLI$TIME)
CLI$YearMonth <- as.integer(format(CLI$TIME,format="%Y%m"))
CLI <-
  CLI %>%
  group_by(LOCATION, YearMonth) %>%
  summarise(across( c("Value") , mean))
names(CLI)[3] <- "CLI"

#join
monthlyCaseData <- inner_join(monthlyCaseData, CLI, by=c("iso_code" = "LOCATION", "YearMonth"))

######################

#write final data
write.csv(monthlyCaseData , "CLImonthlyData.csv", row.names = FALSE)