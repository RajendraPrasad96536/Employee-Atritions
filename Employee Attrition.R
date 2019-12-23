######################  Data Reading ##############################################
out_time = read.csv("C:/Users/rajendra/Desktop/Data Mining Assigment III/Employee Records/out_time.csv",sep = ",")
in_time = read.csv("C:/Users/rajendra/Desktop/Data Mining Assigment III/Employee Records/in_time.csv",sep = ",")
manager_survey_data = read.csv("C:/Users/rajendra/Desktop/Data Mining Assigment III/Employee Records/manager_survey_data.csv",sep = ",")
general_data = read.csv("C:/Users/rajendra/Desktop/Data Mining Assigment III/Employee Records/general_data.csv",sep = ",")
employee_survey_data = read.csv("C:/Users/rajendra/Desktop/Data Mining Assigment III/Employee Records/employee_survey_data.csv",sep = ",")

#############################################################################################3333

missing  = data.frame(sapply(in_time, function(y) sum(length(which(is.na(y))))))
missing1  = data.frame(sapply(out_time, function(y) sum(length(which(is.na(y))))))
date = colnames(in_time)
working_time = format(as.POSIXct(strptime(in_time$X02.01.2015,"%d/%m/%Y %H:%M",tz="")), format = "%H:%M") 

#####################################################################################################

colnames(employee_survey_data)
colnames(general_data)#9, 16 unique value, 12 can not be encoded because it has more than 600 different values
colnames(manager_survey_data)#1
Data = cbind(employee_survey_data,general_data[,-c(9,16,12, 8, 18)],manager_survey_data[,-1])
dim(Data)
summary(Data)



########################  Removing NA's Values #####################################################

Data = na.omit(Data)
dim(Data)
summary(Data)
######################## Applying Random Forest ####################################################

library(randomForest)
set.seed(15)
model = randomForest(Attrition~.,data = Data[,-1], importance = TRUE)
model
importance(model)
model$importanceSD

# Performance Rating, Bussiness Travel, department and Gender are not usefull for deciding attrition of employee 

varImpPlot(model)

older = subset(Data, (Data$Age>37 & Data$Attrition == 'Yes'))
Younger = subset(Data, (Data$Age<37 & Data$Attrition == 'Yes'))

barplot(c(older=nrow(older), Younger=nrow(Younger)),main = "Attrition Yes", ylab = "Number of Employee", col = "green")

########################################################################################################

income_Attrition = subset(Data, ( Data$Attrition == 'Yes'))
summary(income_Attrition$MonthlyIncome)
Income_NoAttrition = subset(Data, ( Data$Attrition == 'No'))
summary(Income_NoAttrition$MonthlyIncome)

########################################################################################################

Hike = subset(Data, ( Data$Attrition == 'Yes'))
summary(Hike$PercentSalaryHike)
Hike_NoAttrion = subset(Data, (Data$Attrition == 'No'))
summary(Hike_NoAttrion$PercentSalaryHike)

#######################################################################################################

Distance = subset(Data, ( Data$Attrition == 'Yes'))
table(Distance$DistanceFromHome)
Distance_NoAttrition = subset(Data, (Data$Attrition == 'No'))
summary(Distance_NoAttrition$DistanceFromHome)

#######################################################################################################

