#check directory
getwd()
setwd("C:/Users/Lenovo/Desktop/Thinithi/Monash/Sem 1-2023/FIT5147VIZUALIZATION/Assignments/Australia_Tourism")

#import data
library("readxl")
data <- read_excel("australia_arrivals_by_country_of_residence.xlsx",sheet="Data1")
head(data)

#checking for data stucture
str(data)

#loop to restructure the data
countries<-list()
variable_type<-list()
seried_id<-list()
records<-list()
date<-list()

for (i in 2:70) {
  for (j in 3:386) {
    countries<-append(countries,names(data[0,i]))
    variable_type<-append(variable_type,data[[1,i]])
    seried_id<-append(seried_id,data[[2,i]])
    records<-append(records,data[[j,i]])
    date<-append(date,data[j,1])
  }
}

countries <- unlist(countries)
variable_type<-unlist(variable_type)
seried_id<-unlist(seried_id)
records<-unlist(records)

countries<-data.frame(countries)
variable_type<-data.frame(variable_type)
seried_id<-data.frame(seried_id)
records<-data.frame(records)
date<-data.frame(t(as.data.frame(date)))

data2<-cbind(countries,variable_type,seried_id,date,records)
colnames(data2)<-c("countries","variable_type","seried_id","date","records")

#checking for data stucture
str(data2)

#checking for missing values
colSums(is.na(data2))

#checking for inconsistency
check_countries<-unique(data2$countries)
check_countries<-as.data.frame(check_countries)
check_variable_type<-as.data.frame(unique(data2$variable_type))

#create a cleaned file
write.csv(data2,"austrlia_visitor_time.csv")
