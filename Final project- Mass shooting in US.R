library(data.table) #Creating data frames
library(tidyverse) #To reshape data
library(readr) # To read csv files
library(tidyr) #To reshape data
Dateformat = cols(Date = col_date(format = "%m/%d/%Y"))

MassShooting <- read_csv("Mass Shootings Dataset Ver 5.csv"
                         ,col_types=Dateformat)

Shooting <- data.table(MassShooting)

dim(Shooting)
str(Shooting)
head(Shooting)


## Pre-Processing Data
#  Preprocessing Date

#library(lubridate) #For dates
Shooting[,Month<-as.factor(month(Date))]
Shooting[,Day<-as.factor(day(Date))]
Shooting[,Year<-as.factor(year(Date))]

#Preprocessing Gender

library(plyr)
Shooting$Gender[is.na(Shooting$Gender)]<-"Unknown"
qual_label <- c("F" ="Female","M"= "Male", "M/F"="Male/Female")
Shooting$Gender<-as.factor(revalue(Shooting$Gender, qual_label, warn_missing = FALSE))
table(Shooting$Gender)

#Preprocessing Cause

Shooting[is.na(Cause)|Cause=="unknown",Cause:="Unknown"]
Shooting[Cause=='domestic dispute'|Cause=='domestic disputer', Cause:='domestic dispute']
Shooting[Cause=="anger"|Cause=="frustration",Cause:="Anger and Fustration"]
Shooting[Cause=="failing exams" | Cause=="suspension", Cause:="Failing exams and suspension"]
table(Shooting$Cause)


#Preprocessing Race

Shooting[Race=="unclear",Race:="Unknown"]
Shooting[is.na(Race),Race:="Other"]
Shooting[ Race=="black" | Race=="Black American or African American" | Race=="Black American or African American/Unknown",Race:="Black"]
Shooting[Race=="white" | Race=="White American or European American" | Race=="White American or European American/Some other Race" ,Race:="White"]
Shooting[Race=="Asian American"| Race=="Asian American/Some other race" ,Race:="Asian"]
Shooting[Race=="Unknown"|Race=="Two or more races" | Race=="Some other race",Race:="Other"]
table(Shooting$Race)


#Preprocessing Target

Shooting[is.na(Target),Target:="Unknown"]
Shooting[Target=='Family'|Target=='Family/Neighbors'|Target=='Family+students'|Target=='Coworkers'|Target=='coworkers'
         |Target=='Students+Parents'|Target=='Family+random'|Target=="partner's family"|Target=="Coworker's Family"
         |Target=='neighbors'|Target=='Girlfriend'|Target=="House Owner"
         |Target=="Friends",Target:="Family,Girlfriend(Relationships at that time)"]
Shooting[Target=='Children'|Target=='school girls'|Target=='Students'
         |Target=='Students+Teachers'|Target=='Teachers',Target:="School(Teachers or Students)"] 

Shooting[Target=='Ex-Girlfriend'|Target=='Ex-Wife & Family'|Target == 'Ex-Girlfriend & Family' |Target == 'Ex-GirlFriend'
         |Target=='Ex-Girlfriend+random'|Target=='Ex-girlfriend'|Target=="Ex-Coworkers"|Target=='Ex-Wife'|Target=='Ex-Girlfriend', Target:="Girlfriend/Wife/coworkers ( past relationships)"]

Shooting[Target=='Sikhs'|Target=='monks'|Target=='prayer group',Target:="Motives against religion"] 

Shooting[Target=="Marines"|Target=="Policeman+Council Member"| Target=="police"|
           Target=="Policeman"|Target=="Trooper"|Target=="Social Workers", Target:="Marines,Police and Social Workers(Police Brutality)"]

Shooting[Target=="birthday party bus"|Target=="party guests"|Target=="uninvited guests", Target:="Parties"]

table(Shooting$Target)

#Preprocessing Mental Health Issues

Shooting[`Mental Health Issues`=="unknown",`Mental Health Issues`:="Unknown"]
table(Shooting$`Mental Health Issues`)


#Preprocessing Age

Shooting[is.na(Age),Age:=0]
Shooting$NewAge <-
  sapply(Shooting$Age,function(x){
    if(x>=10 && x<20){
      "10+ Teen" }
    else if(x>=20 && x<30){
      "20's"}
    else if(x>=30 && x<40){
      "30's"}
    else if(x>=40 && x<50){
      "40's"}
    else if(x>=50 && x<60){
      "50's"}
    else if(x>=60 && x<70){
      "60's"}
    else if(x>=70 && x<80){
      "70's"}
    else if(x>80){
      "Multiple Shooters"}
    else{
      "Age Unknown"}
  })
table(Shooting$NewAge)


#Preprocessing State

Shooting$State <- sapply(Shooting$Location, function(parts)
{
  temp <- strsplit(parts, split = ",")
  sapply(temp, function(new)
  {   
    new[2]
    
  }
  )
})

Shooting$City <- sapply(Shooting$Location, function(parts)
{
  temp <- strsplit(parts, split = ",")
  sapply(temp, function(new)
  {   
    new[1]
    
  }
  )
})

Shooting[is.na(`State`), `State`:="Unknown"]
Shooting[`State`==' CA'| `State`== ' San Diego'| `State`==" LA" |`State`== " California", `State`:="California"]
Shooting[`State`==' NV'| `State`==" Nevada",`State`:="Nevada"]
Shooting[`State`==' CO'| `State` == " Colorado",`State`:="Colorado"] 
Shooting[`State`=='  Virginia'|`State`==" Virginia",`State`:="Virginia"]
Shooting[`State`==" TX"| `State` == " Texas",`State`:="Texas"]
Shooting[`State`==" MD",`State`:="Maryland"]
Shooting[`State`==" PA"|`State`==" Lancaster"|`State`==" Souderton",`State`:="Pennsylvania"]
Shooting[`State`==" WA"|`State`==" Washington",`State`:="Washington"]

table(Shooting$State)
