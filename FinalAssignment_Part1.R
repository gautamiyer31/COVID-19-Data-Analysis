setwd('C:/Users/gauta/Documents/R Files')
covid_data <- read.csv('covid19casesdemographics(1).csv')
head(covid_data)


#Creating subsets for each demographic categories
covid_data_age<- subset(covid_data,demographic_category == 'Age Group')
view(covid_data_age)

covid_data_gender<- subset(covid_data,demographic_category == 'Gender')
view(covid_data_gender)

covid_data_race<- subset(covid_data,demographic_category == 'Race Ethnicity')
view(covid_data_race)


#Detecting missing values in the data set

library('Amelia')
library(ggplot2)
missmap(covid_data_age,main = 'COVID 19 data-missing values',legend = FALSE)
p1<- ggplot(covid_data_age, aes(demographic_value,deaths))+ geom_boxplot(aes(fill = factor(demographic_value),group = demographic_value,alpha = 0.5))
p1 
#Impute the deaths
impute_deaths<- function(Deaths,demographic_value){
  out<-Deaths
  for (i  in 1:length(Deaths)) {
    if(is.na(Deaths[i])){
      if(demographic_value[i]=='0-17'){
        out[i]<-0
      }else if (demographic_value[i]=='18-49'){
        out[i]<-3530
      }else if (demographic_value[i]=='50-64'){
        out[i]<-9567
      }else{out[i]<-34307
      }
    }else{out[i]<- Deaths[i]
    }
  } 
  return(out)
}
covid_data_age$deaths<-impute_deaths(covid_data_age$deaths,covid_data_age$demographic_value)
missmap(covid_data_age,legend = FALSE)

#For the Gender data set.
library(ggplot2)
p1<- ggplot(covid_data_gender, aes(demographic_value,deaths))+ geom_boxplot(aes(fill = factor(demographic_value),group = demographic_value,alpha = 0.5))
p1 
#A function to replace null data with the mean
impute_deaths_gender<- function(Deaths,demographic_value){
  out<-Deaths
  for (i  in 1:length(Deaths)) {
    if(is.na(Deaths[i])){
      if(demographic_value[i]=='Female'){
        out[i]<-19449
      }else if (demographic_value[i]=='Male'){
        out[i]<-27155
      }else if (demographic_value[i]=='Unknown'){
        out[i]<-182
      }
      
    }else{out[i]<- Deaths[i]
    }
  } 
  return(out)
}
covid_data_gender$deaths<-impute_deaths_gender(covid_data_gender$deaths,covid_data_gender$demographic_value)
missmap(covid_data_gender,main = 'COVID 19 data-missing values',legend = FALSE)

#For the race data set.

p1<- ggplot(covid_data_race, aes(demographic_value,deaths))+ geom_boxplot(aes(fill = factor(demographic_value),group = demographic_value,alpha = 0.5))
p1 

impute_deaths_race<- function(Deaths,demographic_value){
  out<-Deaths
  for (i  in 1:length(Deaths)) {
    if(is.na(Deaths[i])){
      if(demographic_value[i]=='American Indian or Alaska Native'){
        out[i]<-186
      }else if (demographic_value[i]=='Asian'){
        out[i]<-5220
      }else if (demographic_value[i]=='Black'){
        out[i]<-3071
      }
      else if (demographic_value[i]=='Latino'){
        out[i]<-20703
      }
      else if (demographic_value[i]=='Multi-Race'){
        out[i]<-631
      }
      else if (demographic_value[i]=='Native Hawaiian and other Pacific Islander'){
        out[i]<-263
      }
      else if (demographic_value[i]=='Other'){
        out[i]<-610
      }
      else if (demographic_value[i]=='White'){
        out[i]<-14627
      }
    }else{out[i]<- Deaths[i]
    }
  } 
  return(out)
}
covid_data_race$deaths<-impute_deaths_race(covid_data_race$deaths,covid_data_race$demographic_value)
missmap(covid_data_race,main = 'COVID 19 data-missing values',legend = FALSE)

#Merging the data sets as one and performing regression analysis.
library(tidyverse)
final_data = bind_rows(covid_data_age,covid_data_gender, covid_data_race)


Model<- lm(deaths ~ demographic_value, data=final_data); summary(Model)

