getwd()
setwd("C:\\Users\\rmuwani\\Desktop\\R_PROJECT")
install.packages("readxl")
library(readxl)
mydata=read_excel("crime data.xlsx")
View(mydata)
str(mydata)
head(mydata)
summary(mydata)
dim(mydata)
mydata
names(mydata)

table(mydata$UCR_PART)

# Pie Chart with Percentages
slices <- c(1285, 63231, 100283,162928)
lbls <- c("Other", "Level", "Level2", "Level3")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Crime Levels")
par(mar=c(1,1,1,1))
library(plotrix)
slices <- c(1285, 63231, 100283,162928)
lbls <- c("Other", "Level", "Level2", "Level3")
pie3D(slices,labels=lbls,explode=0.15,
      main="Pie Chart of Countries ")

table(mydata$YEAR,mydata$UCR_PART)

addmargins(xtabs(~YEAR+UCR_PART,data=mydata))

addmargins(xtabs(~DISTRICT+UCR_PART,data=mydata))

addmargins(xtabs(~OFFENSE_CODE_GROUP+UCR_PART,data=mydata))

addmargins(xtabs(~UCR_PART+OFFENSE_CODE_GROUP,data=mydata))

addmargins(xtabs(~DAY_OF_WEEK+UCR_PART,data=mydata))
addmargins(xtabs(~MONTH+UCR_PART,data=mydata))
addmargins(xtabs(~HOUR+UCR_PART,data=mydata))
prop.table(xtabs(~YEAR+OFFENSE_CODE,data=mydata))
prop.table(xtabs(~YEAR+UCR_PART,data=mydata))

ggplot(mydata,aes(x=YEAR,fill=UCR_PART,color="green"))+
  theme_bw()+
  geom_bar()+
  labs(X="Year of Incident", y="Number of Incidents",title="OFFENCE RECORD PER YEAR")


ggplot(mydata,aes(x=as.factor(MONTH),fill=UCR_PART,color="red"))+
  theme_bw()+
  geom_bar()+
  labs(X="Year of Incident", y="Number of Incidents",title="OFFENCE RECORD PER MONTH")

ggplot(mydata,aes(x=as.factor(HOUR),fill=UCR_PART,color="red"))+
  theme_bw()+
  geom_bar(color="red")+
  labs(X="Year of Incident", y="Number of Incidents",title="OFFENCE RECORD PER MONTH")


levels("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
ggplot(mydata,aes(x=as.factor(DAY_OF_WEEK),fill=UCR_PART,
                  color="green"))+
  theme_bw()+
  geom_bar()+
  labs(X="Year of Incident", y="Number of Incidents",title="OFFENCE RECORD PER YEAR")

#most dangerous streets
street_crime <- sort(table(mydata$STREET), decreasing = TRUE)
head(street_crime, 10)

#least dangerous streets
(street_crime <- sort(table(mydata$STREET), decreasing = TRUE))
(tail(street_crime, 10))


#crime by offense description, top 10
OFFENSE_DESCRIPTION <- data.frame(sort(table(mydata$OFFENSE_DESCRIPTION), decreasing = TRUE))
-head(OFFENSE_DESCRIPTION,10)

#crime by reporting area
plot(prop.table(table(mydata$REPORTING_AREA)))
REPORTING_AREA <- data.frame(sort(table(mydata$REPORTING_AREA), decreasing = TRUE))
head(REPORTING_AREA, 10)
