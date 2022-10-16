#DATA PREPROCESSING
data = read.csv(file.choose())
colnames(data) #displaying column names
df=data.frame(data) #creating data frame
#clean NA
#coulmn payment frequency NA=0
(df$PAYMENT_FREQUENCY[is.na(df$PAYMENT_FREQUENCY)] <- 0)
df$PAYMENT_FREQUENCY<-as.factor(df$PAYMENT_FREQUENCY)

install.packages("moments")
library(moments) #calculate skewness
print(skewness(data$RISK_RATED_AREA_B))
data$RISK_RATED_AREA_B

#replace NA with median
#Risk rate area B
library("dplyr")
install.packages("tidyr")
library(tidyr)
(mean_Risk_rate_area_B <- median(df$RISK_RATED_AREA_B, na.rm=TRUE))
(df$RISK_RATED_AREA_B <- replace_na(df$RISK_RATED_AREA_B,mean_Risk_rate_area_B))
#Risk rate area C
(mean_Risk_rate_area_C <- median(df$RISK_RATED_AREA_C, na.rm=TRUE))
(df$RISK_RATED_AREA_C <- replace_na(df$RISK_RATED_AREA_C,mean_Risk_rate_area_C))

#factor
df$SUM_INSURED_BUILDINGS <-as.factor(df$SUM_INSURED_BUILDINGS)
df$LISTED <-as.factor(df$LISTED)
df$PAYING_GUESTS <-as.factor(df$PAYING_GUESTS)

#Data Wrangling
df$POL_STATUS
colnames(df)
#library(data.table)
#df <- as.data.table(df)
df
total <- nrow(data) #256136
status_groups <- df[ , .(count=.N, percent=round((.N/total)*100, 2)) ,by =POL_STATUS] #.N count of rows with duplicate policy status
status_groups <- status_groups[order(count, decreasing = TRUE)]
status_groups

#pie chart
?pie3D
install.packages("plotrix")
library(plotrix)
pieLabels <- paste(status_groups$POL_STATUS,' ', status_groups$percent, '%')
pie3D(status_groups$count,labels=pieLabels,explode=0.1, radius=0.4,height=0.1, col=rainbow(length(status_groups$POL_STATUS)),
      main="Pie Chart of Policy Status ")

status_groups[POL_STATUS != 'Lapsed', POL_STATUS:= "Non Resiliated"]
status_groups[POL_STATUS == 'Lapsed', POL_STATUS:= "Resiliated"]
status_groups <- status_groups[, .(count=sum(count), percent = round((.N*100)/sum(count), 2)), by = POL_STATUS]
status_groups[,percent := round((count*100)/sum(count), 2)]
status_groups <- status_groups[order(count, decreasing = TRUE)]
status_groups

#===============pie_chart_for_resiliation=======================
pieLabels <- paste(status_groups$POL_STATUS,' ', status_groups$percent, '%')
pie_resi <- pie3D(status_groups$percent,labels=pieLabels,explode=0.1, radius=1,height=0.1, col=rainbow(length(status_groups$POL_STATUS)),
             main="Pie Chart of Resiliation")

df$Resiliated[df$POL_STATUS == 'Lapsed'] <- 1
df$Resiliated[df$POL_STATUS != 'Lapsed'] <- 0
t(head(df, 3))
table(df$Resiliated)

df$Resiliated
#========Explore customer occupation
Resiliated <- df[which(Resiliated==1),]
Non.Resiliated <-df[which(Resiliated==0),]
(total.Resiliated <- nrow(Resiliated))
(total.Non.Resiliated <- nrow(Non.Resiliated))
