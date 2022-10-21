#DATA PREPROCESSING
install.packages("magrittr")
library(magrittr)
library(ggplot2)
library(knitr)
install.packages("ggpubr")
library(ggpubr)
library("gridExtra")
#install.packages("cowplot")
library("cowplot")
install.packages("lubridate")
library(lubridate)
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

#Policy best covered
#I am curious to discover the most covered policy among the others. I will wrangle the data to find out adding a new column that show the total coverage of the policy
dt <- df[, .(SUM_INSURED_BUILDINGS, SUM_INSURED_CONTENTS, SPEC_SUM_INSURED)]
name <- 'total_coverage'
df[, (name):= SUM_INSURED_BUILDINGS+ SUM_INSURED_CONTENTS+ SPEC_SUM_INSURED]
ordered_table <- df[order(total_coverage, decreasing = TRUE), .(Police, SUM_INSURED_BUILDINGS, SUM_INSURED_CONTENTS, SPEC_SUM_INSURED, total_coverage)]

ordered_table <- data.table(ordered_table)
head(ordered_table, 8)
#==================================================
status_client <- df[!is.na(P1_EMP_STATUS), .(count=.N), by = P1_EMP_STATUS]
status_client <- status_client[order(count, decreasing = TRUE)]
status_client

#the main goal of this project is just get some insights, even when we don't have the complete information.

#R = Retired,
#E = Employed,
#N = Not Available,
#H = House person,
#S = Student ,
#U = Unemployed.
#Anyways, there are over 11 professional status represented in the HI dataset (avoiding the null status). The Retired clients form the overwhelmingly majority. The Employees and Students come at a very distant second and third respectively.

#------------------------------------------------------------
palette_colors <-  c("#5e482c", "#f7d2a7", "#df45a4", "#d1223d", "#fbdb50", "#cd500d", "#d5addf", "#206536", "#b98d9b", "#ebaa7b", "#85a664", "#ef99fa")
ordered_status <- status_client$P1_EMP_STATUS
ggbarplot(status_client, x= "P1_EMP_STATUS", y= "count", xlab="Client's professional status", ylab ="quantity policies", fill = "P1_EMP_STATUS", label=TRUE, title = "Clients professional status", label.pos = "out", order = ordered_status, palette = palette_colors)

#As mentioned earlier, Retired and Employees clients are the most commonly occurring professional status. V, A, F, I, C form the minority. I can imagine that this last statuses are two or the following:
#The richest clients of the sample.
#The clients living in a very uncomfortable zone/building.

#=======================================================================================================================================================================================================================================================================================

#Correlations
#Geographical Classification of Risk - Building and Assured Sum - Building

ggscatter(df, x = "SUM_INSURED_BUILDINGS", y = "RISK_RATED_AREA_B", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Assured Sum - Building", ylab = "Geographical Classification of Risk - Building")
#By Analysing above plot we can interprete that,
#As we can see in the plot, the Pearson Coefficient of the two  quantities is -0.012 which suggests that there is not a tangible correlation. In other words, the buildings Geographical Classification of Risk and the Assured Sum are independent quantities. The points fall close to the line, which indicates that there is a strong negative relationship between the variables.

#=====================================================================================================-================================
risk <- df[LAST_ANN_PREM_GROSS > 0]
#head(risk, 2)
ggqqplot(df$LAST_ANN_PREM_GROSS, ylab = "Premium - Total for the previous year")

#By Analysing above plot we can interprete that,
#We can see that for those premium clients with Total for the previous year bonus (more than 0/null) are between 0 and 1,000.
#Now i will check the most popular and most successful months and day for quotation and cover start.

#==========================================================================================================================================

month_order <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
day_order <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
df$quotemonth_n <- month(as.POSIXlt(df$QUOTE_DATE, format="%m/%d/%Y"))
df$covermonth_n <- month(as.POSIXlt(df$COVER_START, format="%d/%m/%Y"))
#i name the columns
quotesmonthDF <- data.frame(month_n = df$quotemonth_n )
coversmonthDF <- data.frame(month_n = df$covermonth_n)
#head(quotesmonthDF, 2)
#i avoid the null values and make the group by each month to get the monthly total
quotesmonthgroup <- data.table(quotesmonthDF)
quotesmonthgroup <-quotesmonthgroup[month_n <= 12]
quotesmonthgroup <-quotesmonthgroup[(order(month_n)), .(count=.N), by=month_n]

coversmonthgroup <- data.table(coversmonthDF)
coversmonthgroup <-coversmonthgroup[month_n <= 12]
coversmonthgroup <-coversmonthgroup[(order(month_n)), .(count=.N), by=month_n]
#i add the name of the month
quotesmonthgroup$month_s <- month_order[quotesmonthgroup$month_n ]
coversmonthgroup$month_s <- month_order[coversmonthgroup$month_n ]
head(quotesmonthgroup, 12)
head(coversmonthgroup, 12)

#===================================================================================

Model_Data$quotemonth_s <- month_order[Model_Data$quotemonth_n ]
Model_Data$covermonth_s <- month_order[Model_Data$covermonth_n ]

#=====================================================================================================================================================================================================================================================

ggbarplot(quotesmonthgroup, x= "month_s" , y= "count", xlab="Month", ylab ="quantity policies", label=TRUE, title = "Most successful months in Quotation date", label.pos = "out", fill = "month_s", color = "month_s", palette = palette_colors)

#By Analysing above plot we can interprete that,
#It appears that January is the most popular month when it comes to policies quotations. This is maybe for the salary bonus that employees get in christmas and the clients makes theirs plan come true making the quote.

#=================================================================================================================================================================================================================================================================================================================================================

#Most successful months in Coverage date
ggbarplot(coversmonthgroup, x= "month_s" , y= "count", xlab="Month", ylab ="quantity policies", fill = "month_s", palette = palette_colors, label=TRUE, title = "Most successful months in Coverage date", label.pos = "out")

#By Analysing above plot we can interprete that,
#It seems that the beggining and ending months of the year have the highest number of policies. This can be attributed to the fact that people tend to dedicate their money in the summer to vacances when the kids are out of school, the parents are on vacation and therefore, the policies is more likely to be practically none.
#Now i want to know if there are months that tend to be more successful than others. For this i will create a boxplot between the total coverage and the months.

#===================================================================================================================================================================================================================================================================================================================================================
boxplotDF <- df[covermonth_n <= 12, .(covermonth_n, covermonth_s, total_coverage)]
ggboxplot(boxplotDF, x = "covermonth_s", y = "total_coverage", xlab="Coverage month", ylab="total coverage amount", width = 0.8, fill="covermonth_s", palette = palette_colors, order=month_order)
#We see that effectively the months of January, February, March and November tend to yield the highest median returns. However December does not have a high total coverage, even when this month has the highest number of policies. On the other hand June and August have a not so high coverage and finally the resting months are the least successful months on the aforementioned metrics. Again, the success of the starting and ending months can be attributed to the fact that in summer the people tend to spend their money on vacation stuffs.

#=======================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================
#Number of buildings by year
year_built <- df[df$YEARBUILT != 'null' & df$YEARBUILT > 0, .(count=.N), by = YEARBUILT]
year_built <- year_built[order(YEARBUILT)]
year_built

#=================================================================================================================================================================================================================================================================================
#Number of buildings by year construction
qplot(x = year_built$YEARBUILT, y = year_built$count, xlab="Year of building construction", ylab="Number of Policies", main = "Number of buildings by year construction" , geom="line")
#We notice that there is a rise in the number of buildings constructed the 1870s decade and there is a peak in the 1940s decade with more than 60 thousands policies. It can be concluded that the majority of buildings have between 20 and 80 years of constructed.

#============================================================================================================================================================================================================================================================================
#Relation between age of client (at the time that made the first payment) and the duration of the policy (for those who have a cancelled policy)

#set client age
birthday_year <- year(as.POSIXlt(df$P1_DOB, format="%d/%m/%Y"))
cover_year <- year(as.POSIXlt(Model_Data$COVER_START, format="%d/%m/%Y"))
df$client_age <- cover_year-birthday_year
#=========================================================================================================================================================
#set policy duration
cancelation_year <- year(as.POSIXlt(df$MTA_DATE, format="%d/%m/%Y"))
Model_Data$police_duration <- cancelation_year-cover_year
#========================================================================================================================================================================================

age_bar <- df[!is.na(client_age) & client_age > 0, .(count=.N), by = client_age]
age_bar <- age_bar[order(client_age)]
head(age_bar, 20)

years_table <-df[!is.na(police_duration) & police_duration > 0 & !is.na(client_age) & client_age > 0, .(client_age,police_duration)]

ggdensity(years_table, x = "client_age", y = "..count..", xlab = "Age of client", ylab="Policies", add = "mean", fill="#00AFBB", rug = TRUE, palette = c("#00AFBB", "#E7B800"))

#Interpretation of above plot
#Surprisingly, the median age of the clients at the moment of the coverage payment is 66 years old and the peak is around 70's. This makes sense with the the previous discovery that most clients are retired.
#Now, i want to know if there is a correlation between the age of the client and the policy duration.

#------------SCATTER PLOT OF client age and count--------------
ggscatter(years_table, x = "client_age", y = "police_duration", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Client Age", ylab = "Policy duration")
#As we can see in the scattered graphic, the Pearson Coefficient between the client's age and the policy duration (in years) is -0.074 which suggests that there is no tangible correlation. So we can say that this quantitative variables are independent quantities. The relationship is negative because, as one variable increases, the other variable decreases.

#=================================================================================================================================

#Bar chart for Resiliated (P1_EMP_STATUS)
status_client <- Resiliated[!is.na(P1_EMP_STATUS), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = P1_EMP_STATUS]
status_client <- status_client[order(percent, decreasing = TRUE)]
status_client
palette_colors <-  c("#5e482c", "#f7d2a7", "#df45a4", "#d1223d", "#fbdb50", "#cd500d", "#d5addf", "#206536", "#b98d9b", "#ebaa7b", "#85a664", "#ef99fa")
ordered_status <- status_client$P1_EMP_STATUS
plot1<-ggbarplot(status_client, x= "P1_EMP_STATUS", y= "percent", xlab="Resiliated professional status", ylab ="quantity policies", fill = "P1_EMP_STATUS", label=TRUE, title = "Resiliated Clients professional status", 
                 label.pos = "out", order = ordered_status, palette = palette_colors)
plot1

#Bar chart for Non-Resiliated (P1_EMP_STATUS)
status_client1 <- Non.Resiliated[!is.na(P1_EMP_STATUS), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = P1_EMP_STATUS]
status_client1 <- status_client1[order(percent, decreasing = TRUE)]
status_client1
palette_colors <-  c("#5e482c", "#f7d2a7", "#df45a4", "#d1223d", "#fbdb50", "#cd500d", "#d5addf", "#206536", "#b98d9b", "#ebaa7b", "#85a664", "#ef99fa")
ordered_status1 <- status_client1$P1_EMP_STATUS
plot2<-ggbarplot(status_client1, x= "P1_EMP_STATUS", y= "percent", xlab="Non Resiliated professional status", ylab ="quantity policies", fill = "P1_EMP_STATUS", label=TRUE, title = "Non Resiliated professional status", 
                 label.pos = "out", order = ordered_status1, palette = palette_colors)
plot2

#======================================================================================================================================================================================================================================
#Comparison chart of features we are interested in
#Bar chart for Resiliated (Bus_used)
status_client <- Resiliated[!is.na(BUS_USE), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = BUS_USE]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$BUS_USE
plot1<-ggbarplot(status_client, x= "BUS_USE", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "BUS_USE", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = palette_colors)

##Bar chart for Non-Resiliated (Bus_used)
status_client1 <- Non.Resiliated[!is.na(BUS_USE), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = BUS_USE]
#table(status_client1)
status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$BUS_USE
plot2<-ggbarplot(status_client1, x= "BUS_USE", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "BUS_USE", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = palette_colors)
plot_grid(plot1,plot2)

#=========================================================================
#Bar chart for Resiliated (CLAIM3YEARS)
status_client <- Resiliated[!is.na(CLAIM3YEARS), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = CLAIM3YEARS]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$CLAIM3YEARS
plot1<-ggbarplot(status_client, x= "CLAIM3YEARS", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "CLAIM3YEARS", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette =  )

#Bar chart for Non-Resiliated (CLAIM3YEARS)
status_client1 <- Non.Resiliated[!is.na(CLAIM3YEARS), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = CLAIM3YEARS]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$CLAIM3YEARS
plot2<-ggbarplot(status_client1, x= "CLAIM3YEARS", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "CLAIM3YEARS", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = )
plot_grid(plot1,plot2)

#====================================================================================================

#Bar chart for Resiliated (P1_PT_EMP_STATUS)
status_client <- Resiliated[!is.na(P1_PT_EMP_STATUS), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = P1_PT_EMP_STATUS]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$P1_PT_EMP_STATUS
plot1<-ggbarplot(status_client, x= "P1_PT_EMP_STATUS", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "P1_PT_EMP_STATUS", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette =  )

#Bar chart for Non-Resiliated (P1_PT_EMP_STATUS)
status_client1 <- Non.Resiliated[!is.na(P1_PT_EMP_STATUS), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = P1_PT_EMP_STATUS]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$P1_PT_EMP_STATUS
plot2<-ggbarplot(status_client1, x= "P1_PT_EMP_STATUS", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "P1_PT_EMP_STATUS", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = )
plot_grid(plot1,plot2)

#============================================================================================================================

#Bar chart for Resiliated (CLERICAL)
status_client <- Resiliated[!is.na(CLERICAL), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = CLERICAL]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$CLERICAL
plot1<-ggbarplot(status_client, x= "CLERICAL", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "CLERICAL", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette =  c("green","yellow"))

#Bar chart for Non-Resiliated (CLERICAL)
status_client1 <- Non.Resiliated[!is.na(CLERICAL), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = CLERICAL]
status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$CLERICAL
plot2<-ggbarplot(status_client1, x= "CLERICAL", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "CLERICAL", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("green","yellow") )
plot_grid(plot1,plot2)
#====================================================================================================

#Bar chart for Resiliated (AD_BUILDINGS)
status_client <- Resiliated[!is.na(AD_BUILDINGS), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = AD_BUILDINGS]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$AD_BUILDINGS
plot1<-ggbarplot(status_client, x= "AD_BUILDINGS", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "AD_BUILDINGS", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette =  c("green","black"))

#Bar chart for Non-Resiliated (AD_BUILDINGS)
status_client1 <- Non.Resiliated[!is.na(AD_BUILDINGS), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = AD_BUILDINGS]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$AD_BUILDINGS
plot2<-ggbarplot(status_client1, x= "AD_BUILDINGS", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "AD_BUILDINGS", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("green","black") )
plot_grid(plot1,plot2)

#=====================================================================================================================================================================================================================================

#Bar chart for Resiliated (AD_CONTENTS )
status_client <- Resiliated[!is.na(AD_CONTENTS), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = AD_CONTENTS]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$AD_CONTENTS
plot1<-ggbarplot(status_client, x= "AD_CONTENTS", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "AD_CONTENTS", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette =  c("yellow","black"))
#Bar chart for Non-Resiliated (AD_CONTENTS )
status_client1 <- Non.Resiliated[!is.na(AD_CONTENTS), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = AD_CONTENTS]
status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$AD_CONTENTS
plot2<-ggbarplot(status_client1, x= "AD_CONTENTS", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "AD_CONTENTS", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("yellow","black") )
plot_grid(plot1,plot2)
#===================================================================================================================================================================================================================================

#Bar chart for Resiliated (CONTENTS_COVER  )
status_client <- Resiliated[!is.na(CONTENTS_COVER), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = CONTENTS_COVER]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$CONTENTS_COVER
plot1<-ggbarplot(status_client, x= "CONTENTS_COVER", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "CONTENTS_COVER", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette =  c("red","black"))

#Bar chart for Non-Resiliated (CONTENTS_COVER  )
status_client1 <- Non.Resiliated[!is.na(CONTENTS_COVER), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = CONTENTS_COVER]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$CONTENTS_COVER
plot2<-ggbarplot(status_client1, x= "CONTENTS_COVER", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "CONTENTS_COVER", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("red","black") )
plot_grid(plot1,plot2)

#============================================================================================================================================================================================================================================

#Bar chart for Resiliated (BUILDINGS_COVER)
status_client <- Resiliated[!is.na(BUILDINGS_COVER), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = BUILDINGS_COVER]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$BUILDINGS_COVER
plot1<-ggbarplot(status_client, x= "BUILDINGS_COVER", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "BUILDINGS_COVER", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette =  c("red","orange"))

#Bar chart for Non-Resiliated (BUILDINGS_COVER)
status_client1 <- Non.Resiliated[!is.na(BUILDINGS_COVER), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = BUILDINGS_COVER]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$BUILDINGS_COVER
plot2<-ggbarplot(status_client1, x= "BUILDINGS_COVER", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "BUILDINGS_COVER", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("red","orange") )
plot_grid(plot1,plot2)
#==============================================================================================

#Bar chart for Resiliated (P1_POLICY_REFUSED)
status_client <- Resiliated[!is.na(P1_POLICY_REFUSED), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = P1_POLICY_REFUSED]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$P1_POLICY_REFUSED
plot1<-ggbarplot(status_client, x= "P1_POLICY_REFUSED", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "P1_POLICY_REFUSED", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette =  )

#Bar chart for Non-Resiliated (P1_POLICY_REFUSED)
status_client1 <- Non.Resiliated[!is.na(P1_POLICY_REFUSED), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = P1_POLICY_REFUSED]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$P1_POLICY_REFUSED
plot2<-ggbarplot(status_client1, x= "P1_POLICY_REFUSED", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "P1_POLICY_REFUSED", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette =  )
plot_grid(plot1,plot2)

#================================================================================================================================================================================================================================================

#Bar chart for Resiliated (P1_MAR_STATUS )
status_client <- Resiliated[!is.na(P1_MAR_STATUS), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = P1_MAR_STATUS]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$P1_MAR_STATUS
plot1<-ggbarplot(status_client, x= "P1_MAR_STATUS", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "P1_MAR_STATUS", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette =   )

#Bar chart for Non-Resiliated (P1_MAR_STATUS )
status_client1 <- Non.Resiliated[!is.na(P1_MAR_STATUS), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = P1_MAR_STATUS]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$P1_MAR_STATUS
plot2<-ggbarplot(status_client1, x= "P1_MAR_STATUS", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "P1_MAR_STATUS", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette =  )
plot_grid(plot1,plot2)

#=========================================================================================================================================================================================================================================

#Bar chart for Resiliated (P1_SEX)
status_client <- Resiliated[!is.na(P1_SEX), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = P1_SEX]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$P1_SEX
plot1<-ggbarplot(status_client, x= "P1_SEX", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "P1_SEX", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette =  c("white","black") )

#Bar chart for Non-Resiliated (P1_SEX)
status_client1 <- Non.Resiliated[!is.na(P1_SEX), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = P1_SEX]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$P1_SEX
plot2<-ggbarplot(status_client1, x= "P1_SEX", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "P1_SEX", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("white","black")  )
plot_grid(plot1,plot2)

#========================================================================================================
#Bar chart for Resiliated (APPR_ALARM)
status_client <- Resiliated[!is.na(APPR_ALARM), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = APPR_ALARM]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$APPR_ALARM
plot1<-ggbarplot(status_client, x= "APPR_ALARM", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "APPR_ALARM", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette =  )

#Bar chart for Non-Resiliated (APPR_ALARM)
status_client1 <- Non.Resiliated[!is.na(APPR_ALARM), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = APPR_ALARM]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$APPR_ALARM
plot2<-ggbarplot(status_client1, x= "APPR_ALARM", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "APPR_ALARM", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette =   )
plot_grid(plot1,plot2)
#===================================================================================================================================================================================================================================

#Bar chart for Resiliated (APPR_LOCKS)
status_client <- Resiliated[!is.na(APPR_LOCKS), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = APPR_LOCKS]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$APPR_LOCKS
plot1<-ggbarplot(status_client, x= "APPR_LOCKS", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "APPR_LOCKS", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("pink","green") )

#Bar chart for Non-Resiliated (APPR_LOCKS)
status_client1 <- Non.Resiliated[!is.na(APPR_LOCKS), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = APPR_LOCKS]
table(status_client1)
status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$APPR_LOCKS
plot2<-ggbarplot(status_client1, x= "APPR_LOCKS", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "APPR_LOCKS", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("pink","green")  )
plot_grid(plot1,plot2)
#============================================================================================================================================================================================================================

#Bar chart for Resiliated (FLOODING)
status_client <- Resiliated[!is.na(FLOODING), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = FLOODING]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$FLOODING
plot1<-ggbarplot(status_client, x= "FLOODING", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "FLOODING", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("pink","blue") )

#Bar chart for Non-Resiliated (FLOODING)
status_client1 <- Non.Resiliated[!is.na(FLOODING), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = FLOODING]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$FLOODING
plot2<-ggbarplot(status_client1, x= "FLOODING", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "FLOODING", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("pink","blue")  )
plot_grid(plot1,plot2)
#=================================================================================================================================================================================================================

#Bar chart for Resiliated (NEIGH_WATCH)
status_client <- Resiliated[!is.na(NEIGH_WATCH), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = NEIGH_WATCH]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$NEIGH_WATCH
plot1<-ggbarplot(status_client, x= "NEIGH_WATCH", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "NEIGH_WATCH", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("gold","red") )

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$NEIGH_WATCH
plot2<-ggbarplot(status_client1, x= "NEIGH_WATCH", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "NEIGH_WATCH", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("gold","red")  )
plot_grid(plot1,plot2)
#===============================================================================================================================================================================================================

#Bar chart for Resiliated (OCC_STATUS )
status_client <- Resiliated[!is.na(OCC_STATUS), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = OCC_STATUS]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$OCC_STATUS
plot1<-ggbarplot(status_client, x= "OCC_STATUS", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "OCC_STATUS", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette =  )

#Bar chart for Non-Resiliated (OCC_STATUS )
status_client1 <- Non.Resiliated[!is.na(OCC_STATUS), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = OCC_STATUS]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$OCC_STATUS
plot2<-ggbarplot(status_client1, x= "OCC_STATUS", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "OCC_STATUS", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = )
plot_grid(plot1,plot2)

#===================================================================

#Bar chart for Resiliated (SAFE_INSTALLED)
status_client <- Resiliated[!is.na(SAFE_INSTALLED), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = SAFE_INSTALLED]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$SAFE_INSTALLED
plot1<-ggbarplot(status_client, x= "SAFE_INSTALLED", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "SAFE_INSTALLED", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette =  )

#Bar chart for Non-Resiliated (SAFE_INSTALLED)
status_client1 <- Non.Resiliated[!is.na(SAFE_INSTALLED), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = SAFE_INSTALLED]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$SAFE_INSTALLED
plot2<-ggbarplot(status_client1, x= "SAFE_INSTALLED", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "SAFE_INSTALLED", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = )
plot_grid(plot1,plot2)

#=====================================================================================================

#Bar chart for Resiliated (SEC_DISC_REQ)
status_client <- Resiliated[!is.na(SEC_DISC_REQ), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = SEC_DISC_REQ]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$SEC_DISC_REQ
plot1<-ggbarplot(status_client, x= "SEC_DISC_REQ", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "SEC_DISC_REQ", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("red","black") )

#Bar chart for Non-Resiliated (SEC_DISC_REQ)
status_client1 <- Non.Resiliated[!is.na(SEC_DISC_REQ), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = SEC_DISC_REQ]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$SEC_DISC_REQ
plot2<-ggbarplot(status_client1, x= "SEC_DISC_REQ", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "SEC_DISC_REQ", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("red","black"))
plot_grid(plot1,plot2)

#==============================================================================

#Bar chart for Resiliated (SUBSIDENCE)
status_client <- Resiliated[!is.na(SUBSIDENCE), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = SUBSIDENCE]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$SUBSIDENCE
plot1<-ggbarplot(status_client, x= "SUBSIDENCE", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "SUBSIDENCE", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("blue","white") )

#Bar chart for Non-Resiliated (SUBSIDENCE)
status_client1 <- Non.Resiliated[!is.na(SUBSIDENCE), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = SUBSIDENCE]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$SUBSIDENCE
plot2<-ggbarplot(status_client1, x= "SUBSIDENCE", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "SUBSIDENCE", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("blue","white"))
plot_grid(plot1,plot2)

#========================================================================================
#Bar chart for Resiliated (PAYMENT_METHOD )
status_client <- Resiliated[!is.na(PAYMENT_METHOD), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = PAYMENT_METHOD]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$PAYMENT_METHOD
plot1<-ggbarplot(status_client, x= "PAYMENT_METHOD", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "PAYMENT_METHOD", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette =  )

#Bar chart for Non-Resiliated (PAYMENT_METHOD )
status_client1 <- Non.Resiliated[!is.na(PAYMENT_METHOD), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = PAYMENT_METHOD]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$PAYMENT_METHOD
plot2<-ggbarplot(status_client1, x= "PAYMENT_METHOD", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "PAYMENT_METHOD", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = )
plot_grid(plot1,plot2)

#====================================================================================================================================================================================================
#Bar chart for Resiliated (LEGAL_ADDON_PRE_REN)
status_client <- Resiliated[!is.na(LEGAL_ADDON_PRE_REN), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = LEGAL_ADDON_PRE_REN]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$LEGAL_ADDON_PRE_REN
plot1<-ggbarplot(status_client, x= "LEGAL_ADDON_PRE_REN", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "LEGAL_ADDON_PRE_REN", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("green","yellow") )

#Bar chart for Non-Resiliated (LEGAL_ADDON_PRE_REN)
status_client1 <- Non.Resiliated[!is.na(LEGAL_ADDON_PRE_REN), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = LEGAL_ADDON_PRE_REN]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$LEGAL_ADDON_PRE_REN
plot2<-ggbarplot(status_client1, x= "LEGAL_ADDON_PRE_REN", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "LEGAL_ADDON_PRE_REN", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("green","yellow"))
plot_grid(plot1,plot2)
#=========================================================================================================================================================================================================================================

#Bar chart for Resiliated (LEGAL_ADDON_POST_REN)
status_client <- Resiliated[!is.na(LEGAL_ADDON_POST_REN), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = LEGAL_ADDON_POST_REN]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$LEGAL_ADDON_POST_REN
plot1<-ggbarplot(status_client, x= "LEGAL_ADDON_POST_REN", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "LEGAL_ADDON_POST_REN", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("green","yellow") )
plot_grid(plot1,plot2)

#Bar chart for Non-Resiliated (LEGAL_ADDON_POST_REN)
status_client1 <- Non.Resiliated[!is.na(LEGAL_ADDON_POST_REN), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = LEGAL_ADDON_POST_REN]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$LEGAL_ADDON_POST_REN
plot2<-ggbarplot(status_client1, x= "LEGAL_ADDON_POST_REN", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "LEGAL_ADDON_POST_REN", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("green","yellow"))
plot_grid(plot1,plot2)

#====================================================================================
#Bar chart for Resiliated (LEGAL_ADDON_PRE_REN)
status_client <- Resiliated[!is.na(LEGAL_ADDON_PRE_REN), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = LEGAL_ADDON_PRE_REN]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$LEGAL_ADDON_PRE_REN
plot1<-ggbarplot(status_client, x= "LEGAL_ADDON_PRE_REN", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "LEGAL_ADDON_PRE_REN", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("green","yellow") )

#Bar chart for Non-Resiliated (LEGAL_ADDON_PRE_REN)
status_client1 <- Non.Resiliated[!is.na(LEGAL_ADDON_PRE_REN), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = LEGAL_ADDON_PRE_REN]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$LEGAL_ADDON_PRE_REN
plot2<-ggbarplot(status_client1, x= "LEGAL_ADDON_PRE_REN", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "LEGAL_ADDON_PRE_REN", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("green","yellow"))
plot_grid(plot1,plot2)
#============================================================================================================================================================================================================================================

#Bar chart for Resiliated (LEGAL_ADDON_POST_REN)
status_client <- Resiliated[!is.na(LEGAL_ADDON_POST_REN), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = LEGAL_ADDON_POST_REN]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$LEGAL_ADDON_POST_REN
plot1<-ggbarplot(status_client, x= "LEGAL_ADDON_POST_REN", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "LEGAL_ADDON_POST_REN", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("green","yellow") )

#Bar chart for Non-Resiliated (LEGAL_ADDON_POST_REN)
status_client1 <- Non.Resiliated[!is.na(LEGAL_ADDON_POST_REN), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = LEGAL_ADDON_POST_REN]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$LEGAL_ADDON_POST_REN
plot2<-ggbarplot(status_client1, x= "LEGAL_ADDON_POST_REN", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "LEGAL_ADDON_POST_REN", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("green","yellow"))

plot_grid(plot1,plot2)

#===============================================================================================================

#Bar chart for Resiliated (HOME_EM_ADDON_PRE_REN)
status_client <- Resiliated[!is.na(HOME_EM_ADDON_PRE_REN), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = HOME_EM_ADDON_PRE_REN]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$HOME_EM_ADDON_PRE_REN
plot1<-ggbarplot(status_client, x= "HOME_EM_ADDON_PRE_REN", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "HOME_EM_ADDON_PRE_REN", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("pink","purple") )

#Bar chart for Non-Resiliated (HOME_EM_ADDON_PRE_REN)
status_client1 <- Non.Resiliated[!is.na(HOME_EM_ADDON_PRE_REN), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = HOME_EM_ADDON_PRE_REN]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$HOME_EM_ADDON_PRE_REN
plot2<-ggbarplot(status_client1, x= "HOME_EM_ADDON_PRE_REN", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "HOME_EM_ADDON_PRE_REN", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("pink","purple"))

plot_grid(plot1,plot2)

#==================================================================================================================================================================================================
#Bar chart for Resiliated (HOME_EM_ADDON_POST_REN)
status_client <- Resiliated[!is.na(HOME_EM_ADDON_POST_REN), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = HOME_EM_ADDON_POST_REN]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$HOME_EM_ADDON_POST_REN
plot1<-ggbarplot(status_client, x= "HOME_EM_ADDON_POST_REN", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "HOME_EM_ADDON_POST_REN", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("blue","black") )

#Bar chart for Non-Resiliated (HOME_EM_ADDON_POST_REN)
status_client1 <- Non.Resiliated[!is.na(HOME_EM_ADDON_POST_REN), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = HOME_EM_ADDON_POST_REN]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$HOME_EM_ADDON_POST_REN
plot2<-ggbarplot(status_client1, x= "HOME_EM_ADDON_POST_REN", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "HOME_EM_ADDON_POST_REN", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("blue","black"))

plot_grid(plot1,plot2)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 
#Bar chart for Resiliated (GARDEN_ADDON_PRE_REN)
status_client <- Resiliated[!is.na(GARDEN_ADDON_PRE_REN), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = GARDEN_ADDON_PRE_REN]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$GARDEN_ADDON_PRE_REN
plot1<-ggbarplot(status_client, x= "GARDEN_ADDON_PRE_REN", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "GARDEN_ADDON_PRE_REN", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("black","pink") )
#Bar chart for Non-Resiliated (GARDEN_ADDON_PRE_REN)
status_client1 <- Non.Resiliated[!is.na(GARDEN_ADDON_PRE_REN), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = GARDEN_ADDON_PRE_REN]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$GARDEN_ADDON_PRE_REN
plot2<-ggbarplot(status_client1, x= "GARDEN_ADDON_PRE_REN", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "GARDEN_ADDON_PRE_REN", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("black","pink"))

plot_grid(plot1,plot2)

#==================================================================================================================================

#Bar chart for Resiliated (GARDEN_ADDON_POST_REN)
status_client <- Resiliated[!is.na(GARDEN_ADDON_POST_REN), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = GARDEN_ADDON_POST_REN]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$GARDEN_ADDON_POST_REN
plot1<-ggbarplot(status_client, x= "GARDEN_ADDON_POST_REN", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "GARDEN_ADDON_POST_REN", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("black","green") )

#Bar chart for Non-Resiliated (GARDEN_ADDON_POST_REN)
status_client1 <- Non.Resiliated[!is.na(GARDEN_ADDON_POST_REN), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = GARDEN_ADDON_POST_REN]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$GARDEN_ADDON_POST_REN
plot2<-ggbarplot(status_client1, x= "GARDEN_ADDON_POST_REN", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "GARDEN_ADDON_POST_REN", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("black","green"))

plot_grid(plot1,plot2)

#===============================================================================================================================================================================================================================================
#Bar chart for Resiliated (KEYCARE_ADDON_PRE_REN)
status_client <- Resiliated[!is.na(KEYCARE_ADDON_PRE_REN), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = KEYCARE_ADDON_PRE_REN]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$KEYCARE_ADDON_PRE_REN
plot1<-ggbarplot(status_client, x= "KEYCARE_ADDON_PRE_REN", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "KEYCARE_ADDON_PRE_REN", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette =  )

#Bar chart for Non-Resiliated (KEYCARE_ADDON_PRE_REN)
status_client1 <- Non.Resiliated[!is.na(KEYCARE_ADDON_PRE_REN), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = KEYCARE_ADDON_PRE_REN]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$KEYCARE_ADDON_PRE_REN
plot2<-ggbarplot(status_client1, x= "KEYCARE_ADDON_PRE_REN", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "KEYCARE_ADDON_PRE_REN", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = )

plot_grid(plot1,plot2)

#=================================================================================

#Bar chart for Resiliated (KEYCARE_ADDON_POST_REN)
status_client <- Resiliated[!is.na(KEYCARE_ADDON_POST_REN), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = KEYCARE_ADDON_POST_REN]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$KEYCARE_ADDON_POST_REN
plot1<-ggbarplot(status_client, x= "KEYCARE_ADDON_POST_REN", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "KEYCARE_ADDON_POST_REN", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("green","pink") )

#Bar chart for Non-Resiliated (KEYCARE_ADDON_POST_REN)
status_client1 <- Non.Resiliated[!is.na(KEYCARE_ADDON_POST_REN), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = KEYCARE_ADDON_POST_REN]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$KEYCARE_ADDON_POST_REN
plot2<-ggbarplot(status_client1, x= "KEYCARE_ADDON_POST_REN", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "KEYCARE_ADDON_POST_REN", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("green","pink"))

plot_grid(plot1,plot2)

#========================================================================================================

#Bar chart for Resiliated (HP1_ADDON_PRE_REN)
status_client <- Resiliated[!is.na(HP1_ADDON_PRE_REN), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = HP1_ADDON_PRE_REN]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$HP1_ADDON_PRE_REN
plot1<-ggbarplot(status_client, x= "HP1_ADDON_PRE_REN", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "HP1_ADDON_PRE_REN", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("green","red") )

#Bar chart for Non-Resiliated (HP1_ADDON_PRE_REN)
status_client1 <- Non.Resiliated[!is.na(HP1_ADDON_PRE_REN), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = HP1_ADDON_PRE_REN]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$HP1_ADDON_PRE_REN
plot2<-ggbarplot(status_client1, x= "HP1_ADDON_PRE_REN", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "HP1_ADDON_PRE_REN", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("green","red"))

plot_grid(plot1,plot2)

#===========================================================================================================

#Bar chart for Resiliated (HP1_ADDON_POST_REN)
status_client <- Resiliated[!is.na(HP1_ADDON_POST_REN), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = HP1_ADDON_POST_REN]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$HP1_ADDON_POST_REN
plot1<-ggbarplot(status_client, x= "HP1_ADDON_POST_REN", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "HP1_ADDON_POST_REN", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("pink","red") )

#Bar chart for Non-Resiliated (HP1_ADDON_POST_REN)
status_client1 <- Non.Resiliated[!is.na(HP1_ADDON_POST_REN), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = HP1_ADDON_POST_REN]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$HP1_ADDON_POST_REN
plot2<-ggbarplot(status_client1, x= "HP1_ADDON_POST_REN", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "HP1_ADDON_POST_REN", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("pink","red"))

plot_grid(plot1,plot2)

#=================================================================================================================================================================================
#Bar chart for Resiliated (HP2_ADDON_PRE_REN)
status_client <- Resiliated[!is.na(HP2_ADDON_PRE_REN), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = HP2_ADDON_PRE_REN]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$HP2_ADDON_PRE_REN
plot1<-ggbarplot(status_client, x= "HP2_ADDON_PRE_REN", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "HP2_ADDON_PRE_REN", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("pink","blue") )

#Bar chart for Non-Resiliated (HP2_ADDON_PRE_REN)
status_client1 <- Non.Resiliated[!is.na(HP2_ADDON_PRE_REN), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = HP2_ADDON_PRE_REN]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$HP2_ADDON_PRE_REN
plot2<-ggbarplot(status_client1, x= "HP2_ADDON_PRE_REN", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "HP2_ADDON_PRE_REN", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("pink","blue"))

plot_grid(plot1,plot2)

#=================================================================================================================

#Bar chart for Resiliated (HP2_ADDON_POST_REN)
status_client <- Resiliated[!is.na(HP2_ADDON_POST_REN), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = HP2_ADDON_POST_REN]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$HP2_ADDON_POST_REN
plot1<-ggbarplot(status_client, x= "HP2_ADDON_POST_REN", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "HP2_ADDON_POST_REN", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("pink","white") )

#Bar chart for Non-Resiliated (HP2_ADDON_POST_REN)
status_client1 <- Non.Resiliated[!is.na(HP2_ADDON_POST_REN), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = HP2_ADDON_POST_REN]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$HP2_ADDON_POST_REN
plot2<-ggbarplot(status_client1, x= "HP2_ADDON_POST_REN", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "HP2_ADDON_POST_REN", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("pink","white"))

plot_grid(plot1,plot2)

#==============================================================================================================

#Bar chart for Resiliated (HP3_ADDON_PRE_REN)
status_client <- Resiliated[!is.na(HP3_ADDON_PRE_REN), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = HP3_ADDON_PRE_REN]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$HP3_ADDON_PRE_REN
plot1<-ggbarplot(status_client, x= "HP3_ADDON_PRE_REN", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "HP3_ADDON_PRE_REN", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("pink","yellow") )

#Bar chart for Non-Resiliated (HP3_ADDON_PRE_REN)
status_client1 <- Non.Resiliated[!is.na(HP3_ADDON_PRE_REN), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = HP3_ADDON_PRE_REN]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$HP3_ADDON_PRE_REN
plot2<-ggbarplot(status_client1, x= "HP3_ADDON_PRE_REN", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "HP3_ADDON_PRE_REN", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("pink","yellow"))
plot_grid(plot1,plot2)

#====================================================================
#Bar chart for Resiliated (HP3_ADDON_POST_REN)
status_client <- Resiliated[!is.na(HP3_ADDON_POST_REN), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = HP3_ADDON_POST_REN]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$HP3_ADDON_POST_REN
plot1<-ggbarplot(status_client, x= "HP3_ADDON_POST_REN", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "HP3_ADDON_POST_REN", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette =  )

#Bar chart for Non-Resiliated (HP3_ADDON_POST_REN)
status_client1 <- Non.Resiliated[!is.na(HP3_ADDON_POST_REN), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = HP3_ADDON_POST_REN]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$HP3_ADDON_POST_REN
plot2<-ggbarplot(status_client1, x= "HP3_ADDON_POST_REN", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "HP3_ADDON_POST_REN", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = )

plot_grid(plot1,plot2)

#=============================================================================================================
#Bar chart for Resiliated (PAYMENT_FREQUENCY)
status_client <- Resiliated[!is.na(PAYMENT_FREQUENCY), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = PAYMENT_FREQUENCY]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$PAYMENT_FREQUENCY
plot1<-ggbarplot(status_client, x= "PAYMENT_FREQUENCY", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "PAYMENT_FREQUENCY", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette =  )

#Bar chart for Non-Resiliated (PAYMENT_FREQUENCY)
status_client1 <- Non.Resiliated[!is.na(PAYMENT_FREQUENCY), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = PAYMENT_FREQUENCY]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$PAYMENT_FREQUENCY
plot2<-ggbarplot(status_client1, x= "PAYMENT_FREQUENCY", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "PAYMENT_FREQUENCY", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = )

plot_grid(plot1,plot2)

#Bar chart for Resiliated (SUM_INSURED_BUILDINGS)
status_client <- Resiliated[!is.na(SUM_INSURED_BUILDINGS), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = SUM_INSURED_BUILDINGS]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$SUM_INSURED_BUILDINGS
plot1<-ggbarplot(status_client, x= "SUM_INSURED_BUILDINGS", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "SUM_INSURED_BUILDINGS", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette = c("black","gold") )

#Bar chart for Non-Resiliated (SUM_INSURED_BUILDINGS)
status_client1 <- Non.Resiliated[!is.na(SUM_INSURED_BUILDINGS), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = SUM_INSURED_BUILDINGS]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$SUM_INSURED_BUILDINGS
plot2<-ggbarplot(status_client1, x= "SUM_INSURED_BUILDINGS", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "SUM_INSURED_BUILDINGS", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = c("black","gold"))
plot_grid(plot1,plot2)

#=======================================================================================

#Bar chart for Resiliated (LISTED)
status_client <- Resiliated[!is.na(LISTED), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = LISTED]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$LISTED
plot1<-ggbarplot(status_client, x= "LISTED", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "LISTED", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette =  )

#Bar chart for Non-Resiliated (LISTED)
status_client1 <- Non.Resiliated[!is.na(LISTED), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = LISTED]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$LISTED
plot2<-ggbarplot(status_client1, x= "LISTED", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "LISTED", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = )

#=====================================================================

#Bar chart for Resiliated (PAYING_GUESTS)
status_client <- Resiliated[!is.na(PAYING_GUESTS), .(percent=round((.N/total.Resiliated)*100, 2)), 
                            by = PAYING_GUESTS]
status_client <- status_client[order(percent, decreasing = TRUE)]
ordered_status <- status_client$PAYING_GUESTS
plot1<-ggbarplot(status_client, x= "PAYING_GUESTS", y= "percent", xlab="Resiliated Commercial Indicator", ylab ="quantity policies", fill = "PAYING_GUESTS", label=TRUE, title = "Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status, palette =  )

#Bar chart for Non-Resiliated (PAYING_GUESTS)
status_client1 <- Non.Resiliated[!is.na(PAYING_GUESTS), .(percent=round((.N/total.Non.Resiliated)*100, 2)), 
                                 by = PAYING_GUESTS]

status_client1 <- status_client1[order(percent, decreasing = TRUE)]
ordered_status1 <- status_client1$PAYING_GUESTS
plot2<-ggbarplot(status_client1, x= "PAYING_GUESTS", y= "percent", xlab="Non Resiliated Commercial Indicator", ylab ="quantity policies", fill = "PAYING_GUESTS", label=TRUE, title = "Non Resiliated Commercial Indicator", 
                 label.pos = "out", order = ordered_status1, palette = )

plot_grid(plot1,plot2)

#==============================================================================================================================================================================================

str(data)
summary(df$SUM_INSURED_BUILDINGS)#factor
summary(df$SUM_INSURED_CONTENTS)#num
summary(df$LISTED)#Factor
summary(df$MAX_DAYS_UNOCC)#Num
summary(df$OWNERSHIP_TYPE)#Num
summary(df$PAYING_GUESTS)#factor
summary(df$PROP_TYPE)#Num
          
#BEDROOM,YEAR BUILD
Quant_feature <- data %>% select(Resiliated,
                                 RISK_RATED_AREA_B,
                                 RISK_RATED_AREA_C,
                                 SUM_INSURED_CONTENTS,
                                 SPEC_SUM_INSURED,
                                 SPEC_ITEM_PREM,
                                 UNSPEC_HRP_PREM,
                                 MAX_DAYS_UNOCC,
                                 OWNERSHIP_TYPE,
                                 MTA_FAP,
                                 MTA_APRP,
                                 LAST_ANN_PREM_GROSS,
                                 PROP_TYPE,
                                 BEDROOMS,
                                 YEARBUILT)

(Quant_feature %>% group_by(Resiliated) %>%
    summarize(RISK_B_avg = mean(RISK_RATED_AREA_B, na.rm = TRUE),
              RISK_C_avg = mean(RISK_RATED_AREA_C, na.rm = TRUE),
              SUM_INSURED_CONTENTS_avg = mean(SUM_INSURED_CONTENTS, na.rm = TRUE),
              INSURED_avg = mean(SPEC_SUM_INSURED, na.rm = TRUE),
              ITEM_PREM_avg = mean(SPEC_ITEM_PREM, na.rm = TRUE),
              HRP_PREM_avg = mean(UNSPEC_HRP_PREM, na.rm = TRUE),
              DAYS_avg = mean(MAX_DAYS_UNOCC, na.rm = TRUE),
              FAP_avg = mean(MTA_FAP, na.rm = TRUE),
              APRP_avg = mean(MTA_APRP, na.rm = TRUE),
              GROSS_avg = mean(LAST_ANN_PREM_GROSS, na.rm = TRUE),
              PROP_TYPE_avg = mean(PROP_TYPE, na.rm = TRUE),
              BEDROOMS_avg = mean(BEDROOMS, na.rm = TRUE),
              YEARBUILT_avg = mean(YEARBUILT, na.rm = TRUE),
    ))

(Quant_feature %>% group_by(Resiliated) %>%
    summarize(
      FAP_avg = mean(MTA_FAP, na.rm = TRUE),
      APRP_avg = mean(MTA_APRP, na.rm = TRUE),
      GROSS_avg = mean(LAST_ANN_PREM_GROSS, na.rm = TRUE),
      PROP_TYPE_avg = mean(PROP_TYPE, na.rm = TRUE),
      BEDROOMS_avg = mean(BEDROOMS, na.rm = TRUE),
      YEARBUILT_avg = mean(YEARBUILT, na.rm = TRUE),
    ))

(Quant_feature %>% group_by(Resiliated) %>%
    summarize(
      FAP_avg = median(MTA_FAP, na.rm = TRUE),
      APRP_avg = median(MTA_APRP, na.rm = TRUE),
      GROSS_avg = median(LAST_ANN_PREM_GROSS, na.rm = TRUE),
      PROP_TYPE_avg = median(PROP_TYPE, na.rm = TRUE),
      BEDROOMS_avg = median(BEDROOMS, na.rm = TRUE),
      YEARBUILT_avg = median(YEARBUILT, na.rm = TRUE),
    ))
(Quant_feature %>% group_by(Resiliated) %>%
    summarize(RISK_B_avg = median(RISK_RATED_AREA_B, na.rm = TRUE),
              RISK_C_avg = median(RISK_RATED_AREA_C, na.rm = TRUE),
              SUM_INSURED_CONTENTS_avg = median(SUM_INSURED_CONTENTS, na.rm = TRUE),
              INSURED_avg = median(SPEC_SUM_INSURED, na.rm = TRUE),
              ITEM_PREM_avg = median(SPEC_ITEM_PREM, na.rm = TRUE),
              HRP_PREM_avg = median(UNSPEC_HRP_PREM, na.rm = TRUE),
              DAYS_avg = median(MAX_DAYS_UNOCC, na.rm = TRUE),
              
    ))

##feature_selection
#
#CLAIM3YEARS,P1_EMP_STATUS,P1_PT_EMP_STATUS,AD_BUILDINGS,SUM_INSURED_BUILDINGS,SUM_INSURED_CONTENTS
#CONTENTS_COVER,SPEC_SUM_INSURED,SPEC_ITEM_PREM,P1_MAR_STATUS,P1_SEX,APPR_LOCKS,MAX_DAYS_UNOCC
#OCC_STATUS,SEC_DISC_REQ,PAYMENT_METHOD,LEGAL_ADDON_PRE_REN,LEGAL_ADDON_POST_REN
#HOME_EM_ADDON_PRE_REN,HOME_EM_ADDON_POST_REN,GARDEN_ADDON_PRE_REN,GARDEN_ADDON_POST_REN
#KEYCARE_ADDON_PRE_REN,KEYCARE_ADDON_POST_REN,HP2_ADDON_PRE_REN,HP2_ADDON_POST_REN
#MTA_FAP,MTA_APRP,LAST_ANN_PREM_GROSS

#Transform Data

(selected_data <- data%>%select(CLAIM3YEARS,P1_EMP_STATUS,P1_PT_EMP_STATUS,AD_BUILDINGS,
                                SUM_INSURED_BUILDINGS,SUM_INSURED_CONTENTS,
                                CONTENTS_COVER,SPEC_SUM_INSURED,SPEC_ITEM_PREM,
                                P1_MAR_STATUS,P1_SEX,APPR_LOCKS,MAX_DAYS_UNOCC,
                                OCC_STATUS,SEC_DISC_REQ,PAYMENT_METHOD,
                                LEGAL_ADDON_PRE_REN,LEGAL_ADDON_POST_REN,
                                HOME_EM_ADDON_PRE_REN,HOME_EM_ADDON_POST_REN,
                                GARDEN_ADDON_PRE_REN,GARDEN_ADDON_POST_REN,
                                KEYCARE_ADDON_PRE_REN,KEYCARE_ADDON_POST_REN,
                                HP2_ADDON_PRE_REN,HP2_ADDON_POST_REN,
                                MTA_FAP,MTA_APRP,LAST_ANN_PREM_GROSS,Resiliated
))
str(selected_data)

#transform
selected_data$CLAIM3YEARS  <- as.factor(selected_data$CLAIM3YEARS)
selected_data$P1_EMP_STATUS   <- as.factor(selected_data$P1_EMP_STATUS )
selected_data$P1_PT_EMP_STATUS   <- as.factor(selected_data$P1_PT_EMP_STATUS )
selected_data$AD_BUILDINGS   <- as.factor(selected_data$AD_BUILDINGS )

selected_data$CONTENTS_COVER   <- as.factor(selected_data$CONTENTS_COVER )
selected_data$P1_MAR_STATUS   <- as.factor(selected_data$P1_MAR_STATUS )
selected_data$P1_SEX   <- as.factor(selected_data$P1_SEX )
selected_data$APPR_LOCKS    <- as.factor(selected_data$APPR_LOCKS  )

selected_data$OCC_STATUS   <- as.factor(selected_data$OCC_STATUS )
selected_data$SEC_DISC_REQ    <- as.factor(selected_data$SEC_DISC_REQ  )
selected_data$PAYMENT_METHOD    <- as.factor(selected_data$PAYMENT_METHOD  )


selected_data$LEGAL_ADDON_PRE_REN     <- as.factor(selected_data$LEGAL_ADDON_PRE_REN   )
selected_data$LEGAL_ADDON_POST_REN   <- as.factor(selected_data$LEGAL_ADDON_POST_REN )
selected_data$HOME_EM_ADDON_PRE_REN    <- as.factor(selected_data$HOME_EM_ADDON_PRE_REN  )
selected_data$HOME_EM_ADDON_POST_REN    <- as.factor(selected_data$HOME_EM_ADDON_POST_REN )

selected_data$GARDEN_ADDON_PRE_REN     <- as.factor(selected_data$GARDEN_ADDON_PRE_REN   )
selected_data$GARDEN_ADDON_POST_REN   <- as.factor(selected_data$GARDEN_ADDON_POST_REN )
selected_data$KEYCARE_ADDON_PRE_REN  <- as.factor(selected_data$KEYCARE_ADDON_PRE_REN)
selected_data$KEYCARE_ADDON_POST_REN   <- as.factor(selected_data$KEYCARE_ADDON_POST_REN )

selected_data$HP2_ADDON_PRE_REN     <- as.factor(selected_data$HP2_ADDON_PRE_REN   )
selected_data$HP2_ADDON_POST_REN   <- as.factor(selected_data$HP2_ADDON_POST_REN )


str(selected_data)

#ONE HOT ENCODE
library(mltools)
library(data.table)
selected_dataOH<-selected_data[,1:29]

selected_dataOH<- one_hot(as.data.table(selected_dataOH))
selected_dataOH$Resiliated<-selected_data$Resiliated
str(selected_dataOH)

#Naive Bayes
## Split in train + test set
set.seed(2)
idxs <- sample(1:nrow(selected_data),as.integer(0.7*nrow(selected_data)))
selected_data$Resiliated <- as.factor(selected_data$Resiliated)
#ncol(selected_data)
trainData <- selected_data[idxs,]
testData <- selected_data[-idxs,]
trainData1 <- trainData[,29]
testData1 <- testData[,29]
train_labels <- trainData$Resiliated 
test_labels <- testData$Resiliated

model <- naiveBayes(Resiliated~., data = trainData)
test_pred = predict(model, newdata = testData1)

library(caret)
confusionMatrix(testData$Resiliated, test_pred)

#Decision Tree
## Split in train + test set
set.seed(2)
idxs <- sample(1:nrow(selected_data),as.integer(0.7*nrow(selected_data)))
selected_data$Resiliated <- as.factor(selected_data$Resiliated)
#ncol(selected_data)
trainData <- selected_data[idxs,]
testData <- selected_data[-idxs,]
trainData1 <- trainData[,29]
testData1 <- testData[,29]
train_labels <- trainData$Resiliated 
test_labels <- testData$Resiliated

model <- naiveBayes(Resiliated~., data = trainData)
test_pred = predict(model, newdata = testData1)

library(caret)
confusionMatrix(testData$Resiliated, test_pred)
(accuracy <- sum(diag(confMat))/sum(confMat))#0.68131
#plot

fancyRpartPlot(tree)
#==================================================================================
#RANDOM_FOREST
library(randomForest)
require(caTools)
sample = sample.split(selected_data$Resiliated, SplitRatio = .7)
train = subset(selected_data, sample == TRUE)
test  = subset(selected_data, sample == FALSE)
train$Resiliated<-as.factor(train$Resiliated)
summary(train$Resiliated)
summary(test$Resiliated)

rf <- randomForest(Resiliated ~ .,train)

pred = predict(rf, newdata=test[,1:29])

cm = table(test$Resiliated, pred)
confusionMatrix(cm)
(accuracy<-((cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])))

#=======================================================================================

#XGBOOST
library(xgboost)
library(readr)
library(stringr)
library(ggplot2)
library(caret)
library(car)
feature<-selected_data[,1:29]
label<-selected_data$Resiliated
label <- replace(label, label == 1, 0)
label <- replace(label, label == 2, 1)

trainXG<-train[,1:29]
testXG<-test[,1:29]
trainXGLabel<-train[,30]
testXGLabel<-test[,30]
testXGLabelDF<-data.frame(testXGLabel)
trainXG<-(data.matrix(trainXG, rownames.force = NA))
testXG<-(data.matrix(testXG, rownames.force = NA))
trainXGLabel<-(data.matrix(trainXGLabel, rownames.force = NA))

trainXGLabel <- replace(trainXGLabel, trainXGLabel == 1, 0)
trainXGLabel <- replace(trainXGLabel, trainXGLabel == 2, 1)
summary(trainXGLabel)
testXGLabel<-(data.matrix(testXGLabel, rownames.force = NA))
testXGLabel <- replace(testXGLabel, testXGLabel == 1, 0)
testXGLabel <- replace(testXGLabel, testXGLabel == 2, 1)

model <- xgboost(data = trainXG, label = trainXGLabel,eta = 0.1605,nrounds = 26,objective = "binary:logistic")

y_pred <- predict(model,testXG)
y_pred<-sapply(y_pred,round,digits=0)
y_predDF=data.frame(y_pred)
result = sum(y_predDF == testXGLabelDF)/nrow(y_predDF)

cm = table(testXGLabel,y_pred)
confusionMatrix(cm)
(accuracy<-((cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])))

#KNN #0.6633
idxs <- sample(1:nrow(selected_dataOH),as.integer(0.7*nrow(selected_dataOH)))
trainData <- selected_dataOH[idxs,]
testData <- selected_dataOH[-idxs,]
trainData1 <- trainData[,1:79]
testData1 <- testData[,1:79]
train_labels <- trainData$Resiliated 
test_labels <- testData$Resiliated
library(class)
knn.1 <- knn(train=trainData1, test=testData1, cl=train_labels, k=499)
(ACC.1 <- 100 * sum(test_labels == knn.1)/NROW(test_labels))
table(knn.1 ,test_labels)
library(caret)
(confusionMatrix(table(knn.1 ,test_labels)))