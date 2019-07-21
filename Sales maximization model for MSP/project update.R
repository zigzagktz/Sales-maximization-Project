install.packages("cluster")  # INSTALLING THE APPROPRIATE PACKAGES
install.packages("ggplot2")
install.packages("maptools")
install.packages("forecast")
library(ggplot2) #Lib needed for visual plot
library(cluster) #LAODING THE PACKAGES
library(maptools) #Lib to use mapping with Countries we have market
library(forecast)  #USE INSTALL.PACKAGE("FORECAST") IF YOU DONT HAVE THE LIBRARY


# READING THE FILE WITH NAME "sample_sales_data"
df <- read.csv(file.choose())
str(df) # CHECKING THE STUCTURE OF DATA


#_____________________________________________________________

#Histogram of Dealsize by color (keep)
ggplot(df, aes(x=df$SALES,fill=df$DEALSIZE)) + geom_histogram(binwidth =50) + labs(x="Sales")


df$COUNTRY = gsub("USA","United States", df$COUNTRY) #Substituting USA to United States
data(wrld_simpl)

myCountries = wrld_simpl@data$NAME %in% df$COUNTRY
plot(wrld_simpl, col = c(gray(.80), "green")[myCountries+1]) #Plot countries where we sell

#______________________________________________________________



#______________________________________________________________

#CONVERT DATA TO TIME SERIES
apptime<-ts(df,start = c(2003,1),end = c(2005,4), frequency = 4)
summary(apptime) #summary of time series


#creating a box plot of sales based on status 
boxplot(df$SALES~df$STATUS,col=c("gray","red","gray","gray","gray","gray"),main="SALES STATUS",ylab="SALES",xlab="SATATUS")

#SEASONAL PLOT basedo on each quarter
seasonplot(apptime[,5],year.labels = TRUE,year.labels.left = TRUE,col = 1:40,pch=19,ylab = "SALES",main = "SEASONAL PLOT: SALES")

#SALES BY QUATERS
boxplot(apptime[,5]~ cycle(apptime[,5]),col=c("gray","gray","red","gray"),main="QUATERLY SALES",ylab="SALES",xlab="QUATERS")



#AUTO ARIMA MODEL
app.forc<-auto.arima(apptime[,5]) #RELY ON THE MODEL TO AUTOMATICALLY CHOOSE THE COEFFICIENT
#FORECAST
forecast(object=app.forc,h=10, level = c(99.5))->sales_forecast
#plotting the forecast result
plot(sales_forecast,xlab="YEARS",ylab="SALES")

#checking if the model produce residual that are normal
plot.ts(app.forc$residuals,main="Time series of residual")
qqnorm(app.forc$residuals)
qqline(app.forc$residuals)
acf(app.forc$residuals)


#____________________________________________________________




# EXTRACTING ONLY USEFUL COLUMNS
df1 <- df[,c(2,3,4,5,6,7,8,9,10,11,12,21,25)]

# CONVERTING CATEGORICAL VARIABLES INTO FACTORS BECAUSE THEY WERE STORED AS INTIGERS
df1$ORDERLINENUMBER <- as.factor(df1$ORDERLINENUMBER)
df1$QTR_ID <- as.factor(df1$QTR_ID)
df1$MONTH_ID <- as.factor(df1$MONTH_ID)
df1$YEAR_ID <- as.factor(df1$YEAR_ID)
df1$PRODUCTLINE <- as.factor(df1$PRODUCTLINE)
df1$COUNTRY <- as.factor(df1$COUNTRY)
df1$DEALSIZE <- as.factor(df1$DEALSIZE)

# EXTRACTING ONLY VARIABLES THAT CAN HELP IN CLUSTERING 
fr <- df1[,c(1,6,7,13,4)]

# CALCULATING THE GOWER DISTANCE WHICH IS USEFUL FOR DISTANCE BETWEEN CONTINOUS AND CATEGORICAL VARIABLES
one <- daisy(fr,metric = "gower")
# CREATING A CLUSTER SUEING THE DISTANCE MATRIX
onecls <- hclust(one,method = "complete")
# PLOTTING THE CLUSTERS
plot(onecls)
# DRAWING A RED LINE ACROSS CLUSTERS TO MAKE THEM APPEAR CLEAR
rect.hclust(onecls,3)

# CUTTING THE THREE INTO THREE DIFFERENT CLUSTERS
cu <- cutree(onecls,3)

# CHECKING HOW MANU VALUES EACH CLUSTER HAS
table(cu)

# SUBSETTING VALUES OF MAIN DATASET BASED ON THE CLUSTERS VALUES
cluster1 <- df1[cu==1,]
cluster2 <- df1[cu==2,]
# CONVERTING THE SCREEN TO SHOW TWO GRAPS AT A TIME
dev.off()
par(mfrow=c(1,2))
# PLOTTING THE SALES IF CLUSTER ONE
plot(cluster1$SALES,ylim = c(0,max(cluster2$SALES)),col=cluster1$DEALSIZE,ylab= "sales",main = "Only small size deal")
abline(h=mean(cluster1$SALES)) # DRAWING A MEAN LINE OF SALES IN CLSUETR 2

# PLOTTINF THE SALES OF CLUSTER TWO
plot(cluster2$SALES,ylim = c(0,max(cluster2$SALES)),col=cluster2$DEALSIZE,ylab = "sales",main = "Only medium size deal")
abline(h=mean(cluster2$SALES)) # DRAWING THE MEAN LINE OF SALES IN CLUSTER 2

#looking at the distribution of largest deals
table(df1$PRODUCTLINE[df1$DEALSIZE=="Large"])
