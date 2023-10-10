install.packages("ggplot2")
library(ggplot2)
install.packages("data.table")
library(data.table)
install.packages("dplyr")
library(dplyr)
install.packages("car")
library("car")
install.packages("moments")
library("moments")

#test GIt

install.packages("knitr")
library(knitr)
install.packages("arules")
library("arules")
suppressWarnings({
  # Code that generates warning messages
})



#1

setwd("D:\D\DataSience\R-Mehdinia\Project\Project")
c<-read.csv(file.choose())

#2
head(c) 
names(c)
str(c)
summary(c)
class(c)

#3
names(c)[16]<-"Action"
names(c)

#4
sum(is.na(c$Product_Date))
colMeans(is.na(c))*100
# up 50% so delete
c<-c[,-c(7)] 
names(c)



c[c==""]<-NA  
colMeans(is.na(c))*100
k<-which(is.na(c))
c<-na.omit(c)
summary(c)
#5
mean(c$TAT01)
mean(c$TAT02)






#6
m<-c("day","month","year")
y<-strsplit(c$Receipt_Date,"/",fixed=T)
class (y)

df <- data.frame(matrix(unlist(y), nrow=length(y), byrow=TRUE))

setnames(df, old = c('X1','X2','X3'), new = c('Day','Month','Year'))

total<-cbind(c,df)

str(total)

count(total,"month","year","Serial_No")

mx<-total %>% count(Year, Month,Serial_No)
mx$n>1
Return_Rate <- sum(mx$n>1)/sum(mx$n)*100
Return_Rate
#7

y<-table(c$Cost_Type)


barplot(y,horiz=FALSE,col = c("red","yellow","orange"),
        border = NA,main="Freq of Cost_Type")


#8
length(c)
names(c)

c2<- c[-c(1,5,7:10)]
names(c2)
summary(c2)
str(c2)
col.names<-c('Cost_Type','Product_Group','City','Defect_Des','Symptom_Desc'
             ,'Action','Labor_Charge_Desc','Engineer')


c2[col.names] <- do.call(cbind.data.frame, lapply(c2[col.names], as.factor))
c2[col.names] <- do.call(cbind.data.frame, lapply(c2[col.names], as.numeric))

names(c2)
summary(c2)
c2<- as.data.frame(scale(c2))
par(mar = c(1, 1, 1, 1))

par(mfrow = c(4, 4))  # 2 rows 3 columns


for (i in 1:15) {
  plot(c2[,i],c2$Total_Invoice_Amount, xlab = "", main = paste("plot of", names(c2)[i]))
}

cor_table <- round(cor(c2[,c(1:15)]),2)  
cor_table
    ## Service_type,action,Labor_Charge_Desc, all (amount) 
    #service_type, action is VIF
    #Labor_Charge_Desc, action is VIF
#9



summary(c2)
names(c2)

par(mar = c(1, 1, 1, 1))
par(mfrow = c(1, 1)) 

plot(c2$Cost_Type, c2$Total_Invoice_Amount)
c3<- as.data.frame(scale(c2))
c3
c2$Cost_Type
cor( c3$Cost_Type,c3$Total_Invoice_Amount, method = "pearson")
#-0.1438456 رابطه خطی  ندارند 
# از مقدار t value هم مشخص است

Regression <- lm(Total_Invoice_Amount ~ Cost_Type , data =c3)    # lm= تابع تحلیل رگرسیون

summary(Regression)$r.squared
summary(Regression)

#10

names(c)

ass <- c[,c(14,15)] 
summary(ass)
#'Service_type','Cost_Type','Symptom_Desc','City',
col.names<-c('Symptom_Desc'
             ,'Action')

col.names
ass[col.names] <- do.call(cbind.data.frame, lapply(ass[col.names], as.factor))



ass2<-ass[!duplicated(ass), ]

ass2
sum(duplicated(ass2))
str(ass2)
summary(ass2)


arules_model <- apriori(ass2, parameter = list(supp = 0.01, conf = 0.3))

arules_model

inspect(arules_model[1:4])
inspect(subset(arules_model, lift > 3))








#---------------------11

install.packages("cluster")
library("cluster") 
install.packages("e1071")
library("e1071") 

library("ggplot2")
install.packages("factoextra")
library("factoextra")                   



summary(c)
names(c)
names(c)[16]<-"Action"
names(c)
colMeans(is.na(c))*100
c<-c[,-c(7)] 
c[c==""]<-NA  
colMeans(is.na(c))*100
k<-which(is.na(c))
c<-na.omit(c)


df1 <- c[,-c(1,5,7,8,9,19,20)]

names(df1)
sum(duplicated(df1))
df1<-df1[!duplicated(df1), ]
col.names<-c('Service_type','Cost_Type','Product_Group','City','Defect_Des','Job_Satus','Symptom_Desc'
             ,'Action','Labor_Charge_Desc','Engineer')


df1[col.names] <- do.call(cbind.data.frame, lapply(df1[col.names], as.factor))
df1[col.names] <- do.call(cbind.data.frame, lapply(df1[col.names], as.numeric))
df1<- as.data.frame(scale(df1))
summary(df1)


#### Hirarical
d<-daisy(df1)           
as.matrix(d)[1:6, 1:6]
seg_hc<-hclust(d,method="complete")
par(mar = c(1, 1, 1, 1))
par(mfrow = c(1, 1)) 

plot(seg_hc)
plot(cut(as.dendrogram(seg_hc), h = 0.3)$lower[[8]])
cor(cophenetic(seg_hc), d) 
rect.hclust(seg_hc, k = 15, border="red") 



###Kmeans

set.seed(234)

wss <- function(k) {
  kmeans(df1, k, nstart = 10 )$tot.withinss
} 


k_values <- 1:20
wss_values <- data.frame(k = k_values)


for (i in k_values) {
  wss_values$wss[i] <- wss(wss_values$k[i]) 
}

plot(wss_values$k, wss_values$wss,
     type = "b", pch = 20, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")




seg_km <- kmeans(df1, centers = 7)
seg_km$cluster              
table(seg_km$cluster)        






summary(df1)

names(df1)
df1$segment <- seg_km$cluster   
head(df1)
fviz_cluster(seg_km, geom = "point", data = df1) + 
  ggtitle("Number of Clusters K = 9")

seg_km$cluster 


ggplot(data = df1, aes(Total_Invoice_Amount,Product_Group
                       , color = factor(segment))) +
  geom_point()

ggplot(data = df1, aes(Symptom_Desc,Product_Group
                       , color = factor(segment))) +
  geom_point()


tapply(df1$Symptom_Desc, df1$segment, mean)

