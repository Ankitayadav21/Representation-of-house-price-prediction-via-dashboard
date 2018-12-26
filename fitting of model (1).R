data1<-read.table(file.choose(),header = T,sep = "\t")
data1

data3<-data1[-"Alley"]
data3<-data1[-73]
data4<-data2[-c(7,5,4,3,1)]
data4<-data3[-c(7,5,4,3,1)]



data5<-na.omit(data4)
nrow(data5)
data4<-data3[-,c(7,5,4,3,1)]
data4<-data3[,-c(7,5,4,3,1)]
sum(is.na(data4))
colSums(is.na(data4))
data5<-data4[,-c(2)]
data6<-data5[,-c(1,3,4,5,9,34,35,36,37,39,69)]
colSums(is.na(data6))
data6<-data5[,-c(1,3,4,5,9,38,35,36,37,39,69)]
colSums(is.na(data6))
View(data6)
data7<-data6[,-c(4,20)]
       
                data8<-na.omit(data7)
                colSums(is.na(data8)              
                sapply(data1,class)
                sapply(data8,class)
                mod3 <-lm(SalePrice ~ . , data = data8)
                summary(mod3)
                # R^2 - approximately 91% variation can be explained by our model.
                # H0: all the model coefficient are 0.(the slope for all independent variable are 0.)
                # RSE- how far observed saleprice are from predicted Y(hat)
                # estimate mean value of Y when all X are 0.(estimate saleprice when all independent variable is 0.)
                # effect of all variable adjusted for others.
                confint(mod3,conf.level=0.95)
                plot(mod3)
                # approximately linear.
                par(mfrow=c(2,2))
                plot(mod3)
                dataA<-data4[,c(2,26,70)]
                dataA
                dataA<-data4[,-c(2,26,70)]
                
              view(dataA)
                colSums(is.na(dataA))
                dataA[is.na(dataA)]<-mean(dataA[is.na(dataA)])
                
                colSums(dataA,class)
                sapply(dataA,class)
                data_f<-[ ,c(3,5,8,9,10,23,25,26,27,28,33,34,35,36,38,42,43,45,46,47,50,51,52,53,57,58,59,60,61,63,65,66,67,70,71)]
                data_f<-dataA[,c(3,5,8,9,10,23,25,26,27,28,33,34,35,36,38,42,43,45,46,47,50,51,52,53,57,58,59,60,61,63,65,66,67,70,71)]
                data.frame(data_f)            
                data_n<-dataA[,-c(3,5,8,9,10,23,25,26,27,28,33,34,35,36,38,42,43,45,46,47,50,51,52,53,57,58,59,60,61,63,65,66,67,70,71)]
                sapply(data_n,class)
                
                data_f<-dataA[,c(3,5,8,9,10,23,25,26,27,28,33,34,35,36,38,42,43,45,46,47,50,51,52,53,57,58,59,60,61,63,65,66,67,70,71)
        data_n<-dataA[,-c(3,5,8,9,10,23,25,26,27,28,33,34,35,36,38,42,43,45,46,47,50,51,52,53,57,58,59,60,61,63,65,66,67,70,71)]
        sapply(data_n,class)
        data_n[is.na(data_n)]<-mean(data_n[!is.na(data_n)])
        data_n
        for(i in 1:length(names(data_n))){
          
          if(is.factor(data_n[,i])){
            
            data_n[,i] <- as.integer(data_n[,i])
            
          }
          
        }
        
        colSums(sapply(data_n, is.na))
        data_f[is.na(data_f)]<-mode(data_f[!is.na(data_f)[,-c(1,2)]])
        data_f
        colSums(is.na(data_f))
        colSums(is.na(data_n))
        
        library(VIM)
        data_c<-kNN(data_f)
        summary(data_c)
        colSums(is.na(data_c))
        is.na(data_c)
        cor(data_c,data_n)
        hist(data_n$SalePrice,probability=T, main="Histogram of normaldata",xlab="Approximately normally distributed")
        lines(density(data_n$SalePrice),col=2)
        hist(data_n$SalePrice,probability=T, main="Histogram of saleprice",xlab="Approximately normally distributed data",breaks = c(25,10,15,20))
        qqnorm(data_n$SalePrice,main="QQ plot of normal data",pch=19)
        Saleprice_2 = log(data_n$SalePrice)
        install.packages("rcompanion")
        library(rcompanion)
        plotNormalHistogram(Saleprice_2)
        View(Saleprice_2)
        data.frame(Saleprice_2)
        data_N<-cbind(data_n,Saleprice_2)
        data.frame(data_N)
        head(data_N)
        dataNN<-data_N[,-37]
        plotNormalHistogram(Saleprice_2)
        data_N<-cbind(data_n,Saleprice_2)
        data.frame(data_N)
        head(data_N)
        savehistory("C:/Users/ankita/Desktop/r hist.Rhistory")
        head(dataNN)
        data_new<-cbind(data_f,dataNN)
data.frame(data_new)        
head(data_new)
# creating a new dataframe name data_new1 by removing unnessary column.
data_new1<-data_new[,c(14,15,16,22,23,24,39,40,43,44,45,53,65,67,68,74,78)]
dim(mod3)
data.frame(mod3)
mod3 <-lm(Saleprice ~ . , data = data8)
summary(mod3)
library("MASS")
view(data_f)
# data_NN for numerical data after normalizing and data_ for after converting the variable into a numeric.
data_final<-cbind(data_f,dataNN)
for(i in 1:length(names(data_f))){
  
  if(is.factor(data_f[,i])){
    
    data_f[,i] <- as.integer(data_f[,i])
    
  }
  data.frame(data_final)
sapply(data_final,class)  
is.numeric(data_final)
is.factor(data_final)
colSums(is.na(data_final))

# creating 2nd model
mod<-lm(Saleprice_2~ .,data_final)
summary(mod)
length(mod)
plot(mod)

data_final1<-data_final[,c(1,2,6,7,9,10,12,14,15,20,21,24,28,29,31,32,33,34,36,40,42,46,48,49,52,53,54,55,56,59,60,61,64,67,68,70,72)]
data.frame(data_final1)


# creating 3rd model by taking more precise value.
mod1<-lm(Saleprice_2~ .,data_final1)
summary(mod1)
plot(mod1)
confint(mod1,conf.level=0.95)
anova(mod1)

data_final2<-data_final1[,c(1,2,3,4,6,9,11,16,17,18,21,26,27,28,29,33,34,35)]



# creating 4th model.
mod2<-lm(Saleprice_2~ .,data_final2)
data.frame(data_final2)
summary(mod2)
plot(mod2)
confint(mod2,conf.level=0.95)
anova(mod2)

data_final3<-data_final2[,c(1,2,3,5,8,9,12,13,14,15,16,17)]


# creating 5th model
mod3<-lm(Saleprice_2~ .,data_final3)
summary(mod3)
plot(mod3)

data_final4<-data_final3[,c(1,2,3,4,5,6,7,8,9,11,12)]


# creating 6th model
mod5<-lm(Saleprice_2~ .,data_final4)
plot(mod5)
summary(mod5)
anova(mod5)
sqrt(0.037)
?mfrow
par(mfrow=c(2,2))



# model equation: 



#dividing the model into train and test set.




data.frame(train)
length(train$GarageType)
length(train$GarageFinish)

# creating a model for train
length(train$Fireplaces)
mod0<-lm(Saleprice_2~ ., train)
resid2 <- residuals(train,type="deviance")
model<-lm(Saleprice_2~ .,train)
fit1 <- lm(Saleprice_2~ ., data=train[,3:11],na.action=na.omit)
is.na(train)
sum(is.na(train))
install.packages("quantmod", dependencies=TRUE, repos='http://cran.rstudio.com/')
library(quantmod)
install.packages("dlnm", dependencies=TRUE, repos='http://cran.rstudio.com/')
library(mgcv) 
require(dlnm)
model1<-gam(Saleprice_2 ~ ., data=train, na.action=na.omit)
model2_1 <- update(mod,.~.+ lag(GarageType,1),  na.action=na.omit,train)
summary(model2_1)
install.packages("ISLR")
install.packages("gam")
library(gam)
length(train$GarageType)
length(train$GarageFinish)
length(train$Functional)
sum(is.na(train))
length(data_final4$GarageType)
length(data_final4$GarageFinish)
sum(is.na(data_final4))
sum(is.na(train$variablename))
sum(is.na(train$GarageType))
sapply(train,class)
data_final5<-cbind(data_final4,Saleprice_2)


# fitting lm() model for train.
mod4<-lm(Saleprice_2~ .,data_final5)
summary(mod4)
plot(mod4)
anova(mod4)
pred<-predict(mod4,test)
data.frame(pred)
View(Saleprice_2)


# extracting a predicted value.
attach(test)
test[Saleprice_2]
sapply(test,class)
attach(test)
actual<- pull(test,Saleprice_2)
data.frame(actual)
length(actual)
data.frame(pred)
length(pred)



# Function that returns Root Mean Squared Error










# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

mae(error)


# Function that returns Mean Bias Error
mae <- function(error)
{
  mean(abs(error))
}

mae(error)









# importing the real test file.
orig_test<-read.csv(file.choose(),header = T)
head(orig_test)

# removing the NA values.(removing variable having too large missing values)
colSums(is.na(orig_test))
orig_test1<-orig_test[,-c(1,5,6,7)]
colSums(is.na(orig_test1))
orig_test2<-orig_test1[,-c(72,69)]
colSums(is.na(orig_test2))


#removing NA by imputation method.
library(VIM)
orig_test3<-kNN(orig_test2)
orig_test4<-orig_test3[,1:74]
sum(is.na(orig_test4))


# creating 6th model
mod5<-lm(Saleprice_2~ .,data_final4)
plot(mod5)
summary(mod5)
anova(mod5)
sqrt(0.037)
?mfrow
par(mfrow=c(2,2))
pred<-predict(mod5,orig_test4)
data.frame(pred)
 
  if(is.factor(orig_test4[,i])){
    for(i in 1:length(names(orig_test4))){
 
    orig_test4[,i] <- as.integer(orig_test4[,i])
    
  }
  
  }
length(orig_test4)
view(orig_test4)
fdata<-exp(pred)
data.frame(fdata)
plot(fdata,test$OverallCond)
length(fdata)
length(data_final4$OverallCond)
dataF<-data_final4[-1460,]
length(dataF$OverallCond)
plot(dataF$OverallCond,fdata)
plot(dataF$GarageType,fdata)
plot(dataF$GarageFinish,fdata)
plot(dataF$Fireplaces,fdata)
plot(dataF$Functional,fdata)
plot(dataF$Electrical,fdata)
plot(dataF$LotShape,fdata)
plot(dataF$YearRemodAdd,fdata)
plot(dataF$MSZoning,fdata)
plot(dataF$X1stFlrSF,fdata)
plot(dataF$X2ndFlrSF,fdata)



# using glmnet (for train data)

install.packages("glmnet")
library(glmnet)
data<-data_final[,-72]
d<-data_final[,72]
data.frame(d)
CV<-cv.glmnet(x=DD,y=d,type.measure="class",alpha=1,nlambda=100)
CV
# where data_a is defined below.
data_a<-data.matrix(data_final)
sum(length(data_a[1,]))
plot(CV)
fit=glmnet(x=data_a,y=d,alpha=1,lambda=CV$lambda.1se)
fit$beta[,1]


# extracting significant column directed by glmnet.(for 72 variable)
data_b<-data_a[,-c(3,4,6,7,9,11,12,14,15,16,17,18,19,20,21,22,23,25,26,27,28,29,30,31,33,34,37,38,42,43,44,46,48,49,50,52,55,56,59,60,62,64,65,67,68,70,71)]
data_c<-data.frame(data_b)
length(data_b)
is.na(data_b)
sum(is.na(data_b))

View(data_c)











# using glmnet for train data(applying glmnet on all variable after removing NA values):
library(glmnet)
data_d<-data.matrix(data3)
CV1<-cv.glmnet(x=DD,y=d,type.measure="class",alpha=1,nlambda=100)
plot(CV1)
fit=glmnet(x=DD,y=d,alpha=1,lambda=CV1$lambda.1se)
fit$beta[,1]
sum(length(DD[1,]))

# extracting significant column directed by glmnet.(for 80 variable)
data_e<-DD[,-c(1,2,4,5,6,7,8,10,13,14,15,17,18,19,20,22,26,27,28,29,31,32,35,36,37,38,40,41,42,44,45,46,47,49,50,54,56,57,58,59,60,61,64,65,66,67,68,69,70,71,72,75,76,77,78)]
sum(length(data_e[1,]))
data_e<-data.frame(data_e)
View(data_e)
## removation of NA values.
library(VIM)
D<-kNN(data3)
N<-D[,1:80]
DD<-data.matrix(N)
head(data3)
dim(orig_test)
## model fitting using lm() model.
mod8<-lm(Saleprice_2~ .,data = data_g)
dim(data_g)
dim(data_g$GarageType)
data_g <- data_g[-1460,]
plot(mod8)
summary(mod8)
anova(mod8)
data_g<-data_f[,-24]
mod8
sqrt(0.008)
head(data)
dat<-data[,1:81]
dat
##taking test data and removing the variable which we omit from train test(suggested by glmnet).
length(orig_test6)
orig_test5<-orig_test[,-c(1,4,5,6,7,8,10,13,14,15,17,18,19,20,22,26,27,28,29,31,32,35,36,37,38,40,41,42,44,45,46,47,49,50,54,56,57,58,59,60,61,64,65,66,67,68,69,70,71,72,75,76,77,78)]
orig_test6<-orig_test5[,-1]
length(data_e)
sapply(orig_test7,class)

# predicting values using glmnet model.
pred<-predict(mod8,orig_test7)
data.frame(pred)
dim(orig_test7)
p<-data.frame(pred)
summary(pred)
plot(predict)
pred
exp(11.61629)
predict<-exp(pred)
predict
predicted<-data.frame(predict)
actual<-exp(Saleprice_2)
library(ggplot2)
ggplot(data = predicted,aes_string(x=predicted$predict))+geom_freqpoly(bins=100)+xlim(c(0,500000))
#fitting 7th model by using glmnet packages.
mod7<-lm(Saleprice_2~ .,data_c)
plot(mod7)
summary(mod7)
anova(mod7)
sqrt(0.023)
?mfrow
par(mfrow=c(2,2))
pred<-predict(mod7,orig_test7)
data.frame(pred)


for(i in 1:length(names(orig_test7))){
  
  if(is.factor(orig_test7[,i])){
    
    orig_test7[,i] <- as.integer(orig_test7[,i])
    
  }
  
}



data_e[0,]
orig_test6[0,]
orig_test7<-orig_test6[,-c(22,24)]
data_f<-data_e[,-23]
error<- act-z
rmse <- function(error)
{
  sqrt(mean(error^2))
}
rmse(error)
rmse<-1.101


# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

mae(error)
mae<-0.7990416

# Function that returns Mean Bias Error
mae <- function(error)
{
  mean(abs(error))
}

mae(error)


act<-data.frame(actual)
length(act)
P<-data.frame(predi)
predi<-na.omit(predict)
length(predi)
data.frame(predi)
z<-P[-1332,]

# visualisation of the above data.
library(corrplot)
correlations <- cor(data_e[,1:24])
correlations
corrplot(correlations, method="circle")
#creating correlation with dependent variable.
pairs(data, col=data$Neighborhood)
library(ggplot2)
ggplot(data=data_e,aes(x=dataA$SalePrice))+geom_histogram()
## to see greater variatian,i increase no. of bins.
ggplot(data=data_e,aes(x=data_e$SalePrice))+geom_histogram(fill="yellow",col="red")
ggplot(data=data_e,aes(x=sale,fill=LotShape))+geom_histogram()
head(data_e)
ggplot(data=data_e,aes(y=data_e$SalePrice,x=data_e$TotalBsmtSF,col=data_e$Fireplaces))+geom_point()+geom_smooth(method = "lm",se=F)
sum(is.na(data_e))

# making gg plot for fireplaces.
ggplot(data=data_e,aes(y=data_e$SalePrice,x=data_e$YearBuilt,col=data_e$Fireplaces))+geom_point()+geom_smooth(method = "lm",se=F)+facet_grid(~data_e$Fireplaces)
# there is greater no. of houses which has one or two fireplace.
# there are two houses which have 5 fireplaces.
# adding title there.
ggplot(data=data_e,aes(y=data_e$SalePrice,x=data_e$YearBuilt,col=data_e$Fireplaces))+geom_point()+geom_smooth(method = "lm",se=F)+facet_grid(~data_e$Fireplaces)->obj1

# adding title to the graph 
obj1+labs(title="Sale_price vs Year_built",x="data_e$YearBuilt")
obj1+labs(title="Sale_price vs Year_built",x="data_e$YearBuilt",fill="data_e$YearBuilt")->obj2

# to fill colour in rectangular boxes of panel.
obj2+theme(panel.background = element_rect(fill = "lemonchiffon"))
obj2+theme(panel.background = element_rect(fill = "lemonchiffon"))->obj3

# to fill color in background of plot.
obj3+theme(plot.background = element_rect(fill="skyblue"))->obj4
# now using theme to centre align the text and making the title bold.
## here we are using hjust to align the function in center.
obj4+theme(plot.title = element_text(hjust = 0.5,face = "bold",color = "blue"))
obj4+theme(plot.title = element_text(hjust = 0.5,face = "bold",color = "blue"))->obj5

# to convert prices from ruppee in dollar.
obj5+scale_y_continuous(labels = dollar)

# now we are trying other factor such as central air and going to make futher changes in this.
ggplot(data=data_e,aes(y=data_e$SalePrice,x=data_e$YearBuilt,col=as.factor(CentralAir)))+geom_point()+geom_smooth(method = "lm",se=F)+facet_grid(~as.factor(CentralAir))->obj6
attach(data_e)

##adding color to background and panel.
obj6+theme(plot.background = element_rect(fill = "skyblue"))
obj7+theme(panel.background = element_rect(fill = "yellow"))


# adding a new type of gg plot in which boxes are seperated by a particular variable.(in this case we have
# garage type).
data%>%
  ggplot(aes(YearBuilt,MSSubClass))+geom_point(alpha=0.25)+
  facet_wrap(~data$GarageType,scales = "free_y",ncol=3)


data_g
write.csv(data_g,file="exported data.csv")
data_g
