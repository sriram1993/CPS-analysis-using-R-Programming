#----------------------------------------------------------
# Reset R's brain
#----------------------------------------------------------
rm(list=ls())

#----------------------------------------------------------
# Reset graphic device
# As long as there is any dev open (exept "null device") 
# close the active one!
# Caution: closes all open plots!!!!
#----------------------------------------------------------
while(!is.null(dev.list()))
{
  dev.off()
}

install.packages("gam")
install.packages("ggplot2")
install.packages("GGally")
install.packages("ggpubr")
require(ISLR)
library(ISLR)
library (gam) 
library(GGally) # ggpairs plot
library("ggpubr")
attach(Wage)
assessment_dataframe <- Wage[sample(nrow(Wage), 3000), ]
my_data2 <- assessment_dataframe %>% select(year,age,education,wage)


#Research Question #1
# Is there a relationship between age/education/calendar year and wage? 
# Our first goal should be to determine whether the data provide evidence of an association between 
# age/education/calendar year and wage. 


# AGE VS WAGE
ggplot(my_data2, aes(x=age, y=wage)) + 
  geom_point(color = "blue") +  
  geom_smooth(method="lm", formula = " y~x", color="red")


ggplot(my_data2) + geom_bar(mapping = aes(x = age, y = wage), stat = "identity")

# YEAR VS WAGE
ggplot(my_data2, aes(x=year, y=wage)) + 
  geom_point(color = "blue") +  
  geom_smooth(method="lm", formula = " y~x", color="red")

ggplot(my_data2, aes(x=year, y=wage))  + geom_boxplot(color = "blue")

# Education vs Wage

indexes = sapply(my_data2, is.factor)
indexes["wage"] = TRUE
my_data2[,indexes]%>%
  gather(-wage, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = wage, color = value)) +
  geom_boxplot() +
  facet_wrap(~ var, scales = "free")+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.85),legend.position="none")


#Research Question #2
#How strong are the relationships?
#Assuming that there are relationships, we would like to know the strength of those

#Pair plot
#Convert education to numerical variable
numData <- assessment_dataframe %>% select(year,age,wage,education)
numData$education <- as.numeric(numData$education)
ggpairs(data=numData)
#Correlation table
correlation_table(my_data2,target = "wage")

lm.fit2=lm(wage ~ (age+education+year),data=my_data2)
anova(lm.fit2)

cor.test(numData$education,numData$wage,method = "pearson")
cor.test(numData$age,numData$wage,method = "pearson")
cor.test(numData$year,numData$wage,method = "pearson")


ggscatter(numData, x = "year", y = "wage", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")


#Research question 3
#Given a certain age/education/calendar year, can we predict wage with a high level of accuracy? 
#This would be a strong relationship. 
#Or is a prediction of wage based on age/education/calendar year only slightly better than a random guess? 
#This would be a weak relationship

#Annova
fit.1= lm(wage~age + s(year,3) +education ,data=my_data2)
fit.2= lm(wage~poly(age,2) + s(year,3) + education,data=my_data2)
fit.3= lm(wage~poly(age,3) + s(year,3)  + education ,data=my_data2)
fit.4= lm(wage~poly(age,4) + s(year,3) + education  ,data=my_data2)
fit.5= lm(wage~poly(age,5) + s(year,3) + education ,data=my_data2)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)


# Training and Test data
set.seed(8)
trainset=sample(1:nrow(my_data2),0.6*nrow(my_data2))
wageDf.testset=my_data2[-trainset,]
wageVar.testset=my_data2$wage[-trainset]

install.packages("caret")
library(caret) # Showing Confusion Matrix Data
set.seed(1)
foldss=createFolds(my_data2$wage[trainset], k=10)
polyDf = 8
cvErrors = matrix(nrow=10,ncol=polyDf)

for(polyDegFree in 1:polyDf){
  # Loop over folds of cv
  for(k in 1:10){
    fit=gam(wage~poly(age, polyDegFree)+ s(year,3) + education , data = my_data2, subset = -foldss[[k]])
    preds=predict(fit,newdata=my_data2[foldss[[k]],])
    cvErrors[k,polyDegFree]=mean((preds-my_data2[foldss[[k]],c("wage")])^2)
  }   
}


meanCvErrorsPoly = apply(cvErrors,2,mean)
plot(meanCvErrorsPoly, type = 'b', xlab = "Polynomial Degree", ylab = "Squared Error")
bestPoly = which.min(meanCvErrorsPoly)


set.seed(4)

# Loop over degrees of polynomial 
degFreeToTry = 20
cvErrorsSmSp = matrix(nrow=10,ncol=degFreeToTry)
cvErrorsNaSp = matrix(nrow=10,ncol=degFreeToTry)
cvErrorsCut  = matrix(nrow=10,ncol=degFreeToTry+1)

for(degFree in 1:degFreeToTry){
  
  my_data2$tmp =  cut(my_data2$age,degFree+1)
  # Loop over folds of cv
  for(k in 1:10){
    ### Cut
    fit=gam(wage~ tmp + s(year,3) + education , data = my_data2, subset = -foldss[[k]])
    preds=predict(fit,newdata=my_data2[foldss[[k]],])
    cvErrorsCut[k,degFree+1]=mean((preds-my_data2[foldss[[k]],c("wage")])^2)
    ### Smoothing spline
    fit=gam(wage~ s(age, degFree)+ s(year,3)  + education , data = my_data2, subset = -foldss[[k]])
    preds=predict(fit,newdata=my_data2[foldss[[k]],])
    cvErrorsSmSp[k,degFree]=mean((preds-my_data2[foldss[[k]],c("wage")])^2)
    ### natural spline
    fit=gam(wage~ ns(age, degFree)+ s(year,3)  + education , data = my_data2, subset = -foldss[[k]])
    preds=predict(fit,newdata=my_data2[foldss[[k]],])
    cvErrorsNaSp[k,degFree]=mean((preds-my_data2[foldss[[k]],c("wage")])^2)
  }   
}


### Local Regression
spanNums = seq(0.1, 0.7, 0.06)
cvErrorsLocReg  = matrix(nrow=10,ncol=length(spanNums))
currIdx = 0
for(spanNum in spanNums){
  # Loop over folds of cv
  currIdx = currIdx + 1
  for(k in 1:10){
    #  local regression
    fit=gam(wage~ lo(age, span = spanNum)+ s(year,3) + education , data = my_data2, subset = -foldss[[k]])
    preds=predict(fit,newdata=my_data2[foldss[[k]],])
    cvErrorsLocReg[k,currIdx]=mean((preds-my_data2[foldss[[k]],c("wage")])^2)
  }   
}
# Find which degree has lowest average MSE over all k folds:
par(mfrow= c(2,2))
meanCvErrorsSmSp = apply(cvErrorsSmSp,2,mean)
meanCvErrorsNaSp = apply(cvErrorsNaSp,2,mean)
meanCvErrorsCut = apply(cvErrorsCut,2,mean)
meanCvErrorsLocReg = apply(cvErrorsLocReg,2,mean)
plot(meanCvErrorsSmSp, type = 'b', xlab = "Smooth Spline Degrees", ylab = "Squared Error")
plot(meanCvErrorsNaSp, type = 'b', xlab = "Natural Spline Degrees", ylab = "Squared Error")
plot(meanCvErrorsCut, type = 'b', xlab = "Number of Cuts", ylab = "Squared Error")
plot(spanNums, meanCvErrorsLocReg, type = 'b', xlab = "Span (Local regression)", ylab = "Squared Error")

bestSmSpDegFr = which.min(meanCvErrorsSmSp)
bestNaSpDegFr = which.min(meanCvErrorsNaSp)
bestCut = which.min(meanCvErrorsCut)
bestLocRegSpan = spanNums[which.min(meanCvErrorsLocReg)]

### Cut
my_data2$tmp =  cut(my_data2$age,bestCut)
fit=gam(wage~ tmp + s(year,3)  + education , data = my_data2, subset = train)
preds=predict(fit,newdata=my_data2[-train,])
cutMSE = mean((preds-wageVar.testset)^2)
cutTrainingMse= sum(fit$residuals^2)/fit$df.residual

### Smoothing spline
fit=gam(wage~ s(age, bestSmSpDegFr)+ s(year,3) + education , data = my_data2, subset = train)
preds=predict(fit,newdata=wageDf.testset)
smthSplMSE = mean((preds-wageVar.testset)^2)
smthSplTrainingMse= sum(fit$residuals^2)/fit$df.residual

### natural spline
fit=gam(wage~ ns(age, bestNaSpDegFr)+ s(year,3) + education , data = my_data2, subset = train)
preds=predict(fit,newdata=wageDf.testset)
naturSplMSE = mean((preds-wageVar.testset)^2)
natSplTrainingMse= sum(fit$residuals^2)/fit$df.residual

### Local Regression
fit=gam(wage~ lo(age, span = bestLocRegSpan)+ s(year,3) + education , data = my_data2, subset = train)
preds=predict(fit,newdata=wageDf.testset)
locRegMSE = mean((preds-wageVar.testset)^2)
locRegTrainingMse= sum(fit$residuals^2)/fit$df.residual

### Poly
fit=gam(wage~poly(age, bestPoly)+ s(year,3) + education , data = my_data, subset = -foldss[[k]])
preds=predict(fit,newdata=wageDf.testset)
polyMSE = mean((preds-wageVar.testset)^2)
polyTrainingMse= sum(fit$residuals^2)/fit$df.residual

## Display Results in Table Format
rowLabels = c("CV mean MSE (for Tuning the Parameters)",
              "Training Data MSE","Test Data MSE",
              "Tuning Value (DoF, Cuts, Span, Deg)")

# Agrregate desored data
var1 = c(meanCvErrorsSmSp[bestSmSpDegFr],smthSplTrainingMse, smthSplMSE, bestSmSpDegFr)
var2 = c(meanCvErrorsNaSp[bestNaSpDegFr],natSplTrainingMse, naturSplMSE, bestNaSpDegFr)
var3 = c(meanCvErrorsCut[bestCut],cutTrainingMse, cutMSE, bestCut)
var4 = c(meanCvErrorsLocReg[which.min(meanCvErrorsLocReg)],locRegTrainingMse, locRegMSE, bestLocRegSpan)
var5 = c(meanCvErrorsPoly[bestPoly],polyTrainingMse,polyMSE, bestPoly)

# Create dataframe and display it in table format
library(knitr)
df = data.frame(var1, var2, var3,var4,var5, row.names = rowLabels)
colnames(df) = c("Smooth Spline", "Natural Spline", "Cut", "Local Regression", "Polynomial")
kable(df, format = 'markdown')

par(mfrow = c(2,3))
### Smoothing spline
fit=gam(wage~ poly(age, 2)+ s(year,3) + education , data = my_data2, subset = trainset)
plot(fit, se=TRUE ,col ="yellow")

#Research question 4
#Which factors contribute to wage? Do all three factors — age, education and calendar year — contribute to wage,
#or do just one or two of the factors contribute? 

#Research question 5
#Is the relationship linear? If there is approximately a straight-line relationship between wage 
#and age/education/calendar year, then linear regression is an appropriate tool.
#How well does the linear model fit the data? If the relationship is not linear, what could be considered? 
ggplot(numData, aes(x=year, y=wage)) + 
  geom_point(color = "blue") +  
  geom_smooth(method="lm", formula = " y~x", color="red")

ggplot(numData, aes(x=age, y=wage)) + 
  geom_point(color = "blue") +  
  geom_smooth(method="lm", formula = "y~poly(x,2)", color="red")

ggplot(my_data2, aes(x=education, y=wage)) + 
  geom_boxplot(color = "blue") +  
  geom_smooth(method="lm", formula = "y~x", color="red")

#Reseach question 6
#Are there interaction effects?
summary(lm(wage ~ (age + education + year),data = my_data2))
summary(lm(wage ~ (age*education*year),data = my_data2))


ggplot(my_data2, aes(x=age, y=wage)) + 
  geom_point() + 
  geom_smooth(method="lm")

ggplot(my_data2, aes(x=age, y=wage)) + 
  geom_point(color = "blue") +  
  geom_smooth(method="lm", formula = "y ~ x + I(x^2)", color="red")


ggplot(my_data2, aes(x=age, y=wage)) + 
  geom_point(color = "blue") +  
  geom_smooth(method="loess", formula = " y~poly(x,2)", color="red")