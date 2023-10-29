getwd()
setwd('Desktop/Buiness Analytics /BE 277/')
#packages:
install.packages("dplyr")
install.packages("ggplot2")


library(dplyr)

#Understand the Business Problem 
#Data importation 

learnLoss <- read_excel('learning_loss_r_upload.xlsx')
#View(learnLoss)

#Data inspection - From the data we want to check for missing data and any incorrect data types
#summary(learnLoss)

length(which(is.na(learnLoss)))
#We have two missing values in LogWeeks , on for australia and one for sweden, to aviod the Log Zeoro issure we can a small constant of 1 to the week values and 
#take the log, this doesn't affect the data much 
constant <- 1
learnLoss$logweeks2 <- log(learnLoss$weeks + constant)
View(learnLoss)

#Remove the incomplete logweeks 

learnLoss <- subset(learnLoss, select = -logweeks)
View(learnLoss)

#Check for complete data
length(which(is.na(learnLoss))) #Data Complete 

#check Summmary Statistics for Learn Loss table 
summary_stats <- summary(learnLoss)
summary_stats

#Data Exploration 

#Univariate Exploration (Single variable)

#Histogram of Loss 
#summary(learnLoss$loss)
interval_loss <- seq(0,0.8,0.1)
?hist
hist(learnLoss$loss,breaks = interval_loss,right = TRUE,ylim = c(0,14),main = 'Histogram of Learning Loss')
#the plot of loss is skewed to the right, with the futhermost loss at 0.8, 
#Investigating further with a box plot , if there are any outliers.

boxplot(learnLoss$loss, main = "Box Plot of Learning Loss", ylab = "Learning Loss")
#From the boxplot we can see that 2 outliers are present to see the countries
outliers_loss <- learnLoss[(learnLoss$loss >= 0.50),c(1:2)]
View(outliers_loss)
#We can see that the Countries that have Outlier Losses are : India, Nepal.with 0.52 & 0.8 respectively 
Nepal_loss = 0.8/0.33
Indian_Loss = 0.52/0.33
#Years of loss : according to Hanushek and Woessmann(2020) cited in Patrinos(2022), Nepal experienced 2.42 school years of learning loss while india experienced 1.58 school years of learning Loss
#Not removing the outliers from the data because they are not extremely high, plus to also preserve the data quantity

#Histogram 

# 2.What are the 5 Countries with the Highest Learning Loss and Ones with the Lowest Learning Loss

#Using Functions to avoid repetitive codes 

my_plots <- function(type,plot_data,high,marker, ylimit, ax_size, xlabel, ylabel, title) {
  
    if(type == 'Barchart'){
      bars <- barplot(high ,
                      names.arg = marker,
                      ylim = ylimit,
                      cex.names = ax_size,
                      xlab = xlabel,
                      ylab = ylabel,
                      main = title)
      text(x = bars, y = high, labels = high, pos = 3, cex = 0.8, col = "black")
      return(bars)
    }
}


# 2a)
#Top 5 Countries with the Highest Loss 

my_plots('Barchart',top5_loss,top5_loss$loss,top5_loss$country,c(0,1),0.65,'countries','Learning Loss','Top 5 Learn Loss')


#2b)
#Countries with Least Loss 

least5_loss <- learnLoss[order(learnLoss$loss,decreasing = FALSE)[1:5],c(1,2)]
least5_loss

my_plots('Barchart',least5_loss,least5_loss$loss, least5_loss$country,c(0,0.05),0.65,'countries','Learning Loss','Least 5 Learn Loss')


#3) The distribution of weeks school closed
summary(learnLoss$weeks)
#On average Schools closed for 20 weeks 
interval_weeks <- seq(0,96,12)
hist(learnLoss$weeks,breaks = interval_weeks,right = TRUE,xlim = c(0,100),main = 'Histogram for Weeks of School Closure')
box
boxplot(learnLoss$weeks,main="Plot of weeks school closed",ylab='Weeks closed')
outliers_weeks <- learnLoss[order(learnLoss$weeks,decreasing = TRUE)[1:2],c(1,3)]
View(outliers_weeks)

#Explanation: The histogram for weeks is Right-skewed with two outliers, These countries are India and Bangladesh with 93 and 63 weeks closed respectively


#Bivariate Exploration
#4)
#What is the relationship between Learning Loss and Weeks closed
plot(learnLoss$weeks,learnLoss$loss, pch=18,
     cex=0.9, 
     col="#69b3a2",xlab='Weeks closed',ylab='Learning Loss',main = 'Learning Loss vs Weeks Closed ')
abline(lm(learnLoss$loss~learnLoss$weeks), col="red")
cor(learnLoss$loss,learnLoss$weeks)

#Explanation: From the Plot of Learning Loss versus Weeks of school closure, It can be seen that there is a good positive linear relationship with Learning Loss 
#Explanation: From the Plot of Learning Loss versus Weeks of school closure, It can be seen that there is a good positive relationship with Learning Loss 

#4b) sINCE INTERNET IS AN INPORTANT FACTOR FOR HOME STUDY, LET US SEE IF LOSS NEGATIVELY CORRELATES WITH HIGH Internet
#What is the relationship between Learning Loss and Weeks closed
plot(learnLoss$internet,learnLoss$loss, pch=18,
     cex=0.9, 
     col="#69b3a2",xlab='Internet',ylab='Learning Loss',main = 'Learning Loss vs Internet Access ')
abline(lm(learnLoss$loss~learnLoss$internet), col="red")

#The Graph of Learning Loss vs internet access shows some inverse relationship. The more interent access in a country, the lesser the learning loss

#Hypothesis Tests for Learning Loss for High Income and Low Income demography
High_income <- learnLoss[(learnLoss$high == 1),2]
low_income <- learnLoss[(learnLoss$high == 0),2]
low_income

#Hypothesis Tests 
# Perform the t-test
loss_income_test<- t.test(High_income, low_income,alternative='less')
loss_income_test

#The P value for the test is 0.000325 , this means that the loss in low income countries is less than high income countries and did not happen by chance 



#Mean there is difference in average Learning 
#5) What is the distribution of loss for High income and Low Income countries
?boxplot
boxplot(learnLoss$loss~learnLoss$high,xlab="0: Low-income  1:High-income",ylab = 'Learning Loss',main='Loss by Low & High Income Country')

#Explanation: From the Box plot, It can be seen that the Learning Loss for Low income countries are more spread out than for High Income countries,
#in fact maximum loss experience by a high income country is less the median Loss for Low Income Country 



#5b) #Schooling and Internet in High Income countries 
#Adding Income Level to the dataframe 
learnLoss$income_level <- ifelse(learnLoss$high == 1,'High','Low')

library(ggplot2)

ggplot(learnLoss, aes(internet, schooling, colour = income_level)) + 
  geom_point()




#Examine the Region Variable
regionCat <- table(learnLoss$region_code)
regionCat
barplot(regionCat)
regionCatProp <- prop.table(regionCat)
regionCatProp
regionBar <- barplot(regionCat,main = 'Number of countries in regions',las=2,ylim = c(0 , 20),ylab = 'count of countries')
abline(h=0,col='black') #this adds a line to height for height 0 
text(0.75,regionCat[1]+1,regionCat[1])
text(1.8,regionCat[2]+1,regionCat[2])
text(3.0,regionCat[3]+1,regionCat[3])
text(4.2,regionCat[4]+1,regionCat[4])
text(5.4,regionCat[5]+1,regionCat[5])
text(6.6,regionCat[6]+1,regionCat[6])

#REGION VS iNCOME LEVEL
loss_table <- table(learnLoss$income_level,learnLoss$region_code)
loss_table
#oecd is the row , high income country is column 
?barplot

barplot(loss_table,col = c('green','grey'),ylab = 'frequency',cex.names = 0.35,las=2,main='Income Level By Region')
legend(x="topright",fill=c("green","grey"),legend=c('High_income','Low_Income'),cex=0.7,inset=0)

#Group By AVERAGE Learning LOSS FOR EACH REGION

df_loss <- group_by(learnLoss,region_code)
region_loss_mean <- summarise(df_loss,region_mean = mean(loss))
region_mean_table <- region_loss_mean[order(region_loss_mean$region_mean,decreasing = TRUE),]
View(region_mean_table)
#my_plots('Barchart',region_loss_mean,region_loss_mean$my_mean, region_loss_mean$region_code,c(0,0.6),0.35,'reg','Average Loss','Average Loss by Region')

#Facet Wrap - Learning Loss vs Weeks In Each Region 
ggplot(data = learnLoss) + 
  geom_point(mapping = aes(x = weeks, y = loss)) + 
  facet_wrap(~ region_code, nrow = 2)
#It can be seen that a MAJORITY OF COUNTRIES FOR aDVANCED ECONOMMICS , East Asia and Pacific, Europe and Central Asia has their Leaning lOSS BELOW 25 weeks, South Asia and Latain America had 
#had a large number of countries experiencing lEANING lOSS PAST 25 WEEKS.

#Predictive Analytics
#To begin with Predictive analysis, we check correlation 
#Since R-SQAURED is a function of the number of independent variable and sample size 
#a larger sample size will give a better model

#Using the Systematic Model Building Approach
#correlation exceeding +/- 0.7 may indicate Multicollinearity , 
#Using Trial and Error

#Variables to be used from literature review , Correlation with loss and Logic


learnLossPred <- learnLoss[,c(2,3,5,6,7,8,10,13,16,19,20,21,22,23,24)]
View(learnLossPred)
write.csv(learnLossPred,'shortlist_loss.csv')

#correlation : Correlation is important to determine association as it measure the extent of linearity between two variables
?cor
corr_matrix <- round(cor(learnLossPred),2)
View(corr_matrix)
#Variables to be used from literature review , Correlation and Logic
write.csv(corr_matrix,'Corr_matrix.csv')
install.packages("corrplot")
library(corrplot)

corrplot(corr_matrix, method = "circle")

#adding region_code to selected variables, for its a geographical location element 
learnLossPredR <- learnLoss[,c(2,3,5,6,7,8,10,13,16,18,19,20,21,22,23,24)]
View(learnLossPredR)
#Creating Dummy variables for region code , using Latin America & the Caribbean as reference

learnLossPredR$advanced_eco <- ifelse(learnLossPredR$region_code == 'Advanced Economies',1,0)
learnLossPredR$south_asia <- ifelse(learnLossPredR$region_code == 'South Asia',1,0)
learnLossPredR$east_asia_pac<- ifelse(learnLossPredR$region_code == 'East Asia and the Pacific',1,0)
learnLossPredR$europe_cent_asia <- ifelse(learnLossPredR$region_code == 'Europe and Central Asia',1,0)
learnLossPredR$sub_sah_africa <- ifelse(learnLossPredR$region_code == 'Sub-Saharan Africa',1,0)



#Removing region code because we now have dummy variables
learnLossPredR <- subset(learnLossPredR, select = -region_code)
View(learnLossPredR)

View(cor(learnLossPredR))
write.csv(learnLossPredR,'loss4Reg.csv')
corrplot(cor(learnLossPredR), method = "circle")

#Using the systematic model Building approach 
#Model 1
#Build Regression With all selected Independent varibles, to serve as Base model before tuning 
model1 <- lm(loss~weeks+gdp+private+internet+hlo+stringency+loggdp+schooling+hlo25+oecd+covid+high+covid25+logweeks2+advanced_eco+south_asia+east_asia_pac+europe_cent_asia+sub_sah_africa,data = learnLossPredR)
summary(model1)

#Model 1: Adjusted R-Square of 1 , this is rare, and the model may be overfitting or there exists high multicollinearity in the data

#Model99:
#Model 1 showed significant for hlo and internet, weeks is highest unsignificant

model99 <- lm(loss~gdp+private+internet+hlo+stringency+loggdp+schooling+hlo25+oecd+covid+high+covid25+logweeks2+advanced_eco+south_asia+east_asia_pac+europe_cent_asia+sub_sah_africa,data = learnLossPredR)
summary(model99)
#model99 r square is 1 

model98 <- lm(loss~gdp+private+internet+hlo+stringency+loggdp+schooling+hlo25+oecd+covid+high+logweeks2+advanced_eco+south_asia+east_asia_pac+europe_cent_asia+sub_sah_africa,data = learnLossPredR)
summary(model98)
#model98 r square is 1

model97 <- lm(loss~gdp+private+internet+hlo+stringency+loggdp+schooling+hlo25+covid+high+covid25+logweeks2+advanced_eco+south_asia+east_asia_pac+europe_cent_asia+sub_sah_africa,data = learnLossPredR)
summary(model97)
#model98 rsquare is 1

model96 <- lm(loss~gdp+private+internet+hlo+stringency+loggdp+schooling+hlo25+covid+high+logweeks2+advanced_eco+south_asia+east_asia_pac+europe_cent_asia+sub_sah_africa,data = learnLossPredR)
summary(model96)

model95 <- lm(loss~gdp+private+hlo+stringency+loggdp+schooling+hlo25+covid+high+logweeks2+advanced_eco+south_asia+east_asia_pac+europe_cent_asia+sub_sah_africa,data = learnLossPredR)
summary(model95)

model94 <- lm(loss~gdp+private+hlo+stringency+loggdp+schooling+hlo25+covid+high+logweeks2+advanced_eco+east_asia_pac+europe_cent_asia+sub_sah_africa,data = learnLossPredR)
summary(model94)

model93 <- lm(loss~gdp+private+hlo+stringency+schooling+hlo25+covid+high+logweeks2+advanced_eco+east_asia_pac+europe_cent_asia+sub_sah_africa,data = learnLossPredR)
summary(model93)

model92 <- lm(loss~gdp+private+hlo+stringency+hlo25+covid+high+logweeks2+advanced_eco+east_asia_pac+europe_cent_asia+sub_sah_africa,data = learnLossPredR)
summary(model92)

model91 <- lm(loss~gdp+private+hlo+stringency+covid+high+logweeks2+advanced_eco+east_asia_pac+europe_cent_asia+sub_sah_africa,data = learnLossPredR)
summary(model91)

model90 <- lm(loss~gdp+private+hlo+stringency+covid+logweeks2+advanced_eco+east_asia_pac+europe_cent_asia+sub_sah_africa,data = learnLossPredR)
summary(model90)

model89 <- lm(loss~gdp+private+hlo+stringency+covid+advanced_eco+east_asia_pac+europe_cent_asia+sub_sah_africa,data = learnLossPredR)
summary(model89)

model88 <- lm(loss~private+hlo+stringency+covid+advanced_eco+east_asia_pac+europe_cent_asia+sub_sah_africa,data = learnLossPredR)
summary(model88)

model87 <- lm(loss~private+stringency+covid+advanced_eco+europe_cent_asia+sub_sah_africa,data = learnLossPredR)
summary(model87)

model86 <- lm(loss~private+hlo+stringency+covid+advanced_eco+europe_cent_asia,data = learnLossPredR)
summary(model86)

model85 <- lm(loss~hlo+stringency+covid+advanced_eco+europe_cent_asia,data = learnLossPredR)
summary(model85)
#removing hlo
model84 <- lm(loss~ covid + stringency+advanced_eco,data = learnLossPredR)
summary(model84)

model83 <- lm(loss~+covid+advanced_eco,data = learnLossPredR)
summary(model83)

model82 <- lm(loss~covid25+schooling,data = learnLossPredR)
summary(model82)

#Model 3: 
#selecting variables with moderate correlation of greater than +/- 0.4 correlation with loss, Model 4, we will adjust based on Multicollinearity
#These are : weeks ,GDP,Internet,logGDP, schooling, Oecd,Covid,high,Covid25,logweeks2,advanced economics, South Asia 
model_3 <-  lm(loss~weeks+gdp+internet+loggdp+schooling+oecd+covid+high+covid25,logweeks2+advanced_eco+south_asia,data = learnLossPredR)
summary(model_3)

#model_3 r-square is 1 , there is perfect multicollinearity,with 5 Coefficients not defined because of singularities
#This means that one or more independent variables (predictors) are 
#linearly dependent on other independent variables in the model, causing the model to be over-fitting

#Model 4:
#We want to optimize for a good correlation with the dependent variable and minimize correlation with other independent variables, 
#Adjusting for Multicollinearity , the following variables have moderate and above correlation with Loss , and show Multicorrelation less than +/-0.8  
#logweeks #loggdp, #covid #high #southasia, #advanced_eco

#model_4 <-  lm(loss~weeks+gdp+internet+loggdp+schooling+oecd+covid+high+logweeks2+advanced_eco+south_asia,data = learnLossPredR)

install.packages("car")
library(car)
model_4 <- lm(loss ~ logweeks2 + loggdp + covid + high + south_asia + advanced_eco, data = learnLossPredR)
summary(model_4)
model_4_vif <- vif(model_4)
model_4_vif

model_4 <- lm(loss ~ logweeks2  + covid + high + south_asia + advanced_eco, data = learnLossPredR)
summary(model_4)
model_5_vif <- vif(model_5)
model_5_vif


#Replacing loggdp with high, since its higher in VIF than loggdp
model_6 <- lm(loss ~ logweeks2  + covid + south_asia, data = learnLossPredR)
summary(model_6)
model_6_vif <- vif(model_6)
model_6_vif

#substituting covid for covid25
model_7 <- lm(loss ~ logweeks2  + covid25 + loggdp + south_asia + advanced_eco, data = learnLossPredR)
summary(model_7)
model_7_vif <- vif(model_7)
model_7_vif

#Schooling is also a good predictor of learning loss, from literature review, as higher years of schooling for adults can be of help to students and motivate then to learn
#Although it correlates with Log GDP we can test it effect in the equation.

model_8 <- lm(loss ~ logweeks2  + covid25 + loggdp + south_asia + advanced_eco + schooling, data = learnLossPredR)
summary(model_8)
model_8_vif <- vif(model_8)
model_8_vif

#Model 8 didn't change much compared to model 7, let's add some interaction between schooling and advanced economy
learnLossPredR$adv_sch_intrt <- learnLossPredR$advanced_eco * learnLossPredR$schooling
View(learnLossPredR)
model_9 <- lm(loss ~ logweeks2  + covid25 + loggdp + south_asia + advanced_eco + schooling+adv_sch_intrt, data = learnLossPredR)
summary(model_9)
model_9_vif <- vif(model_9)
model_9_vif
#model_9 was lower in performance than model 8 

#removing schooling variable and using only interaction
View(learnLossPredR)
model_10 <- lm(loss ~ logweeks2  + covid25 + loggdp + south_asia + advanced_eco+adv_sch_intrt, data = learnLossPredR)
summary(model_10)
model_10_vif <- vif(model_10)
model_10_vif

#Model 10 is still subpar, trying another interaction with schooling and loggdp
learnLossPredR$sch_lgdp_intrt <- learnLossPredR$schooling * learnLossPredR$loggdp
model_11 <- lm(loss ~ logweeks2  + covid25 + loggdp + south_asia + advanced_eco+schooling+sch_lgdp_intrt, data = learnLossPredR)
summary(model_11)
model_11_vif <- vif(model_11)
model_11_vif
#model 11 is 0.45

#Model with interaction of schooling and loggdp minus schooling or gdp
model_12 <- lm(loss ~ logweeks2  + covid25 + south_asia + advanced_eco+sch_lgdp_intrt, data = learnLossPredR)
summary(model_12)
model_12_vif <- vif(model_12)
model_12_vif
#model_12 gives a shows an improvement with adjusted rsquare of 0.467 and all vif factors less than 5

#model 13 interaction with logweeks2 and south_asia
learnLossPredR$lweeks_asia_intrt <- learnLossPredR$logweeks2 * learnLossPredR$south_asia
model_13 <- lm(loss ~ logweeks2  + covid25 + loggdp + south_asia + advanced_eco+lweeks_asia_intrt+sch_lgdp_intrt, data = learnLossPredR)
summary(model_13)
model_13_vif <- vif(model_13)
model_13_vif
#the model gives an adjusted r-square of 0.5065,

#model 14 using hlo in place of covid because they are perfectly correlated
model_14 <- lm(loss ~ logweeks2 + hlo + loggdp + south_asia + advanced_eco+lweeks_asia_intrt+sch_lgdp_intrt, data = learnLossPredR)
summary(model_14)
model_14_vif <- vif(model_14)
model_14_vif
#Trying the principle of parsimony, taking out loggdp ,hlo, advanced economy
model_5 <- lm(loss ~ logweeks2 + south_asia + lweeks_asia_intrt + sch_lgdp_intrt, data = learnLossPredR)
summary(model_5)


#New resid
model_5_resid <- resid(model_5)
model_5_resid

plot(fitted(model_5),model_5_resid)
abline(0,0)

plot(density(model_5_resid))

#New Resid
model_4_resid <- resid(model_4)
model_4_resid

plot(fitted(model_4),model_4_resid)
abline(0,0)

plot(density(model_4_resid))

residreg3 <- resid(model_4)
residreg3

plot(fitted(model_4),residreg3)
abline(0,0)

plot(density(residreg3))

#MODEL 6 
model_6_resid <- resid(model_6)
model_6_resid

plot(fitted(model_6),model_6_resid)
abline(0,0)

plot(density(model_6_resid))

#Cross validating for model 5 and Model 6
TData <- learnLossPredR[1:29,]
VData <- learnLossPredR[30:41,]
View(TData)

#creating a new model with same vairables
model_five_train <- lm(loss ~ logweeks2 + south_asia + lweeks_asia_intrt + sch_lgdp_intrt, data = TData)
summary(model_five_train)

model_six_train <- lm(loss ~ logweeks2  + covid + south_asia, data = TData)
summary(model_six_train)

#predict
Pred1 <- predict(model_five_train, VData)
Pred1
sqrt(mean((VData$loss-Pred1)^2))

Pred2 <- predict(model_six_train, VData)
Pred2
sqrt(mean((VData$loss-Pred2)^2))


