#1.	Describe data of response variable and predictors in terms of key summary statistics like mean, mode, median, standard deviation, range, skewness and kurtosis. Show histogram and box plots also for each variables. [hint: describe command in R]
#Each variable to be explained in 30 words maximum.

# Read the dataset in object grades
grades<-read.csv(file.choose())
summary(grades)

library('psych')
describe(grades)
grades


describe(grades$gpa)
hist(grades$gpa, main = "Histogram of GPA", xlab = "GPA", ylab = "Frequency", col = "Blue")
boxplot(grades$gpa,  main = "Box plot of GPA", xlab = "GPA", col = "Blue", horizontal = T)
stem(grades$gpa)

summary(grades$quiz1)
describe(grades$quiz1)
hist(grades$quiz1, main = "Histogram of quiz1", xlab = "Quiz1 Marks", ylab = "Marks", col = "Blue")
boxplot(grades$quiz1,  main = "Box plot of Quiz1", xlab = "Quiz1", col = "Blue", horizontal = T)
stem(grades$quiz1)

summary(grades$quiz2)
describe(grades$quiz2)
hist(grades$quiz2, main = "Histogram of quiz2", xlab = "Quiz2 Marks", ylab = "Frequency", col = "Blue")
boxplot(grades$quiz2,  main = "Box plot of Quiz2", xlab = "Quiz2", col = "Blue", horizontal = T)
stem(grades$quiz2)

summary(grades$quiz3)
describe(grades$quiz3)
hist(grades$quiz3, main = "Histogram of quiz3", xlab = "Quiz3 Marks", ylab = "Frequency", col = "Blue")
boxplot(grades$quiz3,  main = "Box plot of Quiz3", xlab = "Quiz3", col = "Blue", horizontal = T)
stem(grades$quiz3)

summary(grades$quiz4)
describe(grades$quiz4)
hist(grades$quiz4, main = "Histogram of quiz4", xlab = "Quiz4 Marks", ylab = "Frequency", col = "Blue")
boxplot(grades$quiz4,  main = "Box plot of Quiz4", xlab = "Quiz4", col = "Blue", horizontal = T)
stem(grades$quiz4)

summary(grades$quiz5)
describe(grades$quiz5)
hist(grades$quiz5, main = "Histogram of quiz5", xlab = "Quiz5 Marks", ylab = "Frequency", col = "Blue")
boxplot(grades$quiz5,  main = "Box plot of Quiz5", xlab = "Quiz5", col = "Blue", horizontal = T)
stem(grades$quiz5)

summary(grades$final)
describe(grades$final)
hist(grades$final, main = "Histogram of final", xlab = "Final Marks", ylab = "Frequencys", col = "Blue")
boxplot(grades$final,  main = "Box plot of Final", xlab = "Fianl", col = "Blue", horizontal = T)
stem(grades$final)

summary(grades$total)
describe(grades$total)
hist(grades$total, main = "Histogram of Total Marks", xlab = "Total Marks", ylab = "Frequency", col = "Blue")
boxplot(grades$total,  main = "Box plot of Total Marks", xlab = "Total", col = "Blue", horizontal = T)
stem(grades$total)

#2.	How predictor/s is related to response variable (final)? [hint: first plat scatter diagram followed by correlation test]
#Present diagram/s and correlations in the following space. Before diagrams explain relationship in 3 or 4 lines.
plot(final~gpa, data = grades, main= "Scatter plot of GPA vs Final", col= 'blue', xlab= 'GPA', ylab="Final")
abline(lm(grades$final~grades$gpa), col="red")

plot(final~quiz1, data = grades, main= "Scatter plot of Quiz1 vs Final",col= 'blue', ylab= 'Fianal', xlab="Quiz1")
abline(lm(grades$final~grades$quiz1), col="red")

plot(final~quiz2, data = grades, main= "Scatter plot of Quiz2 vs Final",col= 'blue', ylab= 'Fianal', xlab="Quiz2")
abline(lm(grades$final~grades$quiz2), col="red")

plot(final~quiz3, data = grades, main= "Scatter plot of Quiz3 vs Final",col= 'blue', ylab= 'Fianal', xlab="Quiz3")
abline(lm(grades$final~grades$quiz3), col="red")

plot(final~quiz4, data = grades, main= "Scatter plot of Quiz4 vs Final",col= 'blue', ylab= 'Fianal', xlab="Quiz4")
abline(lm(grades$final~grades$quiz4), col="red")

plot(final~quiz5, data = grades, main= "Scatter plot of Quiz5 vs Final",col= 'blue', ylab= 'Fianal', xlab="Quiz5")
abline(lm(grades$final~grades$quiz5), col="red")

cor.test(grades$final, grades$gpa)

cor.test(grades$final, grades$quiz1)
cor.test(grades$final, grades$quiz2)
cor.test(grades$final, grades$quiz3)
cor.test(grades$final, grades$quiz4)
cor.test(grades$final, grades$quiz5)

# A regression model for collective set of all the predictors
model1<-lm(final ~ quiz1+quiz2+quiz3+quiz4+quiz5, data=grades)
summary(model1)

# check the variance factors
library('faraway')
vif(model1)
vif(model1)>5 


#Since Quiz1 reported to have a vif value >5, it is descarded from and a 
# new model excluding quiz1 is to be built for further investigations.
model2<-lm(final ~ quiz2+quiz3+quiz4+quiz5, data=grades)
summary(model2)


# check the variance factors
#install.packages('faraway')
library('faraway')
vif(model2)>5 

#From R output of summary(model2) it is evident that slope for 
# predictor variable - quiz2, quiz4, and quiz5 are not significant
# and hence can be descarded from the model.
#so, new model is model3 as follows
model3<-lm(final ~ quiz3, data=grades)
model3
summary(model3)

# check the variance factors
library('faraway')
vif(model3)>5 


#lets check anova
anova(model2)

#let check Durbin watson statistics valur for model4
# install.packages('car')
# library(car)
durbin.watson(model2)
dwt(model2)
dwtest(model2)


#Q9-Verification of Assumptions - Assumption of Normality
residual<-residuals(model3)
boxplot(residual,  main = "Assumption of Normality", col = 'red', horizontal = T)

# Histogram with overlay curve
hstError <- hist(residual)
multiplier<-hstError$counts / hstError$density
densError <- density(residual)
densError$y<-densError$y * multiplier[1]
plot(hstError, main = "Assumption of Normality", col = 'red')
lines(densError, col = "darkblue", lwd = 5)

library('psych')
describe(residual)


#Q10- Verification of Assumptions - Assumption of Independence of Error
library('psych')
residual<-residuals(model3)
obsNo<-seq(1:length(residual))
dsIOE<-data.frame(obsNo, residual)
plot(dsIOE$obsNo, dsIOE$residual, main= "Independence of Error",
     col= 'red', ylab= 'Residuals', xlab="Observation numbers")
abline(h = 0, v = 0, col = "gray60")

# Q11-Verification of Assumptions - Assumption of Linearity
library('psych')
residual<-residuals(model3)
plot(grades$quiz3, residual, 
     main= "Assumption of Linearity - plot of predictor vs Response",
     col= 'red', ylab= 'Response variable - final', xlab="Predictor - quiz3")
abline(h = 0, col = "red", lwd=4)

# Q12- Verification of Assumptions - Assumption of Constant Error Variance
library('psych')
residual<-residuals(model3)
dsPredict<-predict(model3)
plot(dsPredict, residual, main= "Assumption of Constant Error Variance - Predicted vs Error/Residual",
     col= 'red', ylab= 'Residuals', xlab="Observation numbers")

#Q13 - Standard error of estimate
plot(final~quiz3, data = grades, main= "Scatter plot of Quiz3 vs Final",
     col= 'blue', ylab= 'Fianal', xlab="Quiz3")
abline(lm(grades$final~grades$quiz3), col="red")
abline(h=mean(grades$final),col='green',lwd=4)

# elaboration of hypothetical values of predictors
plot(predict(model3),col="rosybrown4",lwd=2,
     main='Plot of Predicted values',
     xlab='quiz3 Index - Running numbers',
     ylab = 'Predicted Values - final')
abline(model3, col='red', lwd=4)

library(car) 
scatterplot(final ~ quiz3|passfail, data=grades, 
            xlab='quiz3 for pass/fail',
            ylab= 'final Scores',
            main='enhanced Scatter plot',
            labels=row.names(grades))

scatterplot(final ~ quiz3|gender, data=grades, 
            xlab='quiz3 for gender',
            ylab= 'final Scores',
            main='enhanced Scatter plot',
            labels=row.names(grades))

scatterplot(final ~ quiz3|section, data=grades, 
            xlab='quiz3 for sections',
            ylab= 'final Scores',
            main='enhanced Scatter plot',
            labels=row.names(grades))
                                     
scatterplot(final ~ quiz3|ethnicity, data=grades, 
            xlab='quiz3 for ethnicity',
            ylab= 'final Scores',
            main='enhanced Scatter plot',
            labels=row.names(grades))


#install.packages('scatterplot3d')
library(scatterplot3d)
scatterplot3d(quiz3,section,final, 
              main='3D scatterplot', data=grades)
