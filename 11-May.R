# Multiple Regression
# yn = b0 + b1X1 + b2X2 + b3X3....+E

#X is Predictor and independent here.
#Y is dependent variable here.

#1. Corelation between columns of dataset with cor function

#2. observe relation


# OLS (Ordinary Least) technique use to draw line

#Pvalue should be less than 0.05. Pvalue can be seen in summary of model.
# Confidence Interval = (1-Pvalue) * 100 - This will help us to derive the confidence on Model to use it or not.

A = read.csv(file.choose())

# Select numarical column to see corelation.
cor(A[,c(1,2,3,5)])

SF = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))

trd = A[SF==1,]
tsd = A[SF==2,]


model1 = lm(PROFIT ~ RND,data = trd)
model2 = lm(PROFIT ~ RND+MKT,data = trd)
model3 = lm(PROFIT ~ RND+MKT+ADMIN,data = trd)
model4 = lm(PROFIT ~ MKT,data = trd)
model5 = lm(PROFIT ~ RND+ADMIN,data = trd)

# Observe R-Square & R-Square Adjusted values of all 4 model to see which one has higest value
# Observe PValue and check the confidence interval with above mention formula


summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)

#Model1	Multiple R-squared:  0.9462,	Adjusted R-squared:  0.9447
#Model2	Multiple R-squared:  0.9483,	Adjusted R-squared:  0.9453
#Model3	Multiple R-squared:  0.9483,	Adjusted R-squared:  0.9436
#Model4	Multiple R-squared:  0.5714,	Adjusted R-squared:  0.5591 
#Model5	Multiple R-squared:  0.9464,	Adjusted R-squared:  0.9432

# Model 2 with RND and MKT can be use to predict profit

predict1 = predict(model1,tsd)
predict2 = predict(model2,tsd)
predict3 = predict(model3,tsd)
predict4 = predict(model4,tsd)

cbind(tsd,predict1,predict2,predict3,predict4)

summary(model2)

# Please refer below explanation which will be use to predict profit

#Summary(model2) will give below 
#Coefficients:
#             Estimate  
#(Intercept) 4.748e+04 
#RND         8.148e-01 
#MKT         2.235e-02 

# PROFIT Predicted value = Profit Coefficient + RND actual value * RND Coefficient + MKT actual value * MKT Coefficient

########## Adding Factor column into Model 2 ###########



modelx = lm(PROFIT ~ RND+MKT+STATE,data = trd)
modelx
summary(modelx)
