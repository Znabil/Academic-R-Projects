# Basic output and parameter interpretation for linear models
# loading data
hbat=read.csv("hbat.csv",header=TRUE,sep=",")
# if we have imported the id column as first column, delete it
head(hbat)
hbat=hbat[-1]

# One explanatory variable
simple_1=lm(X19~X20, hbat)
summary(simple_1)
cor(hbat$X19,simple_1$fitted)^2 # = to Multiple R-squared
cor(hbat$X19,hbat$X20)^2 # = to Multiple R-squared ONLY IN THE SIMPLE LINEAR REGRESION MODEL

# Standardizing both variables
data=cbind(hbat[,c(19,20)])
data=data.frame(scale(data, scale=TRUE))
standard=lm(data[,1]~data[,2], data) # the intercept is 0 and \hat{beta_1} is the correlation
# R-squared remains the same

# Introducing a quadratic term, although it is not needed
quad_1=lm(X19~X20+I(scale(X20, scale=F)^2), hbat)
summary(quad_1)

# Two explanatory variables  
bi_1=lm(X19~X20+X21, hbat)
summary(bi_1)
# recovering the partial correlation coefficient between YX21.X20
anova(bi_1)
# Total sum of squares
TSS=sum(anova(bi_1)[,2])
sqrt(anova(bi_1)[2,2]/(TSS-anova(bi_1)[1,2]))
pcor(hbat[,c(19,20,21)])$estimate[3,1]

# you can recover this partial coefficient from the summary table
sqrt(summary(bi_1)$coefficients[3,3]^2/(summary(bi_1)$coefficients[3,3]^2+100-3))

# Introducing categorical variables: One-way anova
hbat$X1_=factor(hbat$X1)
str(hbat$X1_)
anova_1=lm(X19~X1_, data=hbat)
summary(anova_1)

# Ancova
ggplot(hbat, aes(x=X19,y=X6, color=X1_))+geom_point()+geom_smooth(method="lm")
ancova_1=lm(X19~X1_+X6, data=hbat)
summary(ancova_1)
# Interactions
ancova_2=lm(X19~X1_+X6+X1_:X6, data=hbat)
summary(ancova_2)
ancova_2=lm(X19~X1_*X6, data=hbat)

# Multiple regression model  
basics_model1=lm(X19~X6+X9+X7+X11+X12, hbat)
summary(basics_model1)
basics_model2=lm(X19~X12+X6+X9+X7+X11, hbat)
summary(basics_model2)
anova(basics_model1)
anova(basics_model2)

##### Testing hypothesis about the parameters (model simplifications)
# Assume now that we want to test if parameters \beta_X6 (point estimation = 0.37) and 
# \beta_X9 ( point estimation =0.31)
hyp.test_1=lm(X19~X6+I(X6+X9)+X7+X11+X12, hbat) #restricted model

# Assume now we want to test whether parameter \beta_X6 could be equal to 1
hyp.test_2=lm(X19~X6+offset(1*X6)+X9+X7+X11+X12, hbat)

# or equal to 2
hyp.test_3=lm(X19~X6+offset(2*X6)+X9+X7+X11+X12, hbat)

# Partial F-tests on a nested sequence of models
model_1=lm(X19~X6+X9, hbat)
model_2=lm(X19~X6+X9+X7, hbat)
model_3=lm(X19~X6+X9+X7+X11+X12, hbat)
anova(model_1,model_2,model_3)

# But
model_1_2=lm(X19~X6+X9, hbat)
model_2_2=lm(X19~X6+X9+X7, hbat)
model_3_2=lm(X19~X6+X9+X7+X11, hbat)
anova(model_1_2,model_2_2, model_3_2)

# Interaction between continuous variables  