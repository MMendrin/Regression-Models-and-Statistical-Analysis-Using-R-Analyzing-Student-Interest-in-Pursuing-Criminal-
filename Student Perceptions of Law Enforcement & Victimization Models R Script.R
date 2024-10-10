setwd("~/math 193c")
library(tidyverse)
library(nnet)
projectdata <- read_csv("CAS1_ Cleaned_January 30, 2023.csv")

#Modify Columns
projectdata <- projectdata %>% mutate(Acc3= 7-Acc3, Acc4= 7-Acc4) 
projectdata
view(projectdata)



# CHOOSE  this model
model1 <- lm(APLS ~ GBJW, data = projectdata) # apls and gbjw are the most correlated 
summary(model1)
# simple linear model with
# attitude police APLS as response (Y) and World Views GBJW as predictor (X)


# PLOTS 
projectdata %>% ggplot(aes(x = APLS)) + geom_histogram()

projectdata %>% ggplot(aes(y = APLS, x= GBJW)) + geom_point()+geom_smooth(method='lm')
plot(model1)

model2 <- glm(APLS ~ASI_HostileSexism, data = projectdata)
summary(model2)

model3 <- glm(APLS ~ GBJW + ASI_HostileSexism, data = projectdata)
summary(model3)

model4 <- glm(APLS ~ GBJW + ASI_HostileSexism + ASI_BenevolentSexism +
                ATM_HostileSexism + ATM_BenevolentSexism, data = projectdata)
summary(model4)


which(is.na(projectdata$APLS)) # sees which row there is a "na" filled in the data

# Does attitude toward police affect whether a person wants to purse criminal justice career?

which(projectdata$CJCareerInterest == 2) # finding which row has a 2 
# it is row 59, so remove row 59 in line below in model

model5 <- glm(CJCareerInterest ~ APLS, data = projectdata[-59,] , family= binomial) 
#logistic regression
summary(model5)

model6 <- glm(CJCareerInterest ~ GBJW, data = projectdata[-59,] , family= binomial) #logistic regression
summary(model6)

# choose model 7 same as model 13
model7 <- glm(CJCareerInterest ~ APLS + 
                ATM_BenevolentSexism, data = projectdata[-59,] , family= binomial)
#logistic regression
summary(model7)

model8 <- glm(CJCareerInterest ~  
                ATM_BenevolentSexism, data = projectdata[-59,] , family= binomial)
summary(model8)

model9 <- glm(CJCareerInterest ~  
                GBJW + APLS + ASI_HostileSexism + ASI_BenevolentSexism + ASI_HostileSexism +
                ATM_HostileSexism + ATM_BenevolentSexism, data = projectdata[-59,] , family= binomial)
summary(model9)

model10 <- glm(CJCareerInterest ~  
                 GBJW + APLS + ASI_HostileSexism +
                 ATM_HostileSexism + ATM_BenevolentSexism, data = projectdata[-59,] , family= binomial)
summary(model10)

model11 <- glm(CJCareerInterest ~  
                 GBJW + APLS +
                 ATM_HostileSexism + ATM_BenevolentSexism, data = projectdata[-59,] , family= binomial)
summary(model11)

model12 <- glm(CJCareerInterest ~  
                 APLS +
                 ATM_HostileSexism + ATM_BenevolentSexism, data = projectdata[-59,] , family= binomial)
summary(model12)

# lets choose this model same as model 7

model13 <- glm(CJCareerInterest ~  
                 APLS +
                 ATM_BenevolentSexism, data = projectdata[-59,] , family= binomial)
summary(model13)

# does cj career interest without the ppl who responded 2
projectdata %>% filter(CJCareerInterest != 2) %>% 
  ggplot(aes( y= APLS, x= factor(CJCareerInterest))) + geom_boxplot() +
  labs(title="Plot Model 7 APLS",x='CJ Interest')

projectdata %>% filter(CJCareerInterest != 2) %>% 
  ggplot(aes( y= ATM_BenevolentSexism, x= factor(CJCareerInterest))) + geom_boxplot() +
  labs(title="Plot Model 7 ATM_BenevSexism",x='CJ Interest')



table(projectdata$CJFutureCareer)
table(projectdata$CJFutureCareer_10_TEXT)

newdata <- data.frame(GBJW = c(5))
print(newdata1)
summary(model1)

# have to change the model type from glm to lm to do confidence interval
# so for model 1 we changed from glm to lm

predict(model1, newdata, interval = "confidence")
predict(model1, newdata, interval = "prediction")


########################################################
# multinomial model

multi_mo <- multinom(CJFutureCareer ~ APLS, data = projectdata )
summary(multi_mo)

# CHANCE STUDENT WANTS TO GO TO CAREER 2 FOR THEIR AVERAGE RESPONSE IN APLS

# CAREER 2
exp(1.58-.64*1)/(1+exp(1.58-.64*1))
# 72 percent chance they want to go to career 2 if they have a 1 response average in apls
exp(1.58-.64*6)/(1+exp(1.58-.64*6))
exp(1.58-.64*4)/(1+exp(1.58-.64*4))
a <- seq(1,6,length.out=100)
plot(a, exp((1.58-.64*a)/(1+exp(1.58-.64*a))), type ="l")


# CAREER 3
exp(-7.5+1.81*1)/(1+exp(-7.5+1.81*1))
# 0.34 percent chance they want to go to career 3 if they have a 1 response avg in apls
exp(-7.5+1.81*4)/(1+exp(-7.5+1.81*4))
# 43.5 percent chance they want to go to career 3 if they have a 4 response avg in apls
b <- seq(1,6,length.out=100)
plot(b, exp((-7.5+1.81*b)/(1+exp(-7.5+1.81*b))), type ="l")


# CAREER 4 
exp(-0.43+.21*2)/(1+exp(-0.43+.21*2))
# 50 percent chance they want to go to career 4 if they have a 2 response avg in apls
exp(-0.43+.21*5)/(1+exp(-0.43+.21*5))
c <- seq(1,6,length.out=100)
plot(c, exp(-0.43+.21*c)/(1+exp(-0.43+.21*c)), type ="l")

# CAREER 5
exp(-.12+.38*3)/(1+exp(-0.12+.38*3))
# 73 percent chance they want to go to career 5 if they have a 3 response avg in apls
exp(-.12+.38*5)/(1+exp(-0.12+.38*5))
d <- seq(1,6,length.out=100)
plot(d, exp(-.12+.38*d)/(1+exp(-0.12+.38*d)), type ="l")

# CAREER 6
exp(-3.41+.99*3)/(1+exp(-3.41+.99*3))
# 39 percent chance they want to go to career 6 if they have a 3 response avg in apls
e <- seq(1,6,length.out=100)
plot(e, exp(-3.41+.99*e)/(1+exp(-3.41+.99*e)), type ="l")

# CAREER 7
exp(.09+.31*4)/(1+exp(.09+.31*4))
# 79 percent chance they want to go to career 7 if they have a 4 response avg in apls
f <- seq(1,6,length.out=100)
plot(f, exp(.09+.31*f)/(1+exp(.09+.31*f)), type ="l")

# CAREER 9
exp(2.6-.37*4)/(1+exp(2.6-.37*4))
# 75 percent chance they want to go to career 9 if they have a 4 response avg in apls
g <- seq(1,6,length.out=100)
plot(g, exp(2.6-.37*g)/(1+exp(2.6-.37*g)), type ="l")

# CAREER 10
exp(-1.87+.39*4)/(1+exp(-1.87+.39*4))
# 42 percent chance they want to go to career 10 if they have a 4 response avg in apls
h <- seq(1,6,length.out=100)
plot(h, exp(-1.87+.39*h)/(1+exp(-1.87+.39*h)), type ="l")

# CAREER 11
exp(-.07+.33*4)/(1+exp(-.07+.33*4))
# 78 percent chance they want to go to career 11 if they have a 4 response avg in apls
i <- seq(1,6,length.out=100)
plot(i, exp(-.07+.33*i)/(1+exp(-.07+.33*i)), type ="l")


###########################################################################

library("tidyr") # use this package to remove NA rows

myvars <- c("APLS", "CJFutureCareer")
newdata <- projectdata[myvars]
newdata <- newdata %>% drop_na()   # remove all rows with NA
view(newdata)


########################################################
########################################################

# USING FACTANAL FUNCTION

# FACTOR ANALYSIS MODEL 1


apls_items <- projectdata %>% dplyr::select(APLS1:APLS34, HelpPolice_1:HelpCJS_3) %>%
  drop_na()

factanal(apls_items, 2)

# Just looking at Apls
apls_items2 <- projectdata %>% dplyr::select(APLS1:APLS34) %>% 
  drop_na()

# FINDING NUMBER OF FACTORS USING SCREE
# Determine Number of Factors to Extract
library(nFactors)
ev1 <- eigen(cor(apls_items)) # get eigenvalues
ap1 <- parallel(subject=nrow(apls_items),var=ncol(apls_items),
                rep=100,cent=.05)
nS1 <- nScree(x=ev1$values, aparallel=ap1$eigen$qevpea, )
plotnScree(nS1)

summary(nS1)

?nScree

N <- nScree(x=cor(apls_items), aparallel=ap1$eigen$qevpea, )
plotnScree(N)

library(ltm)
cronbach.alpha(apls_items)

####################################################

# FACTOR ANALYSIS MODEL 2

all_questions <- projectdata %>% dplyr::select(APLS1:APLS34, HelpPolice_1:HelpCJS_3, Acc1:Acc4, ASI_1:ASI_12, 
                                               AMI_1:AMI_12, GBJW_1:GBJW_7, Eff1:Eff4) %>% drop_na()
factanal(all_questions, 5)

# FINDING NUMBER OF FACTORS
# Determine Number of Factors to Extract
ev2 <- eigen(cor(all_questions)) # get eigenvalues
ap2 <- parallel(subject=nrow(all_questions),var=ncol(all_questions),
                rep=100,cent=.05)
nS2 <- nScree(x=ev2$values, aparallel=ap2$eigen$qevpea)
plotnScree(nS2)

library(ltm)
cronbach.alpha(all_questions) #Higher cronbach alpha value indicates research reliability
# cronbach alpha value is 0.962 (very reliable)


