library(tidyverse)
library(ResourceSelection)

df <- read.csv(file = "C:\\Users\\seanc\\Documents\\heart.csv")
df$sex <- ifelse(df$sex == 1, "Male", "Female")
df$sex <- factor(df$sex)
df$cp <- factor(df$cp)
df$fbs <- factor(df$fbs)
df$exang <- factor(df$exang)
df$target <- ifelse(df$target == 1, "HD", "No HD") 
df$target <- factor(df$target)
df$restecg <- factor(df$restecg)
df$slope <- factor(df$slope)

colnames(df)[1] <- "age"

qplot(sex, data = df, fill = target) + labs(title = "Heart Disease by Sex Histogram", xlab = "Sex", ylab = "Frequency", fill = "Heart Disease")
qplot(age, chol, data = df, color = sex, shape = target)

tab <- table(df$sex, df$target)
colnames(tab) <- c("No HD", "HD")
rownames(tab) <- c("Female", "Male")
tab

chisq.test(tab, correct = F)$expected
#expected counts >5, we are good Since sample size is large, no yates continuity correction
chisq.test(tab, correct = F)
#There is convincing evidence that heart disease diagnosis and sex are not independent.

model <- glm(target~., data = df, family = "binomial")
summary(model)
model1 <- glm(target~age+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal, data = df, family = "binomial")
summary(model1)

hoslem.test(df$target, fitted(model))

LRT <- model1$deviance-model$deviance
LRT

df.LRT <- model1$df.residual-model$df.residual
df.LRT

1-pchisq(LRT,df.LRT)
#Convincing evidence of an association between sex and heart disease diagnosis after accounting for
#other variables (drop-in-deviance test p-value <0.001)

unname(exp(model$coefficients[3]*(1-0)))
#The estimated odds of a man getting diagnosed with heart disease is 6.23 times the odds of a woman
#holding all other variables constant.
exp(-1*confint(model)[6,c(2,1)])
#2.66, 34.09
#The odds of heart disease among men is estimated to be 6.23 times as large as the odds of 
#heart disease among women, after accounting for the variables (95% CI: 2.66, 34.09)

model2 <- glm(ca~age, data = df, family = "poisson")
summary(model2)
exp(model2$coefficients[2])
#An increase in 1 year of age is estimated to be associated with a 1.045-fold change in the mean number of 
#major vessels colored by fluoroscopy
#An increase in 1 year of age is estimated to be associated with a 4.6% increase in the mean number of 
#major vessels colored by fluoroscopy
qplot(age,ca, data = df, geom = "point", color = target)
model2$deviance

pres <- residuals(model2, type = "pearson")
sum(pres^2)
model3 <- glm(ca~1, data = df, family = "poisson")

LRT <- model3$deviance-model2$deviance
LRT

df <- model3$df.residual-model2$df.residual
df

1-pchisq(LRT,1)
#evidence of age effect in the log-linear model (drop in deviance test p-value <0.001)

