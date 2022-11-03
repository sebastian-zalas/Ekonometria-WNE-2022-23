#### Ekonometria vol. 5
#install.packages("wooldridge")
library(wooldridge)
library(psych)

# opisy zbiorów danych z pakietu 'wooldridge'
# https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf

## zadanie 1
rm(list = ls())
data("gpa2")
attach("gpa2")

# wyestymowanie modelu i zapisanie go jako model1
model1 = lm(data = gpa2, formula = colgpa ~ hsperc + sat)

# podsumowanie modelu
summary(model1)

# reszty, wartości dopasowane
# sposób 1 - możemy policzyć je sami
bhat = coef(model1)
yhat = bhat["(Intercept)"] + bhat["hsperc"]*gpa2$hsperc + bhat["sat"]*gpa2$sat
uhat = gpa2$colgpa - yhat
head(uhat)

# sposób 2 - skorzystajmy z funkcji
bhat = coef(model1)
yhat = fitted(model1)
uhat = resid(model1)
head(uhat)

## zadanie 2
rm(list = ls())
data("wage2")
lm(data = wage2, formula = educ ~ sibs + meduc + feduc)

## zadanie 3
rm(list = ls())
data("sleep75")
lm(data = sleep75,formula =  sleep ~ totwrk + educ + age)

## zadanie 4
rm(list = ls())
# wczytaj dane prosto z GitHub'a
growth = read.table("https://raw.githubusercontent.com/sebastian-zalas/Ekonometria-WNE-2022-23/main/class5/Growth.csv", sep = ";", header = T, dec = "," )
rownames(growth) = growth$country_name
head(growth)

# (i) scatterplot Growth & TradeShare
plot(tradeshare ~ growth , col="lightblue", pch=19, cex=2,data=growth)

# (ii) czy Malta to outlier?
plot(growth ~ tradeshare , col="lightblue", pch=19, cex=2,data=growth)
text(growth ~ tradeshare , labels=country_name ,data=growth, cex=0.9, font=2, pos=2)

# (iii) model na pełnych danych
model1 = lm(data= growth, growth ~ tradeshare)

# (iv) model bez Malty
growth_nomalta = subset(growth, country_name!="Malta")
model2 = lm(data= growth_nomalta, growth ~ tradeshare)

# (v) wykres regresji
plot(growth ~ tradeshare, col="lightblue", pch=19, cex=2,data=growth)
text(growth ~ tradeshare, labels=country_name ,data=growth, cex=0.9, font=2, pos=2)
abline(model1, col = "blue")
abline(model2, col = "red")

## zadanie 5
# (i) statystyki
describe( growth_nomalta[ , c('growth', 'oil', 'rgdp60', 'tradeshare', 'yearsschool', 'rev_coups', 'assasinations')], fast=TRUE)

# (ii) model
model3 = lm(data = growth_nomalta, growth ~ rgdp60 + tradeshare + yearsschool + rev_coups + assasinations )
summary(model3)

# (iii)
means = colMeans(growth_nomalta[ , c('rgdp60', 'tradeshare', 'yearsschool', 'rev_coups', 'assasinations')])
means = c(1, means)
mean_country_growth = model3$coefficients%*% means
mean_country_growth

# (iv) 
means['tradeshare']
means['tradeshare'] = means['tradeshare'] + sd(growth_nomalta$tradeshare)
means['tradeshare']
mean_country_growth2 = model3$coefficients%*% means
mean_country_growth2

# (v) oil
model4 = lm(data = growth_nomalta, growth ~ rgdp60 + tradeshare + yearsschool + rev_coups + assasinations + oil)
summary(model3)
summary(model4)
unique(growth_nomalta$oil)

## zadanie 6
rm(list = ls())
data("hprice1")

## zadanie 7
rm(list = ls())
data("attend")

