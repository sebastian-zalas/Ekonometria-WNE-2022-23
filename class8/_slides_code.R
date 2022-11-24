### zajęcia 8
# zmienne zero-jedynkowe i kategoryczne
# kody do slajdów
library(wooldridge)
# https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf

############## przykład 1 ######################################################
data(wage1)
wage1= wage1

# model na całych danych
model1 = lm(data=wage1, formula = wage ~ educ + female)
summary(model1)

# model dla mężczyzn
daneM = subset(wage1, female==0)
modelM = lm(data=daneM, formula = wage ~ educ + female)
summary(modelM)
daneM$wage_hat = fitted(modelM)

# model dla kobiet
daneK = subset(wage1, female==1)
modelK = lm(data=daneK, formula = wage ~ educ + female)
summary(modelK)
daneK$wage_hat = fitted(modelK)

wage2 = rbind(daneK, daneM)

# wykres
plot = ggplot(wage2, aes(x=educ, y=wage_hat, group=factor(female), colour=factor(female) )) +
       geom_line()+
       geom_point()
plot

plot = plot + theme_bw() + ylab("płace") + 
       xlab("edukacja") + 
       theme(legend.direction = "horizontal", 
       axis.title.x = element_text(size = 16),
       axis.title.y = element_text(size = 16),
       axis.text.x = element_text(size = 12),
       axis.text.y = element_text(size = 12),
       legend.position = c(.2, .95),
       legend.justification = c("right", "top"),
       legend.box.just = "right",
       legend.margin = margin(6, 6, 6, 6)) +
       labs(colour = "Płeć")
plot

# zapisz plik
png(filename="C:/Users/alter-ego/OneDrive/Pulpit/ekometrics/_class8/_class8_plot1.png", width = 800, height = 480)
plot
dev.off()

############## przykład 2 ######################################################
(model2 = lm(data=wage1, formula = lwage ~ educ + female))
summary(model2)
( 100 * ( exp(model2$coefficients["female"] ) -1 ) )
(100 * model2$coefficients["female"] )

############## przykład 3 ######################################################
wage1$marr_fem = ifelse(wage1$female==1 & wage1$married==1, 1, 0)
wage1$marr_male = ifelse(wage1$female==0 & wage1$married==1, 1, 0)

wage1$sing_fem = ifelse(wage1$female==1 & wage1$married==0, 1, 0)
wage1$sing_male = ifelse(wage1$female==0 & wage1$married==0, 1, 0)

(model3 = lm(data=wage1, formula = lwage ~ marr_male + marr_fem + sing_fem + educ + exper + expersq + tenure + tenursq))

############## przykład 4 ######################################################
rm(list = ls())

data(lawsch85)
lawsch85 = lawsch85

lawsch85$r61_100 = ifelse(lawsch85$rank>=61 & lawsch85$rank<100, 1, 0)
lm(data=lawsch85, formula= lsalary~ top10 + r11_25 + r26_40 + r41_60 + r61_100 + LSAT + GPA + log(libvol) + log(cost) )

