######### class 9 - przykłady #################################################
rm(list = ls())

#install.packages("AER") 
library(ggplot2) # pakiet 'ggplot2' służy do tworzenia wykresów
library(AER)     # pakiet 'AER' zawiera potrzebny zbiór danych 

# kolory
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# ustaw folder roboczy - ustaw własną 
setwd("C:/")

# dane o szkołach z Kalifornii
data("CASchools", package = "AER")

# trasformacje
CASchools$stratio <- with(CASchools, students/teachers)
CASchools$score <- with(CASchools, (math + read)/2)

##### Model z wielomianami ####################################################
# wykres 1
plot_income1 = ggplot(CASchools, aes(x=income, y=score)) +
  geom_point(size=2, colour=cbPalette[1]) +
  geom_smooth(data = CASchools, size = 2, colour = cbPalette[4], method = lm, formula = y~x, se = FALSE)

plot_income1 = plot_income1 + theme_bw() + ylab("Test Score") + 
  xlab("Income") + 
  theme(legend.direction = "horizontal", 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 14),
        plot.caption.position = "panel") +
  labs(caption = "Wyraźnie relacja pomiędzy TestScore oraz Income jest nieliniowa")
# legend.position = c(.2, .95),
# legend.justification = c("right", "top"),
# legend.box.just = "right",
# legend.margin = margin(6, 6, 6, 6))
plot_income1

# zapisz wykres w pliku png.
png(filename="_class9_TestScoreXIncomeLinear.png", width = 800, height = 480)
plot_income1
dev.off()

# wykres 2
plot_income2 = ggplot(CASchools, aes(x=income, y=score)) +
  geom_point(size=2, colour=cbPalette[1]) +
  geom_smooth(data = CASchools, size = 2, colour = cbPalette[4], method = lm, formula = y~x, se = FALSE) + 
  geom_smooth(data = CASchools, size = 2, colour = cbPalette[7], method = lm, formula = y~poly(x, 2), se = FALSE)

plot_income2 = plot_income2 + theme_bw() + ylab("Test Score") + 
  xlab("Income") + 
  theme(legend.direction = "horizontal", 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 14),
        plot.caption.position = "panel") +
  labs(caption = "Model z kwadratem zm. zależnej jest lepiej dopasowany do danych")
# legend.position = c(.2, .95),
# legend.justification = c("right", "top"),
# legend.box.just = "right",
# legend.margin = margin(6, 6, 6, 6))
plot_income2

# zapisz wykres w pliku png.
png(filename="_class9_TestScoreXIncomeSquared.png", width = 800, height = 480)
plot_income2
dev.off()

# wykres 3
plot_income3 = ggplot(CASchools, aes(x=income, y=score)) +
  geom_point(size=2, colour=cbPalette[1]) +
  geom_smooth(data = CASchools, size = 2, colour = cbPalette[4], method = lm, formula = y~x, se = FALSE) + 
  geom_smooth(data = CASchools, size = 2, colour = cbPalette[7], method = lm, formula = y~poly(x, 2), se = FALSE) +
  geom_smooth(data = CASchools, size = 2, colour = cbPalette[8], method = lm, formula = y~poly(x, 3), se = FALSE)

plot_income3 = plot_income3 + theme_bw() + ylab("Test Score") + 
  xlab("Income") + 
  theme(legend.direction = "horizontal", 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 14),
        plot.caption.position = "panel") +
  labs(caption = "Wielomian 3. stopnia nie poprawia sytuacji...")
# legend.position = c(.2, .95),
# legend.justification = c("right", "top"),
# legend.box.just = "right",
# legend.margin = margin(6, 6, 6, 6))
plot_income3

# zapisz wykres w pliku png.
png(filename="_class9_TestScoreXIncomeCubic.png", width = 800, height = 480)
plot_income3
dev.off()

# oszacowanie modelu z wielomianem stopnia 2
CASchools$income2 <- with(CASchools, income^2)
(model_squared = lm(data=CASchools, formula = score ~ income + income2))
summary(model_squared)

# oszacowanie modelu z wielomianem stopnia 3
CASchools$income3 <- with(CASchools, income^3)
(model_cubic = lm(data=CASchools, formula = score ~ income + income2 + income3))
summary(model_cubic)

# zmiany wartości dopasowanych TestScore dla wybranych przedziałów
# korzystam z modelu z kwadratem Income
predicted_changes = matrix(, nrow = 4, ncol = 2)
predicted_changes[,1] <- c("z 5 na 6 tys. pc", "z 10 na 11 tys. pc", "z 25 na 26 tys. pc", "z 45 na 46 tys. pc")
colnames(predicted_changes) <- c("zmiana w Income", "zmiana TestScore")
predicted_changes[1,2] = model_squared$coefficients[2]*(6-5) + model_squared$coefficients[3]*(6^2 - 5^2) # z 5 do 6 tysięcy
predicted_changes[2,2] = model_squared$coefficients[2]*(11-10) + model_squared$coefficients[3]*(11^2 - 10^2) # z 10 do 11 tysięcy
predicted_changes[3,2] = model_squared$coefficients[2]*(26-25) + model_squared$coefficients[3]*(26^2 - 25^2) # z 25 do 26 tysięcy
predicted_changes[4,2] = model_squared$coefficients[2]*(46-45) + model_squared$coefficients[3]*(46^2 - 45^2) # z 45 do 46 tysięcy
predicted_changes = as.data.frame(predicted_changes)
View(predicted_changes)

##### Model z interakcjami ####################################################

# wykres
plot_stratio = ggplot(CASchools, aes(x=stratio, y=score)) +
  geom_point(size=2, colour=cbPalette[1]) +
  geom_smooth(data = CASchools, size = 2, colour = cbPalette[4], method = lm, formula = y~x, se = FALSE) +
  geom_smooth(data = CASchools, size = 2, colour = cbPalette[7], method = lm, formula = y~poly(x, 2), se = FALSE)

plot_stratio = plot_stratio + theme_bw() + ylab("Test Score") + 
  xlab("Student-Teacher ratio") + 
  theme(legend.direction = "horizontal", 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 14),
        plot.caption.position = "panel") +
  labs(caption = "Na pierwszy rzut oka, relacja między Test Score a Student-Teacher ratio nie wydaje się nieliniowa...")
# legend.position = c(.2, .95),
# legend.justification = c("right", "top"),
# legend.box.just = "right",
# legend.margin = margin(6, 6, 6, 6))

plot_stratio
# zapisz wykres w pliku png.
png(filename="_class9_TestScoreXstratio.png", width = 800, height = 480)
plot_stratio
dev.off()

# # Interakcje #
# Relacja pomiędzy zmienną Y a zmienną X1 może być uzależniona od wartości innej zmiennej niezależnej,
# powiedzmy, X2. Na przykład, uczniowie uczący się angielskiego mogą bardziej skorzystać z większej atencji
# nauczyciela; jeśli tak, to relacja między wynikami testów a stosunkiem liczbny uczniów do nauczycieli będzie 
# inna w tych dystryktach w których wielu uczniów uczy się angielskiego. W tym przykładzie, efekt zmiejszenia
# stosunku liczby uczniów do liczby nauczycieli (X1) na wynik testów (Y), zależy również od części uczniów 
# uczących się angielskiego w dystrykcie (X2)

# Niech histr będzie zmienną zero-jedynkową, która przyjmuje wartość 1 jeżeli stosunek liczby 
# nauczycieli do liczby  uczniów jest większy niż 20
CASchools$histr <- with(CASchools, ifelse(stratio>=20, 1, 0))

# niech hiel będzie zmienną zero-jedynkową, która przyjmuje wartość 1 jeżeli procent uczniów uczących się
# angielskiego jest większy lub równy 10
CASchools$hiel <- with(CASchools, ifelse(english>=10, 1, 0))

# generujemy zmienną-interackję między hiel a histr
CASchools$hielXhistr <- with(CASchools, histr*hiel)

# regresja z interakcją
model_interact = lm(data=CASchools, formula = score ~ histr + hiel + hielXhistr)
summary(model_interact)

