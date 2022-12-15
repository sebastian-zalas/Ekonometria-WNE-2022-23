######### Symulacja własności estymatora MNK ###################################
library(ggplot2)

# wyczyść środowisko
rm(list = ls())

# kolory
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# ustaw folder roboczy - ustaw własną 
#setwd("C:/")
setwd("C:\\Users\\alter-ego\\OneDrive\\Pulpit\\ekometrics\\_class11")

# ustaw ziarno losowania dla powtarzalności wyników
set.seed(1234567)

# ustaw liczbę obserwacji oraz powtórzeń symulacji
n<-1000; r<-10000

# ustaw prawdziwe parametry: bety oraz odchylenie std. u
b0<-1; b1<-0.5; su<-2

# stwórz wektory b0hat oraz b1hat dla przechowania wyników:
b0hat <- numeric(r)
b1hat <- numeric(r)

# wygeneruj x, takie samo dla wszystkich potórzeń
x <- rnorm(n,4,1)

#### przypadek homoskedastyczny ####
# powtórz r razy:
for(j in 1:r) {
  # wylosuj próbę y:
  u <- rnorm(n,0,su)
  y <- b0 + b1*x + u
  
  # oszacuj parametry i zachowaj je w wektorze
  bhat <- coefficients( lm(y~x) )
  b0hat[j] <- bhat["(Intercept)"]
  b1hat[j] <- bhat["x"]
}

# oszacowanie MC wartości oczekiwanej:
mean(b0hat)
mean(b1hat)

# oszacowanie MC wariancji:
var(b0hat)
var(b1hat)

# rozkład oszacowań
df = data.frame(b1hat)
h11 <-ggplot(df, aes(x=b1hat)) + 
  geom_histogram(color="black", fill=cbPalette[8]) +
  xlim(0.1, 0.9) +
  geom_vline(xintercept = mean(b1hat), size = 2, color=cbPalette[3])

h1 = h11 + theme_bw() + ylab("") + 
  xlab("oszacowania") + 
  theme(legend.direction = "horizontal", 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 14),
        plot.caption.position = "panel")
h1

png(filename="_class11_homosk.png", width = 800, height = 200)
h1
dev.off()

#### przypadek heteroskedastyczny ####
# repeat r times:
for(j in 1:r) {
  # wylosuj próbę y:
  varu <- 0.1 * exp(x)
  u <- rnorm(n, 0, sqrt(varu) )
  y <- b0 + b1*x + u
#/exp(4.5)
  # oszacuj parametry i zachowaj je w wektorze
  bhat <- coefficients( lm(y~x) )
  b0hat[j] <- bhat["(Intercept)"]
  b1hat[j] <- bhat["x"]
}

# oszacowanie MC wartości oczekiwanej:
mean(b0hat)
mean(b1hat)

# oszacowanie MC wariancji:
var(b0hat)
var(b1hat)

# rozkład oszacowań
df = data.frame(b1hat)
h21 <-ggplot(df, aes(x=b1hat)) + 
  geom_histogram(color="black", fill=cbPalette[8])  +
  xlim(0.1, 0.9) +
  geom_vline(xintercept = mean(b1hat), size = 2, color=cbPalette[3])

h2 = h21 + theme_bw() + ylab("") + 
  xlab("oszacowania") + 
  theme(legend.direction = "horizontal", 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 14),
        plot.caption.position = "panel") 
h2

png(filename="_class11_heterosk.png", width = 800, height = 200)
h2
dev.off()
h1
h2