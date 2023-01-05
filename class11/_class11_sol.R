############### Zajęcia nr 11 - Heteroskedastyczność ##########################
# czyszczenie środowiska
rm(list = ls())

# ładowanie pakietów
library(ggplot2)
library(car)
library(lmtest)

# jeśli potrzeba zainstaluj pakiety
#install.packages("car")
#install.packages("lmtest")

# kolory
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# folder roboczy
setwd("C:\\Users\\szalas\\Desktop\\_ekonometria_22_23\\ekometrics\\_class11")

########## Zadanie 2 ##########
#Rozważ model objaśniający zlogarytmowane płace:
#log(w) = β0 + β1educ + β2exper + β3exper2 + β4female + ε
#gdzie educ to liczba lat edukacji, exper to liczba lat zawodowego doświadczenia, 
#female to zm. zerojedynkowa oznaczająca kobiety, oraz ε to składnik losowy.

#### (ii) Użyj danych cps.dta aby oszacować parametry modelu. Oblicz kwadraty reszt 
#     i zrób wykres kwadratów reszt z każdą zmienną objaśniającą. 
#     Czy obserwujesz heterosekdastyczność w tym przypadku? Podaj interpretację ekonomiczną

# załadowanie danych
cps = read.table("https://raw.githubusercontent.com/sebastian-zalas/Ekonometria-WNE-2022-23/main/class11/cps.csv", 
                     sep = ",", header = T, dec = "." )
# szacowanie modelu
cps$exper2 = (cps$exper)^2
model1 = lm(data = cps, log(wage) ~ educ + exper + exper2 + female)

# wyniki
summary(model1)

# obliczenie reszt i kwadratów reszt
cps$residsq = resid(model1)^2
cps$resid   = resid(model1)

# wykres kwadratów reszt i edukacji
plot_educXresidsq = ggplot(cps, aes(x=educ, y=residsq)) +
        geom_point(size=2, colour=cbPalette[2])
plot_educXresidsq = plot_educXresidsq + theme_bw() + ylab("Kwadraty reszt") + 
  xlab("Edukacja") + 
  theme(legend.direction = "horizontal", 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 14),
        plot.caption.position = "panel") 
plot_educXresidsq
png(filename="_class11_educXresidsq.png", width = 800, height = 480)
plot_educXresidsq
dev.off()

# wykres reszt i edukacji
plot_educXresid = ggplot(cps, aes(x=educ, y=resid)) +
  geom_point(size=2, colour=cbPalette[3])
plot_educXresid = plot_educXresid + theme_bw() + ylab("Reszty") + 
  xlab("Edukacja") + 
  theme(legend.direction = "horizontal", 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 14),
        plot.caption.position = "panel") 
plot_educXresid
png(filename="_class11_educXresid.png", width = 800, height = 480)
plot_educXresid
dev.off()

# wykres kwadratów reszt i doświadczenia
plot_experXresidsq = ggplot(cps, aes(x=exper, y=resid)) +
  geom_point(size=2, colour=cbPalette[4])
plot_experXresidsq = plot_experXresidsq + theme_bw() + ylab("Kwadraty reszt") + 
  xlab("Doświadczenie") + 
  theme(legend.direction = "horizontal", 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 14),
        plot.caption.position = "panel") 
plot_experXresidsq
png(filename="_class11_experXresidsq.png", width = 800, height = 480)
plot_experXresidsq
dev.off()

# wszystkie wykresy w prostszej wersji
plot(cps$educ, cps$residsq)
plot(cps$exper, cps$residsq)
plot(cps$exper2, cps$residsq)
plot(cps$female, cps$residsq)
plot(cps$female, cps$resid)

#### (iii) Użyj testu White’a do przetestowania heteroskedastyczności.
# sposób 1 - staystyka F 
# oszacuj pomocniczą regresję, objaśniającą kwadraty reszt modelu z (ii)
model2 =lm(data = cps, residsq ~ educ + exper + exper2 + female + 
                      I(educ^2) + I(exper^2) + I(exper2^2) + I(female^2) +
                      + educ:exper + educ:exper2 + educ:female +
                      + exper:female + exper:exper2 + exper2:female )
model2summary = summary(model2)
# wyniki pomocniczej regresji
model2summary
# hipotezę zerową o homoskedsastyczności możemy zweryfikować na podstawie
# statystyki F i p-value zaraportowanej w wynikach pomocniczej regresji:
# wartość stystyki F:
(model2summary$fstatistic[1])
# związana z tą statystyką p-value wynosi niemal 0, co oznacza,
# że mamy podstawy aby hipotezę zerową odrzucić, co oznacza że heteroskedastyczność
# jest obecna.

# Możemy także sami wyliczyć statyskę F, wartość krytyczną oraz p-value 'ręcznie'
# potrzebne składniki: 
  # r-kwadrat 
  r2_u2 = model2summary$r.squared
  # liczba stopni swobody (= liczba obserwacji - liczba zmiennych - 1)
  st_swobody = model2$df.residual
  # ustalamy poziom istotności = 0.05
  alpha = 0.05
# statystyka F:
Fstat = (r2_u2/12) / ((1-r2_u2) / (st_swobody))
Fstat
# wartość krytyczna:
wkF = qf(1-alpha, 12, st_swobody) # df - funkcja gęstości prawdopodobieństwa rozkładu F
wkF
# p-value
pval = pf(Fstat, 12, 4733, lower.tail = FALSE) # pf - funkcja masy prawdopodobieństwa rozkładu F
pval

# wartość statystyki F jest wyższa niż warość krytyczna, p-value jest niższe niż 0.05
# więc konkluzja się nie zmienia: należy odrzucić hipotezę zerową o homoskedastyczności
Fstat>wkF
alpha>pval

# można także zastosować gotową komendę:
bptest(model1, ~ fitted(model1) + I(fitted(model1)^2))
# otrzymujemy nieco inne liczby, ale konkluzja pozostaje ta sama

# sposób 2 - statystyka LM (mnożników Lagrange'a)
# wyliczamy statystykę LM 
statLM = (st_swobody + 12 + 1) * r2_u2
statLM
pval = pchisq(statLM, 12, lower.tail =F, log.p = FALSE)
pval
wkChi2 = qchisq(1-alpha, 12, ncp = 0, log = FALSE)
wkChi2

# wartość statystyki LM jest wyższa niż warość krytyczna, p-value jest niższe niż 0.05
# więc konkluzja się nie zmienia: należy odrzucić hipotezę zerową o homoskedastyczności
statLM > wkChi2
alpha>pval

# (iv) Użyj testu Breuscha-Pagana (BP) do przetestowania heteroskedastyczności.
# test BP to uproszczona wersja testu White'a. Procedura testowa jest taka sama,
# jedynie regresja pomocnicza jest prostsza.

# sposób 1 - staystyka F 
# oszacuj pomocniczą regresję, objaśniającą kwadraty reszt modelu z (ii)
model3 =lm(data = cps, residsq ~ educ + exper + exper2 + female)
model3summary = summary(model3)
model3summary
# Podsumowanie regresji pomocniczej mówi, że staystyka F wynosi:
model3summary$fstatistic[1] 
# Na podsumowaniu modelu widać, że p-value jest niższe od standardowego poziomu 
# istotności (0.05) więc na podstawie testu BP możemy odrzucić hipotezę zerową o
# homoskedastyczności
# Możemy także sami wyliczyć statyskę F, wartość krytyczną oraz p-value 'ręcznie'
# potrzebne składniki: 
# r-kwadrat 
r2_u2 = model3summary$r.squared
# liczba stopni swobody (= liczba obserwacji - liczba zmiennych - 1)
st_swobody = model3$df.residual
# ustalamy poziom istotności = 0.05
alpha = 0.05
# statystyka F:
Fstat = (r2_u2/4) / ((1-r2_u2) / (st_swobody))
Fstat
# wartość krytyczna:
wkF = qf(1-alpha, 4, st_swobody) # df - funkcja gęstości prawdopodobieństwa rozkładu F
wkF
# p-value
pval = pf(Fstat, 4, st_swobody, lower.tail = FALSE) # pf - funkcja masy prawdopodobieństwa rozkładu F
pval
# wartość statystyki F jest wyższa niż warość krytyczna, p-value jest niższe niż 0.05
# więc konkluzja się nie zmienia: należy odrzucić hipotezę zerową o homoskedastyczności
Fstat>wkF
alpha>pval

# można także zastosować gotową komendę:
bptest(model1)
# otrzymujemy nieco inne liczby, ale konkluzja pozostaje ta sama

# sposób 2 - statystyka LM (mnożników Lagrange'a)
# wyliczamy statystykę LM 
statLM = (st_swobody + 4 + 1) * r2_u2
statLM
pval = pchisq(statLM, 4, lower.tail =F, log.p = FALSE)
pval
wkChi2 = qchisq(1-alpha, 4, ncp = 0, log = FALSE)
wkChi2

# wartość statystyki LM jest wyższa niż warość krytyczna, p-value jest niższe niż 0.05
# więc konkluzja się nie zmienia: należy odrzucić hipotezę zerową o homoskedastyczności
statLM > wkChi2
alpha>pval

## (v) Oszacuj model używając odpornych błędów standardowych. Omów różnice.
install.packages("stargazer")
library(stargazer)

# zapisz współczynniki
wspolczynniki = as.matrix(model1$coefficients) 

# zwykłe błędy std.
std_errors = coeftest(model1)
std_errors = as.matrix(std_errors[,2])

# odporne błędy std.
robust_std_errors = coeftest(model1, vcov. = hccm(model1, type="hc0"))
robust_std_errors = as.matrix(robust_std_errors[,2])

# podsumowanie
X = cbind(wspolczynniki, std_errors, robust_std_errors)
X = round(X, 8)
rownames(X) = c("Intercept", "Educ", "Exper", "Exper^2", "Female" ) 
colnames(X) = c("Współczynniki", "Zwykłe błędy std.", "Odporne błędy std.") 
View(X)
# błędy standardowe są delitkatnie różne. Ma to implikacje dla np. testowania
# istotności statystycznej.

# kod produkujący tabelki do notatek:
#stargazer(X, title="Porównanie błędów standardowych", digits = 6, align=TRUE, style = "aer")
#stargazer(model2, model3 , title="Wyniki regresji pomocniczych", align=TRUE, style = "aer" )

########## Zadanie 3 ##########
rm(list = ls())

# załadowanie danych
hprice1 = read.table("https://raw.githubusercontent.com/sebastian-zalas/Ekonometria-WNE-2022-23/main/class11/hprice1.csv", 
                 sep = ",", header = T, dec = "." )
#(i) poziomy
model_level = lm(data=hprice1, price ~ lotsize + sqrft + bdrms)
  
  # zwykłe błędy std.
  coeftest(model_level)
  
  # odporne błędy std.
  coeftest(model_level, vcov. = hccm(model_level, type="hc0"))

#(ii) logarytmy
  model_log = lm(data=hprice1, I(log(price)) ~ I(log(lotsize)) + I(log(sqrft)) + bdrms)
  
  # zwykłe błędy std.
  coeftest(model_log)
  
  # odporne błędy std.
  coeftest(model_log, vcov. = hccm(model_log, type="hc0"))
  
########## Zadanie 4 ##########
rm(list = ls())

# załadowanie danych
hprice1 = read.table("https://raw.githubusercontent.com/sebastian-zalas/Ekonometria-WNE-2022-23/main/class11/hprice1.csv", 
                     sep = ",", header = T, dec = "." )

# model podstawowy
model1 = lm(data=hprice1, I(log(price)) ~ I(log(lotsize)) + I(log(sqrft)) + bdrms)

# reszty
hprice1$residsq = (resid(model1))^2

# model pomocniczy
model2 =lm(data = hprice1, I(log(price)) ~ I(log(lotsize)) + I(log(sqrft)) + bdrms + 
                                           I(log(lotsize))^2 + I(log(sqrft))^2 + bdrms^2 +
                                           I(log(lotsize)):I(log(sqrft)) + I(log(lotsize)):bdrms + I(log(sqrft)):bdrms)

# r kwadrat z pomociczej regresji i liczba stopni swobody
model2summary = summary(model2)
r2_u2 = model2summary$r.squared
st_swobody = 88 - 3 - 1
  
# test White'a ze statystyką LM:
# wyliczamy statystykę LM 
statLM = (st_swobody) * r2_u2
statLM
pval = pchisq(statLM, 3, lower.tail =F, log.p = FALSE)
pval

########## Zadanie 5 ##########
rm(list = ls())

# załadowanie danych
vote1 = read.table("https://raw.githubusercontent.com/sebastian-zalas/Ekonometria-WNE-2022-23/main/class11/vote1.csv", 
                     sep = ",", header = T, dec = "." )

# (i) Oszacuj model z voteA jako zmienną zależną oraz zmiennymi prtystrA, democA,
#     log(expendA), i log(expendB) jako zmiennymi niezależnymi. 
#     Uzyskaj reszty i policz regresję reszt na wszystkie zmienne niezależne. Wyjaśnij dlaczego otrzymujesz R^2=0.
model1 = lm(data=vote1, formula = voteA ~ prtystrA + democA + I(log(expendA)) + I(log(expendB)))
vote1$residsq = resid(model1)^2
model2 = lm(data=vote1, formula = residsq~ prtystrA + democA + I(log(expendA)) + I(log(expendB)))
model2summary = summary(model2)
model2summary

# (ii) Policz test Breusch'a-Pagan'a na heteroskedastyczność. Użyj statystyki F i podaj p-value
# wysatrczy spojrzeć na statystyke F obliczoną przy szacowaniui modelu 2.
FstatBP = model2summary$fstatistic[1]
FstatBP

# p-value
pvalBP = pf(FstatBP, 4, 173-4-1, lower.tail = FALSE)
pvalBP

# (iii) Zastosuj test White'a na heteroskedastyczność, znów używając statystyki F. Jak silny jest dowód na istnienie heteroskedastyczności?
model3 = lm(data=vote1, formula = residsq~ prtystrA + democA + I(log(expendA)) + I(log(expendB)) +
              prtystrA^2 + democA^2 + I(log(expendA))^2 + I(log(expendB))^2 +
              prtystrA:democA + prtystrA:I(log(expendA)) + prtystrA:I(log(expendB)) + 
              democA:I(log(expendA)) + democA:I(log(expendB)) + I(log(expendA)):I(log(expendB)) )

model3summary = summary(model3)

# statystyka F
FstatW = model3summary$fstatistic[1]
FstatW

# p-value
pvalW = pf(FstatW, 10, 173-10-1, lower.tail = FALSE)
pvalW

