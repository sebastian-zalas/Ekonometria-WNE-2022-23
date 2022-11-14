#### zajecia nr 6 ####
# w razie błędów prosze o kontakt
# jeśli ktos się nie zgadza to również ;)

#### zadanie 1 #################################################################
rm(list = ls())

## wprowadź dane
Cena <- c(190, 160, 134, 129, 172, 197, 167, 239, 542, 372, 245, 376, 454, 410)
Połów <- c(7.23, 8.53, 9.82, 10.26, 8.96, 12.27, 10.28, 4.45, 1.78, 4.0, 3.3, 4.3, 0.8, 0.5)
dane = cbind(Cena, Połów)
dane = as.data.frame(dane)

## znajdowanie oszcowań współczynników (ze wzorów)
( beta1 = cov(dane$Cena, dane$Połów) / var(dane$Połów) )
( beta0 = mean(dane$Cena) - beta1*mean(dane$Połów) )

## znajdownie reszt
resid = dane$Cena - beta0 - beta1*dane$Połów

## SSR
resid2 = resid^2
SSR = sum(resid2)

## SST
SST = sum((dane$Cena - mean(dane$Cena))^2)

## R2
(r_squared = 1- SSR/SST)
#interpretacja: zmienna 'połów' objaśnia 73,5% wariancji ceny

#### zadanie 2 #################################################################
rm(list = ls())

# w tym zadaniu nie trzeba nic szacować, ale odpowiedzi zamieszczę tutaj:
# (i) wartość domu wzrośnie o 23 tysiące dolarów, certeris paribus
# (ii) wartośc domu wzrośnie o ok 45 tys. dolarów, ceteris paribus
23.4*1 + 0.156*140
# (iii) wartość domu spadnie o 48.8 tys. dolarów, ceteris paribus
# (iv) oblicz r^2
  # mamy podany dostosowany R kwadrat, wykorzystajmy wzór i obliczmy standardowy R kwadrat
  # skorygowany r2 = 0.72
  # n = 220
  # k = 6
  r_2 = 1 - (1 - 0.72)*( (220 - 6 - 1 ) / (220 - 1) )
  # model wyjaśnia ok 72 proc. wariancji cen domów.

#### zadanie 3 #################################################################
rm(list = ls())

# wczytaj dane 'charity' prosto z GitHub'a
charity = read.table("https://raw.githubusercontent.com/sebastian-zalas/Ekonometria-WNE-2022-23/main/class6/charity.csv", 
                     sep = ",",  # separator - tzn jakim znakiem są oddzielone zmienne w pliku źródłowym
                     header = T, # czy wcztytać pierwszy wiersz jako nazwy zmienych?
                     dec = "." ) # oznaczenie części dziesiętnej

## (i) oszacuj model
model1 = lm(data = charity, formula = gift ~ mailsyear + giftlast + propresp)
summary(model1)

## r kwadrat
ssr = sum((model1$residuals)^2)
sst = sum((charity$gift - mean(charity$gift))^2)
r_square = 1 - ssr/sst
r_square

model1simple = lm(data = charity, formula = gift ~ propresp)
ssr = sum((model1simple$residuals)^2)
sst = sum((charity$gift - mean(charity$gift))^2)
r_square_simple = 1 - ssr/sst
r_square_simple
# r kwadrat z prostej regresji jest oczywiście niższy ponieważ właściwy model zawiera więcej zmiennych objaśniających

## (ii)
model1$coefficients["mailsyear"]
# interpretacja: jeżeli liczba wysyłanych maili w ciągu roku wzrasta o jednostkę, to wartość prezentu (dotacji) wzrosnie o 2.16 guldena holenderskiego
lm(data = charity, formula = gift ~ mailsyear)
vcov(model1)
# współczynnik z prostej regresji jest wyższy, początkowy model zawiera również inne zmienne, co zmienia wartość oszacowania

## (iii)
model1$coefficients["propresp"]
## jeżeli stopa odpowiedzi na maile wzrośnie o 0.1, to wartość prezentu (dotacji) rośnie o ok. 1.5 guldena holenderskieg

## (iv)
## oszacuj model
model2 = lm(data = charity, formula = gift ~ mailsyear + giftlast + propresp + avggift)
model2$coefficients["mailsyear"]
# współczynnik przy 'mailsyear' mocno spadł, o około połowę

## (v)
model1$coefficients["giftlast"]
model2$coefficients["giftlast"]
# współczynnik przy zmiennej `giftlast` zmienił znak, + na - po dodaniu do regresji zmiennej `avggift`
# uwzględnienie średniej dotacji ma sens ponieważ ta zmienna mierzy ogólny poziom dobroczynności danej osoby
# negatywny współczynnik przy `giftlast` może mieć sens, gdyż po dużej doatcji,ludzie mogą przekazywać mniejszą kwotę

#### zadanie 4 #################################################################
rm(list = ls())

# wczytaj dane 'hprice' prosto z GitHub'a
hprice1 = read.table("https://raw.githubusercontent.com/sebastian-zalas/Ekonometria-WNE-2022-23/main/class6/hprice1.csv", 
                     sep = ",", header = T, dec = "." )
# (i) oszacuj model
model1 = lm(data=hprice1, price ~ sqrft + bdrms)
summary(model1)

# (ii)
# przyrost ceny domu z dodatkową sypialnią, przy niezmienionej powierzchni, wyniesie ok. 15 tys dolarów

# (iii)
zmiana_ceny = model1$coefficients["bdrms"]*1 + model1$coefficients["sqrft"]*140
zmiana_ceny
# jeżeli powierzchnia domu wzrośnie o 140mkw oraz dom będzie miał jedną dodatkową sypialnię, to jego cena wzrośnie o ok. 33 tys. doalrów

# (iv)
# model wyjaśnia ok. 63% wariancji cen domów.

# (v) cena pierwszego domu
cena_pierwszego_domu = model1$coefficients["(Intercept)"] + model1$coefficients["bdrms"]*4 + model1$coefficients["sqrft"]*2438
cena_pierwszego_domu

# (vi) reszta pierwszego domu
reszta_pierwszego_domu = 300 -  cena_pierwszego_domu
reszta_pierwszego_domu 
# wartość teoretyczna jest wyższa niż obserwowana.
# tak więc realna cena sprzezdaży jest niższa niż implikowana przez model
# można powiedzieć, że ktoś skorzystał z okazji i zapłacił mniej niż powinien

#### zadanie 5 #################################################################
rm(list = ls())

# wczytaj dane 'attend' prosto z GitHub'a
attend = read.table("https://raw.githubusercontent.com/sebastian-zalas/Ekonometria-WNE-2022-23/main/class6/attend.csv", 
                     sep = ",", header = T, dec = "." )

# (i) Podaj minimum, maksimum oraz  średnią dla zmiennych atndrte, priGPA oraz ACT
summary(attend$atndrte)
summary(attend$priGPA)
summary(attend$ACT)

# jeżeli chcemy zapisać wartości tych statystyk w pamięci
( stats_atndrte = c(min(attend$atndrte), max(attend$atndrte), mean(attend$atndrte)) )

# (ii) oszacuj model
model1 = lm(attend, formula = atndrte ~ priGPA + ACT)

print( paste("Intercept", model1$coefficients[1]))
print( paste("Beta_priGPA", model1$coefficients[2]))
print( paste("Beta_ACT", model1$coefficients[3]))

# zmienna objaśniana 'atndrte' to odsetek obecności.
# Jeżeli beta_0 = 75 to znaczy że ktoś kto miał zerową skumulowaną średnią i zerowy wynik testu ACT, i tak mial 75 proc. obecności.
# wydaje mi się że jest to raczej niemozliwe zeby ktoś miał zerową średnią, myslę że nie należy interpretować stałej w tym przypadku.

# (iii) 
# myślę że niespodzianką jest ujemna wartość oszacowania przy zmiennej 'ACT'
# zdrowy rozsądek raczej podpowiada że ten współczynnik powinien być dodatni

# (iv)
# wartość dopasowana dla priGPA=3.65 i ACT=20
model1$coefficients[1] + model1$coefficients[2]*3.65 + model1$coefficients[3]*20
# y dopasowane w tym przypadku wynosi 104, tzn że odsetek obecności przekracza 100 proc.
# czy są obserwacje z takimi wartościami?
x = subset(attend, ACT==20 )
x = subset(x, priGPA>3.60)
x
# jest jedna obserwacja z priGPA=3.65 oraz ACT=20
# popatrzmy na wykres ACT oraz priGPA
hist(attend$priGPA)
hist(attend$ACT)
plot(attend$priGPA, attend$ACT)
# rozkłady i wykresy pokazują że obserwacje o takich wrtościach występują raczej na brzegu rozkładów.
# wartośc dopasowana nie ma sensownej interpretacji
# regresja dopasowuje się do średniej, kiedy mocno od niej odbiegamy, błąd wrasta.
# ponadto, zmienna zależna jest ograniczona (może być pomiędzy 0 a 100) - w tym przypadku należałoby zastosować model który bierze to pod uwagę.

# (v)
studentA = model1$coefficients[1] + model1$coefficients[2]*3.1 + model1$coefficients[3]*21
studentB = model1$coefficients[1] + model1$coefficients[2]*2.1 + model1$coefficients[3]*26
studentA - studentB
# różnica wynosi około 25, co oznacza że odetek obecności wzrasta o 25 p. proc., cet. par.
