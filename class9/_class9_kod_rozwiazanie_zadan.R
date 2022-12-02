##### class 9 - rozwiązania ####################################################
# tutaj podaję tylko kod, którym uzyskujemy potrzebne oszacowania, obliczenia etc.
# wszytskie interpretacje są zawarte w pliku pdf 'class9_sol'

#### I. Modele z wielomianami ##################################################

## zadanie 1 ###################################################################
rm(list = ls())

# (i)   Oszacuj metoda˛MNK poniższe równanie:
#          log(wage) = β0 + β1educ + β2exper + β3exper2 + u
#       Zapisz wynik w formie równania.
wage1 = read.table("https://raw.githubusercontent.com/sebastian-zalas/Ekonometria-WNE-2022-23/main/class9/wage1.csv", 
                     sep = ",", header = T, dec = "." )               # ładujemy potrzebne dane
model_1 = lm(data=wage1, formula = lwage ~ educ + exper + expersq)    # szacujemy model
summary(model_1)                                                      # wyniki

# (ii)  Używając przybliżenia, znajdź (przybliżony) zwrot z piątego roku doświadczenia. 
#       Jaki jest przybliżony zwrot z dwudziestego roku edukacji?
(zwrot_5_rok = model_1$coefficients["exper"] + 2*model_1$coefficients["expersq"] * 4)*100
(zwrot_20_rok = model_1$coefficients["exper"] + 2*model_1$coefficients["expersq"] * 19)*100

# (iii) Przy jakiej wartości exper, zwiększenie doświadczenia obniża przewidywany logarytm płacy? 
#       Ile osób ma więcej (niż ta wartość) doświadczenia w próbie?
(- model_1$coefficients["exper"]) / (2*model_1$coefficients["expersq"]) # wartość exper dla której log(wage) jest maksymalna

sum(wage1$exper>=29)

## zadanie 2 ###################################################################
rm(list = ls())

# Użyj danych BWGHT2
bwght2 = read.table("https://raw.githubusercontent.com/sebastian-zalas/Ekonometria-WNE-2022-23/main/class9/bwght2.csv", 
                   sep = ",", header = T, dec = "." )               # ładujemy potrzebne dane

# (i) Oszacuj równanie MNK:
#       log(bwght) = β0 + β1npvis + β2npvis2 + u
#     zapisz wyniki w formie równania.
model_2 = lm(data=bwght2, formula = lbwght ~ npvis + npvissq)
summary(model_2)

# (ii) Pokaż korzystając z oszacowania z (i), że liczba wizyt prenatalnych która maksymalizuje log(bwght) wynosi
#      około 22. Jak wiele kobiet miało przynajmniej 22 wizyty w próbie?
(- model_2$coefficients["npvis"]) / (2*model_2$coefficients["npvissq"]) # wartość npvis dla której log(bwght) jest maksymalna
sum(bwght2$npvis >= 22, na.rm=T)

# (iii) Czy ma to sens, że waga urodzenia spada po przekroczeniu 22 wizyt prenatalnych? Wyjaśnij.
# Być może kobiety, które odbywają dużo wizyt, mają problemy z ciążą i objawia się to niższą wagą urodzenia

# (iv) Dodaj wiek matki do równania, używając funkcji kwadratowej. Przy npvis ustalonym, czy jakim wieku
#      matki waga przy narodzinach dziecka jest największa? Ile jest w próbie kobiet starszych niż obliczony
#      optymalny wiek?
model_2iv = lm(data=bwght2, formula = lbwght ~ npvis + npvissq + mage + magesq)
summary(model_2iv)

(mage_opt = (- model_2iv$coefficients["mage"]) / (2*model_2iv$coefficients["magesq"])) # wartość wieku matki dla której log(bwght) jest maksymalna

sum(bwght2$mage>mage_opt) # liczymy ile jest kobiet które mają więcej lat niż obliczona wartość powyżej

# (v) Czy wiek matki oraz liczba wizyt prenatalnych wyjaśnia dużo zróżnicowania w log(bwght)?
(summary(model_2iv)$r.squared) # jedynie trochę ponad 2%

## zadanie 3 ###################################################################
rm(list = ls())

# Użyj danych GPA2
gpa2 = read.table("https://raw.githubusercontent.com/sebastian-zalas/Ekonometria-WNE-2022-23/main/class9/gpa2.csv", 
                    sep = ",", header = T, dec = "." )               # ładujemy potrzebne dane

# (i) Oszacuj model
#       sat = β0 + β1hsize + β2hsize2 + u
#     gdzie hsize to rozmiar klasy absolwentów (w setkach), oraz zapisz model w postaci równania.
model_3 = lm(data=gpa2, formula = sat ~ hsize + hsizesq)
summary(model_3)

# (ii) Używając oszacowań z (i) powiedz jaki jest optymalny rozmiar klasy? Uzasadnij.
(hsize_opt = (- model_3$coefficients["hsize"]) / (2*model_3$coefficients["hsizesq"])) # wartość hsize dla której sat jest maksymalny

# (iii) Czy ta analiza jest reprezentatywnie przedstawia poziom osiągnięć akademickich 
#       wśród starszych uczniów liceum? Wyjaśnij.

# (iv) Znajdź optymalny poziom klasy, używając log(sat) jako zmienną zależną. 
#      Czy jest różny od tego otrzymanego w (iii)?
model_3iv = lm(data=gpa2, formula = log(sat) ~ hsize + hsizesq)
summary(model_3iv)

(hsize_opt_log = (- model_3iv$coefficients["hsize"]) / (2*model_3iv$coefficients["hsizesq"])) # wartość hsize dla której log(sat) jest maksymalny
hsize_opt


#### II. Modele z interakcjami #################################################

## zadanie 1 ###################################################################
rm(list = ls())

# Użyj danych GPA2 (W, 3 p. 259).
twoyear = read.table("https://raw.githubusercontent.com/sebastian-zalas/Ekonometria-WNE-2022-23/main/class9/gpa2.csv", 
                     sep = ",", header = T, dec = "." ) # ładujemy dane

# (i) Oszacuj równanie
#       sat = β0 + β1hsize + β2hsize2 + β3female + β4black + β5(female × black) + u
#     Zmienna sat to łączny wynik testu SAT, hsize to wielkość klasy absolwentów (w setkach), female to zmienna
#     zero-jedynkowa oraz black to zmienna zero-jedynkowa oznaczająca rasę.
#     Korzystając z równania wyznacz optymalną wielkość klasy.
model_1 = lm(data=twoyear, formula= sat~ hsize + hsizesq + female + black + female:black)
summary(model_1)

# optymalny rozmiar klasy
(- model_1$coefficients["hsize"]) / (2*model_1$coefficients["hsizesq"])

# (ii) Przy niezmienionym hsize, jaka jest szacowana różnica w wyniku SAT między nieczarnymi kobietami i
#      czarnymi mężczyznami?
#      E[sat|black=0, female=1] - E[sat|black=1, female=0]
(model_1$coefficients["female"] -  model_1$coefficients["black"])

# (iii) Jaka jest szacowana zmiana w wyniku SAT między czarnymi mężczyznami a pozostałymi mężczyznami?
#      E[sat|black=1, female=0] - E[sat|black=0, female=0]
(model_1$coefficients["black"])

# (iv) Jaka jest szacowana różnica między czarnymi kobietami a pozostałymi kobietami?
#      E[sat|black=1, female=1] - E[sat|black=0, female=1]
(model_1$coefficients["female"]+model_1$coefficients["female:black"]+model_1$coefficients["black"]  -  model_1$coefficients["female"])


## zadanie 2 ###################################################################
rm(list = ls())

# Użyj danych TWOYEAR
twoyear = read.table("https://raw.githubusercontent.com/sebastian-zalas/Ekonometria-WNE-2022-23/main/class9/twoyear.csv", 
                     sep = ",", header = T, dec = "." ) # ładujemy dane

# (i) Oszacuj równanie
#     log(wage) = β0 + β1female + β2totcoll + β3female × totcoll
#     Używając oszacowań , znajdź wartości totcoll takie że przewidywane wartości log(wage) są takie same dla
#     kobiet i mężczyzn.
model_2 = lm(data=twoyear, formula= lwage ~ female + totcoll + totcoll:female)

# (ii) Korzystając z oszacowania z (i) powiedz, czy kobiety naprawdę mogą uzyskać tyle lat edukacji, aby ich
#      zarobki dogoniły zarobki mężczyzn? Wyjaśnij.

## zadanie 3 ###################################################################
rm(list = ls())

wage2 = read.table("https://raw.githubusercontent.com/sebastian-zalas/Ekonometria-WNE-2022-23/main/class9/wage2.csv", 
                     sep = ",", header = T, dec = "." ) # ładujemy dane

# Poniższy model pozwala aby zwrot z edukacji zależał od edukacji rodziców (pareduc) (W, 4 p. 218):
# log(wage) = β0 + b1educ + β2educ × pareduc + β3exper + β4tenure + u.
# (i) Pokaż że w przybliżeniu zwrot z dodatkowego roku edukacji można opisać formułą:
#       Δlog(wage)/Δeduc = β1 + β2pareduc
#     Jakiego znaku spodziewasz się przy β2?

# (ii) Używając danych WAGE2 oszacuj model. Zinterpretuj współczynnik przy interakcji. Pomocne może być
#      wybranie dwóch wartości zmiennej pareduc np.: pareduc = 32 (obaj rodzice mają wykształcenie wyższe),
#      lub pareduc = 24 (obaj rodzice mają wykształcenie średnie) i porównaj oszacowany zwrot z edukacji.
wage2$pareduc = with(wage2, meduc+feduc) # tworzymy zmienną 'pareduc' opisującą łączą edukację rodziców
model_3 = lm(data=wage2, formula= lwage~ educ + educ:pareduc + exper + tenure)
summary(model_3)

#pareduc = 32 (obaj rodzice mają wykształcenie wyższe)
#pareduc = 24 (obaj rodzice mają wykształcenie średnie)
model_3$coefficients["educ:pareduc"]*32- model_3$coefficients["educ:pareduc"]*24

# (iii) Dodaj pareduc jako oddzielną zmienną i oszacuj model. Czy teraz zależność między zwrotem z edukacji a
#       edukacją rodziców jest pozytywna?
model_3iii = lm(data=wage2, formula= lwage~ educ + educ:pareduc + exper + tenure + pareduc)
summary(model_3iii)

model_3iii$coefficients["educ:pareduc"]*32 - model_3iii$coefficients["educ:pareduc"]*24

