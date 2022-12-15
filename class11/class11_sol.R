#Rozważ model objaśniający zlogarytmowane płace:
#log(w) = β0 + β1educ + β2exper + β3exper2 + β4female + ε
#gdzie educ to liczba lat edukacji, exper to liczba lat zawodowego doświadczenia, 
#female to zm. zerojedynkowa oznaczająca kobiety, oraz ε to składnik losowy.

#(i) Czy spodziewasz sie˛ heteroskedastyczności składnika losowego w powyższym modelu?

#(ii) Użyj danych cps.dta aby oszacować parametry modelu. Oblicz kwadraty reszt 
# i zrób wykres kwadratów reszt z każdą zmienną objaśniającą. 
#Czy obserwujesz heterosekdastyczność w tym przypadku? Podaj interpretację ekonomiczną
rm(list = ls())

# załadowanie danych
cps = read.table("https://raw.githubusercontent.com/sebastian-zalas/Ekonometria-WNE-2022-23/main/class11/cps.csv", 
                     sep = ",", header = T, dec = "." )

# model 
cps$exper2 = (cps$exper)^2
model1 = lm(data = cps, log(wage) = educ + exper + exper2 + female)

#(iii) Użyj testu White’a do przetestowania heteroskedastyczności.


#(iv) Użyj testu BP do przetestowania heteroskedastyczności.


#(v) Oszacuj model używając odpornych błędów standardowych. Omów różnice.
