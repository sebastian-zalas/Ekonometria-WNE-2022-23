###### Rozwiązanie ######
# wyczyść środowisko
rm(list = ls())

# załadauj pakiety
#install.packages("tidyverse")
#install.packages("tidyr")
#install.packages("foreign")
#install.packages("vtable")
#install.packages("ggplot2")
library(tidyr)
library(dplyr)
library(vtable)
library(ggplot2)

# ustaw folder roboczy !!!
#setwd("")

# 1. Zaimportuj dane
firms1 = read.table("firms_long_2002.csv", sep="\t", header=TRUE)
industry_codes = read.table("industry2002.csv", sep="\t", header=TRUE, quote = "")

# 2. połącz dane po zmiennej 'id' w jeden zbiór. Zostaw w pamięci tylko finałową ramkę danych.
# sposób 1 - base R
firms = merge(firms1, industry_codes, by.x = "id", by.y = "id", all.x = TRUE, all.y = FALSE)

# sposób 2 - dplyr
firms2 = full_join(firms1, industry_codes,  by = c("id" = "id"), keep = F)

# zostawiamy tylko dane firms
rm("firms1", "firms2", "industry_codes")

# 3. przygotuj zmnienną 'year_inc', która ma opisywać rok powstania firmy
# substring, nchar()
firms$year_inc = substring(firms$dateofincorporation, nchar(firms$dateofincorporation) - 3, nchar(firms$dateofincorporation))
head(firms$year_inc)

# usuń zmienną 'dateofincorporation'
firms = subset(firms, select = -dateofincorporation )

# 4. Usuń obserwacje z brakującymi danymi w 'nacerec1primarycode'. Utwórz zmienną, która opisuje
# sektor na poziomie dwóch cyfr. Korzytając z tej zmiennej, utwórz zmienną która zalicza firmy do
# przemysłu (kody pomiędzy 15 a 45) lub do usług (kody pomiędzy 50 a 99). Usuń obserwacje z kodami
# poniżej 15.

# sprawdż wartości zmiennych
unique(firms$nacerev1primarycode)
table(firms$nacerev1primarycode)

# usuń obserwacje bez kodów sektorowych
firms = drop_na(firms, any_of( "nacerev1primarycode" ) )
#check = subset(firms, nacecodesrev1<1000, select = c("id", "nacecodesrev1", "nacecodesrev1description") )

# Utwórz dwucyfrowy 'industry_code' ze zmiennej 'nacecodesrev1'
firms$industry_code = firms$nacerev1primarycode / 100
firms$industry_code = floor(firms$industry_code)
unique(firms$industry_code)

# Utwórz zmienną 'industry' która zawiera dwie wartości: 
#               1 dla przemysłu (manufactuirng, industrycode>=15 & <=45)
#               2 dla usług (services, industrycode >=50, <=99)
#               pozostałe obserwacje usunąć, możesz dodać odpowiednią etykietę
firms = subset(firms, industry_code >= 15)
firms$industry = ifelse((firms$industry_code >= 15 & firms$industry_code <=45), 1, 2)
firms$industry_factor = factor(firms$industry, labels=c("manufacturing","services"))
unique(firms$industry)
unique(firms$industry_factor)

# 5. Usuń obserwacje z brakującymi danymi w STAF, AV, EMPL, STAF. 
# Czy występują ujemne wartości w tych zmiennych? Czy one maja˛ sens?

# usuwamy brakujące dane
firms = drop_na(firms, any_of( c("STAF", "AV", "EMPL", "TOAS") )  ) 

# czy są ujemne wartości w zmiennych
with(firms, table( sign(AV) ))
with(firms, table( sign(EMPL) ))
with(firms, table( sign(STAF) ))
with(firms, table( sign(TOAS) ))
# ujemne wartości nie mają w tym przypadku ekonomicznego  sensu, dlatego można je usunąć ze zbioru danych

# zostawmy tylko dodatnie wartości
firms = subset(firms, AV>0 & STAF>0 & TOAS>0 )

# 6. Sprawdż rozkłady zmiennych (szczególnie AV oraz EMPL) np. używając histogramu.
# Czyte wykresy są zrozumiałe? Porównaj je z logarytmami zmiennych

# histogram wartości dodanej (AV)
hist(firms$AV, main="Histogram wartości dodanej", xlab="Wartość dodana")
# rozkład jest bardzo skoncetrowany wokół lewej strony

# histogram logarytmu wartości dodanej (lnAV)
hist(log(firms$AV), main="Histogram logarytmu wartości dodanej", xlab="Wartość dodana")
# kiedy spojrzymy na rozkład logarytmu, wpływ skrajnych wartości jest ograniczony
# teraz można lepiej ocenić rozkład danych

# histogram zatrudnienia
hist(firms$EMPL, main="Histogram zatrudnienia", xlab="Zatrudnienie")
hist(log(firms$EMPL), main="Histogram logarytmu zatrudnienia", xlab="Zatrudnienie")

# 7. Przygotuj tabelę ze statystykami podsumowującymi utworzoną bazę danych. 
# Niech zawiera ona liczbę obserwacji, średnią, medianę, 25-ty i 75-ty percentyl, minimum oraz maksimum.
# %>% to operator przetwarzania potokowego (pipe):
firms %>% summary()
# to samo inaczej:
summary(firms)

# Ładniejszą tabelę można otrzymać korzystając z funkcji sumstats z pakietu vtable:
sumstats = firms %>% select(AV, TOAS, EMPL, STAF) %>% sumtable(, out='return',
                                                      summ=c('mean(x)',
                                                             'min(x)',
                                                             'max(x)',
                                                             'pctile(x)[10]',
                                                             'pctile(x)[25]',
                                                             'median(x)',
                                                             'pctile(x)[75]',
                                                             'pctile(x)[90]'))
sumstats

# 8. Teraz oblicz tzw. labor share czyli stosunek płac do wartości dodanej, na poziomie firmy, 
# na poziomie sektora (przemysł vs usługi), oraz dla całego zbioru danych. 
# Narysuj wykres przebiegu obliczonych wskaźników w czasie.

firms$individual_labor_share = firms$STAF / firms$AV

# stwórzmy zbiór danych na poziomie sektora i roku, tzn wysumujmy wartości na poziomie sektora i roku.
aggregated = firms %>% 
  select(year, industry, AV, TOAS, EMPL, STAF) %>% 
    group_by(industry, year) %>% 
      summarise_all(sum)

manufacturing = subset(aggregated, industry==1 & year< 2002)
services = subset(aggregated, industry==2 & year< 2002)

aggregated_wide = merge(manufacturing, services, by.x = "year", by.y = "year", all.x = TRUE, all.y = FALSE)
aggregated_wide$labor_share_total = (aggregated_wide$STAF.x+aggregated_wide$STAF.y) /(aggregated_wide$AV.x+aggregated_wide$AV.y) 
aggregated_wide$labor_share_manuf = (aggregated_wide$STAF.x) /(aggregated_wide$AV.x) 
aggregated_wide$labor_share_servi = (aggregated_wide$STAF.y) /(aggregated_wide$AV.y) 

# wykres
plot(aggregated_wide$year, aggregated_wide$labor_share_manuf , type = "b", frame = FALSE, pch = 19, xlim=c(1996, 2001), ylim=c(0, 1),
     col = "red", xlab = "Year", ylab = "labor share")
# dodaj następną linię
lines(aggregated_wide$year, aggregated_wide$labor_share_servi , pch = 18, col = "blue", type = "b", lty = 2)
# dodaj następną linię
lines(aggregated_wide$year, aggregated_wide$labor_share_total , pch = 18, col = "green", type = "b", lty = 2)

# legenda
legend("topleft", legend=c("Przemysł", "Usługi", "Razem"),
       col=c("red", "blue", "green"), lty = 1:3, cex=0.8)


# 9. Spróbuj utworzyć zmienną opisującą wiek firmy. Czy istnieje zależność
# między wiekiem firmy a np.labor share. Narysuj wykres rozrzutu (scatterplot). 
# Zrób takie wykresy również dla innych zmiennych.
class(firms$year_inc)
# zamień character na numeric
firms$year_inc_numeric = as.numeric(firms$year_inc)
class(firms$year_inc_numeric)
dane = subset(firms, year_inc_numeric>0)
# wyrzuć absurdlane wartości labor share
dane = subset(dane, individual_labor_share <= 1)
dane$age = dane$year - dane$year_inc_numeric

# wykres z użyciem pakietu ggplot
plot = ggplot(dane, aes(x=age, y=individual_labor_share)) +
       geom_point(alpha=0.5, colour="darkblue", size = 3)
plot
# nie widać wyraźniej zależności..

# 10. Korzystając z funkcji lm(), oszacuj funkcję produkcji. Co oznaczają współczynniki?
# potrzebujemy logarytmy
firms$lnY = log(firms$AV)
firms$lnL = log(firms$EMPL)
firms$lnK = log(firms$TOAS)
firms = drop_na(firms, any_of( c("lnY") ) ) 

# dla całego zbioru
prod.fun = lm(lnY ~ lnK + lnL, firms)

# zbiór danych dla przemysłu
firms_m = subset(firms, industry_factor=="manufacturing")
prod.fun.manufacturing = lm(lnY ~ lnK + lnL, firms_m)

# zbiór danych dla usług
firms_s = subset(firms, industry_factor=="services")
prod.fun.services = lm(lnY ~ lnK + lnL, firms_s)

# pokaż wyniki
prod.fun$coefficients
prod.fun.manufacturing$coefficients
prod.fun.services$coefficients
