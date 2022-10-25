## Introduction to R ##
install.packages("wooldridge")
install.packages("rio")

# wyczyść środowisko
rm(list = ls())

# R jako kalkulator
1+1
5*(4-1)^2
sqrt( log(10) )

# utwórz obiekt (bez wyświetlania):
x <- 5

# wyświetl x & x^2: 
x
x^2

# stwórz obiekty y & z oraz wyświetl jes używając ():
(y <- 3)
(z <- y^x)

## wektory:
(a <- c(1,2,3,4,5,6))
(b <- a+1)
(c <- a+b)
(d <- b*c)
sqrt(d)

## Funkcje wektorowe
# utwórz wektor
(a <- c(7,2,6,9,4,1,3))

# podstawowe funkcje:
sort(a)
length(a)
min(a)
max(a)
sum(a)
prod(a)

# tworzenie wektorów specjalnych:
numeric(20)
rep(1,20)
seq(50)
5:15
seq(4,20,2)

## Wektory z wartoścami logicznymi
#
0 == 1
0 < 1

#
( a <- c(7,2,6,9,4,1,3) )
( b <- a<3 | a>=6 )

#
x <- c(3,2,2,3,1,2,3,2,1,2)
xf <- factor(x, labels=c("bad","okay","good")) 
x
xf

## Etykiety
# stwórz wektor "avgs":
avgs <- c(.366, .358, .356, .349, .346)

# stwórz wektor z etykietami:
players <- c("Cobb","Hornsby","Jackson","O'Doul","Delahanty")

# przypisz nazwy do wartości:
names(avgs) <- players
avgs

## odwoływanie się do wektora:
# uzywająć numerów:
avgs[2]
avgs[1:4]

# używając nazw:
avgs["Jackson"]

# odwołanie logiczne:
avgs[ avgs>=0.35 ]

## Macierze
# utwórz macierz z wektora:
v <- c(2,-4,-1,5,7,0)
( A <- matrix(v,nrow=2) )

# utwórz macierz A z dwóch wektorów (jako rzędy):
row1 <- c(2,-1,7); row2 <- c(-4,5,0)
( A <- rbind(row1, row2) )

# utwórz macierz A z trzech wektorów (jako kolumny):
col1 <- c(2,-4); col2 <- c(-1,5); col3 <- c(7,0)
( A <- cbind(col1, col2, col3) )

# nazywanie rzędów i kolumn:
colnames(A) <- c("Alpha","Beta","Gamma")
rownames(A) <- c("Aleph","Bet") 
A

# macierz diagonalna i jednostkowa: 
diag( c(4,2,6) )
diag( 3 )

# odwoływanie się do wartości macierzy:
A[2,1]
A[,2]
A[,c(1,3)]

## Operatory macierzowe
A <- matrix( c(2,-4,-1,5,7,0), nrow=2)
B <- matrix( c(2,1,0,3,-1,5), nrow=2)
A
B
A*B

# transpozycja:
(C <- t(B) )

# mnożenie macierzowe:
(D <- A %*% C )

# odwracanie macierzy:
solve(D)

## Listy
# stwórz listę:
mylist <- list( A=seq(8,36,4), this="that", idm = diag(3))

# wyświetl listę: 
mylist

# wyświetl nazwy elementów listy:
names(mylist)

# wyświetl komponent listy "A":
mylist$A


### Ramki danych - Data frames
# stórzmy ramkę danych z wektorów:
year <- c(2008,2009,2010,2011,2012,2013)
product1<-c(0,3,6,9,7,8)
product2<-c(1,2,3,5,9,6)
product3<-c(2,4,4,2,3,2)

sales_mat <- cbind(product1,product2,product3)
rownames(sales_mat) <- year
sales_mat

# ramka danych:
sales <- as.data.frame(sales_mat)
sales

# odwoływanie się do zmiennej:
sales$product2

# stwórz nową zmienną w ramce danych 'sales':
sales$totalv1 <- sales$product1 + sales$product2 + sales$product3 

# to samo tylko, używająć "with":
sales$totalv2 <- with(sales, product1+product2+product3)

# to samo używająć "attach":
attach(sales)
sales$totalv3 <- product1+product2+product3
detach(sales)

# wynik:
sales

# cała ramka danych
sales

# Subset: all years in which sales of product 3 were >=3
subset(sales, product3>=3)

# zapisz ramkę danych jako plik RData
save(sales, file = "oursalesdata.RData")

# usuń z pamięci ramkę danych "sales"
rm(sales)

# czy zmienna "sales" istnieje?
exists("sales")

# wczytaj dane
load("oursalesdata.RData")

# czy "sales" istnieje?
exists("sales")
sales

# średnia ze wszytkich zmiennych:
colMeans(sales)

# dane affairs wczytywane z różnych żródeł
# Version 1: from package. make sure to install.packages(wooldridge)
data(affairs, package='wooldridge')

# Version 4: directly load from internet
affairs4 <- rio::import("http://fmwww.bc.edu/ec-p/data/wooldridge/affairs.dta")

# Compare, e.g. avg. value of naffairs:
mean(affairs$naffairs)
mean(affairs4$naffairs)
