### Tryb wsadowy 

arg = commandArgs(trailingOnly = TRUE)
dane <- read.csv2(file = arg, sep = ";")

# Wczytanie pliku z danymi medycznymi
#dane <- read.csv2('przykladoweDane-Projekt.csv', sep=';')
View(dane)

# Określenie liczby porównywanych grup
grupy <- c(table(dane$grupa))
grupy <- c(names(grupy))                # NAZWY WSZYSTKICH ANALIZOWANYCH GRUP

# Określenie kompletności danych wejściowych

library(dplyr)
podsumowanie <- group_by(dane, grupa) %>%
  summarise(
    liczność_grupy = n(),
    sredni_wiek = format(round(mean(wiek , na.rm = TRUE), 2) , nsmall=2)
  )
print(podsumowanie)

liczba_badanych <- c(dane %>% group_by(grupa) %>% summarise(count = n()))
liczba_badanych <- liczba_badanych["count"]    # 25 25 25

x <- 1    # indeks początkowy
size <- 0 # rozmiar początkowy grupy 
wielkosci_kolejnych_grup <- liczba_badanych
library(Hmisc)

### Znalezienie wartości NA oraz dodanie nowej wartości na podstawie średniej dla grupy

for (i in 1:length(grupy))
{
  print(i)
  size <- size + wielkosci_kolejnych_grup$count[i]
  print(paste("Brakujące dane dla grupy", i, ":"))
  
  braki_hsCRP <- which(is.na(dane$hsCRP[x:size]))
  print(paste(c("Brak wyniku hsCRP dla indeksu: ", braki_hsCRP), collapse=" "))
  dane$hsCRP[x:size][is.na(dane$hsCRP[x:size])] <- mean(dane$hsCRP[x:size], na.rm = TRUE)
  
  braki_Ery <- which(is.na(dane$ERY[x:size]))
  print(paste(c("Brak wyniku Ery dla indeksu: ", braki_Ery), collapse=" "))
  dane$ERY[x:size][is.na(dane$ERY[x:size])] <- mean(dane$ERY[x:size], na.rm = TRUE)
  
  braki_PLT <- which(is.na(dane$PLT[x:size]))
  print(paste(c("Brak wyniku PLT dla indeksu: ", braki_PLT), collapse=" "))
  dane$PLT[x:size][is.na(dane$PLT[x:size])] <- mean(dane$PLT[x:size], na.rm = TRUE)
  
  braki_HGB <- which(is.na(dane$HGB[x:size]))
  print(paste(c("Brak wyniku HGP dla indeksu: ", braki_HGB), collapse=" "))
  dane$HGB[x:size][is.na(dane$HGB[x:size])] <- mean(dane$HGB[x:size], na.rm = TRUE)
  
  braki_HCT <- which(is.na(dane$HCT[x:size]))
  print(paste(c("Brak wyniku HCT dla indeksu: ", braki_HCT), collapse=" ")) 
  dane$HCT[x:size][is.na(dane$HCT[x:size])] <- mean(dane$HCT[x:size], na.rm = TRUE)
  
  braki_MCHC <- which(is.na(dane$MCHC[x:size]))
  print(paste(c("Brak wyniku MCHC dla indeksu: ", braki_MCHC), collapse=" "))
  dane$MCHC[x:size][is.na(dane$MCHC[x:size])] <- mean(dane$MCHC[x:size], na.rm = TRUE)
  
  braki_MON <- which(is.na(dane$MON[x:wielkosci_kolejnych_grup$count[i]]))
  print(paste(c("Brak wyniku MON dla indeksu: ", braki_MON), collapse=" "))
  dane$MON[x:size][is.na(dane$MON[x:size])] <- mean(dane$MON[x:size], na.rm = TRUE)
  
  braki_LEU <- which(is.na(dane$LEU[x:size]))
  print(paste(c("Brak wyniku LEU dla indeksu: ", braki_LEU), collapse=" "))
  dane$LEU[x:size][is.na(dane$LEU[x:size])] <- mean(dane$LEU[x:size], na.rm = TRUE)
  
  x <- x + wielkosci_kolejnych_grup$count[i]
  cat("\n")
}
which(is.na(dane)) # integer(0) -> dane kompletne

### Identyfikacja wartości odstających przy pomocy boxplot'a ###

library(ggplot2)
library(dplyr)

boxplot(dane$hsCRP ~ dane$grupa,
        main=paste("Rozkład wyników hsCRP"),
        xlab="hsCRP", ylab="wynik",
        col="orange",
        border="brown"
)
stripchart(dane$hsCRP ~ dane$grupa, vertical = TRUE, method = "jitter",
           pch = 16, add = TRUE, col = "blue")

boxplot(dane$ERY ~ dane$grupa,
        main=paste("Rozkład wyników ERY"),
        xlab="ERY", ylab="wynik",
        col="orange",
        border="brown"
)
stripchart(dane$ERY ~ dane$grupa, vertical = TRUE, method = "jitter",
           pch = 16, add = TRUE, col = "blue")

boxplot(dane$PLT ~ dane$grupa,
        main=paste("Rozkład wyników PLT"),
        xlab="PLT", ylab="wynik",
        col="orange",
        border="brown"
)
stripchart(dane$PLT ~ dane$grupa, vertical = TRUE, method = "jitter",
           pch = 16, add = TRUE, col = "blue")

boxplot(dane$HGB ~ dane$grupa,
        main=paste("Rozkład wyników HGB"),
        xlab="HGB", ylab="wynik",
        col="orange",
        border="brown"
)
stripchart(dane$HGB ~ dane$grupa, vertical = TRUE, method = "jitter",
           pch = 16, add = TRUE, col = "blue")

boxplot(dane$HCT ~ dane$grupa,
        main=paste("Rozkład wyników HCT"),
        xlab="HCT", ylab="wynik",
        col="orange",
        border="brown"
)
stripchart(dane$HCT ~ dane$grupa, vertical = TRUE, method = "jitter",
           pch = 16, add = TRUE, col = "blue")

boxplot(dane$MCHC ~ dane$grupa,
        main=paste("Rozkład wyników MCHC"),
        xlab="hsCRP", ylab="wynik",
        col="orange",
        border="brown"
)
stripchart(dane$MCHC ~ dane$grupa, vertical = TRUE, method = "jitter",
           pch = 16, add = TRUE, col = "blue")

boxplot(dane$MON ~ dane$grupa,
        main=paste("Rozkład wyników MON"),
        xlab="MON", ylab="wynik",
        col="orange",
        border="brown"
)
stripchart(dane$MON ~ dane$grupa, vertical = TRUE, method = "jitter",
           pch = 16, add = TRUE, col = "blue")

boxplot(dane$LEU ~ dane$grupa,
        main=paste("Rozkład wyników LEU"),
        xlab="LEU", ylab="wynik",
        col="orange",
        border="brown"
)
stripchart(dane$LEU ~ dane$grupa, vertical = TRUE, method = "jitter",
           pch = 16, add = TRUE, col = "blue")


### Statystyka opisowa dla wartości ilościowych ###

library(dplyr)
library(e1071)   

podsumowanie_hsCRP <- group_by(dane, grupa) %>%
  summarise(
    minimum = min(hsCRP),
    maximum = max(hsCRP),
    średnia = mean(hsCRP),
    mediana = median(hsCRP),
    odchylenie_std = sd(hsCRP),
    iqr = IQR(hsCRP),
    wariancja = var(hsCRP),
    kurtoza = kurtosis(hsCRP)
  )
View(podsumowanie_hsCRP)

podsumowanie_ERY <- group_by(dane, grupa) %>%
  summarise(
    minimum = min(ERY),
    maximum = max(ERY),
    średnia = mean(ERY),
    mediana = median(ERY),
    odchylenie_std = sd(ERY),
    iqr = IQR(ERY),
    wariancja = var(ERY),
    kurtoza = kurtosis(ERY)
  )
View(podsumowanie_ERY)

podsumowanie_PLT <- group_by(dane, grupa) %>%
  summarise(
    minimum = min(PLT),
    maximum = max(PLT),
    średnia = mean(PLT),
    mediana = median(PLT),
    odchylenie_std = sd(PLT),
    iqr = IQR(PLT),
    wariancja = var(PLT),
    kurtoza = kurtosis(PLT)
  )
View(podsumowanie_PLT)

podsumowanie_HGB <- group_by(dane, grupa) %>%
  summarise(
    minimum = min(HGB),
    maximum = max(HGB),
    średnia = mean(HGB),
    mediana = median(HGB),
    odchylenie_std = sd(HGB),
    iqr = IQR(HGB),
    wariancja = var(HGB),
    kurtoza = kurtosis(HGB)
  )
View(podsumowanie_HGB)

podsumowanie_HCT <- group_by(dane, grupa) %>%
  summarise(
    minimum = min(HCT),
    maximum = max(HCT),
    średnia = mean(HCT),
    mediana = median(HCT),
    odchylenie_std = sd(HCT),
    iqr = IQR(HCT),
    wariancja = var(HCT),
    kurtoza = kurtosis(HCT)
  )
View(podsumowanie_HCT)

podsumowanie_MCHC<- group_by(dane, grupa) %>%
  summarise(
    minimum = min(MCHC),
    maximum = max(MCHC),
    średnia = mean(MCHC),
    mediana = median(MCHC),
    odchylenie_std = sd(MCHC),
    iqr = IQR(MCHC),
    wariancja = var(MCHC),
    kurtoza = kurtosis(MCHC)
  )
View(podsumowanie_MCHC)

podsumowanie_MON <- group_by(dane, grupa) %>%
  summarise(
    minimum = min(MON),
    maximum = max(MON),
    średnia = mean(MON),
    mediana = median(MON),
    odchylenie_std = sd(MON),
    iqr = IQR(MON),
    wariancja = var(MON),
    kurtoza = kurtosis(MON)
  )
View(podsumowanie_MON)

podsumowanie_LEU <- group_by(dane, grupa) %>%
  summarise(
    minimum = min(LEU),
    maximum = max(LEU),
    średnia = mean(LEU),
    mediana = median(LEU),
    odchylenie_std = sd(LEU),
    iqr = IQR(LEU),
    wariancja = var(LEU),
    kurtoza = kurtosis(LEU)
  )
View(podsumowanie_LEU)


### Funkcje do analizy statystycznej ###

  # rozklad normalny
rozklad_normalny <- function(numer, wybrane_dane, grupy){   
  p.val <- c()
  for(i in 1:length(grupy)){
    grupa <- with(wybrane_dane, wybrane_dane[grupa == grupy[i],])
    result <- shapiro.test(grupa[,numer])           # test Shapiro-Wilka
    p.val <- append(p.val, result[["p.value"]], after = length(p.val))
  } 
  return(p.val)
}

  # graficzna ocena zgodnosci z rozkladem normalnym
#install.packages("ggpubr", repos = "https://cloud.r-project.org/")
library(ggpubr)
#install.packages("wesanderson")
#library(wesanderson)
graficzny_rozklad_normalny <- function(numer, wybrane_dane){  
  library(ggpubr)
  ggdensity(dane, x = names(wybrane_dane[numer]),
            title = "Graficzna ocena zgodnosci z rokladem normalnym",
            color = "grupa",
            fill = "grupa",
            pallete = c("orange","brown","blue"),
            ylab = "gestosc",
            xlab = names(wybrane_dane[numer])
  ) + facet_wrap(~ grupa, scales = "free")
}

#install.packages("car", repos = "https://cloud.r-project.org/")
library(car)
  # Ocena homogeniczności wariancji
jednorodnosc_wariancji <- function(numer, wybrane_dane){            #wydobycie p−value
  p.value <- leveneTest(wybrane_dane[,numer] ~ grupa, wybrane_dane)$"Pr(>F)"[1]
  return(p.value)
}


# Porównywanie grup (porównywanie średnich) - testy 
Test_Kruskala <- function(numer){
  p.value <- kruskal.test(dane[,numer] ~ grupa, dane)$p.value
  print(kruskal.test(dane[,numer] ~ grupa, dane))
  return(p.value)
}

#install.packages("FSA", repos = "https://cloud.r-project.org/")
library(FSA)
Test_Dunna <- function(numer){
  Tab_Dunna <- dunnTest(dane[,numer], dane$grupa)$res
  print(Tab_Dunna)
  for(i in 1:length(Tab_Dunna)){
    if(names(Tab_Dunna[i]) != "Comparison"){
      for(j in 1:nrow(Tab_Dunna[i])){
        if(Tab_Dunna[j,i] < 0.05){
          print(paste("Roznica miedzy", Tab_Dunna[j,1], "w parametrze '", names(Tab_Dunna[i]), "' jest istotna statycznie"))
        }
      }
    }
  }
}

Test_Anova <- function(numer){
  p.value <- summary(aov(dane[,numer] ~ grupa, dane))[[1]][["Pr(>F)"]][[1]]
  summary(aov(dane[,numer] ~ grupa, dane))
  return(p.value)
}

Test_Tukey <- function(numer){
  TukeyHSD(aov(dane[,numer] ~ grupa, dane))
  Tab_Tukey <- as.data.frame(TukeyHSD(aov(dane[,numer] ~ grupa, dane))$grupa)
  print(Tab_Tukey)
  for(i in 1:length(Tab_Tukey)){
    for(j in 1:nrow(Tab_Tukey)){
      if(Tab_Tukey[j,i] < 0.05){
        print(paste("Sa istotne roznice statyczne w parametrze", names(Tab_Tukey[i]), "w wierszu", j))
      }
    }
  }
}

### Testy dla >2 grup

cat("\n\n")
print("Porównanie kilku (>2) grup niezależnych")

numery_parametryczne_wiele <- c()
a <- 1
numery_nieparametryczne_wiele <- c()
b <- 1

if(length(grupy) > 2){
for(i in 1:length(dane)){
  if(names(dane[i]) != "grupa" && names(dane[i]) != "wiek"){
    if(is.numeric(dane[,i])){
      cat("\n\n")
      print("TESTY:")
      print(paste("Dla parametru", names(dane[i])))
      print(graficzny_rozklad_normalny(i, dane))
      print("Wartosci rozkladu normalnego") 
      print(rozklad_normalny(i, dane, grupy))
      if(sum(rozklad_normalny(i, dane, grupy) > 0.05) == length(grupy) && length(grupy) > 2){
        print(paste("Grupy sa zgodne z rozkladem normalnym"))
        if(jednorodnosc_wariancji(i, dane) > 0.05){
          print("Wystepuje jednorodnosc wariancji -> TEST ANOVA")
          numery_parametryczne_wiele[a] <- i
          a = a+1
          print(Test_Anova(i))
          if(Test_Anova(i) < 0.05){
            cat("\n")
            print("Test ANOVA wykryl, ze sa roznice miedzy tymi grupami -> TEST TUKEY")
            print("Test TUKEY:")
            Test_Tukey(i)
          }else{
            cat("\n")
            print("Test Anova wykryl, ze nie ma roznic miedzy tymi grupami")
          }
        }else{
          print("Nie wystepuje jednorodnosc wariancji -> TEST KRUSKALA")
          numery_nieparametryczne_wiele[b] <- i
          b = b+1
          print(Test_Kruskala(i))
          if(Test_Kruskala(i) < 0.05){
            print("Test KRUSKALA wykryl, ze sa roznice miedzy tymi grupami -> TEST DUNNA")
            print("Test DUNNA:")
            Test_Dunna(i)
          }else{
            print("Test KRUSKALA wykryl, ze nie ma roznic miedzy tymi grupami")
          }
        }
      }else{
        print(paste("Grupy nie sa zgodne z rozkladem normalnym -> TEST KRUSKALA"))
        print("Test Kruskala:")
        numery_nieparametryczne_wiele[b] <- i
        b = b+1
        print(Test_Kruskala(i))
        if(Test_Kruskala(i) < 0.05){
          cat("\n")
          print("Test KRUSKALA wykryl, ze sa roznice miedzy  grupami -> TEST DUNNA")
          print("Test DUNNA:")
          Test_Dunna(i)
        }else{
          cat("\n")
          print("Test KRUSKALA wykryl, ze nie ma roznic miedzy grupami")
        }
      }
    }
  }
}
}
cat("\n")

# Funkcje testów do analizy dokładnie 2 grup

Test_Wilcoxona <- function(numer){
  p.value <- wilcox.test(dane[,numer] ~ grupa, dane)$p.value
  print(wilcox.test(dane[,numer] ~ grupa, dane))
  return(p.value)
}

Test_Studenta <- function(numer){
  p.value <- t.test(dane[,numer] ~ grupa, dane, var.equal = TRUE)$p.value
  print(t.test(dane[,numer] ~ grupa, dane, var.equal = TRUE))
  return(p.value)
}

Test_Welcha <- function(numer){
  p.value <- t.test(dane[,numer] ~ grupa, dane, var.equal = FALSE)$p.value
  print(t.test(dane[,numer] ~ grupa, dane, var.equal = FALSE))
  return(p.value)
}


### Testy dla dokładnie 2 grup 

if(length(grupy) == 2){
 print("Porównanie dwóch grup niezależnych")
  numery_parametryczne_2 <- c()
  a <- 1
  numery_nieparametryczne_2 <- c()
  b <- 1

  for(i in 1:length(dane)){
    if(names(dane[i]) != "grupa" && names(dane[i]) != "wiek"){
      if(is.numeric(dane[,i])){
        cat("\n\n")
        print("TESTY:")
        print(paste("Dla parametru", names(dane[i])))
        print(graficzny_rozklad_normalny(i, dane))
      print("Wartosci rozkladu normalnego") 
      print(rozklad_normalny(i, dane, grupy))
      if(sum(rozklad_normalny(i, dane, grupy) > 0.05) == 2){
        print("Grupy sa zgodne z rozkladem normalnym")
        if(jednorodnosc_wariancji(i, dane) > 0.05){
          print("Wystepuje jednorodnosc wariancji")
          numery_parametryczne_2[a] <- i
          a = a+1
          print("Test T.STUDENTA")
          print(Test_Studenta(i))
          if(Test_Studenta(i) < 0.05){
            print("Test T-STUDENTA wykryl, ze sa znaczace roznice miedzy grupami")
          }else{
            print("Test T-STUDENTA wykryl, ze nie ma znaczacych roznic miedzy grupami")
          }
        }else{
          print("Nie wystepuje jednorodnosc wariancji")
          numery_nieparametryczne_2[b] <- i
          b = b+1
          print("Test WELCHA")
          print(Test_Welcha(i))
          if(Test_Welcha(i) < 0.05){
            print("Test WELCHA wykryl, ze sa znaczace roznice miedzy grupami")
          }else{
            print("Test WELCHA wykryl, ze nie ma znaczacych roznic miedzy grupami")
          }
        }
      }else{
        print("Grupy nie sa zgodne z rozkladem normalnym")
        numery_nieparametryczne_2[b] <- i
        b = b+1
        print("Test WILCOXONA")
        print(Test_Wilcoxona(i))
        if(Test_Wilcoxona(i) < 0.05){
          print("Test WILCOXONA wykryl, ze sa znaczace roznice miedzy grupami")
        }else{
          print("Test WILCOXONA wykryl, ze nie ma znaczych roznic miedzy grupami")
        }
      }
    }
  }
  }
}
#warnings()


### Analizy korelacji ###

# funkcje:

Graficznie_Korelacja <- function(dane, grupa, ktora_grupa, parametr1, parametr2, metoda){
  grupa <- dane %>% filter(grupa == grupy[ktora_grupa])
  ggscatter(grupa, x = names(grupa[parametr1]), y = names(grupa[parametr2]),
            title = paste("Wykres dla metody", metoda, "dla parametrow", names(grupa[parametr1]), "-", names(grupa[parametr2])),
            add = "reg.line", conf.int = TRUE,
            cor.coef = TRUE, cor.method = metoda,
            color = "blue", fill = "grupa",
            ylab = names(grupa[parametr1]),
            xlab = names(grupa[parametr2])
  )
}

#Graficznie_Korelacja(dane, grupy, 1, 4, 11, "spearman")
#Graficznie_Korelacja(dane, grupy, 2, 5, 7, "spearman")
#Graficznie_Korelacja(dane, grupy, 2, 6, 9, "spearman")

Korelacja_Spearmana <- function(dane, grupy, ktora_grupa, parametr1, parametr2){
  grupa <- dane %>% filter(grupa == grupy[ktora_grupa])
  print(paste("Grupa -", grupy[ktora_grupa], "| Parametry:", names(grupa[parametr1]), "-", names(grupa[parametr2])))
  wynik_p.value <- cor.test(grupa[[parametr1]], grupa[[parametr2]], method = "spearman")$p.value
  wynik_cor <- cor.test(grupa[[parametr1]], grupa[[parametr2]], method = "spearman")$estimate
  #if (wynik_p.value < 0.05){
  #  Graficznie_Korelacja(dane, grupy, grupa, parametr1, parametr2, "spearman")
  #}
  return(Wspolczynnik_korelacji(wynik_p.value, wynik_cor))
}

Korelacja_Pearsona <- function(dane, grupy, ktora_grupa, parametr1, parametr2){
  grupa <- dane %>% filter(grupa == grupy[ktora_grupa])
  print(paste("Grupa -", grupy[ktora_grupa], "| Parametry:", names(grupa[parametr1]), "-", names(grupa[parametr2])))
  wynik_p.value <- cor.test(grupa[[parametr1]], grupa[[parametr2]], method = "pearson")$p.value
  wynik_cor <- cor.test(grupa[[parametr1]], grupa[[parametr2]], method = "pearson")$estimate
  #if (wynik_p.value < 0.05){
  #  Graficznie_Korelacja(dane, grupy, grupa, parametr1, parametr2, "pearson")
  #}
  return(Wspolczynnik_korelacji(wynik_p.value, wynik_cor))
}

Wspolczynnik_korelacji <- function(p, r){
  print(paste("Wyniki:"))
  if(p < 0.05){
    print(paste("P.value =", p, "< 0.05 - Korelacja miedzy zmiennymi"))
    print(paste("Rodzaj korelacji:"))
    if(r > 0){
      print(paste("r =", r, "> 0 - Korelacja Dodatnia"))
    }else if(r == 0){
      print(paste("r =", r, "= 0 - Brak korelacji"))
    }else{
      print(paste("r =", r, "< 0 - Korelacja Ujemna"))
    }
    print(paste("Interpretacja sily korelacji:"))
    if(-1 < r && r < -0.7){
      print(paste(r, "- Bardzo silna korelacja ujemna"))
    }else if(-0.7 < r && r < -0.5){
      print(paste(r, "- Silna korelacja ujemna"))
    }else if(-0.5 < r && r < -0.3){
      print(paste(r, "- Korelacja ujemna o srednim stezeniu"))
    }else if(-0.3 < r && r < -0.2){
      print(paste(r, "- Slaba korelacja ujemna"))
    }else if(-0.2 < r && r < 0.2){
      print(paste(r, "- Brak korelacji"))
    }else if(0.2 < r && r < 0.3){
      print(paste(r, "- Slaba korelacja dodatnia"))
    }else if(0.3 < r && r < 0.5){
      print(paste(r, "- Korelacja dodatnia o srednim stezeniu"))
    }else if(0.5 < r && r < 0.7){
      print(paste(r, "- Silna korelacja dodatnia"))
    }else if(0.7 < r && r < 1){
      print(paste(r, "- Bardzo silna korelacja dodatnia"))
    }
  }else{
    print(paste("P.value =", p, ">= 0.05 - Brak korelacji miedzy zmiennymi"))
  }
}

### Testy korelacji dla >2 grup
cat("\n")
if(length(grupy)>2){
# TEST PEARSONA
print("TESTY PARAMETRYCZNE KORELACJI DLA WIECEJ NIZ 2 GRUP")
print("TEST PEARSONA")
start <- 2
for(i in 1:length(dane)-1){
  for(j in start:length(dane)){
    if(i != j && i-1 != j && is.numeric(dane[,i]) && is.numeric(dane[,j])){
      if( i %in% numery_parametryczne_wiele && j %in% numery_parametryczne_wiele){
        for(grupa in 1:length(grupy)){
          Korelacja_Pearsona(dane, grupy, grupa, i, j)
          cat("\n")
        }
      }
    }
  }
}

#TEST SPEARMANA
print("TESTY NIEPARAMETRYCZNE KORELACJI DLA WIECEJ NIZ 2 GRUP")
print("TEST SPEARMANA")
start <- 2
for(i in 1:length(dane)-1){
  for(j in start:length(dane)){
    if(i != j && i-1 != j && is.numeric(dane[,i]) && is.numeric(dane[,j])){
      if(i %in% numery_nieparametryczne_wiele || j %in% numery_nieparametryczne_wiele){
        for(grupa in 1:length(grupy)){
          Korelacja_Spearmana(dane, grupy, grupa, i, j)
          cat("\n")
        }
      }
    }
  }
}
}

### Testy korelacji dla dokładnie 2 grup

if(length(grupy) == 2){
# TEST PEARSONA
print("TESTY PARAMETRYCZNE KORELACJI DLA DOKLADNIE 2 GRUP")
print("TEST PEARSONA")
start <- 2
for(i in 1:length(dane)-1){
  for(j in start:length(dane)){
    if(i != j && i-1 != j && is.numeric(dane[,i]) && is.numeric(dane[,j]) && names(dane[i]) != "wiek"){
      if( i %in% numery_parametryczne_2 && j %in% numery_parametryczne_2){
        for(grupa in 1:length(grupy)){
          Korelacja_Pearsona(dane, grupy, grupa, i, j)
          cat("\n")
        }
      }
    }
  }
}

#TEST SPEARMANA
print("TESTY NIEPARAMETRYCZNE KORELACJI DLA DOKLADNIE 2 GRUP")
print("TEST SPEARMANA")
start <- 2
for(i in 1:length(dane)-1){
  for(j in start:length(dane)){
    if(i != j && i-1 != j && is.numeric(dane[,i]) && is.numeric(dane[,j]) && names(dane[i]) != "wiek"){
      if( i %in% numery_nieparametryczne_2 || j %in% numery_nieparametryczne_2){
      }else{
        for(grupa in 1:length(grupy)){
          Korelacja_Spearmana(dane, grupy, grupa, i, j)
          cat("\n")
        }
      }
    }
  }
}
}

print("Koniec analizy.")