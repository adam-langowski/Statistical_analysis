# Statistical_analysis
 Analysis of blood count results of independent groups 

•	Uruchomienie programu z trybu wsadowego (BATCH MODE):
1)	Poleceniem ‘cd wybrany_Dysk: ‘ wybieramy dysk z pobranym projektem 
2)	Poleceniem ‘cd sciezka_do_pliku’ ustalamy dokładny adres, w którym znajduje się projekt i plik Projekt_skrypt.R
3)	Będąc już w odpowiednim katalogu – w którym znajdują się pliki Projekt_skrypt.R oraz przykladoweDane-Projekt.csv (to plik z naszymi danymi medycznymi) wpisujemy polecenie:
    "C:\Program Files\R\R-4.2.0\bin\R.exe" CMD BATCH --vanilla "--args przykladoweDane-Projekt.csv" Projekt_skrypt.R

Należy zwrócić uwagę na wersję R – 4.2.0. 
Plik przykladoweDane-Projekt.csv jest argumentem przekazywanym do programu.

   W efekcie poprawnego zadziałania programu w folderze ze skryptem pojawiają się pliki: 

•	plik Projekt_skrypt.Rout (który możemy odczytać dowolnym edytorem tekstowym) – przechowuje on wszystkie wyniki działania programu oraz 

•	plik Rplots.pdf prezentujący wszystkie wykresy wykonane na potrzeby analizy danych
 

*Konieczne biblioteki zainstalowane w R:
library(dplyr), library(Hmisc), library(ggplot2), library(dplyr), library(e1071), library(ggpubr), library(car), library(FSA)
