dane1 <- read.csv("Lessthan_5_victim_count.csv")
dane2 <- read.csv("5_to_14_victim_count.csv")
dane3 <- read.csv("15_to_30_victim_count.csv")
dane4 <- read.csv("Highest_victim_count.csv")

#top 10 panstw gdzie jest najwiecej seryjnych mordercow ####
wszyscy<- rbind(dane1, dane2, dane3, dane4)
wszyscy$Licznik <- 1
kraje <- aggregate(wszyscy["Licznik"], wszyscy["Country"], sum)
colnames(kraje)<- c("Kraj", "Liczba_mordercow")
topka<- kraje[order(kraje$Liczba_mordercow, decreasing = TRUE), ]
top10<-head(topka,15)

#Polscy mordercy ####
mordercy_polscy<- wszyscy[which(grepl("Poland", wszyscy$Country, ignore.case = TRUE)), ]
Polska<- mordercy_polscy[, c("Name", "Years.active", "Proven.victims",
                             "Possible.victims")]
#wykresy i tabele zapisane do pliku ####
library(stringi)
wszyscy$Pierwszy_rok <- as.numeric(stri_extract_first_regex(wszyscy$Years.active, "[0-9]{4}"))
mordercy<-wszyscy[!is.na(wszyscy$Pierwszy_rok),]
mordercy$Licznik1<-1
wykres_dane<- aggregate(mordercy$Licznik1, by = list(Rok = wszyscy$Pierwszy_rok),
                         FUN = sum)
colnames(wykres_dane)<-c("Rok", "Liczba")
wykres_dane<-wykres_dane[order(wykres_dane$Rok),]
png("Wykres_mordercow.png", width = 800, height = 600)
plot(wykres_dane$Rok, wykres_dane$Liczba,
     type = "l",
     lwd = 3,
     col = "blue",
     main = "Liczba seryjnych morderców na prestrzeni lat",
     xlab = "Rok pierwszego morderstwa",
     ylab = "Liczba morderców")
grid()
dev.off()
#zapis danych do tabelki:
library(writexl)
write_xlsx(topka, "Top_Panstwa.xlsx")

# top10 nazwisk ####
wszyscy$ofiary <- as.numeric(stri_extract_first_regex(wszyscy$Proven.victims, "[0-9]+"))
mordercy_liczby<-wszyscy[!is.na(wszyscy$ofiary), ]
top_mordercy<-mordercy_liczby[order(mordercy_liczby$ofiary, decreasing = TRUE), ]
top10_nazwisk <- head(top_mordercy[, c("Name", "Country", "ofiary")], 10)
#zapisuje kolejny plik
write_xlsx(top10_nazwisk, "Top_10_Mordercow.xlsx")
png("Wykres_Top10_Mordercow.png", width = 800, height = 600)
par(mar = c(12, 5, 4, 2))
barplot(top10_nazwisk$ofiary,
        names.arg = top10_nazwisk$Name,
        las = 2,
        col = "red",
        main = "Top 10 morderców z największą liczbą udowodnionch zabójstw",
        ylab = "Liczba wykazanych ofiar")
dev.off()
#metody zabijania ####
(uduszenia <- sum(grepl("strangl|choke|suffocat", wszyscy$Notes, ignore.case = TRUE)))
(zastrzelenia<- sum(grepl("shoot|shot|gun|rifle", wszyscy$Notes, ignore.case = TRUE)))
(zanozowanie<-sum(grepl("knife|stab|slit", wszyscy$Notes, ignore.case = TRUE)))
(zatrucia<-sum(grepl("poison|arsenic|cyanide", wszyscy$Notes, ignore.case = TRUE)))
(narzedzia<-sum(grepl("bludgeon|hammer|rock|brick|beat", wszyscy$Notes, ignore.case = TRUE)))

metody<-data.frame(Metoda = c("Uduszenie", "Zastrzelenie", "Zanożowanie", "Zatrucie", "Użycie narzedzi tępych"),
                   Liczba_Mordercow = c(uduszenia, zastrzelenia, zanozowanie, zatrucia, narzedzia))
metody<-metody[order(metody$Liczba_Mordercow, decreasing = TRUE),]
write_xlsx(metody, "Najpopularniejsze_metody_zabijania.xlsx")
png("Wykres_Metod.png", width = 800, height = 600)
par(mar = c(12, 5, 4, 2))
barplot(metody$Liczba_Mordercow,
        names.arg = metody$Metoda,
        las = 2,
        col = "green",
        main = "Najchętniej używane metody",
        ylab = "Liczba wykazanych ofiar")
dev.off()
#mistrzowie w uciekaniu przed sprawiedliwoscia
wszyscy$Ostatni_rok<- as.numeric(stri_extract_last_regex(wszyscy$Years.active, "[0-9]{4}"))
wszyscy$Lata_Bez_Kary<- wszyscy$Ostatni_rok-wszyscy$Pierwszy_rok + 1
czas<- wszyscy[!is.na(wszyscy$Lata_Bez_Kary), ]
rekordzisci<-czas[order(czas$Lata_Bez_Kary, decreasing = TRUE), ]
bezkarni<-head(rekordzisci[, c("Name", "Pierwszy_rok", "Ostatni_rok", "Lata_Bez_Kary")], 10)
write_xlsx(bezkarni, "Najdluzsza_kariera.xlsx")
png("Wykres_Bezkarni.png", width = 800, height = 600)
par(mar = c(12, 5, 4, 2))
barplot(bezkarni$Lata_Bez_Kary,
        names.arg = bezkarni$Name,
        las = 2,
        col = "pink",
        main = "Najdłuższe kariery",
        ylab = "Lata bezkarności")
dev.off()
