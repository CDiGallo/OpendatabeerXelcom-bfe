library(ggplot2)
library(tidyverse)


# Elcom -------------------------------------------------------------------



df <- read.csv("Median Strompreis per Kanton.csv", encoding = "UTF-8")




df <- df %>% filter(Verbrauchsprofile.typischer.Haushalte == "H4")
df


df2 <- df %>% group_by(Periode, Kanton) %>% count() 
df2

df2 <- df2 %>% filter(n==1)

df3 <- left_join(df,df2, by=c("Periode", "Kanton"))
df3$n <- ifelse(is.na(df3$n), 2, df3$n)


df3
df3$Produkt <-  ifelse(df3$n==1, "Standardprodukt", df3$Produkt)
df3



df3 <- df3 %>% filter(Produkt == "Standardprodukt" &Periode >2014 & Periode <2022 )

df3 <- df3 %>% mutate(variable_kosten = Energielieferungkosten+Netznutzung)



# BFE-PV ------------------------------------------------------------------


pv <- read.csv("Einmalvergütung für Photovoltaikanlagen.csv", encoding = "UTF-8")

pv <- pv %>% filter(Jahr.der.Vergütung..yr.>2014) #pv 2014 ist praktisch nicht existent

pv <- pv %>% mutate(Periode = Jahr.der.Vergütung..yr.)


# rudimentary plot --------------------------------------------------------


df_joined <- left_join(df3, pv, by=c("Periode", "Kanton"))#, "Kanton=Kanton"))
df_joined$Leistung.in.kW.pro.100000.Einwohner <-as.integer(df_joined$Leistung.in.kW.pro.100000.Einwohner) 
df_joined$Anzahl.geförderter.EIV.Anlagen.pro.100000.Einwohner <- as.integer(df_joined$Anzahl.geförderter.EIV.Anlagen.pro.100000.Einwohner)


e <- ggplot(df_joined, aes(Leistung.in.kW.pro.100000.Einwohner,Total.exkl..MWST))
e + geom_point()

e <- ggplot(df_joined, aes(df_joined$Anzahl.geförderter.EIV.Anlagen.pro.100000.Einwohner,Total.exkl..MWST))
e + geom_point()


dfjoined$Energielieferungkosten
e <- ggplot(df_joined, aes(Leistung.in.kW.pro.100000.Einwohner,Energielieferungkosten))
e + geom_point()


#Ok, this does not work. Problem is, maaaaaaaaybeeeee: dass alles eigentlihc nur mit dem Jahr korreliert. ich muss also innerhalb eines Jahres schauen

df_joined$Leistung.in.kW.pro.100000.Einwohner
df_joined %>% group_by(Periode) %>% summarise(mean(Leistung.in.kW.pro.100000.Einwohner))

df_norming <-  df_joined %>% group_by(Periode) %>% summarise( mean_kwh=mean(Leistung.in.kW.pro.100000.Einwohner), sd_kwh=sd(Leistung.in.kW.pro.100000.Einwohner), mean_cost=mean(Total.exkl..MWST), sd_cost=sd(Total.exkl..MWST))

df_joined <- left_join(df_joined, df_norming, by="Periode")

df_joined <- df_joined %>% mutate(normed_kwh= (Leistung.in.kW.pro.100000.Einwohner-mean_kwh)/sd_kwh, normed_cost=(Total.exkl..MWST-mean_cost)/sd_cost)




dfjoined$Energielieferungkosten
e <- ggplot(df_joined, aes(normed_kwh,normed_cost))
e + geom_point()


#still nothing


# Trying with a year lagged -----------------------------------------------





df <- read.csv("Median Strompreis per Kanton.csv", encoding = "UTF-8")




df <- df %>% filter(Verbrauchsprofile.typischer.Haushalte == "H4")
df


df2 <- df %>% group_by(Periode, Kanton) %>% count() 
df2

df2 <- df2 %>% filter(n==1)

df3 <- left_join(df,df2, by=c("Periode", "Kanton"))
df3$n <- ifelse(is.na(df3$n), 2, df3$n)


df3
df3$Produkt <-  ifelse(df3$n==1, "Standardprodukt", df3$Produkt)
df3


df3$Periode <- df3$Periode-1  #############################################################here ist the CHANGE
df3 <- df3 %>% filter(Produkt == "Standardprodukt" &Periode >2014 & Periode <2022 )

df3 <- df3 %>% mutate(variable_kosten = Energielieferungkosten+Netznutzung)



# BFE-PV ------------------------------------------------------------------


pv <- read.csv("Einmalvergütung für Photovoltaikanlagen.csv", encoding = "UTF-8")

pv <- pv %>% filter(Jahr.der.Vergütung..yr.>2014) #pv 2014 ist praktisch nicht existent

pv <- pv %>% mutate(Periode = Jahr.der.Vergütung..yr.)


# rudimentary plot --------------------------------------------------------


df_joined <- left_join(df3, pv, by=c("Periode", "Kanton"))#, "Kanton=Kanton"))
df_joined$Leistung.in.kW.pro.100000.Einwohner <-as.integer(df_joined$Leistung.in.kW.pro.100000.Einwohner) 
df_joined$Anzahl.geförderter.EIV.Anlagen.pro.100000.Einwohner <- as.integer(df_joined$Anzahl.geförderter.EIV.Anlagen.pro.100000.Einwohner)
df_joined

e <- ggplot(df_joined, aes(Leistung.in.kW.pro.100000.Einwohner,Total.exkl..MWST))
e + geom_point()

e <- ggplot(df_joined, aes(Anzahl.geförderter.EIV.Anlagen.pro.100000.Einwohner,Total.exkl..MWST))
e + geom_point()


e <- ggplot(df_joined, aes(Leistung.in.kW.pro.100000.Einwohner,Energielieferungkosten))
e + geom_point()  ##### Mit ganz viel Fantasie könnte es hier etwas zu sehen geben


#Ok, this does not work. Problem is, maaaaaaaaybeeeee: dass alles eigentlihc nur mit dem Jahr korreliert. ich muss also innerhalb eines Jahres schauen

df_joined$Leistung.in.kW.pro.100000.Einwohner
df_joined %>% group_by(Periode) %>% summarise(mean(Leistung.in.kW.pro.100000.Einwohner))

df_norming <-  df_joined %>% group_by(Periode) %>% summarise( mean_kwh=mean(Leistung.in.kW.pro.100000.Einwohner), sd_kwh=sd(Leistung.in.kW.pro.100000.Einwohner), mean_cost=mean(Total.exkl..MWST), sd_cost=sd(Total.exkl..MWST))

df_joined <- left_join(df_joined, df_norming, by="Periode")

df_joined
df_joined <- df_joined %>% mutate(normed_kwh= (Leistung.in.kW.pro.100000.Einwohner-mean_kwh)/sd_kwh, normed_cost=(Total.exkl..MWST-mean_cost)/sd_cost)

df_joined


e <- ggplot(df_joined, aes(normed_kwh,normed_cost, color=Periode))
e + geom_point()
