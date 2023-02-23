


library(tidyverse)
library(ggplot2)





df <- read.csv("Median Strompreis per Kanton.csv", encoding = "UTF-8")




df <- df %>% filter(Verbrauchsprofile.typischer.Haushalte == "H4") #Choose the standar Houshold
df


df2 <- df %>% group_by(Periode, Kanton) %>% count() #the next steps are needed, because for some years we only have "standardprodukt" and sometimes we have standard and "günstigstes". We want the Standardprodukt, because it is what most people actually pay, except if there is only data for the cheapest product ( mostly pre 2020)
df2$n

df2 <- df2 %>% filter(n==1)
df2

df3 <- left_join(df,df2, by=c("Periode", "Kanton")) #joining the other columns in
df3$n <- ifelse(is.na(df3$n), 2, df3$n) 
df3$n

df3
df3$Produkt <-  ifelse(df3$n==1, "Standardprodukt", df3$Produkt)
df3


df3$Periode <- df3$Periode-1  ############################################################# To make it lagged, the Year of the Strompreis is reduced. We will combine the Strompreis of say 2013 with the amount of PV-built in 2014
df3 <- df3 %>% filter(Produkt == "Standardprodukt" &Periode >2014 & Periode <2022 )

df3 <- df3 %>% mutate(variable_kosten = Energielieferungkosten+Netznutzung)



# BFE-PV ------------------------------------------------------------------


pv <- read.csv("Einmalvergütung für Photovoltaikanlagen.csv", encoding = "UTF-8")

pv <- pv %>% filter(Jahr.der.Vergütung..yr.>2014) #pv 2014 ist praktisch nicht existent/only for certain cantons, doesn't make much sense

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


e <- ggplot(df_joined, aes(normed_kwh,normed_cost, color=Periode, size= 19))
e + geom_point() + xlab("Neu isntallierte Leistung in KWh pro 100000 Einwohnenden, standardisiert") +ylab("standardisierter Totalpreis (Rp./KWh)") + ggtitle("standarisierte total Lieferpreise und installierte Leistung")


t + facet_wrap(vars(Kanton)) +theme_light()  + theme(axis.text.x=element_text(angle = 45, vjust = 0.5))#, cols = vars(fl))







#Nach Lieferpreise standardisieren

df_joined$Leistung.in.kW.pro.100000.Einwohner
df_joined %>% group_by(Periode) %>% summarise(mean(Leistung.in.kW.pro.100000.Einwohner))

df_norming <-  df_joined %>% group_by(Periode) %>% summarise( mean_kwh=mean(Leistung.in.kW.pro.100000.Einwohner), sd_kwh=sd(Leistung.in.kW.pro.100000.Einwohner), mean_cost=mean(Energielieferungkosten), sd_cost=sd(Energielieferungkosten))

df_joined <- left_join(df_joined, df_norming, by="Periode")

df_joined
df_joined <- df_joined %>% mutate(normed_kwh= (Leistung.in.kW.pro.100000.Einwohner-mean_kwh)/sd_kwh, normed_cost=(Total.exkl..MWST-mean_cost)/sd_cost)

df_joined


e <- ggplot(df_joined, aes(normed_kwh,normed_cost, color=Periode, size=19))
e + geom_point() + xlab("Neu installierte  Leistung in KWh pro 100000 Einwohnenden, standardisiert") +ylab("Energielieferpreis (Rp./KWh), standardisiert") +ggtitle("Lieferpreise und installierte Leistung") #+geom_smooth()



df_joined$Leistung.in.kW.pro.100000.Einwohner
#corelation


fit <- lm(  Leistung.in.kW.pro.100000.Einwohner ~ Periode + Total.exkl..MWST , data=df_joined)
summary(fit) # show results
