library(tidyverse)
library(ggplot2)



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



df3 <- df3 %>% filter(Produkt == "Standardprodukt")




#What the fuck am I doint? Also es gibt nicht immer überall STandart oder günstigstes Produkt. Ich möchte aber von 2011 weg die Daten haben. Also muss ich immer das günstigste nehmen.
# es kann aber sein, dass es nur ein Standardprodutk gibt und kein günstigstes. Also filter ich, bei welchen jahrkantonen nur ein eintrag hat. also nicht günstig und standard.
# Dann mache ich dass es dort immer günstigstes steht. weilw wenn es nur eines gibt, ist es das günstigste per default. 
# Jetzt gibt es ein problem , dass die antwort auf test mit NA die antwort immer na ist. darum muss das zuesrt gefixt werden.

#Zuerst die Energielieferungskosten
t <- ggplot(df3, aes( Periode,Energielieferungkosten)) + geom_point() 

  


t + facet_wrap(vars(Kanton))#, cols = vars(fl))



#Dann alles zusammen



t <- ggplot(df3, aes( Periode,Total.exkl..MWST, color = Total.exkl..MWST)) + geom_point() +   scale_colour_gradient(
  low = "blue", 
  #mid = "black", 
  high = "#FF3300") 

t <- t+ xlab("Jahr (2011-2023)") +ylab("Totalpreis (Rp./KWh)")


t + facet_wrap(vars(Kanton)) +theme_light()  + theme(axis.text.x=element_text(angle = 45, vjust = 0.5))#, cols = vars(fl))




#t + facet_grid(vars(Kanton)) +theme_light()  + theme(axis.text.x=element_text(angle = 45, vjust = 0.5))#, cols = vars(fl))

