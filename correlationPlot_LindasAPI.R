
# correlation plot ElComxPv-Einmalvergütungen -----------------------------


library(tidyverse)
library(ggplot2)

library(curl)
library(httr)
library(stringi)




# download directly from lindas -------------------------------------------



endpoint <- "https://lindas.admin.ch/query"
proxy_url <- curl::ie_get_proxy_for_url(endpoint)
proxy_config <- use_proxy(url=proxy_url)
query <- paste0("
PREFIX schema: <http://schema.org/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?Kanton ?Jahr ?Total ?Installierte_leistungpro_100k ?Produkt  #add here variables you wnat to download
WHERE {
<https://energy.ld.admin.ch/elcom/electricityprice-canton> <https://cube.link/observationSet> ?observationSet0 .
  ?observationSet0 <https://cube.link/observation> ?source0 .
  ?source0 <https://energy.ld.admin.ch/elcom/electricityprice/dimension/canton> ?kanton .
  ?source0 <https://energy.ld.admin.ch/elcom/electricityprice/dimension/category> ?kategorie .
  ?source0 <https://energy.ld.admin.ch/elcom/electricityprice/dimension/period> ?Jahr .
  ?source0 <https://energy.ld.admin.ch/elcom/electricityprice/dimension/product> ?Produkt .
  ?source0 <https://energy.ld.admin.ch/elcom/electricityprice/dimension/total> ?Total .
  ?source0 <https://energy.ld.admin.ch/elcom/electricityprice/dimension/gridusage> ?gridusage .
  ?source0 <https://energy.ld.admin.ch/elcom/electricityprice/dimension/energy> ?energyprice .
  ?source0 <https://energy.ld.admin.ch/elcom/electricityprice/dimension/charge> ?charge .
  ?source0 <https://energy.ld.admin.ch/elcom/electricityprice/dimension/aidfee> ?aidfee .
  
  
  #PV-Verguetungen
  
  <https://energy.ld.admin.ch/sfoe/bfe_ogd84_einmalverguetung_fuer_photovoltaikanlagen/6> <https://cube.link/observationSet> ?observationSet1 .
  ?observationSet1 <https://cube.link/observation> ?source1 .
  ?source1 <https://energy.ld.admin.ch/sfoe/bfe_ogd84_einmalverguetung_fuer_photovoltaikanlagen/Jahr> ?Jahr .
  ?source1 <https://energy.ld.admin.ch/sfoe/bfe_ogd84_einmalverguetung_fuer_photovoltaikanlagen/Kanton> ?kanton .
  ?source1 <https://energy.ld.admin.ch/sfoe/bfe_ogd84_einmalverguetung_fuer_photovoltaikanlagen/AnzahlAnlagen> ?Anzahl_anlagen .
  ?source1 <https://energy.ld.admin.ch/sfoe/bfe_ogd84_einmalverguetung_fuer_photovoltaikanlagen/InstallierteLeistungkW> ?installierte_Leistung_kW .
  ?source1 <https://energy.ld.admin.ch/sfoe/bfe_ogd84_einmalverguetung_fuer_photovoltaikanlagen/VerguetungCHF> ?verguetungCHF .
  ?source1 <https://energy.ld.admin.ch/sfoe/bfe_ogd84_einmalverguetung_fuer_photovoltaikanlagen/AnzahlAnlagenPro100000Einwohner> ?anzahl_anlagen_pro_100k .
  ?source1 <https://energy.ld.admin.ch/sfoe/bfe_ogd84_einmalverguetung_fuer_photovoltaikanlagen/InstallierteLeistungkWPro100000Einwohner> ?Installierte_leistungpro_100k .
  
 ?kanton <http://schema.org/name> ?Kanton. 
 
filter(?kategorie = <https://energy.ld.admin.ch/elcom/electricityprice/category/H4>)  #this controls which profile is used. H4 is standard
filter(lang(?Kanton) = 'de') 

}
") #wwe serach for the graph

querymanual <- paste(endpoint, "?", "query", "=", gsub("\\+", "%2B", URLencode(query, reserved = TRUE)), "", sep = "")
queryres_csv <- GET(querymanual,proxy_config, timeout(60), add_headers(c(Accept = "text/csv")))
queryres_csv$content <- stri_encode(queryres_csv$content, from="UTF-8",to="UTF-8") #without this on government pcs it will not be in utf-8. Probably because utf8 is not standard but latinX, and it chooses standard.
queryres_content_csv <-  queryres_csv$content %>% textConnection() %>% read.csv
df <- as_tibble(queryres_content_csv)
df



# Data manipulation -------------------------------------------------------



df2 <- df %>% group_by(Jahr, Kanton) %>% count() 
df2

df2 <- df2 %>% filter(n==1)

df3 <- left_join(df,df2, by=c("Jahr", "Kanton"))
df3$n <- ifelse(is.na(df3$n), 2, df3$n)

df3
df3$Produkt <-  ifelse(df3$n==1, "https://energy.ld.admin.ch/elcom/electricityprice/product/standard", df3$Produkt) #If there are two products, we chooose the standard, if there is only one, we rename it to standard. 
df3
df3 <- df3 %>% filter(Produkt == "https://energy.ld.admin.ch/elcom/electricityprice/product/standard" &Jahr >2014 & Jahr <2022 ) %>% #not neough data on 2014 and 2022
  mutate(Installierte_leistungpro_100k=as.integer(Installierte_leistungpro_100k))
  

# Plotting ----------------------------------------------------------------



e <- ggplot(df3, aes(Installierte_leistungpro_100k,Total, color =Jahr, size =19))
e + geom_point()+ xlab("Neu installierte  Leistung in KWh pro 100000 Einwohnenden") +ylab("Totalpreis (Rp./KWh)") +ggtitle("Totalstrompreis und installierte Leistung")



# dfjoined$Energielieferungkosten
# e <- ggplot(df3, aes(Leistung.in.kW.pro.100000.Einwohner,Energielieferungkosten, color=Periode))
# e + geom_point()
# 
# e <- ggplot(df3, aes(Leistung.in.kW.pro.100000.Einwohner,Energielieferungkosten, color=Periode, size=19))
# e + geom_point() + xlab("Neu installierte  Leistung in KWh pro 100000 Einwohnenden") +ylab("Energielieferpreis (Rp./KWh)") +ggtitle("Lieferpreise und installierte Leistung")



# We z-standardize in every year ------------------------------------------


df_norming <-  df3 %>% group_by(Jahr) %>% summarise( mean_kwh=mean(Installierte_leistungpro_100k), sd_kwh=sd(Installierte_leistungpro_100k), mean_cost=mean(Total), sd_cost=sd(Total))

df_joined <- left_join(df3, df_norming, by="Jahr")

df_joined <- df_joined %>% mutate(normed_kwh= (Installierte_leistungpro_100k-mean_kwh)/sd_kwh, normed_cost=(Total-mean_cost)/sd_cost)

df_joined


e <- ggplot(df_joined, aes(normed_kwh,normed_cost, color =Jahr, size =19))
e + geom_point()+ xlab("Neu installierte  Leistung in KWh pro 100000 Einwohnenden, normalisiert") +ylab("Totalpreis (Rp./KWh), normalisiert") +ggtitle("Totalstrompreis und installierte Leistung, normalisiert")




# We try again with lagged plot -------------------------------------------
#First we download the price-data from Sparql

query <- paste0(
"PREFIX schema: <http://schema.org/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?Kanton ?Jahr ?Total ?Produkt  #add here variables you wnat to download
WHERE {
<https://energy.ld.admin.ch/elcom/electricityprice-canton> <https://cube.link/observationSet> ?observationSet0 .
  ?observationSet0 <https://cube.link/observation> ?source0 .
  ?source0 <https://energy.ld.admin.ch/elcom/electricityprice/dimension/canton> ?kanton .
  ?source0 <https://energy.ld.admin.ch/elcom/electricityprice/dimension/category> ?kategorie .
  ?source0 <https://energy.ld.admin.ch/elcom/electricityprice/dimension/period> ?Jahr .
  ?source0 <https://energy.ld.admin.ch/elcom/electricityprice/dimension/product> ?Produkt .
  ?source0 <https://energy.ld.admin.ch/elcom/electricityprice/dimension/total> ?Total .
  ?source0 <https://energy.ld.admin.ch/elcom/electricityprice/dimension/gridusage> ?gridusage .
  ?source0 <https://energy.ld.admin.ch/elcom/electricityprice/dimension/energy> ?energyprice .
  ?source0 <https://energy.ld.admin.ch/elcom/electricityprice/dimension/charge> ?charge .
  ?source0 <https://energy.ld.admin.ch/elcom/electricityprice/dimension/aidfee> ?aidfee .
  
  ?kanton <http://schema.org/name> ?Kanton. 
  
  filter(?kategorie = <https://energy.ld.admin.ch/elcom/electricityprice/category/H4>)  
  filter(lang(?Kanton) = 'de') 
  
}"
)


querymanual <- paste(endpoint, "?", "query", "=", gsub("\\+", "%2B", URLencode(query, reserved = TRUE)), "", sep = "")
querymanual
queryres_csv <- GET(querymanual,proxy_config, timeout(60), add_headers(c(Accept = "text/csv")))
queryres_csv$content <- stri_encode(queryres_csv$content, from="UTF-8",to="UTF-8") #without this on government pcs it will not be in utf-8. Probably because utf8 is not standard but latinX, and it chooses standard.
queryres_content_csv <-  queryres_csv$content %>% textConnection() %>% read.csv
df_prices <- as_tibble(queryres_content_csv)
df_prices

df2_prices <- df_prices %>% group_by(Jahr, Kanton) %>% count() 
df2_prices

df2_prices <- df2_prices %>% filter(n==1)

df3_prices <- left_join(df_prices,df2_prices, by=c("Jahr", "Kanton"))
df3_prices$n <- ifelse(is.na(df3_prices$n), 2, df3_prices$n)

df3_prices
df3_prices$Produkt <-  ifelse(df3_prices$n==1, "https://energy.ld.admin.ch/elcom/electricityprice/product/standard", df3_prices$Produkt) #If there are two products, we chooose the standard, if there is only one, we rename it to standard. 
df3_prices
df3_prices <- df3_prices %>% filter(Produkt == "https://energy.ld.admin.ch/elcom/electricityprice/product/standard") %>% mutate(Jahr= (Jahr-1)) %>%  mutate(Total_lagged=Total) %>% select(-Total,-n,-Produkt) #here we make it LAGGED (fist high prices, then more PV and we mark the lagged prices)
df3_prices

df3_lagged <- left_join(df3,df3_prices, by=c("Jahr", "Kanton"))

df3_lagged



# plotting ----------------------------------------------------------------




e <- ggplot(df3_lagged, aes(Installierte_leistungpro_100k,Total_lagged, color =Jahr, size =19))
e + geom_point()+ xlab("Neu installierte  Leistung in KWh pro 100000 Einwohnenden") +ylab("Totalpreis (Rp./KWh)") +ggtitle("Totalstrompreis und installierte Leistung, lagged")



# dfjoined$Energielieferungkosten
# e <- ggplot(df3_lagged, aes(Leistung.in.kW.pro.100000.Einwohner,Energielieferungkosten, color=Periode))
# e + geom_point()
# 
# e <- ggplot(df3_lagged, aes(Leistung.in.kW.pro.100000.Einwohner,Energielieferungkosten, color=Periode, size=19))
# e + geom_point() + xlab("Neu installierte  Leistung in KWh pro 100000 Einwohnenden") +ylab("Energielieferpreis (Rp./KWh)") +ggtitle("Lieferpreise und installierte Leistung")



# We z-standardize in every year ------------------------------------------


df_norming <-  df3_lagged %>% group_by(Jahr) %>% summarise( mean_kwh=mean(Installierte_leistungpro_100k), sd_kwh=sd(Installierte_leistungpro_100k), mean_cost=mean(Total_lagged), sd_cost=sd(Total_lagged))

df_joined <- left_join(df3_lagged, df_norming, by="Jahr")

df_joined <- df_joined %>% mutate(normed_kwh= (Installierte_leistungpro_100k-mean_kwh)/sd_kwh, normed_cost=(Total-mean_cost)/sd_cost)

df_joined


e <- ggplot(df_joined, aes(normed_kwh,normed_cost, color =Jahr, size =19))
e + geom_point()+ xlab("Neu installierte  Leistung in KWh pro 100000 Einwohnenden, normalisiert") +ylab("Totalpreis (Rp./KWh), normalisiert") +ggtitle("Totalstrompreis und installierte Leistung, normalisiert und lagged")


                                    
