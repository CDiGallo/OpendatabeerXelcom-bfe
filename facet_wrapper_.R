
library(tidyverse)
library(ggplot2)

library(curl)
library(httr)
library(stringi)




# download directly from lindas -------------------------------------------



endpoint <- "https://lindas.admin.ch/query"
proxy_url <- curl::ie_get_proxy_for_url(endpoint)
proxy_config <- use_proxy(url=proxy_url)

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
df <- as_tibble(queryres_content_csv)
df




df2 <- df %>% group_by(Jahr, Kanton) %>% count() 
df2

df2 <- df2 %>% filter(n==1)

df3 <- left_join(df,df2, by=c("Jahr", "Kanton"))
df3$n <- ifelse(is.na(df3$n), 2, df3$n)


df3
df3$Produkt <-  ifelse(df3$n==1, "https://energy.ld.admin.ch/elcom/electricityprice/product/standard", df3$Produkt)
df3



df3 <- df3 %>% filter(Produkt == "https://energy.ld.admin.ch/elcom/electricityprice/product/standard")




# Also es gibt nicht immer überall STandart oder günstigstes Produkt. Ich möchte aber von 2011 weg die Daten haben. Also muss ich immer das günstigste nehmen.
# es kann aber sein, dass es nur ein Standardprodutk gibt und kein günstigstes. Also filter ich, bei welchen jahrkantonen nur ein eintrag hat. also nicht günstig und standard.
# Dann mache ich dass es dort immer günstigstes steht. weilw wenn es nur eines gibt, ist es das günstigste per default. 
# Jetzt gibt es ein problem , dass die antwort auf test mit NA die antwort immer na ist. darum muss das zuesrt gefixt werden.




# plotten -----------------------------------------------------------------




t <- ggplot(df3, aes( Jahr,Total, color = Total)) + geom_point() +   scale_colour_gradient(
  low = "blue", 
  #mid = "black", 
  high = "#FF3300") 

t <- t+ xlab("Jahr (2011-2023)") +ylab("Totalpreis (Rp./KWh)")


t + facet_wrap(vars(Kanton)) +theme_light()  + theme(axis.text.x=element_text(angle = 45, vjust = 0.5))#, cols = vars(fl))



