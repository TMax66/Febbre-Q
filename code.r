library(googlesheets)
library(tidyverse)
library(leaflet)
library(maps)
library(rgdal)
library(sp)
library(lubridate)
library(janitor)
library(rmapshaper)
library(DataExplorer)
library(tidyverse)
library(googlesheets)
library(shinydashboard)
library(lubridate)
library(knitr)
library(kableExtra)
library(DT)
library(psych)
library(tidyr)
library(formattable)
library(shinyBS)
#library(sparkline)
#library(htmltools)
#library(shiny)


rm(list=ls())


options(scipen = 999)
options(knitr.kable.NA = '')

# token <- gs_auth(cache = FALSE)
# gd_token()
# saveRDS(token, file = "googlesheets_token.rds")


gs_auth(token = "googlesheets_token.rds")
suppressMessages(gs_auth(token = "googlesheets_token.rds", verbose = FALSE))

#sheet <- gs_title("febbreq")
sheet <- gs_title("fqbg")
fq<-gs_read(sheet)

fq<-fq[,-16]


# names(fq)<-c("nconf","dtprel","codaz","specie","materiale","ncamp","esito","finalita","pr","rg",
#              "com","sacc","san","ran","matrice")

fq$codaz<-casefold(fq$codaz, upper = TRUE)

fq$codaz <- substr(as.character(fq$codaz),1,8)


fq$dtprel<-mdy(fq$dtprel)
fq$dtprel1<-format(fq$dtprel, "%d-%m-%Y")

fq<-mutate(fq,anno=year(dtprel))
fq<-mutate(fq,mese=month(dtprel))


fq<-
  fq %>% 
  mutate("prova"=ifelse(prova=="Febbre Q da Coxiella burnetii: agente eziologico", 
"AgEziologico", "Sierologia"))



fin<-fq %>% 
  filter(prova=="AgEziologico") %>% 
  group_by(fin) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))


fq<-fq %>% 
  filter(finalita %in% c("Diagnostica","Piano monitoraggio latte crudo al consumo",
         "Piano monitoraggio latte crudo","Monitoraggio fauna selvatica Lombardia",
         "Piano aborti bovine da latte","Piano sorveglianza latte crudo ASL",
         "Autocontrollo","Piano sorveglianza latte crudo al consumo ASL",
         "Monitoraggio fauna selvatica","Monitoraggio","Piano Bruc. Leb. Latte Lombardia",
         "Monitoraggio fauna selvatica Emilia Romagna")) %>% 
  mutate("sorveglianza"=ifelse(
    finalita=="Diagnostica" |finalita=="Autocontrollo", "Passiva", "Attiva"
  ))

fq %>% 
  mutate("materiale"=ifelse(is.na(fq$materiale), fq$matrice,fq$materiale)) %>% 
  group_by(materiale) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  top_n(15,n) %>% 
  mutate(materiale = factor(materiale, unique(materiale))) %>%
  ggplot(aes(x=materiale, y=n))+ 
  geom_bar(stat="identity",fill="steelblue3")+labs(x="")+
  coord_flip()+theme(axis.text=element_text(size=10))
  


