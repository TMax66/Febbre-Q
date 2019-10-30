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

sheet <- gs_title("fq")
#sheet <- gs_title("fqbg")
fq<-gs_read(sheet)




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
  mutate("tipo_ricerca"=ifelse(prova=="Febbre Q da Coxiella burnetii: agente eziologico", 
"AgEziologico", "Sierologia"))


names(fq)[c(12:16)]<-c("Agpos","Abneg","Abdub","Agneg","Abpos")

fq<-fq %>% 
  mutate(AgTot = rowSums(.[c(12,15)], na.rm=T),AbTot=rowSums(.[c(13,14,16)], na.rm = T)) %>%
  select(-"Abneg",-"Abdub",-"Agneg", -"x")

fq<-fq %>% 
  filter(fin%in% c("Diagnostica","Piano monitoraggio latte crudo al consumo",
                   "Piano monitoraggio latte crudo","Monitoraggio fauna selvatica Lombardia",
                   "Piano aborti bovine da latte","Piano sorveglianza latte crudo ASL",
                   "Autocontrollo","Piano sorveglianza latte crudo al consumo ASL",
                   "Monitoraggio fauna selvatica","Monitoraggio","Piano Bruc. Leb. Latte Lombardia",
                   "Monitoraggio fauna selvatica Emilia Romagna")) %>% 
  mutate("sorveglianza"=ifelse(
    fin=="Diagnostica" |fin=="Autocontrollo", "Passiva", "Attiva"
  )) 



fin<-fq %>%  
  filter(tipo_ricerca=="AgEziologico") %>% 
  group_by(fin, anno) %>% 
  summarise(
            "totag"=sum(AgTot)) %>% 
  pivot_wider(names_from=anno,values_from=totag ) 





fq %>% 
  filter(tipo_ricerca=="AgEziologico") %>% 
  group_by(matrice) %>% 
    summarise(
      "totag"=sum(AgTot)) %>% 
  arrange(totag) %>% 
  top_n(15,totag) %>% 
  mutate(matrice = factor(matrice, unique(matrice))) %>%
  ggplot(aes(x=matrice, y=totag))+ 
  geom_bar(stat="identity",fill="steelblue3")+labs(x="")+
  coord_flip()+theme(axis.text=element_text(size=10))
  

fq %>% 
  filter(tipo_ricerca=="AgEziologico") %>% 

  group_by(matrice, reg) %>% 
  summarise(
    "totag"=sum(AgTot)) %>% 
  arrange(totag) %>% 
  top_n(15,totag) %>% 
  ungroup() %>% 
  mutate(matrice = factor(matrice, unique(matrice))) %>%
  ggplot(aes(x=matrice, y=totag))+ 
  geom_bar(stat="identity",fill="steelblue3")+labs(x="")+
  coord_flip()+theme(axis.text=element_text(size=10))+facet_wrap(~reg)
