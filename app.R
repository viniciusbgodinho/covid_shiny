library(shiny)



library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("devtools")
library(plotly)
library("sf")  
library("tmap")
library("tmaptools")
library("rgdal")
library("leaflet")
library("shinyWidgets")
library(shinydashboard)
library(shinythemes)

con <- gzcon(url(paste("https://data.brasil.io/dataset/covid19/caso_full.csv.gz", sep=",")))
txt <- readLines(con)
dados <- read.csv(textConnection(txt))


colnames(dados)[8] <-'confirmed'
colnames(dados)[12] <-'deaths'
colnames(dados)[9] <- 'confirmed_per_100k'


dados <- dados %>%
  mutate(deaths_per_100k= (100000*deaths)/estimated_population_2019) 


###Brasil


dados_brasil_total <- dados %>%
  filter(place_type=="state") %>%
  arrange(date) 



dados_brasil <- group_by(dados_brasil_total,date)%>%
  summarise(confirmed=sum(confirmed),deaths=sum(deaths),
            new_confirmed=sum(new_confirmed), new_deaths=sum(new_deaths),
            confirmed_per_100k=sum(confirmed_per_100k), deaths_per_100k=sum(deaths_per_100k))



dados_brasil$date <- as.Date(dados_brasil$date)


dados_brasil <- dados_brasil%>%
  mutate(ma7_confirmed = stats::filter(dados_brasil$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_brasil$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))




####casos

dados_brasil$ma7_confirmed <- as.numeric(dados_brasil$ma7_confirmed)


##óbitos

dados_brasil$ma7_deaths <- as.numeric(dados_brasil$ma7_deaths)





###Centro Oeste
#DF, GO, MT, MS

dados_co <- dados %>%
  filter(place_type == "state", state=="DF" |state=="GO" |state=="MT" |state=="MS") %>%
  arrange(date) 


dados_co$date <- as.Date(dados_co$date)


###Média móvel
#df
dados_df <- dados %>%
  filter(place_type=="state", state =="DF") 


dados_df <- dados_df%>%
  mutate(ma7_confirmed = stats::filter(dados_df$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_df$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))



#goias
dados_go <- dados %>%
  filter(place_type=="state", state =="GO") 


dados_go <- dados_go%>%
  mutate(ma7_confirmed = stats::filter(dados_go$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_go$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#mt
dados_mt <- dados %>%
  filter(place_type=="state", state =="MT") 


dados_mt <- dados_mt%>%
  mutate(ma7_confirmed = stats::filter(dados_mt$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_mt$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))


#ms


dados_ms <- dados %>%
  filter(place_type=="state", state =="MS") 


dados_ms <- dados_ms%>%
  mutate(ma7_confirmed = stats::filter(dados_ms$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_ms$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))




dados_co_full<- rbind.data.frame(dados_df,dados_go,dados_mt,dados_ms)

dados_co_full$date <- as.Date(dados_co_full$date)




###Sudeste
#ES, MG, RJ, SP

dados_sd <- dados %>%
  filter(place_type == "state", state=="ES" |state=="MG" |state=="RJ" |state=="SP") %>%
  arrange(date) 


dados_sd$date <- as.Date(dados_sd$date)


###Média móvel
#ES
dados_es <- dados %>%
  filter(place_type=="state", state =="ES") 


dados_es <- dados_es%>%
  mutate(ma7_confirmed = stats::filter(dados_es$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_es$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))



#mg
dados_mg <- dados %>%
  filter(place_type=="state", state =="MG") 


dados_mg <- dados_mg%>%
  mutate(ma7_confirmed = stats::filter(dados_mg$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_mg$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#rj
dados_rj <- dados %>%
  filter(place_type=="state", state =="RJ") 


dados_rj <- dados_rj%>%
  mutate(ma7_confirmed = stats::filter(dados_rj$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_rj$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))


#sp


dados_sp <- dados %>%
  filter(place_type=="state", state =="SP") 


dados_sp <- dados_sp%>%
  mutate(ma7_confirmed = stats::filter(dados_sp$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_sp$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))




dados_sd_full<- rbind.data.frame(dados_es,dados_mg,dados_rj,dados_sp)

dados_sd_full$date <- as.Date(dados_sd_full$date)





###Sul
#PR, RS, SC

dados_su <- dados %>%
  filter(place_type == "state", state=="PR" |state=="RS" |state=="SC") %>%
  arrange(date) 


dados_su$date <- as.Date(dados_su$date)


###Média móvel
#PR
dados_pr <- dados %>%
  filter(place_type=="state", state =="PR") 


dados_pr <- dados_pr%>%
  mutate(ma7_confirmed = stats::filter(dados_pr$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_pr$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))



#mg
dados_rs <- dados %>%
  filter(place_type=="state", state =="RS") 


dados_rs <- dados_rs%>%
  mutate(ma7_confirmed = stats::filter(dados_rs$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_rs$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#SC
dados_sc <- dados %>%
  filter(place_type=="state", state =="SC") 


dados_sc <- dados_sc%>%
  mutate(ma7_confirmed = stats::filter(dados_sc$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_sc$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))



dados_su_full<- rbind.data.frame(dados_pr,dados_rs,dados_sc)

dados_su_full$date <- as.Date(dados_su_full$date)


###Nordeste

###Sul
#AL,BA,CE,PB,PI,PE,MA,RN,SE

dados_ne <- dados %>%
  filter(place_type == "state", state=="AL" |state=="BA" |state=="CE"|state=="PB"|state=="PE"|state=="PI"|state=="MA"|state=="RN"|state=="SE") %>%
  arrange(date) 


dados_ne$date <- as.Date(dados_ne$date)


###Média móvel
#AL
dados_al <- dados %>%
  filter(place_type=="state", state =="AL") 


dados_al <- dados_al%>%
  mutate(ma7_confirmed = stats::filter(dados_al$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_al$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))



#ba
dados_ba <- dados %>%
  filter(place_type=="state", state =="BA") 


dados_ba <- dados_ba%>%
  mutate(ma7_confirmed = stats::filter(dados_ba$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_ba$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#CE
dados_ce <- dados %>%
  filter(place_type=="state", state =="CE") 


dados_ce <- dados_ce%>%
  mutate(ma7_confirmed = stats::filter(dados_ce$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_ce$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))


#PB
dados_pb <- dados %>%
  filter(place_type=="state", state =="PB") 


dados_pb <- dados_pb%>%
  mutate(ma7_confirmed = stats::filter(dados_pb$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_pb$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))
#PE
dados_pe <- dados %>%
  filter(place_type=="state", state =="PE") 


dados_pe <- dados_pe%>%
  mutate(ma7_confirmed = stats::filter(dados_pe$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_pe$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))
#PI
dados_pi <- dados %>%
  filter(place_type=="state", state =="PI") 


dados_pi <- dados_pi%>%
  mutate(ma7_confirmed = stats::filter(dados_pi$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_pi$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))
#MA
dados_ma <- dados %>%
  filter(place_type=="state", state =="MA") 


dados_ma <- dados_ma%>%
  mutate(ma7_confirmed = stats::filter(dados_ma$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_ma$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))
#RN
dados_rn <- dados %>%
  filter(place_type=="state", state =="RN") 


dados_rn <- dados_rn%>%
  mutate(ma7_confirmed = stats::filter(dados_rn$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_rn$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#SE
dados_se <- dados %>%
  filter(place_type=="state", state =="SE") 


dados_se <- dados_se%>%
  mutate(ma7_confirmed = stats::filter(dados_se$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_se$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))




dados_ne_full<- rbind.data.frame(dados_al,dados_ba,dados_ce,dados_pb,dados_pe,dados_pi,dados_ma,dados_rn,dados_se)

dados_ne_full$date <- as.Date(dados_ne_full$date)




###Norte


#Acre, Amapá, Amazonas, Pará, Rondônia, Roraima e Tocantins


dados_no <- dados %>%
  filter(place_type == "state", state=="AC" |state=="AP" |state=="AM"|state=="PA"|state=="RO"|state=="RR"|state=="TO") %>%
  arrange(date) 


dados_no$date <- as.Date(dados_no$date)



###Média móvel
#AC
dados_ac <- dados %>%
  filter(place_type=="state", state =="AC") 


dados_ac <- dados_ac%>%
  mutate(ma7_confirmed = stats::filter(dados_ac$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_ac$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))



#ap
dados_ap <- dados %>%
  filter(place_type=="state", state =="AP") 


dados_ap <- dados_ap%>%
  mutate(ma7_confirmed = stats::filter(dados_ap$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_ap$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#AM
dados_am <- dados %>%
  filter(place_type=="state", state =="AM") 


dados_am <- dados_am%>%
  mutate(ma7_confirmed = stats::filter(dados_am$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_am$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))


#PA
dados_pa <- dados %>%
  filter(place_type=="state", state =="PA") 


dados_pa <- dados_pa%>%
  mutate(ma7_confirmed = stats::filter(dados_pa$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_pa$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#PE
dados_ro <- dados %>%
  filter(place_type=="state", state =="RO") 


dados_ro <- dados_ro%>%
  mutate(ma7_confirmed = stats::filter(dados_ro$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_ro$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))
#RR
dados_rr <- dados %>%
  filter(place_type=="state", state =="RR") 


dados_rr <- dados_rr%>%
  mutate(ma7_confirmed = stats::filter(dados_rr$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_rr$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))
#TO
dados_to <- dados %>%
  filter(place_type=="state", state =="TO") 


dados_to <- dados_to%>%
  mutate(ma7_confirmed = stats::filter(dados_to$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_to$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))


dados_no_full<- rbind.data.frame(dados_ac,dados_ap,dados_am,dados_pa,dados_ro,dados_rr,dados_to)

dados_no_full$date <- as.Date(dados_no_full$date)




dados_full<- rbind.data.frame(dados_sd_full,dados_su_full,dados_co_full,dados_no_full,dados_ne_full)

dados_full$date <- as.Date(dados_full$date)





###Agrupar regiões


##Brasil

df_brasil <- dados_brasil %>%
  mutate(state= "Brasil") 

##Centro-Oeste

df_co <- group_by(dados_co_full,date)%>%
  summarise(confirmed=sum(confirmed), deaths=sum(deaths),
            confirmed_per_100k = sum(confirmed_per_100k),deaths_per_100k = sum(deaths_per_100k),
            new_confirmed=sum(new_confirmed),new_deaths=sum(new_deaths))

df_co <- df_co%>%
  mutate(ma7_confirmed = stats::filter(df_co$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(df_co$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(state= "Centro-Oeste")

##Nordeste

df_ne <- group_by(dados_ne_full,date)%>%
  summarise(confirmed=sum(confirmed), deaths=sum(deaths),
            confirmed_per_100k = sum(confirmed_per_100k),deaths_per_100k = sum(deaths_per_100k),
            new_confirmed=sum(new_confirmed),new_deaths=sum(new_deaths))

df_ne <- df_ne%>%
  mutate(ma7_confirmed = stats::filter(df_ne$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(df_ne$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(state= "Nordeste")



##Norte

df_no <- group_by(dados_no_full,date)%>%
  summarise(confirmed=sum(confirmed), deaths=sum(deaths),
            confirmed_per_100k = sum(confirmed_per_100k),deaths_per_100k = sum(deaths_per_100k),
            new_confirmed=sum(new_confirmed),new_deaths=sum(new_deaths))

df_no <- df_no%>%
  mutate(ma7_confirmed = stats::filter(df_no$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(df_no$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(state= "Norte")


##Sudeste

df_sd <- group_by(dados_sd_full,date)%>%
  summarise(confirmed=sum(confirmed), deaths=sum(deaths),
            confirmed_per_100k = sum(confirmed_per_100k),deaths_per_100k = sum(deaths_per_100k),
            new_confirmed=sum(new_confirmed),new_deaths=sum(new_deaths))

df_sd <- df_sd%>%
  mutate(ma7_confirmed = stats::filter(df_sd$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(df_sd$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(state= "Sudeste")

##Sul

df_su <- group_by(dados_su_full,date)%>%
  summarise(confirmed=sum(confirmed), deaths=sum(deaths),
            confirmed_per_100k = sum(confirmed_per_100k),deaths_per_100k = sum(deaths_per_100k),
            new_confirmed=sum(new_confirmed),new_deaths=sum(new_deaths))

df_su <- df_su%>%
  mutate(ma7_confirmed = stats::filter(df_su$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(df_su$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(state= "Sul")



df_full_regiao<- rbind.data.frame(df_co,df_ne,df_no,df_sd,df_su)


df_full_regiao <- df_full_regiao 


####capitais


#Porto Velho		 

dados_porto_velho <- dados %>%
  filter(place_type=="city",  city_ibge_code=="1100205") 


dados_porto_velho <- dados_porto_velho%>%
  mutate(ma7_confirmed = stats::filter(dados_porto_velho$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_porto_velho$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Manaus		 

dados_manaus <- dados %>%
  filter(place_type=="city",  city_ibge_code=="1302603") 


dados_manaus <- dados_manaus%>%
  mutate(ma7_confirmed = stats::filter(dados_manaus$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_manaus$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Rio Branco		 

dados_rio_branco <- dados %>%
  filter(place_type=="city",  city_ibge_code=="1200401") 


dados_rio_branco <- dados_rio_branco%>%
  mutate(ma7_confirmed = stats::filter(dados_rio_branco$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_rio_branco$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Campo Grande		 

dados_campo_grande <- dados %>%
  filter(place_type=="city",  city_ibge_code=="5002704") 


dados_campo_grande <- dados_campo_grande%>%
  mutate(ma7_confirmed = stats::filter(dados_campo_grande$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_campo_grande$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Macapá		 

dados_macapa <- dados %>%
  filter(place_type=="city",  city_ibge_code=="1600303") 


dados_macapa <- dados_macapa%>%
  mutate(ma7_confirmed = stats::filter(dados_macapa$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_macapa$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Brasília		 

dados_brasilia <- dados %>%
  filter(place_type=="city",  city_ibge_code=="5300108") 


dados_brasilia <- dados_brasilia%>%
  mutate(ma7_confirmed = stats::filter(dados_brasilia$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_brasilia$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Boa Vista		

dados_boa_vista <- dados %>%
  filter(place_type=="city",  city_ibge_code=="1400100") 


dados_boa_vista <- dados_boa_vista%>%
  mutate(ma7_confirmed = stats::filter(dados_boa_vista$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_boa_vista$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Cuiabá		 

dados_cuiaba <- dados %>%
  filter(place_type=="city",  city_ibge_code=="5103403") 


dados_cuiaba <- dados_cuiaba%>%
  mutate(ma7_confirmed = stats::filter(dados_cuiaba$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_cuiaba$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Palmas		 

dados_palmas <- dados %>%
  filter(place_type=="city",  city_ibge_code=="1721000") 


dados_palmas <- dados_palmas%>%
  mutate(ma7_confirmed = stats::filter(dados_palmas$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_palmas$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Teresina		

dados_teresina <- dados %>%
  filter(place_type=="city",  city_ibge_code=="2211001") 


dados_teresina <- dados_teresina%>%
  mutate(ma7_confirmed = stats::filter(dados_teresina$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_teresina$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#São Paulo		

dados_sao_paulo <- dados %>%
  filter(place_type=="city",  city_ibge_code=="3550308") 


dados_sao_paulo <- dados_sao_paulo%>%
  mutate(ma7_confirmed = stats::filter(dados_sao_paulo$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_sao_paulo$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Rio de Janeiro		

dados_rio_de_janeiro <- dados %>%
  filter(place_type=="city",  city_ibge_code=="3304557") 


dados_rio_de_janeiro <- dados_rio_de_janeiro%>%
  mutate(ma7_confirmed = stats::filter(dados_rio_de_janeiro$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_rio_de_janeiro$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Belém		

dados_belem <- dados %>%
  filter(place_type=="city",  city_ibge_code=="1501402") 


dados_belem <- dados_belem%>%
  mutate(ma7_confirmed = stats::filter(dados_belem$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_belem$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#São Luís	

dados_sao_luis <- dados %>%
  filter(place_type=="city",  city_ibge_code=="2111300") 


dados_sao_luis <- dados_sao_luis%>%
  mutate(ma7_confirmed = stats::filter(dados_sao_luis$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_sao_luis$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Goiânia		 

dados_goiania <- dados %>%
  filter(place_type=="city",  city_ibge_code=="5208707") 


dados_goiania <- dados_goiania%>%
  mutate(ma7_confirmed = stats::filter(dados_goiania$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_goiania$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Salvador		

dados_salvador <- dados %>%
  filter(place_type=="city",  city_ibge_code=="2927408") 


dados_salvador <- dados_salvador%>%
  mutate(ma7_confirmed = stats::filter(dados_salvador$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_salvador$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Maceió		 

dados_maceio <- dados %>%
  filter(place_type=="city",  city_ibge_code=="2704302") 


dados_maceio <- dados_maceio%>%
  mutate(ma7_confirmed = stats::filter(dados_maceio$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_maceio$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Porto Alegre		

dados_porto_alegre <- dados %>%
  filter(place_type=="city",  city_ibge_code=="4314902") 


dados_porto_alegre <- dados_porto_alegre%>%
  mutate(ma7_confirmed = stats::filter(dados_porto_alegre$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_porto_alegre$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Curitiba		

dados_curitiba <- dados %>%
  filter(place_type=="city",  city_ibge_code=="4106902") 


dados_curitiba <- dados_curitiba%>%
  mutate(ma7_confirmed = stats::filter(dados_curitiba$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_curitiba$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Florianópolis		

dados_florianopolis <- dados %>%
  filter(place_type=="city",  city_ibge_code=="4205407") 


dados_florianopolis <- dados_florianopolis%>%
  mutate(ma7_confirmed = stats::filter(dados_florianopolis$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_florianopolis$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Belo Horizonte		

dados_belo_horizonte <- dados %>%
  filter(place_type=="city",  city_ibge_code=="3106200") 


dados_belo_horizonte <- dados_belo_horizonte%>%
  mutate(ma7_confirmed = stats::filter(dados_belo_horizonte$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_belo_horizonte$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Fortaleza		

dados_fortaleza <- dados %>%
  filter(place_type=="city",  city_ibge_code=="2304400") 


dados_fortaleza <- dados_fortaleza%>%
  mutate(ma7_confirmed = stats::filter(dados_fortaleza$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_fortaleza$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Recife		 

dados_recife <- dados %>%
  filter(place_type=="city",  city_ibge_code=="2611606") 


dados_recife <- dados_recife%>%
  mutate(ma7_confirmed = stats::filter(dados_recife$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_recife$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#João Pessoa		 

dados_joao_pessoa <- dados %>%
  filter(place_type=="city",  city_ibge_code=="2507507") 


dados_joao_pessoa <- dados_joao_pessoa%>%
  mutate(ma7_confirmed = stats::filter(dados_joao_pessoa$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_joao_pessoa$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Aracaju		 

dados_aracaju <- dados %>%
  filter(place_type=="city",  city_ibge_code=="2800308") 


dados_aracaju <- dados_aracaju%>%
  mutate(ma7_confirmed = stats::filter(dados_aracaju$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_aracaju$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Natal		 

dados_natal <- dados %>%
  filter(place_type=="city",  city_ibge_code=="2408102") 


dados_natal <- dados_natal%>%
  mutate(ma7_confirmed = stats::filter(dados_natal$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_natal$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))


#Vitória	


dados_vitoria <- dados %>%
  filter(place_type=="city",  city_ibge_code=="3205309") 


dados_vitoria <- dados_vitoria%>%
  mutate(ma7_confirmed = stats::filter(dados_vitoria$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_vitoria$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))


dados_capitais<- rbind.data.frame(dados_aracaju,dados_belem,dados_belo_horizonte,
                                  dados_boa_vista,dados_brasilia,dados_campo_grande,
                                  dados_cuiaba,dados_curitiba,dados_florianopolis,
                                  dados_fortaleza,dados_goiania,dados_joao_pessoa,
                                  dados_macapa,dados_maceio,dados_manaus,dados_natal,
                                  dados_palmas,dados_porto_alegre,dados_porto_velho,
                                  dados_recife,dados_rio_branco,dados_rio_de_janeiro,
                                  dados_salvador,dados_sao_luis,dados_sao_paulo,
                                  dados_teresina,dados_vitoria)





dados_capitais$date <- as.Date(dados_capitais$date)


###Ajustando a Tabela para rodar o painel Shiny

names(dados_brasil)
names(df_full_regiao)
names(dados_full)
names(dados_capitais)

tabela_brasil <- dados_brasil %>%
  mutate(state = "Brasil") %>%
  mutate(tx_letalidade = deaths/confirmed*100)


tabela_regioes <- df_full_regiao %>%
  mutate(tx_letalidade = deaths/confirmed*100)


tabela_estados <- dados_full %>%
  select(date,confirmed,deaths,confirmed_per_100k,deaths_per_100k,new_confirmed,
         new_deaths,ma7_confirmed,ma7_deaths,state) %>%
  mutate(tx_letalidade = deaths/confirmed*100)



tabela_capitais <- dados_capitais %>%
  select(date,confirmed,deaths,confirmed_per_100k,deaths_per_100k,new_confirmed,
         new_deaths,ma7_confirmed,ma7_deaths,city) %>%
  mutate(tx_letalidade = deaths/confirmed*100)

colnames(tabela_capitais)[10] <- "state"


df_total <- rbind.data.frame(tabela_brasil,tabela_regioes,tabela_estados,tabela_capitais)

tabela_regioes <- as.data.frame(tabela_regioes)

plot_date <- df_total$date


# function to plot cumulative


cumulative_plot = function(db, plot_date) {
  g1 = ggplot(db, aes(x = date, y = outcome, colour = state)) + 
    geom_line(size=.8) +
    ylab("Acumulado")+ xlab("Data") + theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  
  ggplotly(g1)
  
  
}


# function to plot por 100
cumulative_per_100k_plot = function(db, plot_date) {
  g1 = ggplot(db, aes(x = date, y = per_100k_outcome, color = state)) +
    geom_line(size=.8) +
    ylab("Acumulado") + xlab("Data") + theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  
  ggplotly(g1)
  
}


# function to plot new  

new_plot = function(db, plot_date) {
  g1 = ggplot(db, aes(x = date, y = new_outcome, fill = state)) + 
    geom_bar(position="stack", stat="identity") + 
    ylab("Novos")+ xlab("Data") + theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  
  
  ggplotly(g1)
  
}


# function to plot average mean  


ma7_plot = function(db, plot_date) {
  g1 = ggplot(db, aes(x = date, y = ma7_outcome, colour = state, group = 1)) + geom_line(size=.8)  +
    ylab("Novos") + xlab("Data") + theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  
  ggplotly(g1)
  
  
  
}




# function to plot fatalite rate  


tx_plot = function(db, plot_date) {
  g1 = ggplot(db, aes(x = date, y = tx_outcome, colour = state, group = 1)) + geom_line(size=.8) +
    ylab("Taxa Letalidade (%)") + xlab("Data") + theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  
  ggplotly(g1)
  
  
}


ui <- bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             "COVID-19: Brasil", id="nav",
             
             
             tabPanel("Gráficos por Local",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          span(tags$i(h6("Selecione para visualizar os gráficos de casos e óbitos de Covid-19 para o Brasil, grandes Regiões, Estados e Capitais.")), style="color:#045a8d"),
                          
                          shinyWidgets::pickerInput("level_select", "Nivel:",   
                                                    choices = c("Brasil", "Regioes", "Estados", "Capitais"),
                                                    selected = c("Estados"),
                                                    multiple = FALSE),
                          
                          shinyWidgets::pickerInput("region_select", "Local:",   
                                                    choices = unique(df_total$state),
                                                    selected = unique(df_total$state),
                                                    options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                                    multiple = TRUE), 
                          
                          shinyWidgets::pickerInput("outcome_select", "Resultados:",   
                                                    choices = c("Casos", "Obitos"), 
                                                    selected = "Casos",
                                                    multiple = FALSE),
                          
                          
                          sliderInput("minimum_date",
                                      "Data:",
                                      min = first(plot_date),
                                      max = last(plot_date),
                                      value=as.Date(plot_date),
                                      timeFormat="%d %b"),
                          
                          "Dados das Secretarias Estaduais de Saude via projeto:",
                                tags$a(href="https://brasil.io/covid19/", "https://brasil.io/covid19/"),
                                span(h6("Autor: Vinicius Barbosa Godinho")),
                                "Codigo:",tags$a(href="https://github.com/viniciusbgodinho/covid_shiny", "Github.")
                                
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Acumulado", plotlyOutput("cumulative_plot")),
                            tabPanel("100 mil Hab", plotlyOutput("cumulative_per_100k_plot")),
                            tabPanel("Novos", plotlyOutput("new_plot")),
                            tabPanel("Média Móvel 7 dias", plotlyOutput("ma7_plot")),
                            tabPanel("Taxa de Letalidade (%)", plotlyOutput("tx_plot"))
                          )
                        )
                      )
             )
             
  )
  
)          





server = shinyServer(function(input, output, session){
  
  
  # update region selections
  observeEvent(input$level_select,{
    if (input$level_select=="Brasil") {
      shinyWidgets::updatePickerInput(session = session, inputId = "region_select", 
                                      choices = unique(tabela_brasil$state), selected = unique(tabela_brasil$state))
    }
    
    if (input$level_select=="Regioes") {
      shinyWidgets::updatePickerInput(session = session, inputId = "region_select", 
                                      choices = unique(tabela_regioes$state), 
                                      selected = tabela_regioes$state)
    }
    
    if (input$level_select=="Estados") {
      shinyWidgets::updatePickerInput(session = session, inputId = "region_select", 
                                      choices = unique(tabela_estados$state), 
                                      selected = tabela_estados$state)
    }
    
    if (input$level_select=="Capitais") {
      shinyWidgets::updatePickerInput(session = session, inputId = "region_select", 
                                      choices = unique(tabela_capitais$state), 
                                      selected = tabela_capitais$state)
      
    }
    
  }, ignoreInit = TRUE)
  
  
  
  reactive_db = reactive({
    if (input$level_select=="Brasil") { 
      db = tabela_brasil
    }
    if (input$level_select=="Regioes") { 
      db = tabela_regioes 
      
    }
    if (input$level_select=="Estados") { 
      db = tabela_estados
      
    }
    if (input$level_select=="Capitais") { 
      db = tabela_capitais
      
    }
    
    if (input$outcome_select=="Casos") { 
      db$outcome = db$confirmed
      db$per_100k_outcome = db$confirmed_per_100k
      db$new_outcome = db$new_confirmed
      db$ma7_outcome = db$ma7_confirmed
      db$tx_outcome = db$tx_letalidade
    }
    
    if (input$outcome_select=="Obitos") { 
      db$outcome = db$deaths
      db$per_100k_outcome = db$deaths_per_100k 
      db$new_outcome = db$new_deaths 
      db$ma7_outcome = db$ma7_deaths
      db$tx_outcome = db$tx_letalidade
    }
    
    
    db %>% filter(state %in% input$region_select)
  })
  
  
  
  
  
  #  plots
  
  output$cumulative_plot <- renderPlotly({
    cumulative_plot(reactive_db(), input$minimum_date)
  })
  
  
  
  output$cumulative_per_100k_plot <- renderPlotly({
    cumulative_per_100k_plot(reactive_db(), input$minimum_date)
  })
  
  
  
  output$new_plot <- renderPlotly({
    new_plot(reactive_db(), input$minimum_date)
  })
  
  
  output$ma7_plot <- renderPlotly({
    ma7_plot(reactive_db(), input$minimum_date)
  })
  
  output$tx_plot <- renderPlotly({
    tx_plot(reactive_db(), input$minimum_date)
  })
  
  
}
)


shinyApp(ui, server)
