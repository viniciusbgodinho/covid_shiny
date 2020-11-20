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


dados <- dados %>%
  rename(confirmed=last_available_confirmed ,deaths=last_available_deaths ,confirmed_per_100k=last_available_confirmed_per_100k_inhabitants) %>%
  mutate(deaths_per_100k= (100000*deaths)/estimated_population_2019) 


###Brasil

#Agrupando e calculando média móvel

dados_brasil <- dados %>%
  filter(place_type=="state") %>%
  arrange(date) %>%
  group_by(date)%>%
  summarise(confirmed=sum(confirmed),deaths=sum(deaths),
            new_confirmed=sum(new_confirmed), new_deaths=sum(new_deaths),
            confirmed_per_100k=sum(confirmed_per_100k), deaths_per_100k=sum(deaths_per_100k))%>%
  mutate(ma7_confirmed = stats::filter(new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

# Transformando média móvel em número

dados_brasil$ma7_confirmed <- as.numeric(dados_brasil$ma7_confirmed)
dados_brasil$ma7_deaths <- as.numeric(dados_brasil$ma7_deaths)


###Região
#Agrupando e calculando a média móvel

dados_regiao <- dados %>%
  filter(place_type=="state") %>%
  arrange(date) %>%
  mutate(state = case_when(
    state == "DF" |state=="GO" |state=="MT" |state=="MS" ~ "Centro-Oeste",
    state=="ES" |state=="MG" |state=="RJ" |state=="SP" ~ "Sudeste",
    state=="PR" |state=="RS" |state=="SC" ~ "Sul",
    state=="AL" |state=="BA" |state=="CE"|state=="PB"|state=="PE"|state=="PI"|state=="MA"|state=="RN"|state=="SE" ~ "Centro-Oeste",
    state=="AC" |state=="AP" |state=="AM"|state=="PA"|state=="RO"|state=="RR"|state=="TO" ~ "Norte")
  )%>%
  group_by(state,date)%>%
  summarise(confirmed=sum(confirmed),deaths=sum(deaths),
            new_confirmed=sum(new_confirmed), new_deaths=sum(new_deaths),
            confirmed_per_100k=sum(confirmed_per_100k), deaths_per_100k=sum(deaths_per_100k))%>%
  mutate(ma7_confirmed = stats::filter(new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

# Transformando média móvel em número

dados_regiao$ma7_confirmed <- as.numeric(dados_regiao$ma7_confirmed)
dados_regiao$ma7_deaths <- as.numeric(dados_regiao$ma7_deaths)


###Estados

dados_estados <- dados %>%
  filter(place_type=="state") %>%
  arrange(date) %>%
  group_by(state,date)%>%
  summarise(confirmed=sum(confirmed),deaths=sum(deaths),
            new_confirmed=sum(new_confirmed), new_deaths=sum(new_deaths),
            confirmed_per_100k=sum(confirmed_per_100k), deaths_per_100k=sum(deaths_per_100k))%>%
  mutate(ma7_confirmed = stats::filter(new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

# Transformando média móvel em número

dados_estados$ma7_confirmed <- as.numeric(dados_estados$ma7_confirmed)
dados_estados$ma7_deaths <- as.numeric(dados_estados$ma7_deaths)


#Agrupando e calculando a média móvel

dados_capitais <- dados %>%
  filter(place_type=="city", city_ibge_code=="1100205" |  city_ibge_code=="1302603"|  
           city_ibge_code=="1200401"|
           city_ibge_code=="5002704"| city_ibge_code=="1600303"|  city_ibge_code=="5300108"|
           city_ibge_code=="1400100"| city_ibge_code=="5103403"|  city_ibge_code=="1721000"|
           city_ibge_code=="2211001"| city_ibge_code=="3550308"|  city_ibge_code=="3304557"| 
           city_ibge_code=="1501402"| city_ibge_code=="2111300"|  city_ibge_code=="5208707"|
           city_ibge_code=="2927408"| city_ibge_code=="2704302"|  city_ibge_code=="4314902"|
           city_ibge_code=="4106902"|  city_ibge_code=="4205407"|  city_ibge_code=="3106200"|
           city_ibge_code=="2304400"| city_ibge_code=="2611606"|  city_ibge_code=="2507507"| 
           city_ibge_code=="2800308"| city_ibge_code=="2408102"|  city_ibge_code=="3205309") %>%
  arrange(date) %>%
  group_by(city,date)%>%
  summarise(confirmed=sum(confirmed),deaths=sum(deaths),
            new_confirmed=sum(new_confirmed), new_deaths=sum(new_deaths),
            confirmed_per_100k=sum(confirmed_per_100k), deaths_per_100k=sum(deaths_per_100k))%>%
  mutate(ma7_confirmed = stats::filter(new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

# Transformando média móvil em número

dados_capitais$ma7_confirmed <- as.numeric(dados_capitais$ma7_confirmed)
dados_capitais$ma7_deaths <- as.numeric(dados_capitais$ma7_deaths)


  





###Ajustando a Tabela para rodar o painel Shiny


tabela_brasil <- dados_brasil %>%
  mutate(state = "Brasil") %>%
  select(date,confirmed,deaths,confirmed_per_100k,deaths_per_100k,new_confirmed,
         new_deaths,ma7_confirmed,ma7_deaths,state) %>%
  mutate(tx_letalidade = deaths/confirmed*100)


tabela_regioes <- dados_regiao %>%
  select(date,confirmed,deaths,confirmed_per_100k,deaths_per_100k,new_confirmed,
         new_deaths,ma7_confirmed,ma7_deaths,state) %>%
  mutate(tx_letalidade = deaths/confirmed*100)


tabela_estados <- dados_estados %>%
  select(date,confirmed,deaths,confirmed_per_100k,deaths_per_100k,new_confirmed,
         new_deaths,ma7_confirmed,ma7_deaths,state) %>%
  mutate(tx_letalidade = deaths/confirmed*100)



tabela_capitais <- dados_capitais %>%
  select(date,confirmed,deaths,confirmed_per_100k,deaths_per_100k,new_confirmed,
         new_deaths,ma7_confirmed,ma7_deaths,city) %>%
  mutate(tx_letalidade = deaths/confirmed*100) %>%
  rename(state=city)




df_total <- rbind.data.frame(tabela_brasil,tabela_regioes,tabela_estados,tabela_capitais)


tabela_regioes <- as.data.frame(tabela_regioes)

df_total$date <- as.Date(df_total$date)
tabela_brasil$date <- as.Date(tabela_brasil$date)
tabela_capitais$date <- as.Date(tabela_capitais$date)
tabela_estados$date <- as.Date(tabela_estados$date)
tabela_regioes$date <- as.Date(tabela_regioes$date)




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
