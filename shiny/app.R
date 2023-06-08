library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(plotly)
library(ggthemes)


##Andmestik

load("andmed.Rda")

riigid <- c("Euroopa", levels(unique(andmed$riik)))


###### Funktsioonid
# Graafikud riikide kohta

riik_graafikud <- function(maa, aasta, kuu) { 
  
  
  # vastavad väärtused
  #Tabelid jooniste jaoks
  
  tabel_a <- andmed %>%
    filter(riik == maa, YEAR == aasta) %>%
    select(MONTH, CO2_QTY_TONNES, CO2_kogus_lennu_kohta)
  
  tabel_k <- andmed %>%
    filter(riik == maa, MONTH == kuu) %>%
    select(YEAR, CO2_QTY_TONNES, CO2_kogus_lennu_kohta)
  
  # väärtused x teljele
  
  if (maa== "Eesti" & aasta == "2017") {
    rooma_nr <- c("IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII")
  } else {
    rooma_nr <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII")}
  
  # Joonised
  
  theme_set(theme_few())
  
  joonis_a1 <- tabel_a %>%
    ggplot(aes(x = MONTH, y = CO2_QTY_TONNES/1000, fill = ifelse(MONTH == kuu, "esil", "tavaline"), color=ifelse(MONTH == kuu, "esil", "tavaline"))) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(name = "MONTH", values = c("red","grey50")) +
    scale_color_manual(name = "MONTH", values = c("red","grey50")) +
    scale_x_discrete(labels = rooma_nr) +
    labs(title = paste0("CO2 üldine heitkogus ", aasta, ". aastal"), x = "", y = "CO2 kogus (1000 t)") +
    theme(axis.text = element_text(size = 11))
  
  joonis_a2 <- tabel_a %>%
    ggplot(aes(x = MONTH, y = CO2_kogus_lennu_kohta, fill = ifelse(MONTH == kuu, "esil", "tavaline"), color = ifelse(MONTH == kuu, "esil", "tavaline"))) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(name = "MONTH", values = c("red","grey50")) +
    scale_color_manual(name = "MONTH", values = c("red","grey50")) +
    scale_x_discrete(labels = rooma_nr) +
    labs(title = paste0("CO2 heitkogus lennu kohta ", aasta, ". aastal"), x = "", y = "CO2 kogus (t)") +
    theme(axis.text = element_text(size = 11))
  
  joonis_k1 <- tabel_k %>%
    ggplot(aes(x = YEAR, y = CO2_QTY_TONNES/1000, fill = ifelse(YEAR == aasta, "esil", "tavaline"), color = ifelse(YEAR == aasta, "esil", "tavaline"))) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(name = "MONTH", values = c("red","grey50")) +
    scale_color_manual(name = "MONTH", values = c("red","grey50")) +
    labs(title = paste0("CO2 üldine heitkogus aastate lõikes (", kuu, ")"), x = "", y = "") +
    theme(axis.text = element_text(size = 11))
  
  joonis_k2 <- tabel_k %>%
    ggplot(aes(x = YEAR, y = CO2_kogus_lennu_kohta, fill = ifelse(YEAR == aasta, "esil", "tavaline"), color = ifelse(YEAR == aasta, "esil", "tavaline"))) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(name = "MONTH", values = c("red","grey50")) +
    scale_color_manual(name = "MONTH", values = c("red","grey50")) +
    labs(title = paste0("CO2 heitkogus lennu kohta aastate lõikes (", kuu, ")"), x = "", y = "") +
    theme(axis.text = element_text(size = 11))
  
  # Jooniste liitmine
  
  tulem <- grid.arrange(joonis_a1, joonis_k1, joonis_a2, joonis_k2)
  
  return(tulem)
}



# Kaart terve Euroopa kohta


euroopa_graafik <- function(aasta, kuu) {
  
  tabel <- andmed %>%
    filter(YEAR == aasta, MONTH == kuu) %>%
    mutate(i_text = paste0(riik, "\n", CO2_QTY_TONNES)) %>%
    select(riik, kood, CO2_QTY_TONNES, i_text)
  
  joonis <- plot_geo(tabel, locations = tabel$kood) %>% 
    add_trace(
      z = ~CO2_QTY_TONNES,
      text = ~i_text,
      color = ~CO2_QTY_TONNES,
      colorscale = 'Reds',
      reversescale = FALSE,
      hoverinfo = "text"
    ) %>%
    layout(
      geo = list(lonaxis = list(range = c(-30, 50)),
                 lataxis = list(range = c(30, 90)),
                 resolution = 50,
                 showland = TRUE, 
                 landcolor = "rgb(229, 229, 229)",
                 showframe = TRUE
      ),
      font = list(family= "Arial", color = "black")) %>%
    colorbar(title = "CO2 heitkogus (t)") %>%
    style(hoverlabel = list(bgcolor = "rgb(238,238,238)", font = list(color = "black")))
  
  
  return(joonis)
}

# UI ----
ui <- fluidPage(
  titlePanel("CO2 heitkogused lennunduses"),
  
  sidebarLayout(
      sidebarPanel(
        selectInput("valik1", "Vali riik", 
                            choices = riigid, selected = head(riigid, 1)),
        selectInput("valik2", "Vali aasta", choices = ""),
        selectInput("valik3", "Vali kuu", choices = ""), width = 3
      ),
      
    mainPanel (
      tabsetPanel(
        tabPanel("Joonised",  br(), conditionalPanel(
          condition = "input.valik1 != 'Euroopa'", plotOutput("joonis")),
          conditionalPanel(
            condition = "input.valik1 == 'Euroopa'", plotlyOutput("kaart"))
        ),
        tabPanel("Selgitus", br(), 'Antud rakendus on tehtud 2023. aasta kevadsemestril õppeaines "Statistiline andmeteadus ja visualiseerimine".
      Andmed pärinevad Eurocontroli (Euroopa Lennunavigatsiooni Ohutuse Organisatsioon) kodulehelt aadressiga ', a("https://www.eurocontrol.int/our-data"), " ja katavad perioodi 01.01.2010-31.03.2023. Esindatud  on 43 Euroopa riiki."))
        
      )
      
     
  )
)

# Server ----
server <- function(input, output, session) {
  
  observe({ 
    if(input$valik1 == "Euroopa") {
      aastad = unique(andmed$YEAR)
    } else {
      aastad = unique(andmed[andmed$riik == input$valik1, "YEAR"])
    }
   
    updateSelectizeInput(session, "valik2",
                         label = "Vali aasta",
                          choices = aastad,
                          selected = head(aastad, 1))})
    
   observe({if (input$valik1 == "Euroopa") {
      kuud = unique(andmed[andmed$YEAR == input$valik2, "MONTH"]) #igaks juhuks
    } else {
      kuud = andmed[andmed$riik == input$valik1 & andmed$YEAR == input$valik2, "MONTH"]
    }
    
    
    updateSelectizeInput(session, "valik3",
                         label = "Vali kuu",
                         choices = kuud,
                         selected = head(kuud, 1))
  })
   
   observe({if (input$valik1 == "Euroopa") {
     output$kaart <- renderPlotly(euroopa_graafik(input$valik2, input$valik3))
     } else {
       output$joonis <- renderPlot({
         riik_graafikud(input$valik1, input$valik2, input$valik3)
       }, height = 600, width = "auto")}
     })


} 

# Shiny app ----
shinyApp(ui = ui, server = server)