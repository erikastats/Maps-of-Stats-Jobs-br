#
# This app shows the follow analysis
# - The amout of Jobs for statisticians in Brazil during 88 days in the last
# semester of 2019
# View the online app here:
#
#    https://erika-sm.shinyapps.io/maps_of_stats_jobs_br/
#


# Load required packages --------------------------------------------------

library(shiny)
library(tidyverse)
library(leaflet)
library(shinyWidgets)
library(readr)
library(readxl)
library(sf)
library(rmapshaper) # to simplify
library(abjutils)
library(DT)
library(leaflet.extras)


# Importing Data ----------------------------------------------------------

Dados <- read_csv("Dados.csv")
cidades_shape = st_read("BRUFE250GC_SIR.shp", quiet = T) %>% 
    ms_simplify() 
Sigla_Estados <- read_excel("Sigla Estados.xlsx") %>% 
    mutate(`Estado do Brasil` = `Estado do Brasil` %>% toupper()) %>% 
    select(-3)

cidades_shape <- cidades_shape %>% 
    left_join(Sigla_Estados, by = c("NM_ESTADO" = "Estado do Brasil"))

# Cleaning Data -----------------------------------------------------------

Clean_data <- Dados %>% select(-c(1,16:41)) %>% 
    distinct(idVaga, Cidade,.keep_all = TRUE ) %>% 
    filter(UF != "NAO INFORMADO") %>% 
    left_join(Sigla_Estados, by = c("UF" = "Sigla"))


# All Choices -------------------------------------------------------------
Clean_data <- Clean_data %>% 
    mutate(Perfil = ifelse(Perfil == "Profissional", "Professional", "Internship"),
           Pesquisa = case_when(
               Pesquisa == "estatístico" ~ "statistician",
               Pesquisa == "cientista de dados" ~ "data scientist",
               TRUE ~ Pesquisa
           ))
jtitle <-  Clean_data %>% select(Pesquisa) %>% unique() %>% pull

jprofile <-  Clean_data %>% select(Perfil) %>% unique() %>% pull
jstates <- Clean_data %>% select(`Estado do Brasil`) %>% unique() %>% pull
jsites <- Clean_data %>% select(Site) %>% unique() %>% pull

# UI ----------------------------------------------------------------------

ui <- navbarPage("Stats Jobs Brazil", id = "nav",
                 
                 tabPanel("Interactive map",
                          div(class="outer",
                              
                              tags$head(
                                  # Include our custom CSS
                                  includeCSS("style.css")
                              ),
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width="100%", height="100%"),
                              
                              # Shiny versions prior to 0.11 should use class = "modal" instead.
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            
                                            h2("Jobs explorer"),
                                            
                                            selectInput("job_title","Job Title",  c(jtitle, "All"), selected = "All"),
                                            selectInput("job_profile","Job Profile",  c(jprofile, "All"), selected = "All"),
                              ),
                              
                              tags$div(id="cite",
                                       'Data source ', tags$em('Catho and Vagas.com, 2020'), ' researched by Érika S. Machado and Kaíque F. H. de Souza.'
                              )
                          )
                 )
                 ,
                 tabPanel("Data explorer",
                          fluidRow(
                              column(6,
                                     selectInput("states", "States", c("All states"="", jstates), multiple=TRUE)
                              ),
                              column(6,
                                     conditionalPanel("input.states",
                                                      selectInput("site", "Reserched Site", c("All sites"="", jsites), multiple=TRUE)
                                     )
                              )
                              
                          ),
                          hr(),
                          DT::dataTableOutput("jobtable")
                 )
                 )

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    output$map <- renderLeaflet({
        
        dados <- Clean_data
        if( input$job_title != "All"){
            dados <- dados %>% filter(Pesquisa == input$job_title)
        }
        if( input$job_profile != "All"){
            dados <- dados %>% filter(Perfil == input$job_profile)
        }
            Summ_data <- dados %>% group_by(UF, `Estado do Brasil`) %>% 
            summarise(Total_jobs = sum(QuantidadeVaga))
            
            pal <- colorNumeric(
                palette = "Blues",
                domain = Summ_data$Total_jobs)
            
        Data_shape <- cidades_shape %>% 
            left_join(Summ_data, by = c("Sigla" = "UF")) %>% 
            mutate(Total_jobs = Total_jobs %>% replace_na(0),
                   legenda = paste0("<strong>",
                                    `NM_ESTADO`,
                                    "</strong> ",
                                    "<br/>","&#9679; ",
                                    "<b>Amount:</b> ",
                                    Total_jobs, 
                                    "<br/>") %>% lapply(htmltools::HTML)) 
            
        
        Data_shape %>% 
            leaflet() %>% 
            addTiles() %>% 
            addPolygons(
                fillColor = ~pal(Total_jobs) ,
                group = "Tipo",
                fillOpacity = 1,
                color = "black", 
                weight = 0.5, 
                opacity = 0.25,
                # popup = ~legenda,
                # popupOptions = ~leg_op,
                highlight = highlightOptions(
                    weight = 3,
                    color = "#666",
                    fillOpacity = 1,
                    bringToFront = F),
                label = ~legenda
                # ,
                # labelOptions = ~leg_op
                ) %>%
            
            addEasyButton(easyButton(
                icon="fa-crosshairs", title="Overview",
                onClick=JS("function(btn, map){ map.setView([-15.966, -49.57], 7); }"))) %>% 
            
            
            addFullscreenControl()
            
        
     
 })
 

    
 output$jobtable <- renderDataTable({
     dados <- Clean_data %>% select(1,2,14,5,6,7,8,9,11,12, 15)

     if(!is.null(input$states)){
         dados <- dados %>% filter(`Estado do Brasil` == input$states)
     }
     if(!is.null(input$site)){
         dados <- dados %>% filter(Site == input$states)
     }
     
     dados %>% datatable()
     
 })

}

# Run the application 
shinyApp(ui = ui, server = server)
