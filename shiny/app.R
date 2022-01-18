library(shiny)
library(plotly)
library(shinythemes)
library(treemapify)
library(ggthemes)
library(highcharter)
library(tidyverse)
library(data.table)
library(readxl)
library(wordcloud2)
library(viridis)
library(DT)
library(scales)
library(png)

options(scipen = 999)


############################## UI #######################################


ui <- fluidPage(
    theme = shinytheme("cosmo"),
    
    
    titlePanel(title= h1('Dashboard Malvinas Argentinas')),
    
    tabsetPanel(
        tabPanel('Imagen de políticos locales',
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(inputId = "politicos_locales", 
                                     label = h3("Político"),
                                     choices = unique(historico_locales$politicos_locales),
                                     selected = unique(historico_locales$politicos_locales)[1],
                                     multiple = FALSE)),
                     mainPanel( 
                         plotlyOutput('grafico_historicos', width = 900, height = 700))
                     
                 )
        ),
        
        
        tabPanel('Gestión local')))



############################## SERVER #######################################

server <- function(input,output){
    
    df_filt_historicos <- reactive({
        df_filt_historicos = historico_locales %>% 
            filter(politicos_locales == input$politicos_locales)
    })
    
    output$grafico_historicos <- renderPlotly({
        
        p <-  df_filt_historicos() %>%
            ggplot(aes(x = factor (fecha, level = fechas_correctas), y = frecuencia, label =  sprintf("%1.1f%%", 100*frecuencia),
                       group = imagenes_locales))+
            geom_line(aes(colour = imagenes_locales))+
            geom_point()+
            scale_y_continuous(labels = scales::percent)+
            theme_classic()+
            theme(axis.ticks=element_blank(),
                  axis.line=element_blank(),
                  axis.text.x = element_text(angle = 90, vjust=0.5, size = 6),
                  axis.title=element_blank(),
                  strip.text.y = element_text(angle = 0))+
            scale_colour_manual(values = color_imagenes)+
            labs(x = element_blank(), 
                 y = element_blank(),
                 label = element_blank(),
                 fill = element_blank())
        ggplotly(p)%>%
            layout(showlegend = FALSE)
    })
}
############################## APP #######################################
shinyApp(ui = ui, server = server)
