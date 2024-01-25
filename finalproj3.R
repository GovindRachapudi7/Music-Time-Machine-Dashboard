library(shiny)
library(tidyverse)
library(janitor)
library(shinythemes)  
library(ggplot2)
library(ggthemes)
spotify <- read_csv("spotify-2023.csv")
spotify <- clean_names(spotify)
spotify$artist_s_name <- iconv(spotify$artist_s_name, from = "UTF-8", to = "UTF-8", sub = "byte")
spotify <- spotify %>%
  filter(!grepl("[^a-zA-Z0-9 ]", artist_s_name))
spotify <- na.omit(spotify)
spotify$key <- na.omit(spotify$key)

songs <- spotify %>%
  select(
    Track = track_name,
    Artist = artist_s_name,
    streams,
    bpm,
    key,
    energy = energy_percent,
    Year = released_year
  )

ui <- fluidPage(
  theme = shinytheme("flatly"),  
  
  titlePanel("Music Time Machine Dashboard!", windowTitle = "Spotify Analytics"),
  helpText("Explore the secrets to the top hits from 1930 to 2023!", align = "left"),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        radioButtons("music_stat", "Choose a musical statistic:",
                     choices = c("Streams", "BPM", "Key", "Energy", "Artist"),
                     selected = "Streams"),
        sliderInput("Year", "Choose release date (year) of songs",
                    min = 1930, max = 2023, value = c(1930, 2023)),
        checkboxInput("show_summary", "Show statistics and summaries", value = TRUE),
        selectInput("color", "Choose a color!",
                    choices = c("indianred", "aquamarine3", "cadetblue3", "gray48", "Coral"),
                    selected = "Blue")
      ),
      img(src = "https://routenote.com/blog/wp-content/uploads/2022/01/Top-10-Spotify-Jul-22.jpg", 
          width = "100%", height = "auto")
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Histogram", 
                 plotOutput("histogram", height = "400px")),
        tabPanel("Summary", fluidRow(
          verbatimTextOutput("summary"),
          tableOutput("summary_table")
        ))
      )
    )
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #414141  ;
        color: white;
      }
      .well { 
        background-color: #5E86A5 ;  
        border: 1px solid #000000 ;  
        border-radius: 7px; 
        padding: 16px; 
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
      }
      .nav-tabs { 
        border-bottom: 1px solid #000000;  
        margin-bottom: 15px;
        color: white;
      }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
        background-color: #000000;  
        color: white;
        border: 1px solid #797979; 
        border-bottom-color: transparent;
      }
    "))
  )
)

server <- function(input, output) {
  filtered_songs <- reactive({
    songs %>%
      filter(between(Year, input$Year[1], input$Year[2]))
  })
  
  stat_change <- reactive({
    switch(input$music_stat,
           "Streams" = as.numeric(filtered_songs()$streams)/1e9,
           "BPM" = filtered_songs()$bpm,
           "Key" = as.factor(filtered_songs()$key),
           "Energy" = filtered_songs()$energy,
           "Artist" = as.factor(filtered_songs()$Artist))
  })
  
  output$histogram <- renderPlot({
    variable <- stat_change()
    
    if (is.numeric(variable)) {
      ggplot(filtered_songs(), aes(x = variable)) +
        geom_histogram(bins = 30, fill = input$color, alpha = 0.9, color = "black") +
        labs(title = input$music_stat, x = input$music_stat, y = 'Frequency in Songs') +
        theme_stata()
    } else if (input$music_stat == "Key") {
      ggplot(data = filtered_songs(), aes(x = key, fill = as.factor(key))) +
        geom_bar() +
        labs(title = "Distribution of Keys in Popular songs", x = "Key", y = "Frequency") +
        theme_stata() +
        scale_fill_discrete(name = "Key") +
        scale_x_discrete(drop = FALSE)
    } else if (input$music_stat == "Artist") {
      artist_song <- filtered_songs() %>%
        drop_na(Artist) %>%
        group_by(Artist) %>%
        filter(!grepl("[^a-zA-Z0-9 ]", Artist)) %>% 
        summarize(Songs = n())
      artist_song <- arrange(artist_song, desc(Songs))
      artist_song <- slice(artist_song, 1:10)
      
      ggplot(artist_song, aes(x = Artist, y = Songs, fill = Artist)) +
        geom_bar(stat = "identity") +
        labs(title = "Distribution of Artists in Popular songs", x = "Artist", y = "Frequency") +
        theme_fivethirtyeight() +
        scale_fill_discrete(name = "Artist") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        coord_flip()
    }
  })
  
  output$summary <- renderText({
    if (input$show_summary) {
      variable <- stat_change()
      if (is.numeric(variable)) {
        paste(
          "Mean:", round(mean(variable, na.rm = TRUE), 2), "\n",
          "Standard Deviation:", round(sd(variable, na.rm = T), 2), "\n",
          "Five Number Summary: ", "\n",
          paste(
            c("Min:", "Lower Quartile:", "Median:", "Upper Quartile:", "Max:"),
            fivenum(variable),
            collapse = "\n"
          )
        )
      }
    }
  })
  
  output$summary_table <- renderTable({
    if(input$show_summary){
      if (input$music_stat == "Artist") {
        table_data <- as.data.frame(table(filtered_songs()[[input$music_stat]]))
        colnames(table_data) <- c(input$music_stat, "Frequency")
        table_data <- arrange(table_data, desc(Frequency))
        table_data <- slice_head(table_data, n=10)
        table_data
      } else if (input$music_stat == "Key"){
        key_data <- as.data.frame(table(filtered_songs()[["key"]]))
        colnames(key_data) <- c(input$music_stat, "Frequency")
        key_data <- arrange(key_data, desc(Frequency))
        key_data <- slice_head(key_data, n=10)
        key_data
      }
    }
  })
}

shinyApp(ui, server)










