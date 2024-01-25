# Music Time Machine Dashboard

This R Shiny dashboard allows interactive exploration of a dataset of 5000 popular songs across 9 decades from the 1930s to 2020s. 

## Key Features

- Select any time period from 1930 to 2020 to filter songs
- Choose variable to analyze: 
    - Beats Per Minute (BPM)
    - Number of Song Streams
    - Artist Names
    - Energy Level
    - Musical Key
- Dynamically generated data visualization using ggplot2
- Customize graph color palette
- Summary statistics table for selected time period and variable 

## Data

The core dataset was compiled by Nidula Elgiriyewithana on kaggle.com and contains 5000 popular songs with the following key variables:

- Year - Year song was released 
- Artist - Song artist
- StreamCount - Number of streams on music platform
- BPM - Beats per minute tempo
- Energy - Subjective energy level rating
- Key - Musical key song is composed in

## Usage

The app is running at [https://hfiirs-govind0na0-rachapudi.shinyapps.io/GovindApp/].

To use, simply:   

1. Select a time period 
2. Choose variable to analyze
3. Pick color palette
4. Explore graph and statistics table!

Additional variables and time periods can be added by modifying `global.R` and `server.R`.

## Built With

- [R](https://www.r-project.org/) - Core data analysis  
- [Shiny](https://shiny.rstudio.com/) - Dashboard framework
- [ggplot2](https://ggplot2.tidyverse.org/) - Visualizations     
- [dplyr](https://dplyr.tidyverse.org/) - Data manipulation

