library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(jcolors)
library(scales)
# library(viridis)

#####################################################################################
######## Pull in the necessary data #################################################
#####################################################################################

get_figure <- function(g) {
  g %>%
    dplyr::rename(County = county) %>%
    ggplot(aes(x=date, y=new_cases_prev7, group = County, color = County, label = County)) +
    geom_point(size=3, shape=16, alpha = .6) +
    geom_line(size = 1, alpha = .6) +
    # geom_smooth() +
    theme_minimal() +
    scale_color_jcolors(palette="pal8") +
    scale_y_continuous(breaks=pretty_breaks(n = 10), labels = comma) +
    labs(x = "Date",
         y = "New cases in previous 7 days",
         caption = "Data source: NY Times",
         title = "COVID-19 Case Tracker: DMV Region") +
    theme(
      # legend.position = "none",
      plot.title = element_text(size = 24, hjust = 0.5),
      legend.position = c(.90, .85),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 16),
      axis.text.x=element_text(hjust=0.5) 
    ) +
    annotate("text",x=as.Date('2020-03-24'), y= .8 * max(g$new_cases_prev7, na.rm = T), label = paste0("Most recent update:\n",month(max(g$date)),"-",day(max(g$date))), color="red", size=5)
  
  
}


#get county-to-msa crosswalk
#xwalk <- readRDS("data/county_to_MSA.rds")
xwalk <- readRDS(url('https://github.com/guga31bb/covid19/raw/master/data/county_to_MSA.rds'))

tictoc::tic('loading csv & joining')
data <- read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")) %>%
  mutate(
    fips = as.integer(fips),
    date = as.Date(date),
    fips = ifelse(county == "New York City", 36005, fips)
  ) %>%
  arrange(fips, date) %>%
  group_by(fips) %>%
  mutate(
    new_cases = cases - dplyr::lag(cases),
    new_deaths = deaths - dplyr::lag(deaths),
    new_cases_prev7 = zoo::rollsumr(new_cases, k = 7, fill = NA)
  ) %>%
  ungroup() %>%
  inner_join(xwalk, by="fips") %>%
  filter(
    cbsa == 'Washington-Arlington-Alexandria, DC-VA-MD-WV'
  )
tictoc::toc()


#####################################################################################
######## Define UI for viewer app ###########################################
#####################################################################################

ui <- fluidPage(
  
  theme = shinytheme("sandstone"),
  # shinythemes::themeSelector(),
  
  # App title ----
  #titlePanel("Choose a game"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      #style = "position:fixed;width:inherit;",
      width = 2,
      
      # Input: Simple integer interval ----
      # sliderInput("year", "Season:",
      #             min = 2009, max = 2019,
      #             value = 2014, sep=""),
      # 
  
      checkboxGroupInput("list", "Counties to show:",
                         unique(data$county),
                         selected = c(
                           'Arlington',
                           'District of Columbia',
                           'Fairfax', 
                            'Frederick', 
                            # "Prince George's",
                            'Montgomery'
                         )
                         ),
      
      actionButton("update", "Update", width = '100%')
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Header + table of distribution ----
      
      # h4(textOutput("text"), align = "center"),
      # h4(textOutput("text2"), align = "center"),
      
      # tags$h2('COVID-19 Cases is DC Region', align = "center"),
      
      plotOutput(outputId = "plot1", height = 'auto')
      
      
    )
    
  )
)

#####################################################################################
######## Define server app ###########################################
#####################################################################################
server <- function(input, output, session) {
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  
  
  output$text <- renderText({
    paste0(input$list, collapse = ', ')
  })
  
  
  #nflscrapr: full data
  fullInput <- eventReactive(
    input$update, {
      data %>% 
        filter(county %in% input$list)
    }, ignoreNULL = FALSE
  )
  
  
  #pass %
  output$plot1 <- renderPlot({
    get_figure(fullInput())
  }, height = function() {
    (9/16) * session$clientData$output_plot1_width
  })
  

}


# Create Shiny app ----
shinyApp(ui, server)
