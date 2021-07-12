## Shiny apps

library(tidyverse)
# downlaod data
drs_df_raw_link <- "https://raw.githubusercontent.com/CartONG/R_training/main/df19.csv?token=ALMJAS5FZNUMPT2YXW2HM23A4B6Z2"

df19 <- read_csv(drs_df_raw_link,
                 col_types = cols(.default = "c")) 

df19s <- df19 %>% 
    select(BE, Country, Crop1, Crop1KG, Crop1HA) %>% 
    filter(!is.na(Crop1)) %>% 
    filter(BE == "Baseline")

df19s$Crop1KG <- as.numeric(df19s$Crop1KG)
df19s$Crop1HA <- as.numeric(df19s$Crop1HA)

df19s <- df19s %>% 
    mutate(Yield = Crop1KG/Crop1HA) %>% 
    filter(!is.na(Yield)) %>% 
    mutate(Check = Yield < 150)
 
library(shiny)



# Define UI for application that draws a histogram
ui <- fluidPage(
    
     titlePanel("UNHCR Liveilhoods 2019 - Yield Data Quality Check"),
     sidebarLayout(
        sidebarPanel(
            selectInput("country", "Select country", 
                        choices= unique(df19s$Country)),
            selectInput("crops", "Select crops", 
                        choices = NULL),
            sliderInput("bins",
                        "Histogram binwidth:",
                        min = 50,
                        max = 500,
                        value = 150),
            
            hr(),
            helpText("Use the binwidth slider to adjust the histogram")
            ),
        mainPanel(
            plotOutput("plot")
                  )
        
    )
)



# Define server logic required to draw a histogram§
server <- function(session, input, output) {
    
    observe({
        print(input$country)
        x <- df19s %>% 
            filter(Country == input$country) %>% 
            select(Crop1)
        updateSelectInput(session, "crops", "Crops",
                          choices = unique(x))
            
    })
    

    output$plot <- renderPlot(ggplot(df19s, aes(x = Yield, fill = Check))+
                                  geom_histogram(binwidth = input$bins, 
                                                 data = df19s[df19s$Crop1 == input$crops,])+
                                                 
                                  scale_fill_manual(values = c("#0072BC","red4"))+
                                  labs(title = paste(input$country, " - ", input$crops, " Yield Check"),
                                       x = paste(input$crops, " yield (Kg/Ha)"),
                                       y = "Number of observations")
                              )
}


# Run the application 
shinyApp(ui = ui, server = server)
