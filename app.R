library(needs)
needs(shiny, tidyverse, lubridate)

options(readr.show_col_types = FALSE)

p.width <- 900
p1.height <- 700

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Gropoint data portal"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("dates", 
                  label = h4("Select date range"), 
                  min = as_date("2023-06-11"), 
                  max = as_date(Sys.time()), 
                  value = c(as_date("2023-06-11"), as_date(Sys.time()))),
      
      fluidRow(
        column(5,
               radioButtons("Tree.view",
                            label = h4("Select tree"),
                            choices = c("ET1", "ET2", "ET3", "ET4",
                                        "ET5", "ET6", "ET7", "ET8",
                                        "FB1", "FB2", "FB3", "FB4",
                                        "FB5", "FB6", "FB7", "FB8",
                                        "TV1", "TV2", "TV3", "TV4"),
                            selected = "ET1")),
        column(7,
               fluidRow(
                 checkboxGroupInput("Depth.view",
                              label = h4("Select Depth"),
                              choices = c("1", "2", "3", "4",
                                          "5", "6", "7"),
                              selected = c("1", "2", "3", "4", "5", "6", "7"))),
               fluidRow(
                 radioButtons("Location.view",
                              label = h4("Select Location"),
                              choices = c("Inner", "Outer"),
                              selected = "Inner")),
               fluidRow(
                 radioButtons("Variable.view",
                              label = h4("Select Variable"),
                              choices = c("Moisture", "Temperature"),
                              selected = "Moisture")))),
      downloadButton("downloadData", "Download")
    ),
    
    mainPanel(
      "Graph shows data until:",
      verbatimTextOutput("maxdate.output"),
      plotOutput("plot1", width = p.width, height = p1.height))
  )
)

# Server -----------------------------------------------------------------

server <- function(input, output, session) {
  
  dataInput <- reactive({
    read_csv(file.path("Soil_data_L2", str_c(input$Tree.view, "_GP_L2.csv"))) 
  })
  
  dataInput2 <- reactive({
    dataInput() %>% 
      filter(Location == input$Location.view) %>% 
      filter(Depth %in% input$Depth.view) %>%
      filter(Timestamp >= as.POSIXct(input$dates[1], tz = "UTC")  &
               Timestamp <= as.POSIXct(input$dates[2], tz = "UTC")) %>% 
      pivot_longer(5:6, names_to = "M.or.T", values_to = "Measure") %>% 
      filter(M.or.T == input$Variable.view)
  })
  
  output$plot1 <- renderPlot({
    ggplot(dataInput2()) +
      geom_line(aes(x = Timestamp, y = Measure, color = factor(Depth))) +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 18),
            axis.title.y = element_text(size = 20),
            axis.text.y = element_text(size = 18),
            plot.title = element_text(size = 24)) +
      ggtitle(str_c(input$Tree.view, "_", input$Location.view, "_", input$Variable.view)) 
  })
  
  output$maxdate.output <- renderText({
    as.character(max(dataInput2()$Timestamp, na.rm = T))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      str_c(str_c(input$Tree.view, input$Variable.view, input$Location.view,
                  input$dates[1], input$dates[2], sep = "_"),
            ".csv")
    },
    content = function(file) {
      write_csv(dataInput2(), file)
    })
}

# Run app ----------------------------------------------------------------

shinyApp(ui, server)

# Troubleshooting --------------------------------------------------------------

# testServer(server, {
#   session$setInputs(Tree.view = "FB1")
#   session$setInputs(Location.view = "Inner")
#   session$setInputs(Variable.view = "Moisture")
#   session$setInputs(Depth.view = c("1", "2"))
#   session$setInputs(dates = c(min = ymd("2023-09-01"),
#                               max = ymd("2023-12-01")))
#   # session$setInputs(Time.res = "15 min")
#   print(dataInput2())
# })
# 
# d <- read_csv(file.path("Soil_data_L2", "ET1_GP_L2.csv"))
# str(d)
