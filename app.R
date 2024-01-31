library(needs)
needs(shiny, tidyverse, lubridate)

options(readr.show_col_types = FALSE)
source("app_functions_soil.R")

p.width <- 900
p1.height <- 700

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Gropoint data portal"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("daterange", 
                  label = h4("Select date range"), 
                  min = as_date("2023-06-11"), 
                  max = as_date(Sys.time()), 
                  value = c(as_date("2023-06-11"), as_date(Sys.time()))),
      
      fluidRow(
        column(3,
               radioButtons("fixed.y",
                            label = h4("Fix y-axis?"),
                            choices = c("Yes", "No"),
                            selected = "No")),
        column(9,
               sliderInput("yrange",
                           label = h4("Select y-axis range"),
                           min = 0,
                           max = 100,
                           value = c(0, 100)))),
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
                              selected = "Moisture")),
               fluidRow(
                 radioButtons("Time.res",
                              label = h4("Select time resolution"),
                              choices = c("Hourly", "Daily", "Weekly"),
                              selected = "Hourly")))),
      
      titlePanel("Download options"),
      
      radioButtons("Time_format",
                   label = h4("Select time format"),
                   choices = list("ISO", "Excel_ready"),
                   selected = "ISO"),
      
      downloadButton("downloadData", "Download")
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot",
                 "Graph shows data until:",
                 verbatimTextOutput("max.date.out"),
                 plotOutput("plot", width = p.width, height = p1.height,
                            click = "plot_click",
                            brush = "plot_brush"),
                 verbatimTextOutput("plot_clickinfo"),
                 plotOutput("plot_brushedpoints")),
        tabPanel("Data", tableOutput("data1"), tableOutput("data2")),
        tabPanel("Summary", tableOutput("summary"))))
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
      filter(Timestamp >= as.POSIXct(input$daterange[1], tz = "UTC")  &
               Timestamp <= as.POSIXct(input$daterange[2], tz = "UTC")) %>% 
      pivot_longer(5:6, names_to = "M.or.T", values_to = "Measure") %>% 
      filter(M.or.T == input$Variable.view)
  })
  
  dataInput3 <- reactive({
    if(input$Time.res == "Hourly"){
      dataInput2()
    } else if(input$Time.res == "Daily"){
      dataInput2() %>%
        group_by(Tree, Location, Depth, M.or.T, Timestamp = floor_date(Timestamp, "day")) %>%
        summarise(across(where(is.numeric), ~mean(., na.rm = T))) %>%
        ungroup()
    } else if(input$Time.res == "Weekly"){
      dataInput2() %>%
        group_by(Tree, Location, Depth, M.or.T, Timestamp = floor_date(Timestamp, "week")) %>%
        summarise(across(where(is.numeric), ~mean(., na.rm = T))) %>%
        ungroup()}
  })

## Plots ------------------------------------------------------------------

  output$max.date.out <- renderText({
    as.character(max(dataInput3()$Timestamp, na.rm = T))
  })
  
  output$plot <- renderPlot({
    p <- ggplot(dataInput3()) +
      geom_line(aes(x = Timestamp, y = Measure, color = factor(Depth))) +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 18),
            axis.title.y = element_text(size = 20),
            axis.text.y = element_text(size = 18),
            plot.title = element_text(size = 24)) +
      labs(color = "Depth") +
      ggtitle(str_c(input$Tree.view, "_", input$Location.view, "_", input$Variable.view)) 
    
    if(input$fixed.y == "Yes"){
      p <- p +
        ylim(input$yrange[1], input$yrange[2])
    }
    p
  })
  
  output$plot_clickinfo <- renderPrint({
    val <- nearPoints(dataInput2(), input$plot_click, maxpoints = 1)
    unique(val$Timestamp)
  })
  
  output$plot_brushedpoints <- renderPlot({
    dat <- brushedPoints(dataInput2(), input$plot_brush)
    if (nrow(dat) == 0)
      return()
    ggplot(dat) +
      geom_line(aes(x = Timestamp, y = Measure, color = factor(Depth))) +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 18),
            axis.title.y = element_text(size = 20),
            axis.text.y = element_text(size = 18),
            plot.title = element_text(size = 24))
  })

## Data and summaries ------------------------------------------------------

  data.for.summaries <- reactive({
    dataInput2() %>% 
      mutate(Timestamp = as.character(Timestamp)) 
  })
  
  output$data1 <- renderTable({
    head(data.for.summaries())
  })
  
  output$data2 <- renderTable({
    tail(data.for.summaries())
  })
  
  output$summary <- renderTable({
    summarise_soil(data.for.summaries())
  })

## Download data options --------------------------------------------------------

  data.for.download <- reactive({
    if(input$Time_format == "ISO"){
      dataInput3()
    } else if(input$Time_format == "Excel_ready"){
      dataInput3() %>%
        mutate(Timestamp = as.character(Timestamp))
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      str_c(str_c(input$Tree.view, "Soil", input$Variable.view, input$Location.view,
                  str_sub(as.character(input$daterange[1]), start = 1, end = 10), 
                  str_sub(as.character(input$daterange[2]), start = 1, end = 10), 
                  sep = "_"),
            ".csv")
    },
    content = function(file) {
      write_csv(data.for.download(), file)
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
#   session$setInputs(daterange = c(min = ymd("2023-09-01"),
#                               max = ymd("2023-12-01")))
#   # session$setInputs(Time.res = "15 min")
#   print(dataInput2())
# })
# 
# d <- read_csv(file.path("Soil_data_L2", "ET1_GP_L2.csv"))
# str(d)
