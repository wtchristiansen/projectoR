library(shiny)
library(ggplot2)
library(shinyWidgets)

example_data <- read.csv("qog.csv") # Update the path to your qog.csv file

ui <- fluidPage(
  titlePanel("projectoR - ggplot2 in seconds"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      actionButton("loadExample", "Load Example Dataset"),
      uiOutput("xVarUI"),
      uiOutput("yVarUI"),
      checkboxInput("transformX", "Transform X Variable", value = FALSE),
      checkboxInput("transformY", "Transform Y Variable", value = FALSE),
      selectInput("transformXType", "X Transformation Type", choices = c("as.factor", "as.numeric"), selected = "as.factor"),
      selectInput("transformYType", "Y Transformation Type", choices = c("as.factor", "as.numeric"), selected = "as.factor"),
      selectInput("geom", "Geometry",
                  choices = c("Point" = "geom_point", "Line" = "geom_line", "Bar" = "geom_bar", "Boxplot" = "geom_boxplot")),
      colorPickr("color", "Color", preview = TRUE),
      pickerInput("fill", "Fill Color", choices = c("Blue", "Red", "Green", "Yellow"), options = list(`style` = "btn-inverse"), multiple = FALSE),
      selectInput("theme", "Theme",
                  choices = c("Default" = "theme_gray", "Minimal" = "theme_minimal", "Classic" = "theme_classic", "Light" = "theme_light", "Dark" = "theme_dark")),
      sliderInput("pointSize", "Point Size", min = 1, max = 5, value = 2),
      selectInput("ciLevel", "Confidence Level",
                  choices = c("None", "90%" = 0.90, "95%" = 0.95, "99%" = 0.99, "99.99%" = 0.9999)),
      checkboxInput("fitLine", "Fit Line (LM)", value = FALSE),
      textInput("xTitle", "X-axis Title"),
      textInput("yTitle", "Y-axis Title"),
      textInput("graphTitle", "Graph Title"),
      textInput("graphCaption", "Graph Caption"),
      numericInput("xMin", "X-axis Min", value = NA),
      numericInput("xMax", "X-axis Max", value = NA),
      numericInput("yMin", "Y-axis Min", value = NA),
      numericInput("yMax", "Y-axis Max", value = NA),
      checkboxInput("discreteX", "Discrete X-axis", value = FALSE),
      checkboxInput("discreteY", "Discrete Y-axis", value = FALSE),
      actionButton("generatePlot", "Generate Plot"),
      downloadButton("downloadPlot", "Download Plot"),
      tags$hr(),
      tags$a(href = "https://william-christiansen.shinyapps.io/data_distillery/", target = "_blank", class = "btn btn-default", "Dirty data?")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Learn It!", tags$iframe(style = "height:600px; width:100%", src = "ggplot2_guide.pdf"))
      ),
      # Footer
      div(
        class = "footer",
        style = "margin-top: 20px; text-align: center; color: #777;",
        "Created by William Christiansen, Ph.D. - ",
        a(href = "mailto:w.t.christiansen@msmary.edu", "w.t.christiansen@msmary.edu"),
        " | ",
        a(href = "https://sites.google.com/vt.edu/williamchristiansen/", "Visit My Website")
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  
  observeEvent(input$file1, {
    df <- read.csv(input$file1$datapath)
    data(df)
  })
  
  
  observeEvent(input$loadExample, {
    data(example_data)
    updateSelectInput(session, "xVar", choices = names(example_data))
    updateSelectInput(session, "yVar", choices = names(example_data))
  })
  
  output$xVarUI <- renderUI({
    req(data())
    selectInput("xVar", "X Variable", choices = names(data()))
  })
  
  output$yVarUI <- renderUI({
    req(data())
    selectInput("yVar", "Y Variable", choices = names(data()))
  })
  
  output$plot <- renderPlot({
    req(input$generatePlot, data())
    df <- data()
    
    # Optionally transform variables
    if(input$transformX) {
      df[[input$xVar]] <- match.fun(input$transformXType)(df[[input$xVar]])
    }
    if(input$transformY) {
      df[[input$yVar]] <- match.fun(input$transformYType)(df[[input$yVar]])
    }
    
    # Use aes() for mapping
    p <- ggplot(df, aes(x = !!sym(input$xVar), y = !!sym(input$yVar))) +
      get(input$geom)(colour = input$color, size = input$pointSize, fill = input$fill) +
      labs(title = input$graphTitle, x = input$xTitle, y = input$yTitle, caption = input$graphCaption) +
      {switch(input$theme,
              "theme_gray" = theme_gray(),
              "theme_minimal" = theme_minimal(),
              "theme_classic" = theme_classic(),
              "theme_light" = theme_light(),
              "theme_dark" = theme_dark())}
    
    # Adjust scales
    if(!is.na(input$xMin) && !is.na(input$xMax)) {
      p <- p + scale_x_continuous(limits = c(input$xMin, input$xMax))
    }
    if(!is.na(input$yMin) && !is.na(input$yMax)) {
      p <- p + scale_y_continuous(limits = c(input$yMin, input$yMax))
    }
    
    # Add confidence interval if selected and fit line
    if(input$ciLevel != "None" && input$fitLine) {
      p <- p + geom_smooth(method = "lm", level = as.numeric(input$ciLevel))
    }
    
    print(p)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      # Use the plot object directly
      ggsave(file, device = "png", width = 10, height = 8)
    }
  )
}

shinyApp(ui, server)
