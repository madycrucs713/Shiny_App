library(shiny)

ui <- fluidPage(
    titlePanel("BGSP 7030 Shiny App"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            tags$hr(),
            checkboxInput("header", "Header", TRUE),
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            tags$hr(),
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            tags$hr(),
            actionButton("modelBtn", "Fit Linear Model")
        ),
        mainPanel(
            plotOutput("distPlot"),
            plotOutput("lmPlot"),
            tableOutput("contents"),
            verbatimTextOutput("modelSummary"),
            verbatimTextOutput("modelDetails")
        )
    )
)

server <- function(input, output, session) {

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    modelData <- reactiveVal(NULL)
    
    observeEvent(input$modelBtn, {
        df <- dataInput()
        model <- lm(y ~ x, data = df)
        modelData(model)
    })
    
    output$distPlot <- renderPlot({
        df <- dataInput()
        plot(df$x, df$y, main = "Scatter Plot of x and y", xlab = "x", ylab = "y", pch = 19, col = "blue")
    })
    
    output$lmPlot <- renderPlot({
        req(modelData())
        df <- dataInput()
        model <- modelData()
        
        plot(df$x, df$y, main = "Linear Model Overlay", xlab = "x", ylab = "y", pch = 19, col = "blue")
        
        abline(model, col = "red", lwd = 2)  # Red line with increased width
    })
    
    output$contents <- renderTable({
        if (input$disp == "head") {
            return(head(dataInput()))
        } else {
            return(dataInput())
        }
    })
    
    output$modelSummary <- renderPrint({
        req(modelData())
        summary(modelData())
    })
    
    output$modelDetails <- renderPrint({
        req(modelData())
        model <- modelData()
        coef <- coef(model)
        slope <- coef[2]
        intercept <- coef[1]
        df <- dataInput()
        correlation <- cor(df$x, df$y)
        
        cat("Slope: ", slope, "\n")
        cat("Intercept: ", intercept, "\n")
        cat("Correlation Coefficient: ", correlation, "\n")
    })
}

shinyApp(ui = ui, server = server)


