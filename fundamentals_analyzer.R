library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(tools)
## Only run examples in interactive R sessions
med <- 22
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            ),
            textInput(inputId = "ticker",
                      label = "Ticker:",
                      placeholder = ""),
            # checkboxInput(inputId = "header",
            #               label = "Header", 
            #               value = TRUE),
            radioButtons(inputId = "financial_data",
                        label = "Financial data:",
                        choices = c("Revenue" = "revenue",
                                    "Free Cash Flow" = "free_cash_flow",
                                    "EPS Diluted" = "EPS_diluted",
                                    "Diluted Shares Outstanding" = "diluted_shares_outstanding",
                                    "Net Income" = "net_income",
                                    "Dividends" = "dividends")),
            numericInput(inputId = "font_size",
                         label = "Font size:",
                         value = med, min = 1, max = 30),
            downloadButton(outputId = "download_plot", label = "Download plot")
        ),
        mainPanel(
            plotOutput(outputId = "scatterplot_with_lines"),
            textOutput(outputId = "text"),
            tableOutput(outputId = "data_table")
        )
    )
)
    
server <- function(input, output, session) {
    financials <- reactive({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        df <- read.csv(inFile$datapath, header = TRUE)
        df$date <- as.Date(df$date, "%Y-%m-%d")
        df
    })
    # dates <- reactive({
    #     req(financials())
    #     financials()$date <- as.Date(financials()$date, "%m/%d/%y")
    #     financials()$date
    # })
    choices <- reactive({
        req(financials())
        df <- financials()[,2:dim(financials())[2]]
        choices_list <-  as.list(names(df))
        choices_names <- c()
        for (i in names(df)) {
            choices_names <- c(choices_names, displayString(i))
        }
        names(choices_list) <- choices_names
        choices_list
    })
    plotInput <- reactive({
        req(financials())
        # req(dates())
        plot_title <- paste(input$ticker, displayString(input$financial_data))
        per_share <- c("EPS_diluted", "dividends")
        if (!input$financial_data %in% per_share) {
            ggplot(financials(), aes_string(x="date", y=input$financial_data)) + 
                geom_point(color = "coral3", size = 4) + 
                geom_line(color = "coral3") +
                ggtitle(plot_title) + 
                ylab("Mil USD") + 
                xlab("Dates") +
                theme(text=element_text(size=input$font_size))
        } else {
            ggplot(financials(), aes_string(x="date", y=input$financial_data)) + 
                geom_point(color = "coral3", size = 4) + 
                geom_line(color = "coral3") +
                ggtitle(plot_title) + 
                ylab("USD") + 
                xlab("Dates") +
                theme(text=element_text(size=input$font_size))
        }

    })
    pct_change <- reactive({
        fin <- financials()[[input$financial_data]]
        round(c(0, diff(fin)) / lag(fin) * 100, 2)
    })
    output$scatterplot_with_lines <- renderPlot({
        print(plotInput())
    })
    output$text <- renderText({
        req(pct_change())
        paste0("Average YoY Growth = ", round(mean(pct_change(), na.rm = TRUE), 2), "%")
        # print(class(pct_change()))
        # fin <- financials()[input$financial_data]
        # diff(fin)
        # pct_change <- c(0, diff(fin)) / lag(fin) * 100
    })
    output$data_table <- renderTable({
        req(financials())
        # date <- format(dates(), "%Y-%m-%d")
        date <- format(financials()$date, "%Y-%m-%d")
        df <- cbind(date, cbind(financials()[[input$financial_data]], pct_change()))
        colnames(df) <- c("date", input$financial_data, "percent_change")
        df
    })
    output$download_plot <- downloadHandler(
        filename <- function() {
            paste(input$financial_data, ".pdf", sep = "")
        },
        content <- function(file) {
            ggsave(file, plot = plotInput(),
                   device = "pdf")
        }
    )
    observe({
        updateRadioButtons(session, "financial_data",
                           choices = choices())
    })
}

displayString <- function(x) {
    toTitleCase(gsub("_", " ", x))
}

shinyApp(ui, server)
