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
            fileInput(inputId = "file1", label = "Choose CSV File",
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            selectInput(inputId = "date_format",
                        label = "File Date Format:",
                        choices = c("mm/dd/yy" = "%m/%d/%y",
                                    "yyyy-mm-dd" = "%Y-%m-%d",
                                    "yyyy/mm/dd" = "%Y/%m/%d")),
            textInput(inputId = "title",
                      label = "Title:",
                      placeholder = ""),
            selectInput(inputId = "financial_data",
                        label = "Financial data:",
                        choices = c("Revenue" = "revenue",
                                    "Free Cash Flow" = "free_cash_flow",
                                    "EPS Diluted" = "EPS_diluted",
                                    "Diluted Shares Outstanding" = "diluted_shares_outstanding",
                                    "Net Income" = "net_income",
                                    "Dividends" = "dividends")),
            numericInput(inputId = "price",
                         label = "Current Price:", value = NA, min = 0),
            numericInput(inputId = "growth_years",
                         label = "Years of Growth:",
                         value = 3, min = 1, max = 200, step = 1),
            numericInput(inputId = "earnings_growth",
                         label = "Earnings Growth Rate (%):",
                         value = 5, min = -100, max = 200, step = 0.01),
            numericInput(inputId = "book_value_growth",
                         label = "Book Value Growth Rate (%):",
                         value = 2, min = -100, max = 200, step = 0.01),
            numericInput(inputId = "discount_rate",
                         label = "Required Rate of Return (%):",
                         value = 10, min = -100, max = 100, step = 0.01),
            numericInput(inputId = "terminal_years",
                         label = "Years of Terminal Growth:",
                         value = 10, min = 1, max = 200, step = 1),
            numericInput(inputId = "terminal_rate",
                         label = "Terminal Growth Rate (%):",
                         value = 0, min = -100, max = 100, step = 0.01),
            numericInput(inputId = "font_size",
                         label = "Font size:",
                         value = med, min = 1, max = 30),
            downloadButton(outputId = "download_plot", label = "Download plot")
        ),
        mainPanel(
            plotOutput(outputId = "plot_financial_data"),
            textOutput(outputId = "growth"),
            tableOutput(outputId = "data_table"),
            tableOutput(outputId = "value")
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
        df$date <- as.Date(df$date, input$date_format)
        df$EPS_payout <- round(df$dividends_per_share / df$EPS_diluted * 100, 2)
        df$operating_margin <- round(df$operating_income / df$revenue * 100, 2)
        df$net_margin <- round(df$net_income / df$revenue * 100, 2)
        df$gross_margin <- round(df$gross_profit / df$revenue * 100, 2)
        df
    })
    special_titles <- c("R_D", "SG_A")
    get_choices <- reactive({
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
    percent_data <- c("EPS_payout", "FCF_payout", 
                      "operating_margin", "net_margin", "gross_margin",
                      "ROA", "ROE", "ROIC", "comparable_store_sales")
    units <- reactive({
        req(financials())
        ## Check whether y-axis should be 'Mil USD' or 'Bil USD'
        per_share <- c("EPS_diluted", "dividends_per_share")
        if (FALSE %in% (financials()[[input$financial_data]] > 1000)) {
            formatter <- function(x) {x}
            ylabel <- "Mil USD"
        } else {
            formatter <- function(x) {x/1000}
            ylabel <- "Bil USD"
        }
        if (input$financial_data == "diluted_shares_outstanding") {
            ylabel <- unlist(strsplit(ylabel, " "))[1]
        }
        if (input$financial_data %in% per_share) {
            formatter <- waiver()
            ylabel <- "USD"
        }
        if (input$financial_data %in% percent_data) {
            # formatter <- scales::percent
            ylabel <- "%"
        }
        list("formatter" = formatter, "ylabel" = ylabel)
    })
    plotInput <- reactive({
        req(financials())
        req(units())
        # req(get_title())
        color <- "coral3"
        plot_financial(financials =financials(),
                       financial_data = input$financial_data, 
                       color = color, 
                       ylabel = units()$ylabel,
                       font_size = input$font_size,
                       plot_title = input$title, 
                       labels = units()$formatter)
    })
    get_title <- reactive({
        req(financials())
        ticker <- unlist(strsplit(input$file1$name, "_"))[1]
        x <- paste(ticker, displayString(input$financial_data))
    })
    pct_change <- reactive({
        req(financials())
        fin <- financials()[[input$financial_data]]
        round(c(0, diff(fin)) / lag(fin) * 100, 2)
    })
    avg_yoy_growth <- reactive({
        req(financials())
        selected_data <- financials()[[input$financial_data]]
        selected_data <- selected_data[!is.na(selected_data)]
        num_years <- length(selected_data) - 1
        ((selected_data[length(selected_data)] / selected_data[1])^(1/num_years) - 1) * 100
    })
    output$plot_financial_data <- renderPlot({
        print(plotInput())
    })
    equity_value <- reactive({
        req(financials())
        n <- length(financials()$net_income)
        # print(n)
        earnings_growth <- input$earnings_growth / 100
        book_value_growth <- input$book_value_growth / 100
        discount_rate <- input$discount_rate / 100
        terminal_rate <-  input$terminal_rate / 100
        num_years <- n - 1
        earnings <- rep(financials()$net_income[n], input$growth_years)
        book_values <- rep(financials()$shareholders_equity[n], input$growth_years)
        eps <- rep(financials()$EPS_diluted[n], input$growth_years)
        book_value <- financials()$shareholders_equity
        # avg_book_value_growth <- ((book_value[length(book_value)] / book_value[1])^(1/num_years) - 1)
        for (i in 1:length(earnings)) {
            earnings[i] <- earnings[i] * (1 + earnings_growth)^i
            eps[i] <- eps[i] * (1 + earnings_growth)^i
            if (i == 1) {
                next()
            }
            book_values[i] <- book_values[i] * (1 + book_value_growth)^i
        }
        # print(eps)
        # Residual earnings
        re <- earnings - (discount_rate * book_values)
        discounted_re <- rep(0, length(re))
        discounted_eps <- rep(0, length(eps))
        for (i in 1:length(re)) {
            discounted_re[i] <- re[i] / (1 + discount_rate)^i
            discounted_eps[i] <- eps[i] / (1 + discount_rate)^i
        }
        r <- (1 + terminal_rate) / (1 + discount_rate)
        terminal_discounted_eps <- tail(discounted_eps, 1) * r*(1 - r^input$terminal_years) / (1 - r)
        earnings_model <- sum(discounted_eps) + terminal_discounted_eps
        terminal_discounted_re <- tail(discounted_re, 1) * r*(1 - r^input$terminal_years) / (1 - r)
        abnormal_earnings_model <- round((book_values[1] + sum(discounted_re) + terminal_discounted_re) / tail(financials()$diluted_shares_outstanding, 1), 2)
        c(abnormal_earnings_model, earnings_model)
    })
    output$growth <- renderText({
        req(avg_yoy_growth())
        paste0("Average YoY Growth = ", round(avg_yoy_growth(), 2), "%")
    })
    output$data_table <- renderTable({
        req(financials())
        df <- financials()
        date <- format(df$date, "%Y-%m-%d")
        df <- cbind(date, cbind(df[[input$financial_data]], pct_change()))
        if (input$financial_data %in% percent_data) {
            selected_data_title <- paste(displayString(input$financial_data), "(%)")
        } else if (input$financial_data %in% special_titles) {
            selected_data_title <- paste(gsub("_", "&", input$financial_data))
        } else {
            selected_data_title <- displayString(input$financial_data)
        }
        colnames(df) <- c("Date", selected_data_title, "Percent Change (%)")
        df
    })
    output$value <- renderTable({
        req(equity_value())
        models <- c("Abnormal Earnings", "Discounted Earnings")
        if (is.na(input$price)) {
            value <- data.frame(models, equity_value())
            colnames(value) <- c("Valuation Model", "Equity Value ($)")
            value
        } else {
            margin_of_safety <- round((equity_value() - input$price)/equity_value()*100, 2)
            value <- data.frame(models, equity_value(), margin_of_safety)
            colnames(value) <- c("Valuation Model", "Equity Value ($)", "Margin of Safety (%)")
            value
            # margin_of_safety <- paste0("Margin of safety = ", round((abnormal_earnings_model() - input$price)/abnormal_earnings_model()*100, 2), "%")
        }
        # value <- paste0("Value of equity = $", abnormal_earnings_model())
        # paste("<b>Abnormal Earnings Valuation Model</b>", value, margin_of_safety, sep = "<br/>")
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
    plot_financial <- function(financials, financial_data, color, ylabel, font_size, plot_title, labels = waiver()) {
        print(class(financials$date))
        ggplot(financials, aes_string(x="date", y=financial_data)) + 
            geom_point(color = color, size = 4) +
            geom_line(color = color) +
            ggtitle(plot_title) + 
            ylab(ylabel) + 
            xlab("Date") +
            theme(text=element_text(size = font_size)) + 
            scale_y_continuous(labels = labels)
        
    }
    displayString <- function(x) {
        if (x %in% special_titles) {
            return(gsub("_", "&", x))
        }
        toTitleCase(gsub("_", " ", x))
    }
    observe({
        updateTextInput(session, "title", value = get_title())
    })
    observe({
        updateSelectInput(session, "financial_data", choices = get_choices())
    })
}

shinyApp(ui, server)
