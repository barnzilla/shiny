# Load packages to extend base R
library(kableExtra)
library(magrittr)
library(rvest)
library(shiny)
library(shinycssloaders)

# Define UI for the shiny app
ui <- navbarPage(
  windowTitle = HTML("Questrade ETF portfolio rebalancer"),
  title = div("Questrade ETF portfolio", style = "margin-right: 48px;"),
  tabPanel("Rebalance",
    sidebarPanel(
      uiOutput("dgrc_to"),
      uiOutput("reet"),
      uiOutput("spem"),
      uiOutput("sptm"),
      uiOutput("xfh_to"),
      uiOutput("cash"),
      uiOutput("cash_note"), br(),
      actionButton("rebalance", "Rebalance", icon("refresh")),
      width = 3
    ),
    mainPanel(
      div(tags$strong("Disclaimer: "), "this Shiny app is intended for educational use only; it should NOT be construed as offering financial advice of any kind.", style = "background-color: #808080; color: #ffffff; border: 1px solid #808080; border-radius: 3px; width: 100%; padding: 10px;"), br(), br(),
      tableOutput("rebalancer") %>% withSpinner(color = "#808080"), br(), br(),
      width = 9
    )
  ),
  tags$head(tags$style(HTML('
  // Custom CSS here
  ')))
)

# Define server logic for the shiny app
server <- function(input, output) {
    # Cache select data structures
    cached <- reactiveValues()
    
    # Round numeric values and output as formatted values
    custom_round <- function(x, n = 2) {
      return(format(round(as.numeric(x), n), nsmall = n, big.mark = ","))
    }
    
    # The input field for the cash quantity
    output$cash <- renderUI({
      numericInput("cash", "Cash (CAD)", min = 0, step = 1, value = 0)
    })
    
    # The note that accompanies the cash input field
    output$cash_note <- renderUI({
      div("Note: 3% is removed from the cash before the rebalance computation is performed to account for USD to CAD conversion fees.", style = "background-color: #808080; color: #ffffff; border: 1px solid #808080; border-radius: 3px; width: 100%; padding: 10px;")
    })
    
    # The input field for the DGRC.TO quantity
    output$dgrc_to <- renderUI({
        numericInput("dgrc_to", "DGRC.TO quantity", min = 0, step = 1, value = 0)
    })
    
    # Main function
    output$rebalancer <- eventReactive(input$rebalance, {
      # Scrape prices and dates from Yahoo
      table <- scrape_data()
      
      # Compute the portfolio allocations
      if(sum(table$market_value) == 0) {
        table$prop <- rep(0, length(table$market_value))
        total_prop <- "0%"
      } else {
        table$prop <- custom_round(unname(prop.table(table$market_value)) * 100)
        total_prop <- paste0(custom_round(sum(unname(prop.table(table$market_value)) * 100)), "%")
      }
      
      # Define the optimal portfolio allocations (aggressive); see https://www.questrade.com/questwealth-portfolios/etf-portfolios
      optimal_allocation <- c(30.6, 4.9, 4.9, 29.4, 28.2) * 100 / sum(c(30.6, 4.9, 4.9, 29.4, 28.2)) * 0.01
      
      # Compute total equity (remove 3% from cash to account for CAD to USD conversion fees)
      total <- sum(table$market_value) + (input$cash * 0.97)
      
      # Compute the buy/sell column
      table$buy_sell <- floor(((optimal_allocation * total) - table$market_value) / table$Price)
      table$buy_sell <- sapply(table$buy_sell, function(x) if(x > 0) paste0("+", x) else as.character(x))
      
      # Add more semantic column names for select columns
      colnames(table)[5] <- "Market value"
      colnames(table)[6] <- "Portfolio allocation"
      colnames(table)[7] <- "Buy/sell"
      
      # Add formatting to select columns
      table[4:5] <- apply(table[4:5], 1:2, function(x) paste0("$", custom_round(x), " CAD"))
      table[6] <- sapply(table[6], function(x) paste0(x, "%"))
      
      # Add rows for cash and for the summary
      table <- rbind(table, c("Cash", rep("-", 3), paste0("$", input$cash, " CAD"), rep("-", 2)))
      table <- rbind(table, c("Summary", rep("-", 3), paste0("$", custom_round(total), " CAD"), total_prop, "-"))
      
      # Output the results as an HTML table
      kable(table, align = c("l", "l", "r", "r", "r", "r", "r"), row.names = FALSE) %>%
        kable_styling(bootstrap_options = c("striped", "hover")) %>% 
          row_spec((nrow(table) - 1):nrow(table), bold = T, color = "white", background = "#808080")
    })
    
    # The input field for the REET quantity
    output$reet <- renderUI({
        numericInput("reet", "REET quantity", min = 0, step = 1, value = 0)
    })
    
    # The input field for the SPEM quantity
    output$spem <- renderUI({
      numericInput("spem", "SPEM quantity", min = 0, step = 1, value = 0)
    })
    
    # The input field for the SPTM quantity
    output$sptm <- renderUI({
      numericInput("sptm", "SPTM quantity", min = 0, step = 1, value = 0)
    })
    
    # The input field for the XFH.TO quantity
    output$xfh_to <- renderUI({
      numericInput("xfh_to", "XFH.TO quantity", min = 0, step = 1, value = 0)
    })
    
    # The function that scrapes financial data for the Questrade ETF portfolio
    scrape_data <- function() {
      # Scrape data for the USD to CAD conversion rate
      cached$usd_to_cad <- scrape_yahoo("CAD", "https://ca.finance.yahoo.com/quote/usdcad=x", 1)
      cached$conversion <- as.numeric(gsub("[$]", "", cached$usd_to_cad$Price))
      
      # Scrape data for the ETFs in the Questrade ETF portfolio
      dgrc_to <- scrape_yahoo("DGRC.TO", "https://ca.finance.yahoo.com/quote/dgrc.to", input$dgrc_to)
      reet <- scrape_yahoo("REET", "https://ca.finance.yahoo.com/quote/reet", input$reet, convert = TRUE)
      spem <- scrape_yahoo("SPEM", "https://ca.finance.yahoo.com/quote/spem", input$spem, convert = TRUE)
      sptm <- scrape_yahoo("SPTM", "https://ca.finance.yahoo.com/quote/sptm", input$sptm, convert = TRUE)
      xfh_to <- scrape_yahoo("XFH.TO", "https://ca.finance.yahoo.com/quote/xfh.to", input$xfh_to)
      
      # Return the results as a data frame
      return(rbind(dgrc_to, reet, spem, sptm, xfh_to))
    }
    
    # The function that scrapes prices and dates data from Yahoo
    scrape_yahoo <- function(symbol, url, quantity, convert = FALSE) {
      url <- suppressWarnings(tryCatch(
        url(url, "rb"),
        error = function(e) { NA }
      ))
      if(is.na(url)) {
        return(data.frame(rep(NA, 5)))
      } else {
        webpage <- tryCatch(
          read_html(url),
          error = function(e) { "URL does not exist" }
        )
        close(url)
        scraped_data <- html_text(html_nodes(webpage, "[id='quote-header-info']") %>% html_nodes("div"))[16]
        price <- strsplit(scraped_data, "[.]")
        price <- as.numeric(paste0(price[[1]][1], ".", substr(price[[1]][2], 1, 2)))
        if(isTRUE(convert)) price <- price * cached$conversion
        date_time <- paste0(strsplit(scraped_data, " ")[[1]][5:6], collapse = " ")
        market_value <- price * as.integer(quantity)
        return(data.frame(Symbol = symbol, Date = date_time, Quantity = quantity, Price = price, market_value = market_value, stringsAsFactors = FALSE))
      }
    }
}

# Run the shiny app 
shinyApp(ui = ui, server = server)