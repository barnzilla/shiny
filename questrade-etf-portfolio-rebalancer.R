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
  tabPanel("Home",
           sidebarPanel(
             div(tags$strong("DGRC.TO")),
             column(6, numericInput("dgrc_to_quantity", "Quantity", min = 0, step = 1, value = "")), 
             column(6, numericInput("dgrc_to_price", "Price", min = 0, step = 0.01, value = "")), br(),
             div(tags$strong("REET")),
             column(6, numericInput("reet_quantity", "Quantity", min = 0, step = 1, value = "")), 
             column(6, numericInput("reet_price", "Price", min = 0, step = 0.01, value = "")), br(),
             div(tags$strong("SPEM")),
             column(6, numericInput("spem_quantity", "Quantity", min = 0, step = 1, value = "")), 
             column(6, numericInput("spem_price", "Price", min = 0, step = 0.01, value = "")), br(),
             div(tags$strong("SPTM")),
             column(6, numericInput("sptm_quantity", "Quantity", min = 0, step = 1, value = "")), 
             column(6, numericInput("sptm_price", "Price", min = 0, step = 0.01, value = "")), br(),
             div(tags$strong("XFH.TO")),
             column(6, numericInput("xfh_to_quantity", "Quantity", min = 0, step = 1, value = "")), 
             column(6, numericInput("xfh_to_price", "Price", min = 0, step = 0.01, value = "")), br(),
             numericInput("cash", "Cash (CAD)", min = 0, step = 1, value = ""),
             div("Note: 3% is removed from the cash before the rebalance computation is performed to account for USD to CAD conversion fees.", style = "background-color: #808080; color: #ffffff; border: 1px solid #808080; border-radius: 3px; width: 100%; padding: 10px;"), br(),
             div(tags$label("Positions held outside portfolio"), style = "margin-bottom: 18px;"),
             column(12, textInput("custom_position1_symbol", "Symbol", value = "")),
             column(6, numericInput("custom_position1_quantity", "Quantity", min = 0, step = 1, value = "")), 
             column(6, numericInput("custom_position1_price", "Price", min = 0, step = 0.01, value = ""), style = "margin-bottom: 18px;"),
             column(12, textInput("custom_position2_symbol", "Symbol", value = "")),
             column(6, numericInput("custom_position2_quantity", "Quantity", min = 0, step = 1, value = "")), 
             column(6, numericInput("custom_position2_price", "Price", min = 0, step = 0.01, value = ""), style = "margin-bottom: 18px;"),
             column(12, textInput("custom_position3_symbol", "Symbol", value = "")),
             column(6, numericInput("custom_position3_quantity", "Quantity", min = 0, step = 1, value = "")), 
             column(6, numericInput("custom_position3_price", "Price", min = 0, step = 0.01, value = "")),
             actionButton("rebalance", "Scrape Yahoo", icon("yahoo"), style = "margin-top: 18px;"),
             width = 4
           ),
           mainPanel(
             div(tags$strong("Disclaimer: "), "this Shiny app is intended for educational use only; it should NOT be construed as offering financial advice of any kind.", style = "background-color: #808080; color: #ffffff; border: 1px solid #808080; border-radius: 3px; width: 100%; padding: 10px;"), br(), br(),
             tableOutput("rebalancer") %>% withSpinner(color = "#808080"), br(), br(),
             width = 8
           )
  ),
  tags$head(tags$style(HTML('
  // Custom CSS here
  ')))
)

# Define server logic for the shiny app
server <- function(input, output, session) {
  # Cache select data structures
  cached <- reactiveValues()
  
  # Round numeric values and output as formatted values
  custom_round <- function(x, n = 2) {
    return(format(round(as.numeric(x), n), nsmall = n, big.mark = ","))
  }
  
  # Get yesterday's portfolio prices
  get_historical_data <- function(ticker) {
    period1 <- as.numeric(as.POSIXlt(Sys.Date() - 5))
    period2 <- as.numeric(as.POSIXlt(Sys.Date()))
    url <- paste0("https://query1.finance.yahoo.com/v7/finance/download/", ticker, "?period1=", period1, "&period2=", period2, "&interval=1d&events=history")
    data <- suppressWarnings(read.csv(url))
    return(data$Close[nrow(data)])
  }
  
  # Main function
  output$rebalancer <- function() {
    # Scrape prices and dates from Yahoo
    table <- scrape_data()
    
    # Update quantities, prices and market values if quantities and/or prices change
    quantities <- c(input$dgrc_to_quantity, input$reet_quantity, input$spem_quantity, input$sptm_quantity, input$xfh_to_quantity)
    manual_price_inputs <- c(input$dgrc_to_price, input$reet_price, input$spem_price, input$sptm_price, input$xfh_to_price)
    custom_symbols <- c(input$custom_position1_symbol, input$custom_position2_symbol, input$custom_position3_symbol)
    custom_quantities <- c(input$custom_position1_quantity, input$custom_position2_quantity, input$custom_position3_quantity)
    custom_prices <- c(input$custom_position1_price, input$custom_position2_price, input$custom_position3_price)
    for(i in 1:length(custom_symbols)) {
      if(length(table$symbol[table$symbol == toupper(custom_symbols[i])]) == 1) {
        quantities <- append(quantities, custom_quantities[i])
        manual_price_inputs <- append(manual_price_inputs, custom_prices[i])
      }
    }
    table$quantity <- sapply(quantities, function(x) if(is.na(x) | ! is.numeric(x)) 0 else x)
    
    for(i in 1:length(manual_price_inputs)) {
      if(is.na(manual_price_inputs[i]) | ! is.numeric(manual_price_inputs[i]) | manual_price_inputs[i] < 0.01) {
        # Do nothing
      } else {
        table$price[i] <- ifelse(table$currency[i] == "USD", manual_price_inputs[i] * cached$usdcadx, manual_price_inputs[i])
        table$date_time[i] <- "Manual entry"
      }
    }
    table$market_value <- table$quantity * table$price
    
    # Compute the portfolio allocations
    table$prop <- rep("-", nrow(table))
    if(sum(table$market_value[table$in_portfolio == "Yes"]) == 0) {
      table$prop[table$in_portfolio == "Yes"] <- rep(paste0(custom_round(0), "%"), length(table$market_value[table$in_portfolio == "Yes"]))
      total_prop <- paste0(custom_round(0), "%")
    } else {
      table$prop[table$in_portfolio == "Yes"] <- paste0(custom_round(unname(prop.table(table$market_value[table$in_portfolio == "Yes"])) * 100), "%")
      total_prop <- paste0(custom_round(sum(unname(prop.table(table$market_value[table$in_portfolio == "Yes"])) * 100)), "%")
    }
    
    # Define the optimal portfolio allocations (aggressive); see https://www.questrade.com/questwealth-portfolios/etf-portfolios
    optimal_allocation <- c(30.6, 4.9, 4.9, 29.4, 28.2) * 100 / sum(c(30.6, 4.9, 4.9, 29.4, 28.2)) * 0.01
    
    # Compute total equity for the portfolio positions (remove 3% from cash to account for CAD to USD conversion fees)
    cash <- ifelse(is.na(input$cash) | ! is.numeric(input$cash), 0, input$cash)
    total_equity <- sum(table$market_value[table$in_portfolio == "Yes"]) + (cash * 0.97)
    
    # Compute the rebalance column
    table$rebalance <- rep("-", nrow(table))
    table$rebalance[table$in_portfolio == "Yes"] <- floor(((optimal_allocation * total_equity) - table$market_value[table$in_portfolio == "Yes"]) / table$price[table$in_portfolio == "Yes"])
    table$rebalance[table$in_portfolio == "Yes"] <- sapply(table$rebalance[table$in_portfolio == "Yes"], function(x) if(x > 0) paste0("+", x) else as.character(x))
    
    # Recompute total equity for all positions, and without the 3% reduction in cash
    total_equity <- sum(table$market_value) + cash
    
    # Compute profit/loss
    table$historical_prices <- unname(sapply(table$symbol, function(x) get_historical_data(x)))
    historical_usdcadx <- get_historical_data("USDCAD%3DX")
    table$historical_prices[table$currency == "USD"] <- table$historical_prices[table$currency == "USD"] * historical_usdcadx
    table$historical_market_value <- table$historical_prices * table$quantity
    table$pl <- (table$market_value - table$historical_market_value) / table$historical_market_value * 100
    total_pl <- ifelse(sum(table$market_value) == 0, "-", red_or_green((sum(table$market_value, na.rm = TRUE) - sum(table$historical_market_value, na.rm = TRUE)) / sum(table$historical_market_value, na.rm = TRUE) * 100))
    table$pl <- sapply(table$pl, function(x) red_or_green(x))
    
    # Remove unwanted columns
    table <- table[-c(2:3,10:11)]
    
    # Add formatting to select columns
    table[c("price", "market_value")] <- apply(table[c("price", "market_value")], 1:2, function(x) paste0("$", custom_round(x), " CAD"))
    
    # Add more semantic column names
    colnames(table) <- c("Symbol", "Date/time", "Price", "Quantity", "Market value", "Portfolio allocation", "Rebalance", "Profit/loss")
    
    # Add rows for cash and for the summary
    table <- rbind(table, c("Cash", rep("-", 3), paste0("$", custom_round(cash), " CAD"), rep("-", 3)))
    table <- rbind(table, c("Total", rep("-", 3), paste0("$", custom_round(total_equity), " CAD"), total_prop, "-", total_pl))
    
    # Output the results as an HTML table
    kable(table, align = c("l", "l", "r", "r", "r", "r", "r", "r"), row.names = FALSE, escape = FALSE) %>%
      kable_styling(bootstrap_options = c("striped", "hover")) %>% 
      row_spec((nrow(table) - 1):nrow(table), bold = T, color = "#808080", background = "#e7e7e7")
  }
  
  # The function that scrapes financial data for the Questrade ETF portfolio
  scrape_data <- eventReactive(input$rebalance, {
    # Scrape data for the USD to CAD conversion rate
    cached$usdcadx <- as.numeric(scrape_yahoo("USDCAD=X")$price)
    
    # Scrape data 
    output <- data.frame(row.names = NULL)
    output <- rbind(output, data.frame(scrape_yahoo("DGRC.TO", input$dgrc_to_quantity, input$dgrc_to_price)))
    output <- rbind(output, data.frame(scrape_yahoo("REET", input$reet_quantity, input$reet_price)))
    output <- rbind(output, data.frame(scrape_yahoo("SPEM", input$spem_quantity, input$spem_price)))
    output <- rbind(output, data.frame(scrape_yahoo("SPTM", input$sptm_quantity, input$sptm_price)))
    output <- rbind(output, data.frame(scrape_yahoo("XFH.TO", input$xfh_to_quantity, input$xfh_to_price)))
    if(! is.na(input$custom_position1_symbol) & input$custom_position1_symbol != "") {
      output <- rbind(output, data.frame(scrape_yahoo(input$custom_position1_symbol, input$custom_position1_quantity, input$custom_position1_price)))
    }
    if(! is.na(input$custom_position2_symbol) & input$custom_position2_symbol != "") {
      output <- rbind(output, data.frame(scrape_yahoo(input$custom_position2_symbol, input$custom_position2_quantity, input$custom_position2_price)))
    }
    if(! is.na(input$custom_position3_symbol) & input$custom_position3_symbol != "") {
      output <- rbind(output, data.frame(scrape_yahoo(input$custom_position3_symbol, input$custom_position3_quantity, input$custom_position3_price)))
    }
    
    # Return the results as a data frame
    return(output)
  })
  
  # Add styling (coloring) to proportion based on whether the value is positive or negative 
  red_or_green <- function(x) {
    if(is.nan(x)) {
      return("-")
    } else if(x < 0) {
      return(paste0("<span style='color: red;'>", custom_round(x), "%</span>"))
    }  else if(x > 0) {
      return(paste0("<span style='color: green;'>+", custom_round(x), "%</span>"))
    } else {
      return(paste0(custom_round(x), "%"))
    }
  }
  
  # This function scrapes prices and dates from Yahoo
  scrape_yahoo <- function(symbol, quantity = 0, manual_price = 0) {
    # Sanitize/validate parameters
    symbol <- toupper(symbol)
    quantity <- ifelse(is.na(as.numeric(quantity)), 0, quantity)
    manual_price <- ifelse(is.na(as.numeric(manual_price)), 0, manual_price)
    
    # Scrape Yahoo
    webpage <- tryCatch(
      read_html(paste0("https://finance.yahoo.com/quote/", symbol)),
      error = function(e) { "URL does not exist" }
    )
    
    # Isolate currency data
    currency <- trimws(strsplit(html_text(html_nodes(webpage, "[id='quote-header-info']") %>% html_nodes("div"))[5], "[.]")[[1]][2])
    currency <- substr(currency, nchar(currency) - 2, nchar(currency))
    
    # Compute/scrape price and date_time data conditionally 
    if(manual_price > 0) {
      price <- manual_price
      date_time <- "Manual entry"
    } else {
      scraped_data <- html_text(html_nodes(webpage, "[id='quote-header-info']") %>% html_nodes("div"))[16]
      price <- strsplit(scraped_data, "[.]")
      price <- as.numeric(paste0(price[[1]][1], ".", substr(price[[1]][2], 1, ifelse(symbol == "USDCAD=X", 4, 2))))
      date_time <- paste0(strsplit(scraped_data, " ")[[1]][5:6], collapse = " ")
    }
    
    # Convert price to CAD if in USD
    if(currency == "USD") price <- price * cached$usdcadx
    
    # Compute market value
    market_value <- price * as.integer(quantity)
    
    # Compute whether security is in Questrade portfolio
    in_portfolio <- ifelse(symbol %in% c("DGRC.TO", "REET", "SPEM", "SPTM", "XFH.TO"), "Yes", "No") 
    
    # Return row
    return(data.frame(symbol = symbol, currency = currency, in_portfolio = in_portfolio, date_time = date_time, price = price, quantity = quantity, market_value = market_value))
  }
}

# Run the shiny app 
shinyApp(ui = ui, server = server)