library(plotly)
library(quantmod)

getSymbols("AAPL",src='yahoo')

df <- data.frame(Date=index(AAPL),coredata(AAPL))
df <- tail(df, 30)

p <- df %>%
  plot_ly(x = ~Date, type="ohlc",
          open = ~AAPL.Open, close = ~AAPL.Close,
          high = ~AAPL.High, low = ~AAPL.Low) %>%
  layout(title = "Basic OHLC Chart",
         xaxis = list(rangeslider = list(visible = F)))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="finance-ohlc-rangeslider")
chart_link
