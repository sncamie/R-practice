library(plotly)
Sys.setenv("plotly_username"="sncamie")
Sys.setenv("plotly_api_key"="Z1s5piGGFSfiWoSQt1g2")

p <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")

p
library(quantmod)

getSymbols("AAPL",src='yahoo')

df <- data.frame(Date=index(AAPL),coredata(AAPL))
df <- tail(df, 30)

p <- df %>%
  plot_ly(x = ~Date, type="ohlc",
          open = ~AAPL.Open, close = ~AAPL.Close,
          high = ~AAPL.High, low = ~AAPL.Low) %>%
  layout(title = "Basic OHLC Chart")

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="finance-ohlc-basic")
chart_link
getSymbols("AMZN",src='yahoo')
df1<- data.frame(Date=index(AMZN),coredata(AMZN))
df1-tail(df1,20)
