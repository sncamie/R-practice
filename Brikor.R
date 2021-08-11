library(quantmod)
start <- as.Date("2021-01-04")
end <- as.Date("2021-06-21")


getSymbols("BIK.JO", src = "yahoo", from = start, to = end)
tail(BIK.JO)
plot(BIK.JO[,"BIK.JO.Close"], main="BIK.JO")
candleChart(BIK.JO, up.col = "black", dn.col = "red", theme = "white")
