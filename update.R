# ------------------------------------------------------------
# TARGET balances tracker
# Downloads the latest TARGET balances from the ECB and
# draws a chart for Germany, France, Italy and Spain.
# ------------------------------------------------------------

library(plotly)       # draws interactive charts
library(htmlwidgets)  # saves the chart as a web page


# 1. Where the data lives
#    ECB Data Portal, dataset "TGB" (TARGET balances)
#    M = monthly, N = national central bank, E = end of month
url <- paste0(
  "https://data-api.ecb.europa.eu/service/data/TGB/",
  "M.DE+FR+IT+ES.N.A094T.U2.EUR.E",
  "?format=csvdata"
)


# 2. Download the data (it arrives as a table)
raw <- read.csv(url)


# 3. Keep only what we need, with simple names
data <- data.frame(
  country = raw$REF_AREA,
  month   = as.Date(paste0(raw$TIME_PERIOD, "-01")),
  billion = raw$OBS_VALUE / 1000      # ECB gives millions, we show billions
)


# 4. Replace country codes with full names
full_names   <- c(DE = "Germany", FR = "France", IT = "Italy", ES = "Spain")
data$country <- full_names[data$country]


# 5. Draw the chart
latest <- format(max(data$month), "%B %Y")

chart <- plot_ly(data, x = ~month, y = ~billion, color = ~country,
                 type = "scatter", mode = "lines") |>
  layout(
    title = paste0("TARGET balances, € billion (latest data: ", latest, ")"),
    xaxis = list(title = ""),
    yaxis = list(title = "€ billion  (above 0 = claim, below 0 = liability)")
  )


# 6. Save the chart as a web page called index.html
saveWidget(chart, "index.html", selfcontained = FALSE,
           libdir = "lib", title = "TARGET balances tracker")
