# ------------------------------------------------------------
# Angela Gales
# Cod formatted with the help of Claude for commented // clear
# code that can be understandable for readers
# TARGET balances tracker
# Downloads the latest TARGET balances from the ECB, draws a
# chart for Germany, France, Italy, Spain and the ECB, and
# builds an html with clickable explanations on the right.
# ------------------------------------------------------------

library(plotly)      # pckg to draw interactive charts
library(htmltools)   # pckg to build the html


# DATA
#    ECB Data Portal, dataset "TGB" (TARGET balances)
#    M  = monthly
#    DE, FR, IT, ES = countries;  4F = the ECB itself
#    N  = national central bank;  E  = ECB
#    E (at the end) = end-of-month value
url <- paste0(
  "https://data-api.ecb.europa.eu/service/data/TGB/",
  "M.DE+FR+IT+ES+4F.N+E.A094T.U2.EUR.E",
  "?format=csvdata"
)


# Download
raw <- read.csv(url)


# DROP REST
data <- data.frame(
  country = raw$REF_AREA,
  month   = as.Date(paste0(raw$TIME_PERIOD, "-01")),
  billion = raw$OBS_VALUE / 1000      # ECB gives millions, convert into bls
)


# Name country codes
full_names   <- c(DE = "Germany", FR = "France", IT = "Italy",
                  ES = "Spain",   "4F" = "ECB")
data$country <- full_names[data$country]


# Draw chart
latest <- format(max(data$month), "%B %Y")

chart <- plot_ly(data, x = ~month, y = ~billion, color = ~country,
                 type = "scatter", mode = "lines", height = 520) |>
  layout(
    xaxis  = list(title = ""),
    yaxis  = list(title = "€ billion  (above 0 = claim, below 0 = liability)"),
    legend = list(orientation = "h", y = -0.12)   # country names below the chart
  )


# Explain for clarity what Target shows
#    Each one is a "details" box: the "summary" is the title you click,
#    and the paragraphs after it only appear once it is opened.

topic_what <- tags$details(
  tags$summary("What are TARGET balances?"),
  p("The euro is one currency, but each euro-area country keeps its own
     national central bank, each with its own balance sheet. When money
     moves from a bank in one country to a bank in another, it travels
     through TARGET, the payment system of the Eurosystem."),
  p("Every cross-border transfer leaves the receiving central bank with a
     claim on the ECB, and the sending central bank with a liability to
     the ECB. TARGET balances are these claims and liabilities added up
     over time.")
)

topic_read <- tags$details(
  tags$summary("How to read the chart"),
  p("Above zero: over time, more money has flowed into that country than
     out of it. Below zero: the opposite."),
  p("By design, all balances together add up to zero, once the ECB and the
     non-euro central banks that take part are included. The five lines on
     this chart are therefore only part of the total.")
)

topic_include <- tags$details(
  tags$summary("What does TARGET include?"),
  p("Today, TARGET reunites several of services, known as TARGET Services:"),
  tags$ul(
    tags$li(strong("T2"), " for large payments between banks and for the
            central bank's own operations"),
    tags$li(strong("T2S"), " for the cash side of securities trades"),
    tags$li(strong("TIPS"), " for instant payments, 24 hours a day")
  ),
  p("At the end of every day, all cross-border flows across these services
     are combined into a single balance for each central bank against the
     ECB. So the figures cover all of TARGET, not only large payments.
     Banknotes are recorded separately and are not included.")
)

topic_history <- tags$details(
  tags$summary("A short history"),
  tags$ul(
    tags$li(strong("1999: "), "TARGET is launched together with the euro,
            linking the national payment systems."),
    tags$li(strong("2007–2008: "), "TARGET2 replaces it with a single
            shared platform."),
    tags$li(strong("2015: "), "T2S starts settling securities in central
            bank money."),
    tags$li(strong("2018: "), "TIPS launches for instant payments."),
    tags$li(strong("2023: "), "T2 replaces TARGET2, and all services run
            on one consolidated platform."),
    tags$li(strong("2025: "), "ECMS, a common system for managing
            collateral, completes the set."),
    tags$li(strong("2026: "), "Pontes links blockchain-based platforms
            to TARGET.")
  )
)

topic_debate <- tags$details(
  tags$summary("Why does this matter?"),
  p("The ECB and many other economists see
     them as the normal result of a single currency in which money can move
     freely, and argue that they pose no risk as long as the euro exists. But 
    TARGET balances grew sharply during the euro crisis of 2010–2012 and
     again during the ECB's large bond-buying programmes.")
)

topic_source <- tags$details(
  tags$summary("Source"),
  p("Data: European Central Bank, ",
    tags$a(href = "https://data.ecb.europa.eu/data/datasets/TGB",
           "ECB Data Portal, dataset TGB"),
    ". End-of-month values.
     Country data start in 2001; the ECB's own balance starts in 2008.")
)


# 7. The look of the page (colours, spacing, two columns)
style <- tags$style("
  body    { font-family: Arial, sans-serif; max-width: 1200px;
            margin: 32px auto; padding: 0 16px;
            line-height: 1.6; color: #222; }
  .small  { color: #666; font-size: 14px; }

  /* two columns: chart on the left (wide), topics on the right */
  .layout { display: grid; grid-template-columns: 2fr 1fr; gap: 32px; }
  .layout > div { min-width: 0; }

  /* the clickable topics */
  details { border-bottom: 1px solid #ddd; padding: 10px 0; }
  summary { cursor: pointer; font-weight: bold; color: #003299; }
  summary:hover { text-decoration: underline; }
  details p, details ul { font-size: 15px; }

  /* the bilateral table under the chart */
  .bt    { border-collapse: collapse; margin: 16px 0; font-size: 14px; }
  .bt th, .bt td { padding: 8px 12px; border: 1px solid #ddd; text-align: right; }
  .bt th { background: #f4f4f4; }

  /* on phones: one column, topics under the chart */
  @media (max-width: 800px) {
    .layout { grid-template-columns: 1fr; }
  }
")


# Bilateral table (made by bilateral.R)
#    bilateral.R saves bilateral_target_simple.csv. If the file is
#    there, I show it as a table under the chart. If not, the page
#    is just built without it.
bilateral_section <- NULL
if (file.exists("bilateral_target_simple.csv")) {
  bt <- read.csv("bilateral_target_simple.csv", check.names = FALSE)
  names(bt)[1] <- ""

  #    header row, then one row per country
  header <- tags$tr(lapply(names(bt), tags$th))
  rows <- lapply(seq_len(nrow(bt)), function(i) {
    tags$tr(tags$th(bt[i, 1]),
            lapply(bt[i, -1], function(x)
              tags$td(if (is.na(x)) "–" else format(x, nsmall = 1))))
  })

  #    clickable explanations, like on the right of the chart
  bt_read <- tags$details(
    tags$summary("How to read the table"),
    p("Each row is a country. Each cell is the money that came into that
       country from the partner in the column, minus the money that went
       out to it, added up from 2009 to 2025, in € billion. Positive means
       more came in, which pushes the country's TARGET balance up."),
    p("The last three columns compare the sum of all these flows with how
       much the TARGET balance actually moved from December 2008 to
       December 2025. The gap is the difference, shown as it is.")
  )
  bt_counted <- tags$details(
    tags$summary("What is counted"),
    p("Only data published by partner country: goods (IMF), services
       (Eurostat), direct investment and its income (Eurostat), shares and
       bonds (IMF) and bank loans and deposits (BIS). Nothing is estimated
       or filled in.")
  )
  bt_missing <- tags$details(
    tags$summary("What is not counted (the gap)"),
    p("Everything that is not published by partner: wages of cross border
       workers, interest and dividends, transfers such as the EU budget,
       derivatives, loans outside banks, errors and omissions. Also the
       years a source does not publish, for example services before 2010
       to 2013 and direct investment before 2013.")
  )
  bt_meaning <- tags$details(
    tags$summary("What it shows"),
    p("Flows between the four countries are small. Most of the money goes
       through the rest of the euro area (financial centres like
       Luxembourg, Ireland, the Netherlands) and the rest of the world.
       The biggest direct link is Germany and Spain.")
  )

  bilateral_section <- div(
    h2("Who owes whom? The money flows behind TARGET, 2009 to 2025"),
    p(class = "small",
      "The ECB only publishes one TARGET balance per country, not who owes
       whom. This table looks at the money that actually moved between
       countries."),
    tags$table(class = "bt", header, rows),
    bt_read, bt_counted, bt_missing, bt_meaning
  )
}


# 8. Put the page together
page <- tagList(
  tags$head(tags$title("TARGET balances tracker")),
  style,

  h1("TARGET balances: Germany, France, Italy, Spain and the ECB"),
  p(class = "small",
    "End-of-month values in € billion. Latest data: ", latest,
    ". This page updates itself every month with new ECB figures."),

  div(class = "layout",
    div(chart),                                  # left column
    div(                                         # right column
      p(class = "small", "Click a topic to read more:"),
      topic_what, topic_read, topic_include,
      topic_history, topic_debate, topic_source
    )
  ),

  bilateral_section,                             # table under the chart

  p(class = "small",
    "Made with R by Angela Gales. ",
    tags$a(href = "https://github.com/angelagales/target-balances",
           "See the code on GitHub"), ".")
)


# 9. Save everything as a web page called index.html
save_html(page, "index.html", libdir = "lib")
