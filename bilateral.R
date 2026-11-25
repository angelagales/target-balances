# ------------------------------------------------------------
# Angela Gales
# Bilateral TARGET balances: Germany, France, Italy, Spain
# I used Claude to clean this code up and make it tidy.
# I trained my Claude to comment my code as I do with previous scripts.
#
# The ECB only gives one TARGET balance per central bank (against
# the ECB), so we don't know who owes what to whom. Here I look at
# the money that actually moved between countries from 2009 to
# 2025 and compare it with how much TARGET moved over the same years.
#
# For each country (row) I add up money IN from a partner minus
# money OUT to it. Partners (columns) are Germany, France, Italy,
# Spain, rest of euro area, rest of world.
#
# Data, all published by partner country:
#   - goods                       (IMF, IMTS)
#   - services                    (Eurostat, bop_its6_det)
#   - direct investment flows     (Eurostat, bop_fdi6_geo)
#   - direct investment income    (Eurostat, bop_fdi6_geo)
#   - portfolio, shares and bonds (IMF, PIP, used to be CPIS)
#   - bank loans and deposits     (BIS, locational banking stats)
# These are different parts of the balance of payments so nothing
# is counted twice. Bank debt securities are taken out of the BIS
# data because they are already in the portfolio data.
#
# Positive = more money came in than went out, so the country's
# TARGET balance goes up.
#
# I don't make imputations. If something is not published it is not
# counted and ends up in the gap. The gap also has everything that
# is not published by partner: wages, interest and dividends,
# transfers (EU budget, remittances), capital account, derivatives,
# loans outside banks, errors and omissions.
# ------------------------------------------------------------

for (pkg in c("httr2", "jsonlite")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}   # installs the pckgs only if missing
library(httr2)     # pckg to ask the IMF, BIS, ECB websites for data
# jsonlite is used further down for Eurostat


#1# GENERAL SETUP
#    The four countries, rows and columns of the table
four <- c("DE", "FR", "IT", "ES")
full_names <- c(DE = "Germany", FR = "France", IT = "Italy", ES = "Spain")

#    All 20 euro countries, because "rest of euro area" is the
#    other 16 added up. 2 letter codes for ECB and BIS, 3 letter
#    codes for the IMF, same order in both lists.
ea2 <- c("AT", "BE", "CY", "DE", "EE", "ES", "FI", "FR", "GR", "HR",
         "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PT", "SI", "SK")
ea3 <- c("AUT", "BEL", "CYP", "DEU", "EST", "ESP", "FIN", "FRA", "GRC", "HRV",
         "IRL", "ITA", "LTU", "LUX", "LVA", "MLT", "NLD", "PRT", "SVN", "SVK")
names(ea3) <- ea2   # so ea3["DE"] gives "DEU"

#    TARGET series only start in May 2008, so first December is 2008.
#    Flows from 2009, TARGET from Dec 2008 to Dec 2025.
base_year   <- 2008
target_year <- 2025

world_imf <- "G001"   # "World" for the IMF
world_bis <- "5J"     # "All countries" for the BIS


#    Helper: download a table (csv) from a statistics website.
#    If the site has nothing for the request we get an empty table.
get_csv <- function(url) {
  resp <- request(url) |>
    req_headers(Accept = "application/vnd.sdmx.data+csv;version=1.0.0") |>
    req_timeout(600) |>
    req_retry(max_tries = 3) |>
    req_error(is_error = function(r) FALSE) |>
    req_perform()
  if (resp_status(resp) == 404) return(data.frame())
  if (resp_status(resp) >= 400) {
    stop("The website answered with error ", resp_status(resp), " for:\n", url,
         "\n", substr(resp_body_string(resp), 1, 500))
  }
  txt <- resp_body_string(resp)
  if (!nzchar(trimws(txt))) return(data.frame())
  read.csv(text = txt, stringsAsFactors = FALSE)
}

#    Helper: the IMF sometimes gives units, sometimes millions or bls.
#    I check one value I know the size of and put everything in mns.
to_millions <- function(x, known_value) {
  if (known_value > 1e9) return(x / 1e6)   # was in units
  if (known_value < 1e4) return(x * 1e3)   # was in bls
  x                                        # already in mns
}


#2# TARGET BALANCES (ECB, dataset TGB), millions of euro
#    Same dataset as update.R.
#    M = monthly, N = national central bank, A094T = TARGET balance,
#    U2 = against the euro area, E = end of month
url <- paste0(
  "https://data-api.ecb.europa.eu/service/data/TGB/",
  "M.", paste(four, collapse = "+"), ".N.A094T.U2.EUR.E",
  "?format=csvdata"
)
tgb <- read.csv(url)

#    balance in December of a given year
level <- function(y) {
  x <- tgb[tgb$TIME_PERIOD == paste0(y, "-12"), ]
  setNames(x$OBS_VALUE, x$REF_AREA)[four]               # NA if not there
}
#    how much TARGET moved from Dec 2008 to Dec 2025
target_change <- level(target_year) - level(base_year)
print(round(target_change / 1000, 1))


#3# EXCHANGE RATE (ECB, dataset EXR), dollars per euro, monthly
#    IMF and BIS are in dollars. Each amount is converted with the
#    rate of its own year or quarter.
url <- paste0(
  "https://data-api.ecb.europa.eu/service/data/EXR/M.USD.EUR.SP00.A",
  "?format=csvdata",
  "&startPeriod=", base_year, "-01&endPeriod=", target_year, "-12"
)
fx <- read.csv(url)
fx$year    <- as.integer(substr(fx$TIME_PERIOD, 1, 4))
fx$month   <- as.integer(substr(fx$TIME_PERIOD, 6, 7))
fx$quarter <- paste0(fx$year, "-Q", (fx$month - 1) %/% 3 + 1)
fx_year    <- tapply(fx$OBS_VALUE, fx$year, mean)       # yearly average
fx_quarter <- tapply(fx$OBS_VALUE, fx$quarter, mean)    # quarterly average
fx_dec     <- setNames(fx$OBS_VALUE[fx$month == 12],    # December
                       fx$year[fx$month == 12])


#    IMF helpers. The IMF website is asked directly.
#    First I ask how a dataset is organised (which columns, which
#    order), then I build the request in that order.
imf <- "https://api.imf.org/external/sdmx/2.1"

imf_structure <- function(flow) {
  url <- paste0(imf, "/dataflow/IMF.STA/", flow, "/latest?references=datastructure")
  txt <- request(url) |>
    req_headers(Accept = "application/vnd.sdmx.structure+xml;version=2.1") |>
    req_timeout(300) |> req_perform() |> resp_body_string()
  tag <- regmatches(txt, gregexpr("<[a-zA-Z]*:?Dataflow [^>]*>", txt))[[1]]
  tag <- tag[grepl(paste0(' id="', flow, '"'), tag)][1]
  version <- if (is.na(tag)) "latest" else sub('.* version="([^"]+)".*', "\\1", tag)
  dims <- regmatches(txt, gregexpr("<[a-zA-Z]*:?Dimension [^>]*>", txt))[[1]]
  ids  <- sub('.*(^|\\s)id="([^"]+)".*', "\\2", dims, perl = TRUE)
  pos  <- suppressWarnings(as.integer(sub('.*position="([0-9]+)".*', "\\1", dims)))
  if (length(pos) && all(!is.na(pos))) ids <- ids[order(pos)]
  list(version = version, dims = unique(ids))
}

#    `choose` = which codes I want in which column. Columns I don't
#    mention are left open (I get everything).
imf_data <- function(flow, choose) {
  s <- imf_structure(flow)
  message(flow, " version ", s$version, ", dimensions: ", paste(s$dims, collapse = " . "))
  key <- paste(sapply(s$dims, function(d)
    if (is.null(choose[[d]])) "" else paste(choose[[d]], collapse = "+")),
    collapse = ".")
  url <- paste0(imf, "/data/IMF.STA,", flow, ",", s$version, "/", key,
                "?startPeriod=", base_year, "&endPeriod=", target_year,
                "&detail=dataonly")
  message("Asking: ", url)
  d <- get_csv(url)
  if (nrow(d) == 0) stop("The IMF sent no data for ", flow, ". Request was:\n", url)
  d$OBS_VALUE <- suppressWarnings(as.numeric(d$OBS_VALUE))
  d
}

#    Empty table, one row and one column per euro country plus
#    "RoW" (rest of world). Cell [i, j] = money INTO i FROM j, mns.
n <- length(ea2)
empty_table <- function() {
  matrix(NA_real_, n, n + 1, dimnames = list(ea2, c(ea2, "RoW")))
}


#4# GOODS (IMF, dataset IMTS), yearly
#    Inside the euro area: what i exports to j (says i) minus what
#    j exports to i (says j). Selling = money in.
#    Rest of world: i's exports to the world minus to the euro area,
#    minus the same for imports (all as i reports it).
#    Imports only exist with freight and insurance included ("CIF").
tr <- imf_data("IMTS", list(COUNTRY = ea3,
                            INDICATOR = c("XG_FOB_USD", "MG_CIF_USD"),
                            COUNTERPART_COUNTRY = c(ea3, world_imf),
                            FREQUENCY = "A"))
tr$year <- as.integer(substr(tr$TIME_PERIOD, 1, 4))
tr <- tr[tr$year > base_year & tr$year <= target_year & !is.na(tr$OBS_VALUE), ]

#    German exports to France as the known value to check the unit
known <- median(tr$OBS_VALUE[tr$COUNTRY == "DEU" & tr$COUNTERPART_COUNTRY == "FRA" &
                               tr$INDICATOR == "XG_FOB_USD"])
tr$eur_mn <- to_millions(tr$OBS_VALUE, known) / fx_year[as.character(tr$year)]

#    add up all years, per country, partner and exports/imports
tr_sum <- aggregate(eur_mn ~ COUNTRY + COUNTERPART_COUNTRY + INDICATOR,
                    data = tr, FUN = sum)

#    get one of these sums (NA if not reported)
tv <- function(reporter, partner, indicator) {
  v <- tr_sum$eur_mn[tr_sum$COUNTRY == reporter &
                       tr_sum$COUNTERPART_COUNTRY == partner &
                       tr_sum$INDICATOR == indicator]
  if (length(v)) v else NA
}

trade <- empty_table()
for (i in ea2) {
  for (j in setdiff(ea2, i)) {
    trade[i, j] <- tv(ea3[i], ea3[j], "XG_FOB_USD") - tv(ea3[j], ea3[i], "XG_FOB_USD")
  }
  #    euro partners with no data are just left out here
  x_ea <- sum(sapply(setdiff(ea3, ea3[i]), tv, reporter = ea3[i], indicator = "XG_FOB_USD"), na.rm = TRUE)
  m_ea <- sum(sapply(setdiff(ea3, ea3[i]), tv, reporter = ea3[i], indicator = "MG_CIF_USD"), na.rm = TRUE)
  trade[i, "RoW"] <- (tv(ea3[i], world_imf, "XG_FOB_USD") - x_ea) -
    (tv(ea3[i], world_imf, "MG_CIF_USD") - m_ea)
}


#5# PORTFOLIO, shares and bonds (IMF, dataset PIP)
#    PIP gives what is HELD at the end of each year, not what was
#    bought. So I take holdings end 2025 minus holdings end 2008,
#    each with its own December rate. Careful: this also moves with
#    prices, not only with purchases.
#    Inside the euro area: what j holds more of i (money into i)
#    minus what i holds more of j (money out of i).
#    Rest of world: what the world holds more of i (L) minus what i
#    holds more of the world (A), without the euro part. If there
#    is no L for i this stays NA.
pip_ask <- function(partners) {
  imf_data("PIP", list(COUNTRY = ea3, INDICATOR = "P_TOTINV_P_USD",
                       SECTOR = "S1", COUNTERPART_SECTOR = "S1",
                       COUNTERPART_COUNTRY = partners))
}
#    P_TOTINV_P_USD = total portfolio (shares + bonds), in dollars
#    S1 = whole economy, for both sides
#    If the IMF doesn't know "World" here, ask again without it.
pp <- tryCatch(pip_ask(c(ea3, world_imf)), error = function(e) {
  message("PIP without 'World': rest-of-world portfolio column will be NA")
  pip_ask(ea3)
})
pp$year <- as.integer(substr(pp$TIME_PERIOD, 1, 4))

#    keep only end of year values, for 2008 and 2025
pp <- pp[grepl("^[0-9]{4}$|(-M?12|-Q4|-S2)$", pp$TIME_PERIOD) &
           pp$year %in% c(base_year, target_year) & !is.na(pp$OBS_VALUE), ]
#    same end of year can come twice (yearly and half yearly), keep one
pp <- pp[!duplicated(pp[, c("COUNTRY", "ACCOUNTING_ENTRY", "COUNTERPART_COUNTRY", "year")]), ]

#    German holdings of French securities as the known value
known <- pp$OBS_VALUE[pp$COUNTRY == "DEU" & pp$COUNTERPART_COUNTRY == "FRA" &
                        pp$ACCOUNTING_ENTRY == "A"][1]
pp$eur_mn <- to_millions(pp$OBS_VALUE, known) / fx_dec[as.character(pp$year)]

#    change in a holding between 2008 and 2025 (NA if one is missing)
pchange <- function(reporter, partner, entry) {
  v <- function(y) {
    x <- pp$eur_mn[pp$COUNTRY == reporter & pp$COUNTERPART_COUNTRY == partner &
                     pp$ACCOUNTING_ENTRY == entry & pp$year == y]
    if (length(x)) x else NA
  }
  v(target_year) - v(base_year)
}

portfolio <- empty_table()
for (i in ea2) {
  for (j in setdiff(ea2, i)) {
    portfolio[i, j] <- pchange(ea3[j], ea3[i], "A") - pchange(ea3[i], ea3[j], "A")
  }
  a_ea <- sum(sapply(setdiff(ea3, ea3[i]), pchange, reporter = ea3[i], entry = "A"), na.rm = TRUE)
  l_ea <- sum(sapply(setdiff(ea3, ea3[i]), pchange, reporter = ea3[i], entry = "L"), na.rm = TRUE)
  portfolio[i, "RoW"] <- (pchange(ea3[i], world_imf, "L") - l_ea) -
    (pchange(ea3[i], world_imf, "A") - a_ea)
}


#6# BANKS (BIS, locational banking statistics), every quarter
#    Q   = quarterly
#    F   = change over the quarter, cleaned of exchange rate effects
#    C+L = claims (loans out) and liabilities (deposits in)
#    G   = loans and deposits only (bonds are already in portfolio)
#    TO1 = all currencies, 5J = banks of any nationality
#    N   = cross border only
#    Money into i from j, as banks in i see it: deposits from j
#    went up minus loans to j went up.
#    Banks in j report the same thing from the other side. If both
#    report I take the average, if only one reports I take that one.
#    Rest of world: i's own report for all countries minus for the
#    euro area (NA if i's banks don't report).
key <- paste("Q", "F", "C+L", "G", "TO1", "A", "5J", "A",
             paste(ea2, collapse = "+"), "A",
             paste(c(ea2, world_bis), collapse = "+"), "N", sep = ".")
url <- paste0("https://stats.bis.org/api/v1/data/WS_LBS_D_PUB/", key, "/all",
              "?startPeriod=", base_year + 1, "-Q1",
              "&endPeriod=", target_year, "-Q4&detail=dataonly")
bk <- get_csv(url)
if (nrow(bk) == 0) {
  #    if the BIS doesn't know "All countries", ask again without it
  message("BIS without 'All countries': rest-of-world bank column will be NA")
  url <- sub(paste0("+", world_bis, ".N/"), ".N/", url, fixed = TRUE)
  bk <- get_csv(url)
}
if (nrow(bk) == 0) stop("The BIS sent no data. Request was:\n", url,
                        "\nIf 'G' (loans and deposits) is the problem, try 'A' ",
                        "(all instruments) in the key above.")
bk$OBS_VALUE <- suppressWarnings(as.numeric(bk$OBS_VALUE))
bk <- bk[!is.na(bk$OBS_VALUE), ]
bk$eur_mn <- bk$OBS_VALUE / fx_quarter[bk$TIME_PERIOD]     # BIS gives mns of dollars

#    add up all quarters
bk_sum <- aggregate(eur_mn ~ L_REP_CTY + L_CP_COUNTRY + L_POSITION, data = bk, FUN = sum)

bv <- function(reporter, partner, position) {
  v <- bk_sum$eur_mn[bk_sum$L_REP_CTY == reporter & bk_sum$L_CP_COUNTRY == partner &
                       bk_sum$L_POSITION == position]
  if (length(v)) v else NA
}
own    <- function(i, j) bv(i, j, "L") - bv(i, j, "C")   # what banks in i say
mirror <- function(i, j) bv(j, i, "C") - bv(j, i, "L")   # what banks in j say

banks <- empty_table()
for (i in ea2) {
  for (j in setdiff(ea2, i)) {
    reports <- c(own(i, j), mirror(i, j))
    banks[i, j] <- if (all(is.na(reports))) NA else mean(reports, na.rm = TRUE)
  }
  own_ea <- sum(sapply(setdiff(ea2, i), own, i = i), na.rm = TRUE)
  banks[i, "RoW"] <- own(i, world_bis) - own_ea
}


#    Keep only my four rows, with six columns.
#    "Rest of euro area" = the other 16 added up (those with data).
rest_ea <- setdiff(ea2, four)
cols6 <- c(four, "Rest of euro area", "Rest of world")
to_four <- function(m) {
  t <- cbind(m[four, four], rowSums(m[four, rest_ea], na.rm = TRUE), m[four, "RoW"])
  dimnames(t) <- list(four, cols6)
  t
}
goods4     <- to_four(trade)
portfolio4 <- to_four(portfolio)
banks4     <- to_four(banks)


#    Eurostat helper. Here filters go by name, so the order of the
#    columns doesn't matter. Gives back one row per number.
eurostat <- function(dataset, filters) {
  q <- paste(mapply(function(n, v) paste0(n, "=", v), names(filters), filters), collapse = "&")
  u <- paste0("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/",
              dataset, "?", q,
              "&sinceTimePeriod=", base_year + 1, "&untilTimePeriod=", target_year)
  message("Asking Eurostat: ", u)
  j <- jsonlite::fromJSON(u)
  if (length(j$value) == 0) return(data.frame())
  idx <- as.integer(names(j$value))
  out <- data.frame(value = as.numeric(unlist(j$value)))
  for (k in seq_along(j$id)) {
    d <- j$id[k]
    stride <- prod(c(j$size[-(1:k)], 1))
    p <- (idx %/% stride) %% j$size[k]
    cats <- unlist(j$dimension[[d]]$category$index)
    out[[d]] <- names(cats)[match(p, cats)]
    out[[paste0(d, "_label")]] <- unlist(j$dimension[[d]]$category$label)[out[[d]]]
  }
  out
}

#    Partners I ask Eurostat for: the four countries, the euro area
#    (EA20, or EA19 if that's what exists) and the whole world
es_partners <- c(four, "EA20", "EA19", "WRL_REST")
partner_filter <- paste(paste0("partner=", es_partners), collapse = "&")
geo_filter     <- paste(paste0("geo=", four), collapse = "&")

#    Eurostat numbers into my six columns, YEAR BY YEAR, as the row
#    country reports them:
#    - the other three countries: every year they exist
#    - rest of euro area = euro area minus the other three, only in
#      years where the euro area AND all three exist
#    - rest of world = world minus euro area, only in years where
#      both exist
#    Years with a missing piece are not counted (they go to the gap),
#    so nothing ends up under the wrong partner.
es_table <- function(d) {
  t <- matrix(0, 4, 6, dimnames = list(four, cols6))
  n <- t                                   # how many years per cell
  val <- function(g, p, y) {
    v <- d$value[d$geo == g & d$partner == p & d$time == y]
    if (length(v)) sum(v) else NA
  }
  for (g in four) {
    others <- setdiff(four, g)
    for (y in sort(unique(d$time[d$geo == g]))) {
      bil <- sapply(others, function(p) val(g, p, y))
      EA  <- val(g, "EA20", y)
      if (is.na(EA)) EA <- val(g, "EA19", y)
      W   <- val(g, "WRL_REST", y)
      ok  <- !is.na(bil)
      t[g, others[ok]] <- t[g, others[ok]] + bil[ok]
      n[g, others[ok]] <- n[g, others[ok]] + 1
      if (!is.na(EA) && all(ok)) {
        t[g, "Rest of euro area"] <- t[g, "Rest of euro area"] + EA - sum(bil)
        n[g, "Rest of euro area"] <- n[g, "Rest of euro area"] + 1
      }
      if (!is.na(EA) && !is.na(W)) {
        t[g, "Rest of world"] <- t[g, "Rest of world"] + W - EA
        n[g, "Rest of world"] <- n[g, "Rest of world"] + 1
      }
    }
  }
  t[n == 0] <- NA                          # no year at all = empty
  for (g in four) t[g, g] <- NA
  attr(t, "years") <- n
  t
}

#    print which years exist for each piece
show_years <- function(d, name) {
  cat("\nYears published,", name, "(row = reporter, column = partner):\n")
  print(tapply(d$time, list(d$geo, d$partner),
               function(t) paste0(min(t), "-", max(t), " (", length(unique(t)), ")")))
}


#6b# SERVICES (Eurostat, bop_its6_det), yearly, mns of euro
#    Services exports minus imports, as the row country reports it.
#    S = all services, BAL = balance (exports minus imports)
svc <- eurostat("bop_its6_det",
                list(geo = paste(four, collapse = "&geo="),
                     partner = paste(es_partners, collapse = "&partner="),
                     bop_item = "S", stk_flow = "BAL", currency = "MIO_EUR"))
if (nrow(svc) == 0) {
  #    if there is no balance, take exports (CRE) minus imports (DEB)
  message("No services balance published: using credit minus debit")
  svc <- eurostat("bop_its6_det",
                  list(geo = paste(four, collapse = "&geo="),
                       partner = paste(es_partners, collapse = "&partner="),
                       bop_item = "S", stk_flow = "CRE&stk_flow=DEB", currency = "MIO_EUR"))
  svc$value[svc$stk_flow == "DEB"] <- -svc$value[svc$stk_flow == "DEB"]
}
if (nrow(svc) == 0) stop("Eurostat sent no services data.")
show_years(svc, "services")
services4 <- es_table(svc)
cat("Years counted per cell, services:\n"); print(attr(services4, "years"))


#6c# DIRECT INVESTMENT (Eurostat, bop_fdi6_geo), yearly, mns of euro
#    As the row country reports it:
#    - investment abroad (DO):  flow = money out, income = money in
#    - investment at home (DI): flow = money in,  income = money out
#    Flows and income are kept apart, as two sources.
fdi <- eurostat("bop_fdi6_geo",
                list(geo = paste(four, collapse = "&geo="),
                     partner = paste(es_partners, collapse = "&partner="),
                     currency = "MIO_EUR"))
if (nrow(fdi) == 0) stop("Eurostat sent no direct investment data.")

#    which one is flows and which one is income, found by the labels
cat("\nDirect investment indicators published:\n")
print(unique(fdi[, c("indic_bp", "indic_bp_label", "fdi_item", "fdi_item_label")]), row.names = FALSE)
is_flow   <- grepl("flow",   fdi$indic_bp_label, ignore.case = TRUE)
is_income <- grepl("income", fdi$indic_bp_label, ignore.case = TRUE)
abroad    <- grepl("^DO", fdi$fdi_item)
at_home   <- grepl("^DI", fdi$fdi_item)
if (!any(is_flow) || !any(is_income) || !any(abroad) || !any(at_home)) {
  stop("Could not recognise flows/income or abroad/at home in the list above. ",
       "Paste the list to Claude.")
}
#    check against double counting: only one item for abroad and
#    one for at home, otherwise stop
if (length(unique(fdi$fdi_item[abroad])) != 1 || length(unique(fdi$fdi_item[at_home])) != 1) {
  stop("More than one direct investment item for abroad or at home (see list above): ",
       "adding them up could count things twice. Paste the list to Claude.")
}

#    money into the row country, per year and partner
fdi_flows <- rbind(
  transform(fdi[is_flow & at_home, ], value =  value),    # investment coming in
  transform(fdi[is_flow & abroad,  ], value = -value))    # investment going out
fdi_income <- rbind(
  transform(fdi[is_income & abroad,  ], value =  value),  # profits earned abroad
  transform(fdi[is_income & at_home, ], value = -value))  # profits paid to foreigners
show_years(fdi_flows,  "direct investment flows")
show_years(fdi_income, "direct investment income")
fdi_flows4  <- es_table(fdi_flows)
fdi_income4 <- es_table(fdi_income)
cat("Years counted per cell, direct investment:\n"); print(attr(fdi_flows4, "years"))


#7# PUT IT ALL TOGETHER (mns of euro)
#    Each cell = sum of the sources that have data for it
sources <- list("Goods"                    = goods4,
                "Services"                 = services4,
                "Direct investment flows"  = fdi_flows4,
                "Direct investment income" = fdi_income4,
                "Portfolio"                = portfolio4,
                "Bank loans and deposits"  = banks4)
total <- Reduce(`+`, lapply(sources, function(m) { m[is.na(m)] <- 0; m }))
for (g in four) total[g, g] <- NA   # a country with itself = empty

observed <- rowSums(total, na.rm = TRUE)   # row total


#8# RESULT (bls of euro)
#    the table, the sum, TARGET change, and the gap
#    gap = TARGET change minus sum of flows
result <- cbind(round(total / 1000, 1),
                "Sum of flows"        = round(observed / 1000, 1),
                "TARGET change (ECB)" = round(target_change / 1000, 1),
                "Gap"                 = round((target_change - observed) / 1000, 1))
rownames(result) <- full_names[four]
colnames(result)[1:4] <- full_names[four]

print(result)
#View(result)
write.csv(result, "bilateral_target_simple.csv")

#    row totals per source, to see what drives each row (bls)
by_source <- sapply(sources, function(m) round(rowSums(m, na.rm = TRUE) / 1000, 1))
rownames(by_source) <- full_names[four]
cat("\nRow totals by source (EUR bn):\n")
print(by_source)
write.csv(by_source, "bilateral_target_simple_by_source.csv")
