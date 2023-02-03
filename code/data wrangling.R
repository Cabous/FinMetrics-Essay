
# Libraries

pacman::p_load("tidyverse", "devtools", "rugarch", "rmgarch", "zoo", "factoextra",
               "forecast", "tbl2xts", "lubridate", "PerformanceAnalytics",
               "ggthemes", "dplyr", "cowplot", "fmxdat", "glue","MTS",
               "robustbase","tidyr")


# Import Data

bonds_raw <- read_rds("data/SA_Bonds.rds")

Indexes_raw <- fmxdat::SA_Indexes

gold_raw <- read_rds("data/comms.rds")

bitcoin_raw <- fmxdat::cryptos

# Filter relevant indexes
# Equities and Property

Indexes <- Indexes_raw %>%

    arrange(date) %>%

    filter(Tickers %in% c("JALSHTR Index", "JSAPYTR Index")) %>%

    select(-ShareName)

colnames(Indexes) = c("date","Ticker","Price")

# Gold
Gold <- gold_raw %>%

    arrange(date) %>%

    filter(Name %in% "Gold")

colnames(Gold) = c("date","Ticker","Price")

# Bitcoin

Bitcoin <- bitcoin_raw %>%

    arrange(date) %>%

    filter(name %in% "Bitcoin") %>%

    select(date, name, close)

colnames(Bitcoin) = c("date","Ticker","Price")

# Combine data

asset_classes <- rbind(Indexes, Gold, Bitcoin)

# Calculate Return
library(rmsfuns)

datescol <- dateconverter(as.Date("2015-01-01"), as.Date("2019-12-31"),
                          "weekdayEOW")

asset_classes_ret <- asset_classes %>%

    group_by(Ticker) %>%

    mutate(dlogret = log(Price) - log(lag(Price))) %>%

    mutate(scaledret = (dlogret - mean(dlogret, na.rm = T)))  %>%

    filter(date > dplyr::first(date)) %>%

    filter(date %in% datescol) %>%

    ungroup()

# Graph
# check for obvious outliers in the returns series:

ggplot(asset_classes_ret) +

    geom_line(aes(x = date, y = Price, colour = Ticker,
                                alpha = 0.5)) +

    ggtitle("Price Changes: Asset Classes") +

    guides(alpha = "none") +

    fmxdat::theme_fmx()


# Or cleaning the plot up a bit....

ggplot(asset_classes_ret) +

    geom_line(aes(x = date, y = Price, colour = Ticker,
                                alpha = 0.5)) +

    ggtitle("Price Changes: Asset Classes") +

    facet_wrap(~Ticker, scales = "free_y") +

    guides(alpha = "none") +

    fmxdat::theme_fmx() +

    scale_color_hue(l = 20) +

    scale_x_date(labels = scales::date_format("'%y"), date_breaks = "2 years") +

    theme(axis.text = element_text(size = 7))


# Histogram

ggplot(asset_classes_ret) +

    geom_histogram(aes(x = dlogret, fill = Ticker, alpha = 0.5)) +

    ggtitle("Log Returns: Asset Classes") +

    facet_wrap(~Ticker, scales = "free") +

    guides(fill = "none", alpha = "none") +

    fmxdat::theme_fmx()

# It seems that there is large clumping together of returns.
#This might bias our model estimates (especially if it is only a few outliers).
# So I'll clean data

pacman::p_load(PerformanceAnalytics)

Tidy_asset_rtn <-

    left_join(

        asset_classes_ret,

        asset_classes_ret %>%

            tbl_xts(., cols_to_xts = "dlogret", spread_by = "Ticker") %>%

            PerformanceAnalytics::Return.clean(., method = c("none", "boudt", "geltner")[2], alpha = 0.01) %>%

            tbl2xts::xts_tbl() %>%

            gather(Ticker, CleanedRet, -date),

        by = c("date", "Ticker")
    )

# Now let's see what changed:
Tidy_asset_rtn %>% filter(CleanedRet != dlogret)

# The MARCH test indicates that all the MV portmanteau tests reject the
# null of no conditional heteroskedasticity, motivating our use of MVGARCH models.

#Auto-Persistence in Returns

# Create the Simple returns:

Simple_asset_rtn <- asset_classes_ret %>%

    group_by(Ticker) %>%

    mutate(SimpleRet = Price / lag(Price)-1) %>%

    ungroup() %>%

    mutate(date = lubridate::ymd(date)) %>% # Easier to work with ymd here

    tbl_xts(., cols_to_xts = SimpleRet, spread_by = Ticker)

Simple_asset_rtn[is.na(Simple_asset_rtn)] <- 0


Weights <- asset_classes_ret %>%

    mutate(YM = format(date, "%Y%b")) %>%

    group_by(YM) %>%

    filter(date == last(date)) %>%

    mutate(weight = 1/n()) %>%

    ungroup() %>%

    select(date, Ticker, weight) %>%

    unique %>%

    mutate(date = lubridate::ymd(date)) %>% # Easier to work with ymd here

    tbl_xts(., spread_by = Ticker)

Weights[is.na(Weights)] <- 0


porteqw <- rmsfuns::Safe_Return.portfolio(Simple_asset_rtn, weight = Weights,
                                          geometric = FALSE)

# ACF's

forecast::Acf(porteqw, main = "ACF: Equally Weighted Return")

forecast::Acf(porteqw^2, main = "ACF: Squared Equally Weighted Return")

forecast::Acf(abs(porteqw), main = "ACF: Absolute Equally Weighted Return")

# A formal test for ARCH effects: LBQ stats on squared returns:

Box.test(coredata(porteqw^2), type = "Ljung-Box", lag = 12)

# The test rejects the nulls of no ARCH effects - hence we need to control
# for the remaining conditional heteroskedasticity in the returns series,

##################################################################

##################################################################

# alternative return calculation

# Calculate Returns for Assets

# equity

Equities <- Indexes %>%

    group_by(Ticker) %>%

    filter(Ticker %in% "JALSHTR Index") %>%

    mutate(dlogret = log(Price) - log(lag(Price))) %>%

    mutate(scaledret = (dlogret - mean(dlogret, na.rm = T))) %>%

    filter(date > dplyr::first(date)) %>%

    select(-Price) %>%

    filter(date > as.Date("2014-12-31")) %>%

    rename("JALSHTR_Index" = dlogret) %>%

    ungroup() %>%

    select(date, JALSHTR_Index)

# property

Property <- Indexes %>%

    group_by(Ticker) %>%

    filter(Ticker %in% "JSAPYTR Index") %>%

    mutate(dlogret = log(Price) - log(lag(Price))) %>%

    mutate(scaledret = (dlogret - mean(dlogret, na.rm = T))) %>%

    filter(date > dplyr::first(date)) %>%

    select(-Price) %>%

    filter(date > as.Date("2014-12-31")) %>%

    rename("JSAPYTR_Index" = dlogret) %>%

    ungroup() %>%

    select(date, JSAPYTR_Index)

# gold

Gold_ret <- gold_raw %>%

    group_by(Name) %>%

    filter(Name %in% "Gold") %>%

    mutate(dlogret = log(Price) - log(lag(Price))) %>%

    mutate(scaledret = (dlogret - mean(dlogret, na.rm = T))) %>%

    filter(date > dplyr::first(date)) %>%

    select(-Price) %>%

    filter(date > as.Date("2014-12-31")) %>%

    rename("Gold" = dlogret) %>%

    ungroup() %>%

    select(date, Gold)


# bitcoin

Bit_ret <- Bitcoin %>%

    mutate(dlogret = log(Price) - log(lag(Price))) %>%

    mutate(scaledret = (dlogret - mean(dlogret, na.rm = T))) %>%

    filter(date > dplyr::first(date)) %>%

    select(-Price) %>%

    filter(date > as.Date("2014-12-31")) %>%

    rename("Bitcoin" = dlogret) %>%

    select(date, Bitcoin)

# Combine and wrangle for DCC models

asset_xts_ret <- left_join(Equities, Property, by = c("date")) %>%

    left_join(., Gold_ret, by = c("date")) %>%

    left_join(., Bit_ret, by = c("date")) %>%

    tbl_xts()

asset_xts_ret <- asset_xts_ret %>%

    xts_tbl() %>%

    filter(date < as.Date("2020-01-01")) %>%

    tbl_xts()

asset_tbl_ret <- asset_xts_ret %>% xts_tbl()


asset_xts_ret <- na.locf(asset_xts_ret, na.rm = F, maxgap = 10)

# Lets test for autocorrelation using the March Test

MarchTest(asset_xts_ret)

# GARCH specifications

uspec <- ugarchspec(variance.model = list(model = "gjrGARCH",
                                          garchOrder = c(1, 1)), mean.model = list(armaOrder = c(1,
                                                                                                 0), include.mean = TRUE), distribution.model = "sstd")

multi_univ_garch_spec <- multispec(replicate(ncol(asset_xts_ret), uspec))

# DCC specifications

spec.dcc = dccspec(multi_univ_garch_spec, dccOrder = c(1, 1),
                   distribution = "mvnorm", lag.criterion = c("AIC", "HQ", "SC",
                                                              "FPE")[1], model = c("DCC", "aDCC")[1])

# Enable clustering for speed:

cl = makePSOCKcluster(10)

# Fit GARCH

multf = multifit(multi_univ_garch_spec, asset_xts_ret, cluster = cl)

# Fit DCC

fit.dcc = dccfit(spec.dcc, data = asset_xts_ret, solver = "solnp",
                 cluster = cl, fit.control = list(eval.se = FALSE), fit = multf)

# Check Model

RcovList <- rcov(fit.dcc)

covmat = matrix(RcovList, nrow(asset_xts_ret), ncol(asset_xts_ret) * ncol(asset_xts_ret),
                byrow = TRUE)

mc1 = MCHdiag(asset_xts_ret, covmat)


# Wrangle output
dcc.time.var.cor <- rcor(fit.dcc)
print(dcc.time.var.cor[, , 1:3])

dcc.time.var.cor <- aperm(dcc.time.var.cor, c(3, 2, 1))
dim(dcc.time.var.cor) <- c(nrow(dcc.time.var.cor), ncol(dcc.time.var.cor)^2)

# Rename Output

dcc.time.var.cor <- renamingdcc(ReturnSeries = asset_xts_ret, DCC.TV.Cor = dcc.time.var.cor)

# plot equity

DCC_eq_plot <- ggplot(dcc.time.var.cor %>%

                          filter(grepl("JSAPYTR_Index_", Pairs),
                                 !grepl("_JSAPYTR_Index", Pairs))) +

    geom_line(aes(x = date, y = Rho, colour = Pairs)) +

    theme_hc() + labs(subtitle = "Dynamic Conditional Correlations: JALSHTR_Index", x = "", y = "") +

    fmx_cols() +

    theme_fmx(subtitle.size = ggpts(25), legend.size = ggpts(15))

plot_grid(DCC_eq_plot, labels = c(''))


###################################

DCCPre <- dccPre(asset_xts_ret, include.mean = T, p = 0)

names(DCCPre)

# We now have the estimates of volatility for each series.

Vol <- DCCPre$marVol

colnames(Vol) <- colnames(asset_xts_ret)

Vol <-
    data.frame( cbind( date = index(asset_xts_ret), Vol)) %>%

    mutate(date = as.Date(date)) %>%

    tbl_df()

TidyVol <- Vol %>%

    gather(Stocks, Sigma, -date)

ggplot(TidyVol) + geom_line(aes(x = date, y = Sigma, colour = Stocks))

# ------- Back to DCC:

# After saving now the standardized residuals:
StdRes <- DCCPre$sresi

detach("package:tidyverse", unload=TRUE)
detach("package:tbl2xts", unload=TRUE)

DCC <- dccFit(StdRes, type="Engle")

pacman::p_load("tidyverse", "tbl2xts", "broom")

Rhot <- DCC$rho.t
# Right, so it gives us all the columns together in the form:
# X1,X1 ; X1,X2 ; X1,X3 ; ....

# So, let's be clever about defining more informative col names.
# I will create a renaming function below:
ReturnSeries = asset_xts_ret
DCC.TV.Cor = Rhot

renamingdcc <- function(ReturnSeries, DCC.TV.Cor) {

    ncolrtn <- ncol(ReturnSeries)
    namesrtn <- colnames(ReturnSeries)
    paste(namesrtn, collapse = "_")

    nam <- c()
    xx <- mapply(rep, times = ncolrtn:1, x = namesrtn)
    # Now let's be creative in designing a nested for loop to save the names corresponding to the columns of interest..

    # TIP: draw what you want to achieve on a paper first. Then apply code.

    # See if you can do this on your own first.. Then check vs my solution:

    nam <- c()
    for (j in 1:(ncolrtn)) {
        for (i in 1:(ncolrtn)) {
            nam[(i + (j-1)*(ncolrtn))] <- paste(xx[[j]][1], xx[[i]][1], sep="_")
        }
    }

    colnames(DCC.TV.Cor) <- nam

    # So to plot all the time-varying correlations wrt SBK:
    # First append the date column that has (again) been removed...
    DCC.TV.Cor <-
        data.frame( cbind( date = index(ReturnSeries), DCC.TV.Cor)) %>% # Add date column which dropped away...
        mutate(date = as.Date(date)) %>%  tbl_df()

    DCC.TV.Cor <- DCC.TV.Cor %>% gather(Pairs, Rho, -date)

    DCC.TV.Cor

}

# Let's see if our function works! Excitement!
Rhot <-
    renamingdcc(ReturnSeries = asset_xts_ret, DCC.TV.Cor = Rhot)

head(Rhot %>% arrange(date))

# Let's now create a plot for all the stocks relative to the other stocks...
#property

gg_property <-
    ggplot(Rhot %>%

               filter(grepl("JSAPYTR_Index_", Pairs ),
                      !grepl("_JSAPYTR_Index", Pairs)) ) +
    geom_line(aes(x = date, y = Rho, colour = Pairs)) +
    theme_hc() +
    ggtitle("Dynamic Conditional Correlations: JSAPYTR_Index")

print(gg_property)

# equity
gg_equity <-
    ggplot(Rhot %>%

               filter(grepl("JALSHTR_Index_", Pairs ),
                      !grepl("_JALSHTR_Index", Pairs)) ) +
    geom_line(aes(x = date, y = Rho, colour = Pairs)) +
    theme_hc() +
    ggtitle("Dynamic Conditional Correlations: JALSHTR_Index")

print(gg_equity)

# Gold

gg_gold <-
    ggplot(Rhot %>%

               filter(grepl("Gold_", Pairs ),
                      !grepl("_Gold", Pairs)) ) +
    geom_line(aes(x = date, y = Rho, colour = Pairs)) +
    theme_hc() +
    ggtitle("Dynamic Conditional Correlations: Gold")

print(gg_gold)

# Bitcoin

gg_bitcoin <-
    ggplot(Rhot %>%

               filter(grepl("Bitcoin_", Pairs ),
                      !grepl("_Bitcoin", Pairs)) ) +
    geom_line(aes(x = date, y = Rho, colour = Pairs)) +
    theme_hc() +
    ggtitle("Dynamic Conditional Correlations: Bitcoin")

print(gg_bitcoin)


########################################

# Go-garch

# Now we create a gogarch model specification:
spec.go <- gogarchspec(multi_univ_garch_spec,
                       distribution.model = 'mvnorm', # or manig.
                       ica = 'fastica') # Note: we use the fastICA
cl <- makePSOCKcluster(10)
multf <- multifit(multi_univ_garch_spec, asset_xts_ret, cluster = cl)

fit.gogarch <- gogarchfit(spec.go,
                          data = asset_xts_ret,
                          solver = 'hybrid',
                          cluster = cl,
                          gfun = 'tanh',
                          maxiter1 = 40000,
                          epsilon = 1e-08,
                          rseed = 100)

print(fit.gogarch)

# Extracting time-varying conditional correlations: You know the drill...
gog.time.var.cor <- rcor(fit.gogarch)

gog.time.var.cor <- aperm(gog.time.var.cor,c(3,2,1))

dim(gog.time.var.cor) <- c(nrow(gog.time.var.cor), ncol(gog.time.var.cor)^2)

# Finally:
gog.time.var.cor <-
    renamingdcc(ReturnSeries = asset_xts_ret, DCC.TV.Cor = gog.time.var.cor)

# plot go garch

gg_go_equity <- ggplot(gog.time.var.cor %>% filter(grepl("JALSHTR_Index_", Pairs),
                                         !grepl("_JALSHTR_Index", Pairs))) +
    geom_line(aes(x = date, y = Rho,
                 colour = Pairs)) + theme_hc() + ggtitle("Go-GARCH: JALSHTR_Index")


print(gg_go_equity)

# prop

gg_go_prop <- ggplot(gog.time.var.cor %>%

                         filter(grepl("JSAPYTR_Index_", Pairs),

                        !grepl("_JSAPYTR_Index", Pairs))) +

    geom_line(aes(x = date, y = Rho,
         colour = Pairs)) + theme_hc() + ggtitle("Go-GARCH: JSAPYTR_Index")

print(gg_go_prop)

# gold

gg_go_gold <- ggplot(gog.time.var.cor %>%

                         filter(grepl("Gold_", Pairs),

                                !grepl("_Gold", Pairs))) +

    geom_line(aes(x = date, y = Rho,
                  colour = Pairs)) + theme_hc() + ggtitle("Go-GARCH: Gold")

print(gg_go_gold)

# bitcoin

gg_go_bitcoin <- ggplot(gog.time.var.cor %>%

                         filter(grepl("Bitcoin_", Pairs),

                                !grepl("_Bitcoin", Pairs))) +

    geom_line(aes(x = date, y = Rho,
                  colour = Pairs)) + theme_hc() + ggtitle("Go-GARCH: Bitcoin")

print(gg_go_bitcoin)


################################################

