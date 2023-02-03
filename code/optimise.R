
# portfolio optimization
install.packages("fPortfolio")
library(fPortfolio)

#forcast

dcc.forcast <- dccforecast(fit.dcc, n.ahead = 1, n.roll = 0)
print(dcc.forcast)

covmat.forcast <- rcov(dcc.forcast)

covmat = covmat.forcast$`2019-12-31`[,,1]

#optimize

# efficiency portfolio

all.data <- as.timeSeries(100*asset_xts_ret)

# equal weight

ewSpec <- portfolioSpec(portfolio = list(weights=rep(1, NCOL(asset_xts_ret))/NCOL(asset_xts_ret) ))

ewportfolio <- feasiblePortfolio(data = all.data,spec = ewSpec, constraints = "LongOnly")

print(ewportfolio)
weightsPie(ewportfolio)
#minimum risk

minriskSpec <- portfolioSpec()

targetReturn <- getTargetReturn(ewportfolio@portfolio)["mean"]

`setTargetReturn<-`(minriskSpec,targetReturn)

#now we optimise

minRiskPortfolio <- efficientPortfolio(data = all.data, spec = minriskSpec,
                                       constraints = "LlongOnly")

print(minRiskPortfolio)
weightsPie(minRiskPortfolio)
# tangency portfolio

Tangecy_Portfolio <- tangencyPortfolio(data = all.data, spec = minriskSpec,
                                       constraints = "LongOnly")

print(Tangecy_Portfolio)

weightsPie(Tangecy_Portfolio)
#######################################

fPortfolio::weightsPlot(portFrontier)

#The black line through the chart indicates the minimum variance portfolio.
###########################################

# alt approach

#Annualized Performance with Risk Free Rate 4.5%
performance_table <- as.data.frame(table.AnnualizedReturns(asset_xts_ret,
                                                           Rf = 0.05/279.8))

performance_table <- rownames_to_column(performance_table)

names(performance_table)[1] <- 'Performance'

#Tidying Annualized Performance Dataframe
performance_df <- performance_table %>%

    gather(key = 'Code', value = 'Values', -Performance) %>%

    spread(key = Performance, value = Values) %>%

    rename('Annualized_Return' = 'Annualized Return',
           'Annualized_Sharpe' = 'Annualized Sharpe (Rf=4.5%)',
           'Annualized_StdDev' = 'Annualized Std Dev' ) %>%

    select(Code,Annualized_Return, Annualized_StdDev, Annualized_Sharpe)

print(performance_df)
print(performance_table)



#Portfolio Daily Return Rebalance every Quarters

Portfolio_Return <- Return.portfolio(asset_xts_ret, weights = ew,
                                     rebalance_on = 'quarters')

#Portfolio Returns Distribution on Period

chart.Histogram(Portfolio_Return,
                main = "Portfolio Daily Returns Distributions (2013-2020)",
                cex.axis = 1.2, cex.lab = 1.5, cex.main = 2,
                colorset = "#F77171",
                note.line = mean(Return.portfolio(asset_xts_ret)),
                note.label = 'Average', note.color = 'black', note.cex = 1.2)

charts.PerformanceSummary(Portfolio_Return,
                          main = 'Portfolio Performance Summary')


#min risk weights

minriskweight <- as.numeric(c(0.2557, 0.2060, 0.5257, 0.0126))

#Portfolio Daily Return Rebalance every Quarters

Portfolio_Return_minrisk <- Return.portfolio(asset_xts_ret, weights = minriskweight,
                                     rebalance_on = 'quarters')

#Portfolio Returns Distribution on Period

chart.Histogram(Portfolio_Return_minrisk,
                main = "Portfolio Daily Returns Distributions (2013-2020)",
                cex.axis = 1.2, cex.lab = 1.5, cex.main = 2,
                colorset = "#F77171",
                note.line = mean(Return.portfolio(asset_xts_ret)),
                note.label = 'Average', note.color = 'black', note.cex = 1.2)

charts.PerformanceSummary(Portfolio_Return_minrisk,
                          main = 'Portfolio Performance Summary')


#Tangency weights

tangent_weight <- as.numeric(c(0.4906, 0.0000, 0.3534, 0.1560))
#Portfolio Daily Return Rebalance every Quarters

Portfolio_Return_tangent <- Return.portfolio(asset_xts_ret, weights = tangent_weight,
                                             rebalance_on = 'quarters')

#Portfolio Returns Distribution on Period

chart.Histogram(Portfolio_Return_tangent,
                main = "Portfolio Daily Returns Distributions (2013-2020)",
                cex.axis = 1.2, cex.lab = 1.5, cex.main = 2,
                colorset = "#F77171",
                note.line = mean(Return.portfolio(asset_xts_ret)),
                note.label = 'Average', note.color = 'black', note.cex = 1.2)

charts.PerformanceSummary(Portfolio_Return_tangent,
                          main = 'Portfolio Performance Summary')


