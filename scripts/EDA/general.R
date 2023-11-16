###### EDA general #####

data <- read.csv(here("scripts", "data", "all_Merge.csv"))

### Boxplots to have an idea of the distribution and outliers ###

# different goals

par(mfrow=c(4,5)) 
for (i in 8:24) { 
  boxplot(data[, i], main = names(data)[i], horizontal = TRUE, ylim = c(0, 100))
}
par(mfrow=c(1,1))

### Corr + histograms

data_question1 <- read.csv(here("scripts", "data", "data_question1.csv"))
data_question24 <- read.csv(here("scripts", "data", "data_question24.csv"))
data_question3_1 <- read.csv(here("scripts", "data", "data_question3_1.csv"))
data_question3_2 <- read.csv(here("scripts", "data", "data_question3_2.csv"))
data_question3_3 <- read.csv(here("scripts", "data", "data_question3_3.csv"))

panel.hist <- function(x, ...){ 
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(usr[1:2], 0, 1.5) ) 
  h <- hist(x, plot = FALSE) 
  breaks <- h$breaks; nB <- length(breaks) 
  y <- h$counts; y <- y/max(y) 
  rect(breaks[-nB], 0, breaks[-1], y, col = "lightgreen", ...)
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){ 
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- (cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits = digits)[1] 
  txt <- paste0(prefix, txt) 
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt) 
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Independent variables 
pairs(data_question1[,c("overallscore", "unemployment.rate", "GDPpercapita", "MilitaryExpenditurePercentGDP", "internet.usage")], upper.panel=panel.cor, diag.panel=panel.hist)
pairs(data_question1[,c("overallscore", "pf_law", "pf_security", "pf_movement", "pf_religion", "pf_assembly", "pf_expression", "pf_identity")], upper.panel=panel.cor, diag.panel=panel.hist)
pairs(data_question1[,c("overallscore", "ef_government", "ef_legal", "ef_money", "ef_trade", "ef_regulation")], upper.panel=panel.cor, diag.panel=panel.hist)

pairs(data_question3_2[,c("overallscore", "cases_per_million", "deaths_per_million", "stringency")], upper.panel=panel.cor, diag.panel=panel.hist)
pairs(data_question3_3[,c("overallscore", "ongoing", "sum_deaths", "pop_affected", "area_affected", "maxintensity")], upper.panel=panel.cor, diag.panel=panel.hist)

### Summary tables

table1 <- summary(data_question24)
kable(table1)


