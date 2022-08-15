#-----Preparetion code--------------------------------
library(tidyverse)
library(writexl)
library(readxl)
library(lobstr)
library(dplyr)
library(tidyr)
library(stringi)
library(plotly)
library(ggplot2)

#PART 1 data loading 'spxallchart0', 'interestrate'
setwd("C:/Users/INSAGNIFICANT/Downloads/R");
#spxallchart0 = read.csv("^GSPC.csv")
#interestrate = read.csv("dat/INTDSRUSM193N.csv")
#originalspxallchar0 = read.csv("^GSPC.csv")
#originalinterestrate = read.csv("dat/INTDSRUSM193N.csv")

#Get SPX close data from the internet
spx_url <- "https://query1.finance.yahoo.com/v7/finance/download/%5EGSPC?period1=-1325635200&period2=1648771200&interval=1d&events=history"
spx <- read.csv(url(spx_url))
spx$Date <- as.Date(levels(spx$Date))[spx$Date]

#Get 10year treasury
year10_url <- "https://query1.finance.yahoo.com/v7/finance/download/%5ETNX?period1=-252374400&period2=1591660800&interval=1d&events=history"
year10 <- read.csv(year10_url)

#mfh <- read.csv("mfh.csv")
#colnames(mfh)[1]
#arrange(mfh %>% mutate(perc = (Feb * 100 / Feb.1)-100), perc)
  
#USA inflation rate
usa_inf_rate <- read.csv("united-states.inflation.monthly.csv")

#clean table of 'null' values
year10[year10$Close == factor('null', levels = levels(year10$Close)),] = NA
year10 <- na.omit(year10)
#convert Date and Close columns into Date and numeric
year10$Date <- as.Date(levels(year10$Date))[year10$Date]
year10$Close <- as.numeric(levels(year10$Close))[year10$Close]
plot_ly(
  x = ~year10$Date,
  y = ~year10$Close
) %>%
  add_lines()
#------------------------------------------
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#PART 2 data formating 'spxallchart0', 'interestrate'. Change field "date"-character to "date"-date
interestrate$DATE = as.Date(interestrate$DATE, "%Y-%m-%d")
colnames(interestrate) = c('Date', 'Intrate')
spxallchart0$Date = as.Date(spxallchart0$Date, "%Y-%m-%d")
#-----------------------------------------------------------
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#PART 3 remove rows in spxallchart0 before '1950-01-01'
#------------using cycles
#-------DELETED----------------------
#----------------------------------------------
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#PART 4 add additional date rows from 'spxallchart0' into 'interestrate'
class(interestrate[1,]) # 1. interestrate is a FRAME!!! every row is a FRAME EITHER!!!
lastrow <- data.frame(Sys.Date(), 0.25) # 2. So lastrow should be a FRAME EITHER!!!
interestrate[nrow(interestrate)+1,] <- lastrow # 3.pull lastrow into last+1 position in interestrate
interestrate <- interestrate[-845,] #remove row number 845
#-------------using cycles
#---------DELETED-----------
#----merge------using vector logic
tmpspx = filter(spxallchart0, spxallchart0$Date < '1951-01-01')
tmprate = filter(interestrate, interestrate$Date < '1951-01-01')
tmpmerge = merge(tmpspx, tmprate) #merge only the firsts days
tmpmerge = merge(spxallchart0, interestrate) #merge() doesn't merge 2020 year. don't know why
#---remove part
na.omit(fill(merge(tmpspx, tmprate, all = TRUE), Intrate))
#---remove part
# PART 4 SOLVED BY VEC LOG-----MERGE(),FILL(),NA.OMIT()-------using vector logic
f1 <- function(x, y) {
  tmp = merge(x, y, all = TRUE)
  fill(tmp, Intrate)
}
f1(spxallchart0, interestrate)
result <- f1(spxallchart0, interestrate)
result <- na.omit(result)
spx_interestrate <- result
rm(result)

#################################################################################################
#################################################################################################
################################ INFLATION RATE #################################################
#################################################################################################
#################################################################################################

#INFLATION RATE USA, editing
#pure monthly inflation rate table
do_usa_inf_rate_data <- function()
{
  usa_inf_rate <- read.csv("united-states.inflation.monthly.csv")
  colnames(usa_inf_rate)[1] <- "date_n"
  
  usa_inf_rate[1,]

  usa_inf_rate_transpose <- t(usa_inf_rate)
  colnames(usa_inf_rate_transpose) <- usa_inf_rate_transpose[1,]
  usa_inf_rate_transpose <- usa_inf_rate_transpose[-1,]

  Sys.setlocale(category = "LC_TIME", locale = "english")
  
  usa_inf_rate <- data.frame(Date = as.Date(paste(colnames(usa_inf_rate_transpose)[rep(1:length(colnames(usa_inf_rate_transpose)), 
                                                                                       rep(12, length(colnames(usa_inf_rate_transpose))))],
                                                  "-",
                                                  rownames(usa_inf_rate_transpose[1:12,]),
                                                  "-01", sep = ""), "%Y-%B-%d"), 
                             inf_rate = as.vector(unlist(usa_inf_rate_transpose)[1:12,]))
  
  add_data_to_usa.inf.rate <- data.frame(Date = seq(as.Date("2021-01-01"), as.Date("2022-02-01"), "month"),
                                         inf_rate = c(0.7,1.7,2.6,4.2,5,5.4,5.4,5.3,5.4,6.2,6.8,7,7.5,7.9))
  
  
  usa_inf_rate <- rbind(usa_inf_rate, add_data_to_usa.inf.rate)
  usa_inf_rate
}
usa_inf_rate <- do_usa_inf_rate_data()
do_spx_for_inf_rate_data <- function(usa_inf_rate)
{
  require(tidyr)
  spx_url <- "https://query1.finance.yahoo.com/v7/finance/download/%5EGSPC?period1=-1325635200&period2=1648771200&interval=1d&events=history"
  spx <- read.csv(url(spx_url))
  spx$Date <- as.Date(levels(spx$Date))[spx$Date]
  spx <- spx[-1,c(-2,-3,-4,-6, -7)]
  usa_inf_rate_filtered <- usa_inf_rate[usa_inf_rate$Date >= as.Date("1927-12-30"),]
  na.omit(merge(spx, usa_inf_rate_filtered, by = "Date", all = TRUE) %>% fill(inf_rate))
}
spx.infrate <- do_spx_for_inf_rate_data(usa_inf_rate)

dividing_by_10years_list <- function(spx.infrate)
{
  tmp <- spx.infrate
  spx.inf.list <- list()
  n = 1
  for(i in seq(1928, 2022, 10))
  {
    j = i + 10
    spx.inf.list[[n]] <- filter(tmp, between(Date, as.Date(paste(as.character(i),"-01-","01",sep="")), 
                                             as.Date(paste(as.character(j),"-12-","31",sep=""))))
    n = n+1
  }
  spx.inf.list
}
spx.inf.list <- dividing_by_10years_list(spx.infrate)
do_plot_infrate <- function(spx.infrate)
{
  plot(spx.infrate$Date, 
       spx.infrate$inf_rate, type = 'h')
  
}
do_plot_infrate(spx.infrate)

#suit value of SPX to value of Inflation Rate
#spx.infrate$Close <- spx.infrate$Close/(spx.infrate$Close[1]*10)
do_plotly_spx_inf <- function(spx.inf.list) 
{
  require(plotly)
  fig <- spx.inf.list %>% plot_ly() %>%
    add_trace(x = ~Date, y = ~Close, type = 'scatter', mode = 'lines') %>%
    add_trace(x = ~Date, y = ~inf_rate, type = 'bar')
  fig
}  
do_plotly_spx_inf(spx.inf.list[[2]])

do_plotly_spx_inf_fitting_spx <- function(spx.inf.list)
{
  require(plotly)
  spx.inf.list$Close <- spx.inf.list$Close / (spx.inf.list$Close[1]*0.4)
  fig <- spx.inf.list %>% plot_ly(
    colors = colorRamp(c("grey", "black"))
  ) %>%
    add_trace(x = ~Date, y = ~inf_rate, type = 'bar', name = "Inflation Rate") %>%
    add_trace(x = ~Date, y = ~Close, type = 'scatter', mode = 'lines', name = "SPX") %>%
    layout(xaxis = list(title = "Date", tickformat = "%Y"), legend = list(x = 0.1, y = 0.9))
  options(warn = -1)
  fig <- fig %>%
    layout(
      xaxis = list(title = "Date",
                   zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'),
      yaxis = list(title = "SPX and Inflation Rate",
                   zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'),
      plot_bgcolor='#e5ecf6')
  fig
}
do_plotly_spx_inf_fitting_spx(spx.inf.list[[10]])
do_plotly_spx_inf_fitting_spx(spx.inf.list[[1]])
do_plotly_spx_inf_fitting_spx(spx.inf.list[[2]])
do_plotly_spx_inf_fitting_spx(spx.inf.list[[3]])
do_plotly_spx_inf_fitting_spx(spx.inf.list[[4]])


ggplot(spx.inf.list[[1]], aes(x = Date, y = inf_rate)) +
  geom_col(color = "red") +
  geom_line(aes(x = Date, y = (Close/(Close[1]))))

#LIBOR CURVE 2022-04-06
df <- data.frame(date = factor(1:5, labels = c("o/n","1m", "3m", "6m", "12m"), ordered = FALSE),
                 value = c(0.32,0.42,0.96,1.49,2.2))
plot(df$date, df$value, type = "h")

#################################################################################################
#################################################################################################
################################ #################################################
#################################################################################################
#################################################################################################



#merge() does merging without intrate$Date column, so the end result
#---------------------------------------------------------------  
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#PART 5 merge 'spxallchart0' and 'interestrate' into one table
spx_interestrate = cbind(spxallchart0, interestrate_fullfilled) # merge tables into new table
new2 <- spx_interestrate[,-8] # remove column 8
colnames(new2)[8] <- 'IntRate' #rename column 8 to appropriete form
spx_interestrate = new2
write_xlsx(spx_interestrate,'dat/spx_interestrate.xlsx')
spx_interestrate = read_xlsx('dat/spx_interestrate.xlsx')
rm(new2)

#-----------------------------------------------------------------------------

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#PART 6 data analysis
date_short = spx_interestrate %>% filter(Date > '2020-01-01') # select rows, where Date >
p2 = spx_interestrate %>% filter(Date < '1970-01-01') #filter data
p2$Date = as.Date(p2$Date)

#PART 7 first days of every month
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
filter(spx_interestrate, 
       spx_interestrate$Date %in% seq(from = spx_interestrate$Date[1], 
                                      to = spx_interestrate$Date[length(spx_interestrate$Date)],
                                      by = 'month'))

        #-------------------------PLOT-------------------------------------
plot(spx_interestrate$Date, 
         spx_interestrate$Close, 
         type = "l", 
         col = "dark red") +
par(new=TRUE)
plot(spx_interestrate$Date, spx_interestrate$IntRate, col = "blue", type = "l")

plot(p2$Date, 
     p2$Close, 
     type = "l", 
     col = "dark red") +
  par(new=TRUE)
plot(p2$Date, p2$IntRate, col = "blue", type = "l")

#сначала формируется чистый plot type="n", в который добавляются диапазоны x и y
xrange = range(date_short$Date)
yrange = range(date_short$IntRate, date_short$Close)
plot(xrange, yrange, xlab = "Date", ylab = "SPX and Int Rate", type = "n")

lines(date_short$Date, date_short$IntRate, col = "blue")
lines(date_short$Date, date_short$Close, col = "Dark Red")

        #-----------------------GGPLOT-------------------------------------
require(ggplot2)
detach()
date_short %>%
ggplot() +
  geom_line(mapping = aes(x = Date, y = Close), ) 
  par(new = TRUE) +
  geom_line(mapping = aes(x = Date, y = IntRate))

matplot(date_short$Date, cbind(date_short$Close, date_short$IntRate), type = "l")


#---------------------plotly----------------------------------------
plot_ly(date_short) %>%
  add_trace(x=~Date, y=~Close, connectgaps = TRUE) %>%
  add_trace(x=~Date, y=~IntRate)
#-----------------------------------------------------------------------------


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#Count days when SPX up or down when IntRate up. And count days when spx U/D and I.R. down
spx_interestrate$Date = as.numeric(spx_interestrate$Date)
cor(select(spx_interestrate, 5), select(spx_interestrate, 8))
lm(spx_interestrate[,5] ~ spx_interestrate[,8], data = spx_interestrate)


#-----------Count pos and neg days, Draw comparing charts-----------------------
temp <- spx$Close[-1]
temp[length(temp)+1] <- temp[length(temp)]

# 3 methods, count TRUE, FALSE, EQUAL
#first
pos <- sum(spx$Close < temp, na.rm = TRUE)
neg <- sum(spx$Close > temp, na.rm = TRUE)
eq  <- sum(spx$Close == temp, na.rm = TRUE)
#second
pos <- table(spx$Close > temp)["TRUE"]
#third
pos <- length((spx$Close > temp)[(spx$Close > temp) == TRUE])
#equal days
sum(spx$Close == temp)
#count FALSE
length(spx$Close > temp) - sum(spx$Close > temp)
#count SPX percentage difference b/n Di and Di+1
temp <- spx$Close[-1]
temp[length(temp)+1] <- temp[length(temp)]
spx <- cbind(spx, "offset" = temp)
spx <- mutate(spx, "percent_diff" = (spx$offset*100)/spx$Close - 100)
sum(spx$percent_diff)
summary(spx$percent_diff)

#count 10year treasury percentage difference b/n Di and Di+1
temp <- year10$Close[-1]
temp[length(temp)+1] <- temp[length(temp)]
year10 <- cbind(year10, "offset" = temp)
year10 <- mutate(year10, "percent_diff" = (year10$offset*100)/year10$Close - 100)
p <- ggplot(year10, aes(Date, percent_diff)) +
  geom_line()
ggplotly(p, dynamicTicks = "y")
style(p, hoveron = "points", hoverinfo = "x+y+text", hoverlabel = list(bgcolor = "white"))

#BOTH, PERCENTAGE DIFFERENCE: SPX and 10 year treasuries 
#-first

tmp <- spx[spx$Date >= '1962-01-02',]
tmp <- spx
fig_spx <- tmp %>% plot_ly(
  x = ~Date,
  y = ~percent_diff
) %>%
  add_lines()
fig_spx
fig_10year <- year10 %>% plot_ly(
  x = ~Date,
  y = ~percent_diff
) %>%
  add_lines()
fig_10year
subplot(fig_spx, fig_10year)
#-second
tmp <- spx[spx$Date >= '1962-01-02',]
fig <- tmp %>% plot_ly(
  x = ~Date,
  y = ~percent_diff,
  name = "SP500"
) %>%
  add_lines()
fig <- fig %>%
  add_lines(
    x = ~year10$Date,
    y = ~year10$percent_diff,
    alpha = 0.3,
    name = "10 year<br>treasuries"
  )
fig <- fig %>%
  layout(
    line = list(
      color = list("red")
    )
  )
fig

#two charts of SPX and 10year T, y and y0 attempt
tmp <- spx[spx$Date >= '1962-01-02',]
tmp2 <- tmp[,c(1,5)]; colnames(tmp2) <- c("Date", "SPX")
tmp3 <- year10[,c(1,5)]; colnames(tmp3) <- c("Date", "US10Y")

#equalize spx and 10 year T by Date, different trading days
df <- merge(tmp2,tmp3, all = TRUE)
df[is.na(df),]
df <- fill(df, US10Y)

#draw two Y-axis charts
fig <- df %>% plot_ly(
  colors = colorRamp(c("grey", "black"))
  ) %>%
  add_trace(
    x = ~Date,
    y = ~SPX,
    type = 'scatter',
    mode = 'lines',
    name = "SPX"
  ) %>%
  add_trace(
    x = ~Date,
    y = ~US10Y,
    type = 'scatter',
    mode = 'lines',
    yaxis = "y2", #exactly !!!yaxis = "y2"!!! this command draw right Y-axis
    opacity = .4,
    name = "10 years T",
    line = list(color = "red")
  ) %>%
  layout(
    xaxis = list(
      title = "Date",
      rangeslider = list(type = "date")
    ),
    yaxis = list(
      title = "SPX",
      side = "left"
    ),
    yaxis2 = list( #help to draw right Y-axis and two charts simultaneously
      overlaying = "y",
      side = "right", 
      title = "10 years T",
      autorange = TRUE,
      automargin = TRUE
    ),
    legend = list(x = 0.1, y = 0.9),
    hovermode = "x unified"
  )
fig

fig <- plot_ly() %>%
  add_trace(
    x = ~tmp$Date,
    y = ~tmp$Close,
    type = 'scatter',
    mode = 'lines'
  ) %>%
  add_trace(
    x = ~year10$Date,
    y = ~year10$Close,
    type = 'scatter',
    mode = 'markers',
    yaxis = "y2"
  ) %>%
  layout(
    yaxis2 = list(
      overlaying = "y",
      side = "right"
    ),
    hovermode = "x unified"
  )
fig
summary(year10[year10$Date >= '2000-01-01' & year10$Date <= '2009-12-31',]$Close)
summary(year10[year10$Date >= '2010-01-01' & year10$Date <= '2019-12-31',]$Close)

#-----------Regression SPX and 10 year T, plots--------------------------------
#--should be columns in ONE table
sol <- glm(US10Y ~ SPX, data = df)
summary(sol)
p <- ggplot(NULL) +
  geom_smooth(data = sol, aes(US10Y, SPX), method = 'lm') +
  geom_point(aes(df$US10Y, df$SPX))
ggplotly(p)
