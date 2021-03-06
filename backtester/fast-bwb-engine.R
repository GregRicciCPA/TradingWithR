# Options Backtester
# v0.3 requirements:
#   1.  DONE Trade log to compare notes with people
#   2.  DONE Keep track of greeks of the position as well to do greek-based exits
#   3.  DONE Do #2 without slowing it down too much
#   4.  Experiment with data.table, especially read/write backdata
#   5.  Start doing the 60-40-20
#       a.  FindBWB
#       b.  Wait -- do you just do this as a whole new file? less juggling?
#   6.  Get the 2 expiration things working
#       a.  maybe just check nrow() of returned stuff? 4? cool. 8? split it.

# Thoughts on input data validation 
#   1.  Check for duplicates
#   2.  Check for what's expected (did you read in july quotes in january?)
#   3.  Check for too many things across total input and each month
#   4.  Check for too few things across total input and each month
#   3-4b. Check you have an option chain date for every day in my.cal?
#   5.  Check you have an OISUF for every option chain date
#   6.  Is InitialCredit() wild because of price errors or inefficiency?


# Use EAE() instead of == on floating point numbers
EAE <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})

# Setup date stuff
library('bizdays')
library('quantmod')
library('PerformanceAnalytics')
library('scales')
library('httr')
library('futile.logger')
library('parallel')

# HaveNetwork: check to see if we have network connectivity and stop if not
HaveNetwork = function() {
  my.site      = "https://www.google.com"
  my.response  = GET(my.site)
  if (my.response$status_code != 200) {
    flog.fatal("Non-200 response from Google. Aborting. Actual code:%s",
               my.response$status_code)
    stop(paste(("Non-200 response from Google. Aborting. Actual code:"),
               my.response$status_code))
  }
}

HaveNetwork()

cal.begin       = "2010-01-01" # TODO support shorter date ranges
cal.end         = "2019-12-31" # Have data until 2018-12-20
my.holidays     = c('2010-01-01', '2010-01-18', '2010-02-15',
                    '2010-04-02', '2010-05-31', '2010-07-05',
                    '2010-09-06', '2010-11-25', '2010-12-24',
                    '2011-01-01', '2011-01-17', '2011-02-21',
                    '2011-04-22', '2011-05-30', '2011-07-04',
                    '2011-09-05', '2011-11-24', '2011-12-26',
                    '2012-01-02', '2012-01-16', '2012-02-20',
                    '2012-04-06', '2012-05-28', '2012-07-04',
                    '2012-09-03', '2012-11-22', '2012-12-25',
                    '2013-01-01', '2013-01-21', '2013-02-18',
                    '2013-03-29', '2013-05-27', '2013-07-04',
                    '2013-09-02', '2013-11-28', '2013-12-25',
                    '2014-01-01', '2014-01-20', '2014-02-17',
                    '2014-04-18', '2014-05-26', '2014-07-03',
                    '2014-09-01', '2014-11-27', '2014-12-25',
                    '2015-01-01', '2015-01-19', '2015-02-16',
                    '2015-04-03', '2015-05-25', '2015-07-03',
                    '2015-09-07', '2015-11-26', '2015-12-25',
                    '2016-01-01', '2016-01-18', '2016-02-15',
                    '2016-03-25', '2016-05-30', '2016-07-04',
                    '2016-09-05', '2016-11-24', '2016-12-26',
                    '2017-01-02', '2017-01-16', '2017-02-20',
                    '2017-04-14', '2017-05-29', '2017-07-04',
                    '2017-09-04', '2017-11-23', '2017-12-25',
                    '2018-01-01', '2018-01-15', '2018-02-19',
                    '2018-03-30', '2018-05-28', '2018-07-04',
                    '2018-09-03', '2018-11-22', '2018-12-25',
                    '2019-01-01', '2019-01-21', '2019-02-18',
                    '2019-04-19', '2019-05-27', '2019-07-04',
                    '2019-09-02', '2019-11-28', '2019-12-25',
                    '2020-01-01')
# sample bizdays call: bizdays("2014-01-02", "2014-01-21", mycal) = 12
# Set up calendar
my.cal = create.calendar(holidays = my.holidays, 
                         start.date = cal.begin, 
                         end.date=cal.end, 
                         weekdays=c("saturday", "sunday"),
                         name="my.cal")
setwd("~/Documents/TradingWithR/backtester")
#getSymbols("^RUT", from=cal.begin) # RIP Yahoo Finance API
my.sym = "SPX"
getSymbols(my.sym, src="csv")
oisuf.raw    = read.csv("oisuf-spx-all.csv") # 2004-2017
oisuf.values = as.xts(oisuf.raw[,2], order.by=as.Date(oisuf.raw[,1]))

# easily get and set oisuf values you don't have
library(timeDate)
tS = timeSequence(from=as.Date("2018-06-02"), to=as.Date("2018-12-20"))
my.weekdays = isBizday(tS)
my.bizdays = as.character(tS[my.weekdays])
added.oisuf = rep(0, length(my.bizdays))
new.oisuf = as.xts(added.oisuf, order.by=as.Date(my.bizdays))
oisuf.values = rbind(oisuf.values, new.oisuf)

#Choose 1TPX or 1TPS
#1TPX = 1 trade per execution. Only one trade allowed open at a time
#1TPS = 1 trade per signal. Open a new trade every day.

# setup for real world path-dependent test:
kOisufThresh = -200
kDTRThresh   = 0.5
kSlippage    = -0.20  # a dime per side entry/exit
kContracts   = 3      # 1/100 for academic mode
kInitBalance = 40000  # 100 for academic mode
kMaxLoss     = 0.02 * kInitBalance # 2% of starting balance
global.mode  = "1TPX"

#path-independent test with 100k and 1 contract and 0.01 slippage
# kOisufThresh = -200
# kDTRThresh   = 0.5
# kSlippage    = -0.01  # a dime per side entry/exit
# kContracts   = 1      # 1/100 for academic mode
# kInitBalance = 100000 # 100 for academic mode
# kMaxLoss     = 270    # $270/contract to match real money mode
# global.mode = "1TPS"

# PickByDelta: return an index of x that has the closest value to y. if there
#              is a tie, return the first one you come to. This function
#              may be erroneous due to floating point comparison. Check
#              with sprintf("%.54f", x) later on.
# inputs:
#   my.deltas, a list of deltas to compare against
#   my.value,  a value to compare to the list of deltas
# output:
#   the index (position) of the closest delta to your given value
# example:
#   my.deltas = c(0.005, 0.076, 0.090, 0.110, 0.121)
#   my.value  = 0.11
#   pick.by.delta(my.deltas, my.value) = 4
#   my.value  = 0.08
#   pick.by.delta(my.deltas, my.value) = 2
PickByDelta = function(my.deltas, my.value) {
  which.min(abs(my.value - my.deltas))
}

# OptionQuotesCsv: return a data frame that consists of the exported OptionVue
#                  quote data
# inputs:
#   ticker, the underlying symbol
#   datecode, the date of the export in format YYYYMMDD
#   timecode, the time of the export in format HHMM (24 hour time)
# output:
#   the data that was in the OptionVue CSV export as a data frame
# example:
#   data in a file called SPX201703201530.csv
#   my.df = get.quotes.csv("SPX", 20170320, 1530)
OptionQuotesCsv = function(options.file) {
  my.data       = read.csv(options.file)
  if (length(grep("Index", my.data$Description)) > 1) {
    stop(paste("Data error: Input file ", options.file," had multiple exports",
               sep=""))
  } else {
    return(my.data[-1,]) # first row is underlying. use price.quotes for that
  }
}

# EnrichOptionsQuotes: return the option quotes df with some extra info
#                      that makes it easier for the backtester to work with
# inputs:
#   my.df, the original quote data frame
#   ASSUMES SOME STUFF ABOUT PREVIOUSLY SET UP CALENDARS
# output:
#  the data frame plus the following new fields: 
#   my.exp.date, the Exp.Date field as a proper YYYY-MM-DD date format
#   biz.dte, business days until expiry
#   cal.dte, calendar days until expiry
#   mid.price, the average of the quoted bid and ask (NAs?)
EnrichOptionsQuotes = function(my.df) {
  my.df = my.df[!is.na(my.df$Date),]
  my.df = my.df[!is.na(my.df$Bid),]
  my.df = my.df[!is.na(my.df$Ask),] # OptionVue now calls this Ask not Asked
  my.df = my.df[!grepl("D\\d{1,2}$", my.df$Symbol, perl = TRUE),]
  my.df = my.df[!grepl("\\d{4}(26|27|28|29|30|31)", my.df$Exp.Date),]
  
  my.exp.date = as.Date(as.character(my.df$Exp.Date), "%y%m%d")
  my.iso.date = as.Date(as.character(my.df$Date), "%y%m%d")
  biz.dte     = bizdays(my.iso.date,
                        my.exp.date,
                        my.cal)
  cal.dte     = as.numeric(my.exp.date - my.iso.date)
  mid.price   = (my.df$Bid + my.df$Ask) / 2 # OptionVue now calls this Ask not Asked
  return(cbind(my.df, my.iso.date, my.exp.date, biz.dte, cal.dte, mid.price))
}

# FindBWB: find the strikes of 60-40-20 trade by delta: long 60/20 short 2x 40
# inputs:
#   my.df, a data frame consisting of a Delta column and Existing.Posn. column
#   is.list, a boolean of whether or not the input is actually a list
# outputs:
#   a data frame consisting of only the trades to take to establish the BWB
#   if there is more than one available expiration, take the min for now
#   TODO: proper multi-expiration handling
FindBWB = function(my.df, is.list = FALSE) {
  if (is.list) {
    my.df = my.df[[1]]
  }
  # clean up to only include traditional monthlies
  # (should be checking this in data export as well)
  my.df = my.df[!grepl("D\\d{1,2}$", my.df$Symbol, perl = TRUE),]
  my.df = my.df[!grepl("\\d{4}(26|27|28|29|30|31)", my.df$Exp.Date),]
  # clean up to only include possible candidates
  my.df = subset(my.df, cal.dte > 59 & cal.dte < 91)
  # if you have no expirations (a few days like this), return NULL
  if (nrow(my.df) == 0) {
    return(NULL)
  } else {
    # if you have two expirations, pick the minimum for now
    my.df = subset(my.df, cal.dte == max(cal.dte))
    # deltas are negative, trading in puts only
    my.df[PickByDelta(my.df[,16],  -60),1] =  1 # col 16 is Delta, 1 is Ex.Pos.
    my.df[PickByDelta(my.df[,16],  -40),1] = -2
    my.df[PickByDelta(my.df[,16],  -20),1] =  1
    # Existing.Posn is NA by default, so this works but is not intuitive:
    my.open.trades = my.df[!is.na(my.df[,1]),]
    # add new $orig.price column for later
    my.open.trades$orig.price = my.open.trades[,27] # orig price = mid price
    return(my.open.trades)
  }
}

# find floating profit given df of open trades w/ column orig.price
# TODO this should have num contracts and slippage passed in, not 
# read from global
FloatingProfit = function(trades) {
  floating.profit = (trades[,27] - trades[,28]) * trades[,1]
  floating.profit = sum(floating.profit, kSlippage)*kContracts*100
}

# find initial credit given df of open trades w/ column orig.price
InitialCredit = function(trades) {
  net.credit = sum(trades[,1] * trades[,28])*kContracts*100
}

# calculate Delta / Theta ratio. always positive.
FindDTR = function(my.df) {
  abs(sum(my.df[,16] * my.df[,1]) / sum(my.df[,1] * my.df[,19]))
}

# check the prices in your current quote (whole thing or maybe one trade)
# input:
#         my.df - any quote in data frame format w/ columns mid.price, Th.Price
#         amount - how much percent difference to alert on (default 0.10 = 10%)
# output: TRUE/FALSE depending on if any prices are off from their theoritcal
#         prices by too much.
#         TRUE  => everything is OK
#         FALSE => prices are too far off
# example:
#   df$mid.price = c(1, 2, 3)
#   df$theo.price = c(1.05, 2.1, 3.2)
#   OKPrices(df) = TRUE
#   --
#   df$mid.price = c(1, 2, 3)
#   df$theo.price = c(1.3, 2.2, 3.3)
#   OKPrices(df) = FALSE # 1.3 > 10% * 1.0
OKPrices = function(my.df, amount = 0.10) {
  final    = my.df$mid.price
  initial  = my.df$Th.Price
  whack.prices = abs(((final-initial)/initial)) > rep(amount, nrow(my.df))
  if (any(whack.prices)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# check the exit price you're about to record
# input:
#         my.df - quote in data frame format w/ column mid.price
#         amount - how low you want to alarm on (default = 0)
# output:
#         TRUE/FALSE depending on if prices are equal to or below amount
#         TRUE => everything is OK
#         FALSE => price is off (probably a bad quote)
# example:
#         df$mid.price = c(1, 2, 3)
#         OKExit(df) = TRUE
#         df$mid.price = c(1, 0, 3)
#         OKExit(df) = FALSE
OKExit = function(my.df, amount = 0) {
  zeroes = my.df$mid.price <= amount
  if(any(zeroes)) return(FALSE)
  else return(TRUE)
}

# see if exit conditions are met for a given condor, xts underlying, char date
####### TODO What are results without the strike changes?
ShouldExit = function(my.df, my.date) {
  if (is.null(my.cal) || !exists("my.cal")) # should check dates too
    stop("global calendar (my.cal) does not exist or is NULL")
  days.open = as.numeric(as.Date(my.date) - my.df[1,]$my.iso.date)
  #if (days.open < 7)
  #  return(FALSE)
  #else if (abs(sum(my.df$Delta * my.df[,1]) / sum(my.df[,1] * my.df$Theta)) > 0.5)
  if (FloatingProfit(my.df) < (kMaxLoss * -1))
    return(TRUE)
  else if (FindDTR(my.df) > kDTRThresh)
    return(TRUE)
  #else if (my.df[2,16] < -50 || my.df[2,16] > -30) # middle strike always 2nd
  #  return(TRUE)
  #else if (my.df[3,16] < -80 || my.df[3,16] > -40) # upper strike always 3rd
  #  return(TRUE)
  else if (bizdays(as.Date(my.date), as.Date(my.df[1,24]), my.cal) <= 21) 
    return(TRUE)
  else 
    return(FALSE)
}

# a copy of ShouldExit to give detail on the exit reason
ExitReason = function(my.df, my.date) {
  if (is.null(my.cal) || !exists("my.cal")) # should check dates too
    stop("global calendar (my.cal) does not exist or is NULL")
  if (FloatingProfit(my.df) < (kMaxLoss * -1))
    return(paste("0: Max loss"))
  else if (FindDTR(my.df) > kDTRThresh) 
    return(paste("1: DTR > ", kDTRThresh))
  #else if (my.df[2,16] < -50 || my.df[2,16] > -30) # middle strike always 2nd
  #  return(paste("2: middle (40) strike outside limits")) #, my.df[2,16])) #dbug
  #else if (my.df[3,16] < -80 || my.df[3,16] > -40) # upper strike always 3rd
  #  return(paste("3: upper (60) strike outside limits")) #, my.df[2,16])) #dbug
  else if (bizdays(as.Date(my.date), as.Date(my.df[1,24]), my.cal) <= 21) 
    return(paste("4: 21 biz DTE"))
  else 
    return(paste("-1: Other"))
}

# see if entry conditions are met for a given day's list 
# idea here will be to check if you have an open trade on that quote day
# for the given condor you'd get out of the quote from that day
# Don't open a trade in more than one expiration at a time
# Don't open a trade outside of given trade window (min/max days)
#   (that's handled by FindCondor function)
ShouldEnter = function(my.trade.list, my.quote) {
  my.open.trades = do.call('rbind', my.trade.list)
  if (is.null(my.open.trades)) {
    return(TRUE)
  } else {
    open.exps = unique(my.open.trades$my.exp.date)
  }
  would.trade.exp = unique(FindBWB(my.quote)$my.exp.date)
  # check for null before running %in%
  if (is.null(would.trade.exp)) {
    return(FALSE)
  } else if (would.trade.exp %in% open.exps && global.mode == "1TPX") {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# output a short trade summary data frame given:
# a trade you're about to close
# the current date as.Date
TradeSummary = function(my.df, my.date) {
  trade.data = list(open.date     = my.df[1,]$my.iso.date,
                    exp.month     = substr(my.df[1,]$Description, 1, 3),
                    strikes       = toString(my.df$Strike.Price),
                    init.debit    = InitialCredit(my.df),
                    close.profit  = FloatingProfit(my.df),
                    cal.days.open = as.numeric(my.date - my.df[1,]$my.iso.date),
                    close.date    = my.date,
                    dtr           = abs(sum(my.df$Delta * my.df[,1]) /
                                          sum(my.df[,1] * my.df$Theta)))
  return(as.data.frame(trade.data))
}

# helper function to see what data in the quote is off from theoretical price
# input is the "i" value in the main backtest loop
SeeBadData = function(iterator) {
  foo = my.data[[iterator]]
  bar = foo$mid.price - foo$Th.Price
  baz = bar / foo$Th.Price
  plot(baz)
}

# function to help parallelize data reading
ReadAFile = function(myFilename, symbol.name) {
  foo = EnrichOptionsQuotes(
          OptionQuotesCsv(
            paste(
              paste(symbol.name, "-new", sep=""),
              "/",
              myFilename, sep="")))
  foo = subset(foo, Call.Put == "P")
  foo = foo[order(foo$Symbol),]
  return(foo)
}

################## LOAD DATA ##################
# redo data load with many files. filename format is mandatory and 
# only works for 3 letter symbols (e.g. SYM): SYMYYYYMMDDHHMM.csv
#file.names     = list.files(path=my.sym, pattern="/*.csv") # old data (2016)
file.names     = list.files(path=paste(my.sym, "-new", sep=""), 
                            pattern="/*.csv") # new data (2018)
my.data        = mclapply(file.names, ReadAFile, symbol.name=my.sym, mc.cores=4)

# for (i in 1:length(file.names)) {
#   my.data[[i]] = EnrichOptionsQuotes(
#                   OptionQuotesCsv(
#                     paste(
#                       paste(my.sym, "-new", sep=""),
#                       "/",
#                       file.names[i], sep=""))
#   )
#   # 6-4-2 only uses puts. Shave some time off of the sort.
#   my.data[[i]] = subset(my.data[[i]], Call.Put == "P")
#   # Sort to be safe (safe?!? TODO figure out what's truly 'safe')
#   my.data[[i]] = my.data[[i]][order(my.data[[i]]$Symbol),]
# }

names(my.data) = as.Date(substr(file.names, 4, 11), "%Y%m%d")

# Other trade keeping method:
#   1.  If there's an open trade, copy the current day's quote into the open 
#       positions object
#   2.  Final profit is max(dates open).mid.price - min(dates open).mid.price
#   3.  Can graph each position's P/L in the end, and all positions floating
#       is just the running sum of balance + open positions P/L
#   4.  Need to add the final P/L to total equity somehow otherwise you never
#       book profit.

################## SIMULATION START ##################
# Every day:
#   1.  Update current trade prices to today's quotes
#   2.  Decide if we should exit them
#   3a. If no exit, do nothing
#   3b. If exit, delete the open position. record P/L somehow
#   4.  Decide if we should make a new trade
#   5.  If we trade, add to today's trade list
#   6.  Copy all open trades to tomorrow
#   7.  Log portfolio stats to today's portfolio stat object

#for (kOisufThresh in ((0:4)*20)) { # OISUF threshold loop

# Create a stats object for every day, then rbind them all together later
# after you've completed the backtest to graph $ and stuff
stats = c("Delta", "Gamma",      "Theta",    "Vega",     "Rho", 
                   "Closed P/L", "Open P/L", "Days P/L", "Reg-T Req")
# this needs to copy the my.data object for dates
portfolio.stats           = rep(0, 9) 
dim(portfolio.stats)      = c(1, 9)
colnames(portfolio.stats) = stats

# 2010 2010-02-12 2010-12-31
# 2011 2011-01-03 2011-12-30
# 2012 2012-01-03 2012-12-31
# 2013 2013-01-02 2013-12-31
# 2014 2014-01-02 2014-12-31
# 2015 2015-01-02 2015-12-31
# 2016 2016-01-04 2016-12-29
# 2017 2017-01-03 2017-12-29
# 2018 2018-01-02 2018-12-20
sim.date.start = "2010-02-12" # earliest is "2010-02-12"
sim.date.end   = "2018-12-20" # latest is "2018-12-20"
# Only run sim for dates given:
i.start = which(names(my.data) == sim.date.start)
i.end   = which(names(my.data) == sim.date.end) # max 2018-12-20
if (length(i.start) == 0 || length(i.end) == 0)
  stop(paste("One date is not a trading day:", sim.date.start, sim.date.end))

# keeping stats as the whole list for now
my.stats         = rep(list(portfolio.stats), length(my.data))
names(my.stats)  = names(my.data)

total.trades   = 0
num.inc.quotes = 0
open.trades    = list()
closed.trades  = list()

# Data before 2010-02-10 has different symbol names.
# 2015-09-08 had fucked up price on SEP 1300 calls, fixed by hand in csv
# 2013-02-08 has fucked up prices on APR 1515 puts, set to 33.50/33.65 by hand
# main backtest loop for now, operates on days
# symbols change names at i=29, delta messed up at i < 88
#profvis({
#system.time(
for (i in i.start:i.end) { # 27 starts new symbol names 2010-02-11
  #browser()
  #if (i > 136) browser()
  # steps 1 through 3b, operates on open trades
  if (median(diff(sort(unique(my.data[[i]]$Strike.Price)))) > 5) {
    stop(paste("Quote for", names(my.data)[i], "lacks 5-point wide strikes"))
  }
  if (length(open.trades) > 0) { # don't run w/ 0 trades open
    # create indicies of today's trades to exit
    to.exit = rep(FALSE, length(open.trades)) # ignore quote df w/ FALSE
    for (j in 1:length(open.trades)) {
      # if you ever close a trade before getting done with this loop
      # j will be ahead of the number of entries. check that.
      if (j > length(open.trades)) {
        cat("something went wrong")
        break
      }
      # update open trades with today's quotes
      # if your quoted symbols aren't there, raise error
      to.update = my.data[[i]]$Symbol %in% open.trades[[j]]$Symbol
      num.true  = length(to.update[to.update == TRUE])
      if (num.true <= 3) {
        if (num.true < 3) {
          #browser()
          num.inc.quotes = num.inc.quotes + 1
          symbols.to.update    = my.data[[i]][to.update,]$Symbol
          tmp.update           = subset(open.trades[[j]], 
                                        Symbol %in% symbols.to.update)
          # TODO how do you know what order this is in?
          tmp.update$mid.price = my.data[[i]][to.update,]$mid.price
          tmp.update$Delta     = my.data[[i]][to.update,]$Delta
          tmp.update$Gamma     = my.data[[i]][to.update,]$Gamma
          tmp.update$Vega      = my.data[[i]][to.update,]$Vega
          tmp.update$Theta     = my.data[[i]][to.update,]$Theta
          tmp.update$Rho       = my.data[[i]][to.update,]$Rho
          open.trades[[j]]     = rbind(tmp.update, 
                                       subset(open.trades[[j]], 
                                              !(Symbol %in% symbols.to.update)))
          open.trades[[j]] = open.trades[[j]][order(
                                open.trades[[j]]$Symbol),]
        } else {
            # check for too big of price differences
            final        = my.data[[i]][to.update,]$mid.price
            initial      = open.trades[[j]]$mid.price
            whack.prices = abs(((final-initial)/initial)) > rep(0.75, 3)
            check.prices = F # (F)/T = (don't) stop for big price changes
            if (any(whack.prices) && check.prices) {
              print(c(names(my.data[i]), 
                      "Price to update very far off from current price."),
                    whack.prices)
            } else {
              # TODO how do you know what order this is in?
              open.trades[[j]]$mid.price = my.data[[i]][to.update,]$mid.price
              open.trades[[j]]$Delta     = my.data[[i]][to.update,]$Delta
              open.trades[[j]]$Gamma     = my.data[[i]][to.update,]$Gamma
              open.trades[[j]]$Vega      = my.data[[i]][to.update,]$Vega
              open.trades[[j]]$Theta     = my.data[[i]][to.update,]$Theta
              open.trades[[j]]$Rho       = my.data[[i]][to.update,]$Rho
            }
        }
        # exit if 90% profit, 200% loss, 5 days till exp, short strike touch
        if (ShouldExit(open.trades[[j]], names(my.data)[i]))
          to.exit[j] = TRUE
        # to.exit is now something like c(TRUE, TRUE, TRUE, FALSE, FALSE)
      } else {
        stop(paste("Quote for", 
                   names(my.data)[i], 
                   "does not contain entries for open trades"))
      }
    }
    # record open P/L as closed P/L for today for those indicies set TRUE
    # use cumsum() later to build an equity curve
    if (length(to.exit[to.exit == TRUE]) > 0) {
      # record profit as closed
      #browser()
      my.stats[[i]][6] = sum(unlist(lapply(open.trades[to.exit], 
                                           FloatingProfit)))
      # add to trade log
      for (k in 1:length(open.trades[to.exit])) {
        #if (OKExit(open.trades[to.exit][[k]]$mid.price)) {
        #  print("Zero price quotes on exit", as.Date(names(my.data)[i]))
        #}
        reason = ExitReason(open.trades[to.exit][[k]], 
                            names(my.data)[i])
        entry.date = open.trades[to.exit][[k]][1,23]
        oisuf.at.entry = as.numeric(oisuf.values[entry.date,])
        closed.trades[[length(closed.trades)+1]] = 
          cbind(oisuf.at.entry,
                TradeSummary(open.trades[to.exit][[k]],
                             as.Date(names(my.data)[i])),
                reason)
      }
      # close trades by setting those indicies to NULL
      open.trades[to.exit] = NULL
    }
  }
  
  # Step 4 and 5. Decide if we should make a new trade
  # in 1 TPS backtest, easy, you create new trade every day
  # in 1 TPX backtest, use ShouldEnter()
  # if FindCondor returns NULL, this shouldn't do anything (I think?)
  potential.bfly = FindBWB(my.data[[i]])
  # next check isn't in main check yet cause still working on what i want
  # to do about actual data being messed up
  if (!is.null(potential.bfly) && !OKPrices(potential.bfly))
    stop("Price too far from theoretical")
  if (global.mode == "1TPX" 
      && as.numeric(oisuf.values[names(my.data)[i]]) > kOisufThresh
      && ShouldEnter(open.trades, my.data[[i]])
      && !is.null(potential.bfly)
      && nrow(potential.bfly) == 3
      && FindDTR(potential.bfly) < 0.5) {
        open.trades[[length(open.trades) + 1]] = potential.bfly
        total.trades = total.trades + 1
  } else if (global.mode == "1TPS"
             && !is.null(potential.bfly)
             && as.numeric(oisuf.values[names(my.data)[i]]) > kOisufThresh
             && nrow(potential.bfly) == 3
             && FindDTR(potential.bfly) < 0.5) {
                open.trades[[length(open.trades) + 1]] = potential.bfly
                total.trades = total.trades + 1
  }
  
  # Record the floating profit
  my.stats[[i]][7] = sum(unlist(lapply(open.trades, FloatingProfit)))
  # Something something portfolio stats something
  
}
#) # end system.time
#}) # end profvis
# Easier view of stats manually after the fact
df.stats = data.frame(
            matrix(
              unlist(my.stats[-length(my.stats)]), 
              nrow=length(my.stats)-1, 
              byrow=T, 
              dimnames=list(names(my.stats)[-length(my.stats)], 
                            colnames(portfolio.stats))))
df.stats.sub = df.stats[i.start:i,]
# Plot the floating and closed profit
plot(i.start:i,
     kInitBalance + cumsum(df.stats.sub$Closed.P.L) + df.stats.sub$Open.P.L,
     type='l',
     col='red')
lines(i.start:i,
      kInitBalance + cumsum(df.stats.sub$Closed.P.L))

df.closed.trades = do.call('rbind', closed.trades)

# Profit factor
sum.wins   = sum(subset(df.stats.sub, Closed.P.L > 0)$Closed.P.L)
sum.losses = sum(subset(df.stats.sub, Closed.P.L < 0)$Closed.P.L)
if (sum.losses == 0) {
  print("Profit factor: inf.")
} else {
  print(paste("From: ", sim.date.start, " To: ", sim.date.end,  
              " OISUF level: ", kOisufThresh))
  print(paste("N trades: ", total.trades, 
              " Profit factor: ", abs(sum.wins / sum.losses), sep=""))
  print(paste("# contracts:", kContracts,
              " Max loss:", kMaxLoss))
  print(data.frame(summary(df.closed.trades$reason)))
}

#} # OISUF threshold loop


x.stats = as.xts(df.stats.sub)
perf = kInitBalance + cumsum(x.stats$Closed.P.L) + x.stats$Open.P.L
charts.PerformanceSummary(ROC(perf))

for (l in 2010:2018) {
  jan.1.bal  = as.numeric(first(perf[paste(l)], "1 day"))
  dec.31.bal = as.numeric(last(perf[paste(l)], "1 day"))
  print(paste(l, (dec.31.bal - jan.1.bal) / jan.1.bal))
}

closed.balance = cumsum(x.stats$Closed.P.L)
print(paste("2010-2018 CAGR: ",
      percent(
        (as.double(kInitBalance + closed.balance[nrow(closed.balance),])
         /
           kInitBalance)^(1/(2018-2010))-1))) # hardcoded dates :(

# loss analysis
sort(df.closed.trades[df.closed.trades$close.profit < (-1*kMaxLoss),]$close.profit)
mean(df.closed.trades[df.closed.trades$close.profit < (-1*kMaxLoss),]$close.profit)
length(df.closed.trades[df.closed.trades$close.profit < (-1*kMaxLoss),]$close.profit)
n.wins   = length(unlist(subset(df.closed.trades, close.profit > 0, close.profit)))
n.losses = length(unlist(subset(df.closed.trades, close.profit < 0, close.profit)))
avg.win  = mean(unlist(subset(df.closed.trades, close.profit > 0, close.profit)))
avg.loss = mean(unlist(subset(df.closed.trades, close.profit < 0, close.profit)))
print(paste("Avg win:", sprintf("%.2f", avg.win)))
print(paste("Avg loss:", sprintf("%.2f", avg.loss)))
print(paste("W/L Ratio:", sprintf("%.2f", abs(avg.win/avg.loss))))
print(paste("Win %:", sprintf("%.1f%%", 100*n.wins/(n.wins+n.losses))))

lagged.returns = Delt(perf, type="arithmetic")
# top10 = function(my.df) {
#   print(sort(my.df$Percent.O.U, decreasing=T)[1:10])
# }