# Test reading in text files in parallel
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
OptionQuotesCsv = function(options.file) {
  my.data       = read.csv(options.file)
  if (length(grep("Index", my.data$Description)) > 1) {
    stop(paste("Data error: Input file ", options.file," had multiple exports",
               sep=""))
  } else {
    return(my.data[-1,]) # first row is underlying. use price.quotes for that
  }
}
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
# Old way: user      system  elapsed 
#          168.219   4.035   175.478 
file.names     = list.files(path="SPX-new", 
                            pattern="/*.csv") # new data (2018)
my.data        = rep(list(), length(file.names))

system.time(
  for (i in 1:length(file.names)) {
    my.data[[i]] = EnrichOptionsQuotes(
                    OptionQuotesCsv(
                      paste(
                        paste("SPX-new", sep=""),
                        "/",
                        file.names[i], sep=""))
    )
    # 6-4-2 only uses puts. Shave some time off of the sort.
    my.data[[i]] = subset(my.data[[i]], Call.Put == "P")
    # Sort to be safe (safe?!? TODO figure out what's truly 'safe')
    my.data[[i]] = my.data[[i]][order(my.data[[i]]$Symbol),]
  }
)

names(my.data) = as.Date(substr(file.names, 4, 11), "%Y%m%d")

# New way:

ReadAFile = function(myFilename) {
  foo = EnrichOptionsQuotes(OptionQuotesCsv(paste("SPX-new",
                                            "/",
                                            myFilename, sep="")))
  foo = subset(foo, Call.Put == "P")
  foo = foo[order(foo$Symbol),]
  return(foo)
}

library('parallel')
# This one only uses 2 cores
# results: 
# user      system  elapsed 
# 100.055   5.445   111.788 
system.time(mclapply(file.names, ReadAFile))
# try to use 4 cores
# results: 
# user      system  elapsed 
# 164.355   6.646   85.630
system.time(mclapply(file.names, ReadAFile, mc.cores=4))

# so old way = 174 seconds, 2 cores = 112 seconds, 4 cores = 85 seconds
# 2 cores = save 60 seconds (34% speedup)
# 4 cores = save 90 seconds vs baseline (51% speedup)
# 4 cores = save 30 seconds vs 2 cores (24% decrease)

names(my.data) = as.Date(substr(file.names, 4, 11), "%Y%m%d")
