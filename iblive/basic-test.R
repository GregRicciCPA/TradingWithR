library('IBrokers')
setwd('iblive')
source('ibrokers-includes.R')
source('eWrapper.opt.R')

# Connect to running terminal
live.port  = 7497
paper.port = 7496

tws = twsConnect(port = live.port) # 7497 = live, 7496 = paper

# Check if connected
if (!isConnected(tws))
  stop("Error: not connected to TWS")

# Meat here

# Request and view account details
ac = reqAccountUpdates(tws)
print(head(ac))

# Print out only the positions from the ac object
positions = twsPortfolioValue(ac)
if (is.null(positions)) {
  print("No positions")
} else {
  print(head(positions))
}

# Real time stock info
# Set security
#spx = twsIndex(symbol = "SPX", exch = "SMART") # not for options?
my.symbol = "SPX"
my.strike = 2635
my.expiry = "20181220"
my.opts   = twsOption(local = "", right = "", symbol = my.symbol, 
                      strike = my.strike, expiry = my.expiry)

if (!is.twsContract(my.opts))
  stop("Error: not a proper TWS contract")
#other.security = twsEquity("AAPL", "SMART")
#print(reqMktData(tws, spx, verbose=TRUE))
spx.quotes = reqContractDetails(tws, my.opts) # 150MB of data or 130 
# get a quote chain for a single option
first.option = spx.quotes[[1]] # specific expiration, calls only

# twsCALLBACK calls processMsg()
# processMsg() calls ???
# can you just pass eventTickOption = eWrapper.opt$tickData to reqMktData?
option.deets = reqMktData(tws, 
                          first.option$contract) # never exits
# TWS Message: 2 1 322 Error processing request:-'bR' : cause - Duplicate ticker id 
#option.deets = reqMktData(tws, 
#                          first.option$contract,
#                          eventTickOption = eWrapper.opt$tickPrice)
#   object of type 'closure' is not subsettable
option.deets = reqMktData(tws, 
                          first.option$contract,
                          eventWrapper = eWrapper.opt(1),
                          CALLBACK = snapShot)

# Historical data API endpoints for options doesn't work

# Close connection
twsDisconnect(tws)

