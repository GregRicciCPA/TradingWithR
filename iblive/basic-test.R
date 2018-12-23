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
my.strike = 2435
my.expiry = "20190117"
# single put at strike and expiration
my.opts   = twsOption(local = "", right = "P", symbol = my.symbol, 
                      strike = my.strike, expiry = my.expiry)
# all puts and calls and strikes at one expiration
spx.full = twsOption(local = "", right = "", symbol = my.symbol, 
                     expiry = my.expiry)

if (!is.twsContract(my.opts))
  stop("Error: not a proper TWS contract")
#print(reqMktData(tws, spx, verbose=TRUE))
spx.quotes = reqContractDetails(tws, my.opts) # 150MB of data or 130 
# more data
spx.full.quotes = reqContractDetails(tws, spx.full)

# get a quote chain for a single option
first.option = spx.quotes[[1]] # specific expiration, calls only

# twsCALLBACK calls processMsg()
# processMsg() calls ???
# can you just pass eventTickOption = eWrapper.opt$tickData to reqMktData?
# option.deets = reqMktData(tws, first.option$contract) # never exits
# TWS Message: 2 1 322 Error processing request:-'bR' : cause - Duplicate ticker id 
#option.deets = reqMktData(tws, 
#                          first.option$contract,
#                          eventTickOption = eWrapper.opt$tickPrice)
#   object of type 'closure' is not subsettable
option.deets = reqMktData(tws, 
                          first.option$contract,
                          eventWrapper = eWrapper.opt(1),
                          CALLBACK = snapShot)
#   object of type 'closure' is not subsettable
option.deets = reqMktData(tws, 
                          first.option$contract,
                          CALLBACK = snapShot(tws, eWrapper=eWrapper.opt))
# works but wrong data points
reqMktData(tws, first.option, snapshot = TRUE)

# works but not offline. first 20 options in the chain
reqMktData(tws, 
           spx.full.quotes[1:20], 
           eventWrapper = eWrapper.data(20),
           CALLBACK = so.snapShot) #named 'so' from stackoverflow
# Historical data API endpoints for options doesn't work

# Close connection
twsDisconnect(tws)

