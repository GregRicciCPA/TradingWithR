library('IBrokers')

# Connect to running terminal
tws = twsConnect(port = 7497) # 7496 = live, 7497 = paper

# Check if connected
if (!isConnected(tws))
  stop("Error: not connected to TWS")

# Meat here

# Request and view account details
ac = reqAccountUpdates(tws)
#print(head(ac))

# Print out only the positions from the ac object
positions = twsPortfolioValue(ac)
#print(positions)

# Real time stock info
security = twsSTK("AAPL")
if (!is.twsContract(security))
  stop("Error: not a proper TWS contract")
other.security = twsEquity("AAPL", "SMART")
print(reqMktData(tws, security))

# Close connection
twsDisconnect(tws)