# eWrapper.opt is a event wrapper that
# updates an in memory data base of values
# upon new input from the TWS
#
# This is only implemented for options reqMktData callbacks
# copied from eWrapper.last

# eWrapper.snapshot stores all the data points in the environemnt, which
#   isn't useful for sending in multiple symbols
# eWrapper.data (and eWrapper.opt) stores data in an xts matrix for each symbol
#   and gets all the data points for each symbol too

# do i need to do something with eW$tickSnapshotEnd?

eWrapper.opt <- function(n) {
  # internally updated data
  #  .data. <- character(8)
  #  
  #  get.data <- function() return(.data.)
  #
  eW <- eWrapper(NULL)  # use basic template
  # 0  1 2     3   4 5 6 7 8   9  10  11 12  13
  # IV D price pvd G V T U LIV LD BIV BD AIV AD
  eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_,17),ncol=17),0),
                                            .Dimnames=list(NULL,
                                                           c("impVol","delta",
                                                             "modelPrice","pvDiv",
                                                             "gamma","vega","theta",
                                                             "underlying",
                                                             "lastIV","lastDelta",
                                                             "bidIV","bidDelta",
                                                             "askIV","askDelta",
                                                             "bidPrice","askPrice",
                                                             "lastPrice")))),n))
  # THIS IS A FUNCTION. processMsg() calls this and should for options too
  eW$tickPrice <- function(curMsg, msg, timestamp, file, ...) {
    tickType = msg[3]
    msg <- as.numeric(msg)
    id <- msg[2] #as.numeric(msg[2])
    data <- eW$get.Data("data") #[[1]]  # list position of symbol (by id == msg[2])
    data[[1]] <- rbind(data[[1]],.xts(matrix(rep(NA_real_,17),nc=17), Sys.time()))
    nr.data <- NROW(data[[id]])
    #data[[id]][1] <- as.numeric(Sys.time()) #timestamp
    # 0  1 2     3   4 5 6 7 8   9  10  11 12  13
    # IV D price pvd G V T U LIV LD BIV BD AIV AD
    # begin paste from e_tick_option()
    if(tickType == .twsTickType$BID_OPTION) { #10
      # msg[4] is IV computed on bid price
      # msg[5] is Delta computed on bid price
      #cat('bidOption:',msg[4],msg[5],'\n',file=file,append=TRUE)
      data[[id]][10:11] = msg[4:5]
      #data[[id]][11] = msg[5]
      cat("assigned a bid_option")
    } else
    if(tickType == .twsTickType$ASK_OPTION) { #11
      # msg[4] is IV computed on ask price
      # msg[5] is Delta computed on ask price
      #cat('askOption:',msg[4],msg[5],'\n',file=file,append=TRUE)
      data[[id]][12:13] = msg[4:5]
      #data[[id]][13] = msg[5]
      cat("assigned an ask_option")
    } else
    if(tickType == .twsTickType$LAST_OPTION) { #12
      # msg[4] is IV computed on last price
      # msg[5] is Delta computed on last price
      #cat('lastOption:',msg[4],msg[5],'\n',file=file,append=TRUE)
      data[[id]][8:9] = msg[4:5]
      #data[[id]][9] = msg[5]
      cat("assigned a last_option")
    } else
    if(tickType == .twsTickType$MODEL_OPTION) { #13
      #cat('modelOption: impVol: ',msg[4],' delta:',msg[5],
      #    ' modelPrice: ',msg[6],' pvDiv: ',msg[7],
      #    ' gamma: ',msg[8],' vega: ',msg[9],
      #    ' theta: ',msg[10],' undPrice: ',msg[11],'\n',file=file,append=TRUE)
      data[[id]][0:7] = msg[4:11]
      #data[[id]][1] = msg[5]
      #data[[id]][2] = msg[6]
      #data[[id]][3] = msg[7]
      #data[[id]][4] = msg[8]
      #data[[id]][5] = msg[9]
      #data[[id]][6] = msg[10]
      #data[[id]][7] = msg[11]
      cat("assigned a model_option")
    } else 
    # end paste from e_tick_option()
    # TODO 'close' and 'bidprice' and 'askprice' tick types
    if(tickType == .twsTickType$BID) {
      data[[id]][14] = msg[4]
    } else
    if(tickType == .twsTickType$ASK) {
      data[[id]][15] = msg[4]
    } else
    if(tickType == .twsTickType$LAST) {
      data[[id]][16] = msg[4]
    }
    cat("trying to put data back in eW environment")
    eW$assign.Data("data", data)
    c(curMsg, msg)
  }
}