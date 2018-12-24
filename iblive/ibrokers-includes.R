snapShot = function (twsCon, eWrapper, timestamp, file, playback = 1, ...) 
{ 
  if (missing(eWrapper)) 
    eWrapper <- eWrapper() 
  # pretty sure you need to actually call it myData if using eWrapper.opt
  names(eWrapper$.Data$myData) <- eWrapper$.Data$symbols # label the rows
  con <- twsCon[[1]] 
  if (inherits(twsCon, "twsPlayback")) { 
    sys.time <- NULL 
    while (TRUE) { 
      if (!is.null(timestamp)) { 
        last.time <- sys.time 
        sys.time <- as.POSIXct(strptime(paste(readBin(con, 
                                                      character(), 2), 
                                              collapse = " "), 
                                        timestamp))
        if (!is.null(last.time)) { 
          Sys.sleep((sys.time - last.time) * playback) 
        } 
        curMsg <- .Internal(readBin(con, "character", 
                                    1L, NA_integer_, TRUE, FALSE)) 
        if (length(curMsg) < 1) 
          next 
        processMsg(curMsg, con, eWrapper, format(sys.time, 
                                                 timestamp), file, ...) 
      } 
      else { 
        curMsg <- readBin(con, character(), 1) 
        if (length(curMsg) < 1) 
          next 
        processMsg(curMsg, con, eWrapper, timestamp, 
                   file, ...) 
        if (curMsg == .twsIncomingMSG$REAL_TIME_BARS) 
          Sys.sleep(5 * playback) 
      } 
    } 
  } 
  else { 
    while (TRUE) { 
      socketSelect(list(con), FALSE, NULL) 
      curMsg <- .Internal(readBin(con, "character", 1L, 
                                  NA_integer_, TRUE, FALSE)) 
      if (!is.null(timestamp)) { 
        processMsg(curMsg, con, eWrapper, format(Sys.time(), 
                                                 timestamp), file, ...) 
      } 
      else { 
        processMsg(curMsg, con, eWrapper, timestamp, 
                   file, ...) 
      } # pretty sure you need to actually call it myData if using eWrapper.opt
      if (!any(sapply(eWrapper$.Data$myData, is.na))) 
        return(do.call(rbind, lapply(eWrapper$.Data$myData, 
                                     as.data.frame))) 
    } 
  } 
} 

# renamed to myCallback from so.snapShot cause that conflates things
myCallback <- function (twsCon, eWrapper, timestamp, file, playback = 1, ...)
{
  if (missing(eWrapper))
    eWrapper <- eWrapper()
  names(eWrapper$.Data$data) <- eWrapper$.Data$symbols
  con <- twsCon[[1]]
  if (inherits(twsCon, "twsPlayback")) {
    sys.time <- NULL
    while (TRUE) {
      if (!is.null(timestamp)) {
        last.time <- sys.time
        sys.time <- as.POSIXct(strptime(paste(readBin(con,
                                                      character(), 2), collapse = " "), timestamp))
        if (!is.null(last.time)) {
          Sys.sleep((sys.time - last.time) * playback)
        }
        curMsg <- .Internal(readBin(con, "character",
                                    1L, NA_integer_, TRUE, FALSE))
        if (length(curMsg) < 1)
          next
        processMsg(curMsg, con, eWrapper, format(sys.time,
                                                 timestamp), file, ...)
      }
      else {
        curMsg <- readBin(con, character(), 1)
        if (length(curMsg) < 1)
          next
        processMsg(curMsg, con, eWrapper, timestamp,
                   file, ...)
        if (curMsg == .twsIncomingMSG$REAL_TIME_BARS)
          Sys.sleep(5 * playback)
      }
    }
  }
  else {
    while (TRUE) {
      socketSelect(list(con), FALSE, NULL)
      curMsg <- .Internal(readBin(con, "character", 1L,
                                  NA_integer_, TRUE, FALSE))
      if (!is.null(timestamp)) {
        processMsg(curMsg, con, eWrapper, format(Sys.time(),
                                                 timestamp), file, ...)
      }
      else {
        processMsg(curMsg, con, eWrapper, timestamp,
                   file, ...)
      }
      if (!any(sapply(eWrapper$.Data$data, is.na)))
        return(do.call(rbind, lapply(eWrapper$.Data$data,
                                     as.data.frame)))
    }
  }
} 