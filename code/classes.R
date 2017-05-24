Team <- function(name, gmSchDf, j=100, 
                 prevGmDate=NA, nextGmDate=NA, 
                 offRnk=NA, defRnk=NA) {
  
  ## Get the environment for this
  ## instance of the function.
  thisEnv <- environment()
  
  ## Initialize
  name <- name
  gmSchDf <- gmSchDf
  j <- j
  prevGmDate <- prevGmDate
  nextGmDate <- nextGmDate
  offRnk <- offRnk
  defRnk <- defRnk
  gmDates <- sort(gmSchDf$date[gmSchDf$team==name])

  ## Create the list used to represent an
  ## object for this class
  me <- list(
    
    ## Define the environment where this list is defined so
    ## that I can refer to it later.
    thisEnv = thisEnv,
    
    ## Define the accessors for the data fields.
    getEnv = function() {
      return(get("thisEnv", thisEnv))
    },
    
    getName = function() {
      return(get("name", thisEnv))
    },
    
    setName = function(value) {
      return(assign("name", value, thisEnv))
    },
    
    getJ = function() {
      return(get("j", thisEnv))
    },
    
    setJ = function(value) {
      return(assign("j", value, thisEnv))
    },
    
    getLastGmDate = function() {
      return(get("lastGmDate", thisEnv))
    },
    
    setLastGmDate = function(value) {
      return(assign("lastGmDate", value, thisEnv))
    },
    
    getPrevGmDate = function(date) {
      date <- as.Date(date)
      gmDates <- get('gmDates', thisEnv)
      if (date %in% gmDates) {
        return(gmDates[which(gmDates==date) - 1])
      } else {
        return(gmDates[max(which(gmDates < date))])
      }
    },

    getNextGmDate = function(date) {
      date <- as.Date(date)
      gmDates <- get('gmDates', thisEnv)
      if (date %in% gmDates) {
        return(gmDates[which(gmDates==date) + 1])
      } else {
        return(gmDates[max(which(gmDates < date)) + 1])
      }
    }
  )
  
  ## Define the value of the list within the current environment.
  assign('this', me, envir=thisEnv)
  
  ## Set the name for the class
  class(me) <- append(class(me), "Team")
  
  ## return
  return(me)
}