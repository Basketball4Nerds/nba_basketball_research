Team <- function(name, gm_sch_df, j=100, 
                 prev_gm_date=NA, next_gm_date=NA, 
                 o_rnk=NA, d_rnk=NA) {
  
  ## Get the environment for this
  ## instance of the function.
  this_env <- environment()
  
  ## Initialize
  name <- name
  gm_sch_df <- gm_sch_df
  j <- j
  prev_gm_date <- prev_gm_date
  next_gm_date <- next_gm_date
  o_rnk <- o_rnk
  d_rnk <- d_rnk
  gm_dates <- sort(gm_sch_df$date[gm_sch_df$team==name])

  ## Create the list used to represent an
  ## object for this class
  me <- list(
    
    ## Define the environment where this list is defined so
    ## that I can refer to it later.
    this_env = this_env,
    
    ## Define the accessors for the data fields.
    get_env = function() {
      return(get("this_env", this_env))
    },
    
    get_name = function() {
      return(get("name", this_env))
    },
    
    set_name = function(value) {
      return(assign("name", value, this_env))
    },
    
    get_j = function() {
      return(get("j", this_env))
    },
    
    set_j = function(value) {
      return(assign("j", value, this_env))
    },
    
    get_last_gm_date = function() {
      return(get("last_gm_date", this_env))
    },
    
    set_last_gm_date = function(value) {
      return(assign("last_gm_date", value, this_env))
    },
    
    get_prev_gm_date = function(date) {
      date <- as.Date(date)
      gm_dates <- get('gm_dates', this_env)
      if (date %in% gm_dates) {
        return(gm_dates[which(gm_dates==date) - 1])
      } else {
        return(gm_dates[max(which(gm_dates < date))])
      }
    },

    get_next_gm_date = function(date) {
      date <- as.Date(date)
      gm_dates <- get('gm_dates', this_env)
      if (date %in% gm_dates) {
        return(gm_dates[which(gm_dates==date) + 1])
      } else {
        return(gm_dates[max(which(gm_dates < date)) + 1])
      }
    }
  )
  
  ## Define the value of the list within the current environment.
  assign('this', me, envir=this_env)
  
  ## Set the name for the class
  class(me) <- append(class(me), "Team")
  
  ## return
  return(me)
}