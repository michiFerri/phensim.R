
keyInput <- function() {
  out <- FALSE

    while(!out){
      keyInput <- readline(prompt = "Please type your APIs token and press Enter (or just press Enter to set it up later): ")
      if(!is.null(keyInput) && keyInput!= "" ){
        message(paste("The user input is ",keyInput))
        if(readline(prompt="Confirm it (y/n)? ") == "y"){
          if(checkValidToken(keyInput)){
            out <- TRUE
          }else{
            message("Invalid API token.\nMake sure to use a valid token. ")
            out <- TRUE
            message(paste("No API token set up \nNOTE: YOU NEED TO SET UP A VALID TOKEN TO START USING THE PACKAGE (check '?setToken')"))
            keyInput <- ""
          }

        } else {
          message(paste("User input aborted."))
        }
      }else {
        out <- TRUE
        message(paste("No API token set up \nNOTE: YOU NEED TO SET UP A VALID TOKEN TO START USING THE PACKAGE (check '?setToken')"))
      }
    }

  return (keyInput)
}


#' APIs token setup procedure
#'
#' Starts an interactive procedure to set up your APIs token (required to use other services)

#'@export
setToken <- function(){
  Sys.setenv(API_TOKEN=keyInput())
}

checkValidToken <- function(token_in){
  url <- "https://phensim.tech/api/v1/ping"
  accept <- "application/json"
  auth <- paste("Bearer", token_in, sep=" ", collapse=NULL)

  apiCall <- httr::GET(url, httr::add_headers("Accept"=accept,"Authorization"=auth))

  status_code <- httr::status_code(apiCall)

  if(status_code == 200){
    return (TRUE)
  }else{
    return (FALSE)
  }

}
