
#infoJob
printInfo <- function(data,id) {

  id_list <- list("id", "name", "readable_status","organism","created_at","updated_at")
  id_list_2 <- list("fast","fdr","epsilon","seed","reactome","enrichMiRNAs", "miRNAsEvidence")

  cat(paste("Printing info for job #",id," ...\n","\n",sep=""))

  for(item in id_list){
    cat(paste("  ",item,":",data$data[[item]],"\n"))
  }

  for(item in id_list_2){
    cat(paste("  ",item,":",data$data$parameters[[item]],"\n"))
  }
}

checkToken <- function(){
  if(is.null(Sys.getenv("API_TOKEN")) || Sys.getenv("API_TOKEN")=="") {
    return (FALSE)
  }else{
    return (TRUE)
  }
}

userInput <- function(dataType, prt){

  confirm <- FALSE;

  while(!confirm){
    if(dataType == "file"){
      message(paste(prt))
      if(readline(prompt="A file is required for this field. Do you want to select one? (y/n): ") == "y"){
        file <- httr::upload_file(choose.files(caption=prt))
        if(is.list(file)){
          confirm = TRUE
          return (file)
        }else{
          message("An error occurred during file uploading.\n")
        }
      }else{
        message("Missing file (DEFAULT value will be used if possible).\n")
        confirm=TRUE
        return ("")
      }

    }else{
      message(paste(prt))
      userI<- readline()
      if(userI == ""){
        message(paste("Simulation field left blank (DEFAULT value will be used).\n"))
        return ("")
      }else {
        #message()
        if(readline(prompt=paste("Entered value is: ", userI,". Confirm it (y/n)? ")) == "y"){
          confirm <- TRUE
        } else {
          message(paste("User input aborted."))
        }
      }
    }
  }

  #STRING BOOLEAN DOUBLE

  if(dataType == "boolean"){
    return (as.integer(userI))
  } else if(dataType == "string"){
    return (userI)
  } else if(dataType == "double"){
    return (as.double(userI))
  }

  return (FALSE)
}

#infoJob
statusResponse <- function(apiCall, job_id){
  status_code <- httr::status_code(apiCall)

  if(status_code == 200){
    data <- jsonlite::fromJSON(rawToChar(apiCall$content))
    printInfo(data,job_id)
  }else if(status_code == 404){
    message(paste("No query results for model", job_id, ". \nBe sure you entered a valid job ID (?listJobs)"))
  }else{
    message(paste(httr::http_status(apiCall)$message, ".\nMake sure you submitted a valid job_id (?listJobs())"))
  }
}

#submitJob
submitResponse <- function(apiCall){
  status_code <- httr::status_code(apiCall)

  if(status_code == 201){
    message("Succesfully submitted the simulation!\n")
  }else{
    message(httr::http_status(apiCall)$message)
  }

}

#submitJob
new_body <- function(lista1){
  lista2 <- list()
  for(attr in names(lista1)){
    if(lista1[attr] != ""){
      lista2[attr] <- lista1[attr]
    }
  }
  return (lista2)
}

