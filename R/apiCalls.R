#'Lists all jobs available to the current user
#'
#'Lists all jobs available to the user at that moment, printing the jobs' IDs, names and current states \cr
#'For more details check PHENSIM docs at \url{https://phensim.tech/docs/api}
#' @return Printed info
#'
#'@export
listJobs <- function(){
  if(checkToken()){
    auth <- paste("Bearer",Sys.getenv("API_TOKEN"), sep=" ", collapse=NULL)
    url <-"https://phensim.tech/api/v1/simulations"

    apiCall <- httr::GET(url, httr::add_headers("Accept"="application/json", "Authorization"=auth))

    data <- jsonlite::fromJSON(rawToChar(apiCall$content))
    n_jobs <- dim(data[[1]])[1]

    if(!is.null(n_jobs) && n_jobs>0){
      cat(paste("Found a total of ",n_jobs," job(s)...","\n",sep=""))
      cat("   Job_Id  Job_Name  Job_Status \n")
      for (i in 1:n_jobs){
        cat(paste("#",i, sep=""),paste(" ",data[[1]]$id[i],as.character(data[[1]]$name[i]),as.character(data[[1]]$readable_status[i]),"\n",sep="  "))
      }
    } else {
      message(paste("No jobs found. Submit one to see it listed (?submitJob)","\n",sep=""))
    }

  }else {
    message(paste("No API token set up \nNOTE: YOU NEED TO SET UP A VALID TOKEN TO START USING THE PACKAGE (check '?setToken')"))
  }
}


#'Interactive procedure to submit a new simulation
#'
#'Starts an interactive procedure to submit a new simulation job in PHENSIM\cr
#'
#'The procedure guides the user through the steps required to fill all simulation fields.\cr
#'In each step the user is asked to insert a value or select a file to set that specific simulation parameter\cr
#'There are 17 parameters the user can set during the procedure:\cr
#'\itemize{
#' \item{\emph{name}}{ : the name that wil be given to the simulation;}
#' \item{\emph{organism}}{ : the organism on which perform the simulation (using KEGG accession number);}
#' \item{\emph{epsilon}}{ : a numeric value to determine non-expressed node;}
#' \item{\emph{seed}}{ : seed value for RNG to allow reproducibility;}
#' \item{\emph{fdr}}{ : string value for FDR alrithm, one of: BH, QV, LOC (default: BH);}
#' \item{\emph{reactome}}{ : boolean value to use reactome with KEGG;}
#' \item{\emph{fast}}{ : boolean value to use the fast method for the perturbation computation (default: true);}
#' \item{\emph{miRNAs}}{ : boolean value to enable MITHrIL miRNA enrichment feature;}
#' \item{\emph{miRNAEvidence}}{ : string value to select miRNA-target interactions (One of: STRONG, WEAK, PREDICTION; Default: STRONG);}
#' \item{\emph{simulationParametersFile}}{ : file of simulation parameters, formatted as a list of entity-regulationtype;}
#' \item{\emph{enrichmentDatabaseFile}}{ : file of enrichment database;}
#' \item{\emph{filter}}{ : filter for the enrichment database;}
#' \item{\emph{nonExpressedNodesFile}}{ : file of non-expressed nodes file;}
#' \item{\emph{knockoutNodesFile}}{ : file of knocked-out nodes;}
#' \item{\emph{customNodeTypesFile}}{ : file of custom node types;}
#' \item{\emph{customEdgeTypesFile}}{ : file of custom edge types;}
#' \item{\emph{customEdgeSubtypesFile}}{ : file of custom edge subtypes;}
#' }
#'
#' NOTE: all parameters are OPTIONAL (default value will be used eventually) except for \emph{name}, \emph{organism} and \emph{simulationParametersFile}, they have to be set otherwise the procedure will fail.\cr
#' Furthermore, the \emph{submit} parameter is automatically set to 1, so the simulation will be automatically submitted once the procedure in done.\cr
#'
#'
#'For more details check PHENSIM docs at \url{https://phensim.tech/docs/api}
#' @return Submitting a new simulation job returns the APIs call response with all details.
#'
#'@export

submitJob <- function(){
  if(checkToken()){
    url <-"https://phensim.tech/api/v1/simulations"
    auth <- paste("Bearer",Sys.getenv("API_TOKEN"), sep=" ", collapse=NULL)
    body_up <- list(name=userInput("string", "Insert simulation name: "),
                    organism=userInput("string","Insert organism name (KEGG number): "),
                    epsilon=userInput("double", "Insert epsilon value (OPTIONAL to determine non expressed nodes): "),
                    seed=userInput("double", "Insert seed value (OPTIONAL for RNG): "),
                    fdr=userInput("string", "Insert FDR value (One of: BH, QV, LOC; Default: BH): "),
                    reactome=userInput("boolean", "Enable REACTOME? (0/1): "),
                    fast=userInput("boolean", "Use fast method for perturbation computation? (0/1): "),
                    miRNAs=userInput("boolean", "Enable MITHrIL miRNA enrichment? (0/1): "),
                    miRNAEvidence=userInput("string", "Select miRNA-target interactions (One of: STRONG, WEAK, PREDICTION; Default: STRONG): "),
                    submit=1,
                    simulationParametersFile=userInput("file", "Select Parameters file ... "),
                    enrichmentDatabaseFile=userInput("file", "Select Enrichment Database file ... "),
                    filter=userInput("string", "Insert filter for enrichment database (OPTIONAL): "),
                    nonExpressedNodesFile=userInput("file", "Select Non-Expressed nodes file ... "),
                    knockoutNodesFile=userInput("file", "Select Knocked-Out nodes file ... "),
                    customNodeTypesFile=userInput("file","Select Custom Node Types file ..."),
                    customEdgeTypesFile=userInput("file","Select Custom Edge Types file ..."),
                    customEdgeSubtypesFile=userInput("file","Select Custom Edge Sub-types file ...") )

    #CONTROLLO INTEGRITA' RICHIESTA
    if(body_up["simulationParametersFile"] == "" || body_up["name"] == "" || body_up["organism"] == ""){

      message("A required simulation field is missing (make sure you entered simulation name, organism and parameters file (check ?submitJob).\n Procedure aborted.")
      return ()

    }else {

      body_final <- new_body(body_up)
      apiCall <- httr::POST(url, httr::add_headers("Accept"="multipart/form-data","Authorization"=auth),body=body_final, encode="multipart")
      submitResponse(apiCall)
      return (invisible(apiCall))

    }
  }else {
    message(paste("No API token set up \nNOTE: YOU NEED TO SET UP A VALID TOKEN TO START USING THE PACKAGE (check '?setToken')"))
  }
}

#'Get simulation job results.
#'
#'Let the user retrieve all results from a specified simulation job.
#'
#' @param job_id Simulation's id whose results the user wants to retrieve (as Integer)
#' @param type Type of results the user is interested in (String value, one of: "output", "pathway_output", "nodes_output"; Default: "output")
#'
#' @details
#'The user can choose from 3 different results, specified by the function parameter \emph{type}.\cr
#'\itemize{
#' \item{\emph{output}}{ : results will be given in "raw" format, an "all-in-one" solution that shows every evaluation from PHENSIM (Activity-Score, P-value, ...) for each node (biological element) from every pathway;}
#' \item{\emph{pathway_output}}{ : matrix-form auxilary details showing all random values generated during the simulation process, given by pathway;}
#' \item{\emph{nodes_output}}{ : matrix-form auxilary details showing all random values generated during the simulation process, given by nodes (biological elements);}
#' }
#' @examples df <- getResults(3777, type="pathway_output")
#' @return The function returns the requested results as data frame.
#'
#'@export

getResults <- function(job_id, type="output"){
  types <- list("output","pathway_output","nodes_output")
  if(!type %in% types){
    message("Error: type value must be one of output, pathway_output, nodes_output (other values are not accepted)")
    return ()
  }

  if(checkToken()){

    url <- paste("https://phensim.tech/api/v1/simulations/", job_id, "/download/", type, sep="")
    auth <- paste("Bearer",Sys.getenv("API_TOKEN"), sep=" ", collapse=NULL)

    apiCall <- httr::GET(url, httr::add_headers("Accept"="text/plain","Authorization"=auth))

    status_code <- httr::status_code(apiCall)
    if(status_code == 200){

      data <- rawToChar(apiCall$content)
      tmpFile <- tempfile(pattern=paste("tmp_sim_",type,"_",job_id, sep=""), tmpdir=tempdir(), fileext = ".tsv")
      writeLines(text=data, tmpFile)
      data_df<-as.data.frame(data.table::fread(tmpFile))
      message(paste(toupper(type), " results successfully retrieved\n", sep=""))
      return (data_df)

    }else{
      message(paste(httr::http_status(apiCall)$message, ".\nMake sure you submitted a valid job_id (?listJobs())"))
    }

  }else{
    message(paste("No API token set up \nNOTE: YOU NEED TO SET UP A VALID TOKEN TO START USING THE PACKAGE (check '?setToken')"))
  }

}


#'Prints more details for queried job
#'
#'Gives all details about the requested job id. Typical work-flow would be calling listJobs (check "?listJobs") and calling infoJob passing the job_id.\cr
#'The method gives information about creation data, job's status, id, name, organism, and simulation parameters.\cr
#'For more details check PHENSIM docs at \url{https://phensim.tech/docs/api}
#' @param job_id Simulation's id (as Integer)
#' @return Details for queried job
#' @examples
#' infoJob(691)
#'@export
infoJob <- function(job_id) {
  if(is.double(job_id) || is.integer(job_id)){
    if(checkToken()){
      auth <- paste("Bearer",Sys.getenv("API_TOKEN"), sep=" ", collapse=NULL)
      url <- paste("https://phensim.tech/api/v1/simulations/",job_id,sep="")
      apiCall <- httr::GET(url, httr::add_headers("Accept"="application/json", "Authorization"=auth))
      statusResponse(apiCall, job_id)
    }
  }else{
    message("infoJob(job_ID) requires a valid job ID (?infoJob)")
  }
}

#'Get job simulation parameters
#'
#'@description
#'Retrieve parameters for specific job simulation.
#'
#'@details
#'Requests and prints the parameters file for a specific job, as inputted during submission. This file should contain only the nodes' expression indication.\cr
#'This method requires the simulation's id, accessible from listJobs() details.\cr
#'Note: this method also gives the possibility to save locally the file.\cr
#'
#'For more details check PHENSIM docs at \url{https://phensim.tech/docs/api}
#' @param job_id Simulation's id whose parameters will be retrieved (as Integer);
#' @param save Set this flag to 1 to save job parameters as local file (default 0);
#' @param view This flag is set to 1 by default and will always print data (can be set to 0 otherwise);
#' @return This function prints the requested info and also returns the data as String (is var assignable).
#' @examples
#'jobParameters(691)
#'
#'@export
jobParameters <- function(job_id, save=0, view=1){
  if(checkToken()){

    if(is.double(job_id)){
      base <-"https://phensim.tech/api/v1/simulations/"
      url <- paste(base,job_id,"/download/input_parameters", sep="")
      accept <- "application/json"
      auth <- paste("Bearer",Sys.getenv("API_TOKEN"), sep=" ", collapse=NULL)


      apiCall <- httr::GET(url, httr::add_headers("Accept"=accept,"Authorization"=auth))
      if(apiCall$status_code == as.integer("200")){
        data <- rawToChar(apiCall$content)
        if(view==1){
          #cat("Simulation parameters for job #",job_id,":\n\n")
          cat(paste("Simulation parameters for job #",job_id," ...\n","\n\n",sep=""))
          cat(data, "\n")
        }

        if(save == 1){
          if(file_name <- readline(prompt="Save the file as (or leave blank for default name)... ") != ""){
            writeLines(data, paste(file_name,".txt", sep=""))
          } else {
            file_name <- paste("input_parameters_job_",job_id,".txt",sep="")
            writeLines(data, file_name)
          }
          cat(paste("File Saved in ",getwd()))
        }
      } else {
        message("Something went wrong... \nMake sure you have entered a valid job_ID (check ?listJobs)")
      }
    }else{
      message("jobParameters requires a valid job_ID (check ?listJobs)")
    }
    return (invisible(data))
  }else {
    message(paste("No API token set up \nNOTE: YOU NEED TO SET UP A VALID TOKEN TO START USING THE PACKAGE (check '?setToken')"))
  }
}

#'Test APIs service status
#'
#'Checks if APIs token is correctly set up. Furthermore, prints all info about the user account registered for that token.\cr
#'For more details check PHENSIM docs at \url{https://phensim.tech/docs/api}
#'
#'@return If the token is correctly set up, prints the user account info
#'
#'@export
serviceInfo <- function(){
  if(checkToken()){
    url <- "https://phensim.tech/api/v1/ping"
    accept <- "application/json"
    auth <- paste("Bearer",Sys.getenv("API_TOKEN"), sep=" ", collapse=NULL)

    apiCall <- httr::GET(url, httr::add_headers("Accept"=accept,"Authorization"=auth))

    data <- jsonlite::fromJSON(rawToChar(apiCall$content))
    json_pretty <- jsonlite::toJSON(data, pretty=TRUE)
    print(json_pretty)
  }else {
    message(paste("No API token set up \nNOTE: YOU NEED TO SET UP A VALID TOKEN TO START USING THE PACKAGE (check '?setToken')"))
  }
}
