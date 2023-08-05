.onLoad <- function(...){

  packageStartupMessage("Package succesfully loaded!\n
    To start using package functions, you have to set up the API token (linked to your registered account on phensim.tech).\n
    If you already have a registered API token, you can set up it now otherwise you can do it later (check “?setToken”)
  ")

}

.onAttach <- function(...){
  setToken()
}

