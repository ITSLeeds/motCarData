#' Download a registration of MOT data
#'
#' @description
#' Download one registration of MOT data
#'
#' @param registration registration number
#' @param key MOT api key
#' @param get_comments logical, should comments be downloaded, default TRUE
#' @param ... extra variaibles
#' @export
#' 
mot_get_reg <- function(registration, key, get_comments = TRUE, ...){
  # Request 
  req <- try(httr::GET(
    url = "https://beta.check-mot.service.gov.uk/trade/vehicles/mot-tests",
    query = list(
      registration = registration
    ),
    httr::add_headers(`Accept` = "application/json+v6", `x-api-key` = key)
  ))
  
  if(class(req) == "try-error"){
    message(paste0("Got try error for registration ",registration))
    return(NULL)
  }else{
    if(req$status_code == 200){
      # convert response content into text
      data <- httr::content(req, as = "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(data)
      if(!"vehicleId" %in% names(data)){
        # Strange Data format
        return(list(main = data, tests = NULL, comments = NULL))
      } else {
        data_main <- data[,c("vehicleId","registration","make","model",
                             "firstUsedDate","fuelType","primaryColour",
                             "registrationDate", "manufactureDate", "engineSize")]
        data_tests <- data$motTests
        names(data_tests) <- data_main$vehicleId
        data_tests <- dplyr::bind_rows(data_tests, .id = "vehicleId")
        if(get_comments){
          data_comments <- data_tests$rfrAndComments
          names(data_comments) <- data_tests$motTestNumber
          data_comments <- dplyr::bind_rows(data_comments, 
                                            .id = "motTestNumber")
        }
        data_tests$rfrAndComments <- NULL
        
        #Compress data
        #data_main <- mot_clean_main(data_main, ...)
        #data_tests <- mot_clean_tests(data_tests, ...)
        if(get_comments){
          #data_comments <- mot_clean_comments(data_comments, ...)
        } else {
          data_comments <- NA
        }
      }
      
      
      
      # Add to list
      return(list(data_main, data_tests, data_comments))
    }else{
      message(paste0("Failed to get registration ",registration," error ",req$status_code))
      return(NULL)
    }
  }
}


#' Download multiple pages of MOT data
#'
#' @description
#' Download one page of MOT data
#'
#' @param registrations a range o page numberspage number
#' @param key API Key
#' @param ncores Now many cores to use
#' @param get_comments logical, should comments be downloaded, default TRUE
#' @param ... extra variaibles passed to `mot_get_page()`
#' 
#' @details
#' This function wraps `mot_get_reg()` for bulk downloading of data, by default
#'    it is multicore optimised and will run with 8 cores as this is the upper 
#'    limit allowed by the API.
#'    
#' 
#' @export

mot_get_regs <- function(registrations, 
                          key = Sys.getenv("MOT_key"),
                          ncores = 8L,
                          get_comments = TRUE,
                          ... ){
  
  checkmate::assert_integer(ncores, lower = 1, upper = 8)
  
  pb <- utils::txtProgressBar(max = length(registrations), style = 3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  cl <- parallel::makeCluster(ncores)
  doSNOW::registerDoSNOW(cl)
  export_funcs <- c("mot_get_reg","mot_clean_main",
                    "mot_clean_tests","mot_clean_comments")
  boot <- foreach::foreach(i = registrations, .options.snow = opts, .export = export_funcs) #expoet needed in global envrion
  res <- foreach::`%dopar%`(boot, mot_get_reg(registration = i, key = key, get_comments = get_comments, ... ))
  parallel::stopCluster(cl)
  rm(cl, boot, opts, pb, progress)
  
  message("      ")
  # Check for missing results
  lths <- lengths(res)
  if(any(lths != 3)){
    message(paste0("Trying again to get",sum(lths != 3)," requests of missing data"))
    for(j in seq(1, length(registrations))){
      if(lths[j] != 3){
        res[[j]] <- mot_get_reg(registration = registrations[j], key = key, get_comments = get_comments, ... )
      }
    }
  }
    
  # Extrat and combine
  main     <- lapply(res, `[[`, 1)
  tests    <- lapply(res, `[[`, 2)
  if(get_comments){
    comments <- lapply(res, `[[`, 3)
  } else {
    comments <- NA
  }
  rm(res)
  
  main <- dplyr::bind_rows(main)
  tests <- dplyr::bind_rows(tests)
  if(get_comments){
    suppressWarnings(comments <- dplyr::bind_rows(comments))
  }
  
  main$make <- as.factor(main$make)
  main$model <- as.factor(main$model)
  tests$vehicleId <- as.factor(tests$vehicleId)
  if(get_comments){
    comments$text <- as.factor(comments$text)
  }
  return(list(main = main, tests = tests, comments = comments))
  
}