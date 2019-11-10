
key <- apikey
page <- 1

dl_mot_page <- function(page, key){
  
  # Request 
  req <- try(httr::GET(
    url = "https://beta.check-mot.service.gov.uk/trade/vehicles/mot-tests",
    query = list(
      page = page
    ),
    httr::add_headers(`Content-type` = "application/json", `x-api-key` = key)
  ))
  
  if(class(req) == "try-error"){
    message(paste0("Got try error for page ",page))
    return(NULL)
  }else{
    if(req$status_code == 200){
      # convert response content into text
      data <- httr::content(req, as = "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(data)
      data$id <- paste0(page,"-",seq(1, nrow(data)))
      data_main <- data[,c("id","registration","make","model","firstUsedDate","fuelType","primaryColour")]
      data_tests <- data$motTests
      names(data_tests) <- data_main$id
      data_tests <- dplyr::bind_rows(data_tests, .id = "id")
      data_comments <- data_tests$rfrAndComments
      data_tests$rfrAndComments <- NULL
      names(data_comments) <- data_tests$motTestNumber
      data_comments <- dplyr::bind_rows(data_comments, .id = "motTestNumber")
      
      #Compress data
      ## Main
      data_main$fuelType <- factor(data_main$fuelType, 
                                   levels = c("CNG","Diesel","Electric",
                                              "Electric Diesel","Fuel Cells",
                                              "Gas","Gas Bi-Fuel","Gas Diesel",
                                              "Hybrid Electric (Clean)", 
                                              "LNG","LPG","Other","Petrol","Steam"))
      
      data_main$primaryColour <- factor(data_main$primaryColour,
                                        levels = c( "Beige","Black","Blue","Bronze",
                                                    "Brown","Cream","Gold","Green",
                                                    "Grey","Maroon","Multi-colour",
                                                    "Not Stated","Orange","Pink",
                                                    "Purple","Red","Silver",
                                                    "Turquoise","White","Yellow"))
      
      data_main$firstUsedDate <- lubridate::ymd(data_main$firstUsedDate)
      
      ## Tests
      #data_tests$id            <- as.factor(data_tests$id)
      data_tests$completedDate <- lubridate::ymd_hms(data_tests$completedDate)
      data_tests$expiryDate    <- lubridate::ymd(data_tests$expiryDate)
      data_tests$testResult    <- factor(data_tests$testResult,
                                         levels = c("FAILED", "PASSED"))
      data_tests$odometerUnit  <- factor(data_tests$odometerUnit,
                                         levels = c("km","mi"))
      data_tests$odometerValue <- as.integer(data_tests$odometerValue)
      data_tests$motTestNumber <- as.numeric(data_tests$motTestNumber)
      
      ## Comments
      data_comments$motTestNumber <- as.numeric(data_comments$motTestNumber)
      data_comments$type <- factor(data_comments$type,
                                   levels = c("ADVISORY","FAIL","MINOR",
                                              "PRS","USER ENTERED"))
      data_comments$text <- stringr::str_to_sentence(data_comments$text)
      data_comments$text <- as.factor(data_comments$text)
      
      
      # Add to list
      return(list(data_main, data_tests, data_comments))
    }else{
      message(paste0("Failed to get page ",page," error ",req$status_code))
      return(NULL)
    }
  }
}


dl_mot_pages <- function(pages = 2:11, 
                         key = Sys.getenv("MOT_key"),
                         ncores = 1,
                         max_pages = 1000,
                         dir = tempdir(),
                         recombine = FALSE){
  
  
  #Make a list of how many requests can be made per loop
  btchs <- ceiling(length(pages)/max_pages)
  splts <- rep(seq(1,btchs), each = max_pages )
  splts <- splts[seq(1,length(pages))]
  message(paste0("To get ",length(pages)," pages of data will need ",btchs," batches of ",max_pages," pages"))
  pagesSplit <- split(pages, splts)
  
  for(i in seq(1, length(pagesSplit))){
    message(paste0(Sys.time()," doing batch ",i))
    pagesSub <- pagesSplit[[i]]
    cl <- parallel::makeCluster(ncores)
    res <- pbapply::pblapply(pagesSub, dl_mot_page, key = key)
    parallel::stopCluster(cl)
    rm(cl)
    
    # Check for missing results
    lths <- lengths(res)
    if(any(lths != 3)){
      message(paste0("Trying again to get",sum(lths != 3)," pages of missing data"))
      for(j in seq(1, length(pagesSub))){
        if(lths[j] != 3){
          res[[j]] <- dl_mot_page(pagesSub[j], key = key)
        }
      }
    }
    
    # Extrat and combine
    main     <- lapply(res, `[[`, 1)
    tests    <- lapply(res, `[[`, 2)
    comments <- lapply(res, `[[`, 3)
    rm(res)
    
    main <- dplyr::bind_rows(main)
    tests <- dplyr::bind_rows(tests)
    suppressWarnings(comments <- dplyr::bind_rows(comments))

    
    if(btchs > 1){
      saveRDS(main,file.path(dir,paste0("mot_tmp_main_btch_",i,".Rds")))
      saveRDS(tests,file.path(dir,paste0("mot_tmp_tests_btch_",i,".Rds")))
      saveRDS(comments,file.path(dir,paste0("mot_tmp_comments_btch_",i,".Rds")))
      rm(main,tests,comments)
    }
    
  }
  
  if(btchs > 1){
    if(recombine){
      # Readback in batches and combine
      main <- readRDS(file.path(dir,paste0("mot_tmp_main_btch_",1,".Rds")))
      for(i in seq(2, btchs)){
        main_nxt <- readRDS(file.path(dir,paste0("mot_tmp_main_btch_",i,".Rds")))
        rbind(main, main_nxt)
      }
      rm(main_nxt)
      main$make <- as.factor(main$make)
      main$model <- as.factor(main$model)
      
      tests <- readRDS(file.path(dir,paste0("mot_tmp_tests_btch_",1,".Rds")))
      for(i in seq(2, btchs)){
        tests_nxt <- readRDS(file.path(dir,paste0("mot_tmp_tests_btch_",i,".Rds")))
        rbind(tests, tests_nxt)
      }
      rm(tests_nxt)
      tests$id <- as.factor(tests$id)
      
      comments <- readRDS(file.path(dir,paste0("mot_tmp_comments_btch_",1,".Rds")))
      for(i in seq(2, btchs)){
        comments_nxt <- readRDS(file.path(dir,paste0("mot_tmp_comments_btch_",i,".Rds")))
        rbind(comments, comments_nxt)
      }
      rm(comments_nxt)
      comments$text <- as.factor(comments$text)

      return(list(main,tests,comments))
      
    } else {
      message(paste0("recombine is FALSE, the data has been saved to ",dir))
      return(NULL)
    }
    
    
  } else {
    # return the one batch
    main$make <- as.factor(main$make)
    main$model <- as.factor(main$model)
    
    tests$id <- as.factor(tests$id)
    
    comments$text <- as.factor(comments$text)
    
    
    return(list(main,tests,comments))
  }
  
  
  
  
}
