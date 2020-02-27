#' Download a page of MOT data
#'
#' @description
#' Download one page of MOT data
#'
#' @param page page number
#' @param key API Key
#' @param get_comments logical, should comments be downloaded, default TRUE
#' @param ... extra variaibles
#' @export

mot_get_page <- function(page, key,
                         get_comments = TRUE,
                         ...
                         ){
  
  # Request 
  req <- try(httr::GET(
    url = "https://beta.check-mot.service.gov.uk/trade/vehicles/mot-tests",
    query = list(
      page = page
    ),
    httr::add_headers(`Accept` = "application/json+v6", `x-api-key` = key)
  ))
  
  if(class(req) == "try-error"){
    message(paste0("Got try error for page ",page))
    return(NULL)
  }else{
    if(req$status_code == 200){
      # convert response content into text
      data <- httr::content(req, as = "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(data)
      #data$id <- paste0(page,"-",seq(1, nrow(data)))
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
      data_main <- mot_clean_main(data_main, ...)
      data_tests <- mot_clean_tests(data_tests, ...)
      if(get_comments){
        data_comments <- mot_clean_comments(data_comments, ...)
      } else {
        data_comments <- NA
      }
      
      # Add to list
      return(list(data_main, data_tests, data_comments))
    }else{
      message(paste0("Failed to get page ",page," error ",req$status_code))
      return(NULL)
    }
  }
}

#' Download multiple pages of MOT data
#'
#' @description
#' Download one page of MOT data
#'
#' @param pages a range o page numberspage number
#' @param key API Key
#' @param ncores Now many cores to use
#' @param max_pages Manximum number of pages to download per batch
#' @param dir where to save batch files
#' @param recombine logical should batches be combined togther
#' @param get_comments logical, should comments be downloaded, default TRUE
#' @param ... extra variaibles passed to `mot_get_page()`
#' 
#' @details
#' This function wraps `mot_get_page()` for bulk downloading of data, by default
#'    it is multicore optimised and will run with 8 cores as this is the upper 
#'    limit allowed by the API.
#'    
#' The function will create batches of pages defined by `max_pages` and save them
#'    to disk. This prevents you running out of RAM. The default 1000 pages per
#'    batch is about 600 Mb in size. However you can reduce this significantly by 
#'    setting `get_comments` to false. The MOT test comments are by far the largest
#'    part of the data, and are not needed for many use cases. Without comments
#'    1000 pages is around 300 Mb, so you could safely increase `max_pages`.
#' 
#' @export

mot_get_pages <- function(pages = 2:11, 
                         key = Sys.getenv("MOT_key"),
                         ncores = 8L,
                         max_pages = 1000,
                         dir = tempdir(),
                         recombine = FALSE,
                         get_comments = TRUE,
                         ...){
  
  checkmate::assert_integer(ncores, lower = 1, upper = 8)
  
  #Make a list of how many requests can be made per loop
  btchs <- ceiling(length(pages)/max_pages)
  splts <- rep(seq(1,btchs), each = max_pages )
  splts <- splts[seq(1,length(pages))]
  message(paste0("To get ",length(pages)," pages of data will need ",btchs," batches of ",max_pages," pages"))
  pagesSplit <- split(pages, splts)
  
  for(i_btch in seq(1, length(pagesSplit))){
    message(paste0(Sys.time()," doing batch ",i_btch))
    pagesSub <- pagesSplit[[i_btch]]
    # cl <- parallel::makeCluster(ncores)
    # res <- pbapply::pblapply(pagesSub, mot_get_page, key = key)
    #parallel::stopCluster(cl)
    # rm(cl)
    
    pb <- utils::txtProgressBar(max = length(pagesSub), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    cl <- parallel::makeCluster(ncores)
    doSNOW::registerDoSNOW(cl)
    export_funcs <- c("mot_get_page","mot_clean_main",
                      "mot_clean_tests","mot_clean_comments")
    boot <- foreach::foreach(i = pagesSub, .options.snow = opts, .export = export_funcs)
    res <- foreach::`%dopar%`(boot, mot_get_page(page = i, key = key, get_comments = get_comments))
    parallel::stopCluster(cl)
    rm(cl, boot, opts, pb, progress)
    
    message("      ")
    # Check for missing results
    lths <- lengths(res)
    if(any(lths != 3)){
      message(paste0("Trying again to get",sum(lths != 3)," pages of missing data"))
      for(j in seq(1, length(pagesSub))){
        if(lths[j] != 3){
          res[[j]] <- mot_get_page(pagesSub[j], key = key, ...)
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
    
    if(btchs > 1){
      saveRDS(main,file.path(dir,paste0("mot_tmp_main_btch_",i,".Rds")))
      saveRDS(tests,file.path(dir,paste0("mot_tmp_tests_btch_",i,".Rds")))
      if(get_comments){
        saveRDS(comments,file.path(dir,paste0("mot_tmp_comments_btch_",i,".Rds")))
        rm(comments)
      }
      rm(main,tests)
    }
    
  }
  
  if(btchs > 1){
    if(recombine){
      # Readback in batches and combine
      main <- readRDS(file.path(dir,paste0("mot_tmp_main_btch_",1,".Rds")))
      for(k in seq(2, btchs)){
        main_nxt <- readRDS(file.path(dir,paste0("mot_tmp_main_btch_",k,".Rds")))
        rbind(main, main_nxt)
      }
      rm(main_nxt)
      main$make <- as.factor(main$make)
      main$model <- as.factor(main$model)
      
      tests <- readRDS(file.path(dir,paste0("mot_tmp_tests_btch_",1,".Rds")))
      for(k in seq(2, btchs)){
        tests_nxt <- readRDS(file.path(dir,paste0("mot_tmp_tests_btch_",k,".Rds")))
        rbind(tests, tests_nxt)
      }
      rm(tests_nxt)
      tests$vehicleId <- as.factor(tests$vehicleId)
      
      if(get_comments){
        comments <- readRDS(file.path(dir,paste0("mot_tmp_comments_btch_",1,".Rds")))
        for(k in seq(2, btchs)){
          comments_nxt <- readRDS(file.path(dir,paste0("mot_tmp_comments_btch_",k,".Rds")))
          rbind(comments, comments_nxt)
        }
        rm(comments_nxt)
        comments$text <- as.factor(comments$text)
      } else {
        comments <- NA
      }
      
      return(list(main,tests,comments))
  
    } else {
      message(paste0("recombine is FALSE, the data has been saved to ",dir))
      return(NULL)
    }

  } else {
    # return the one batch
    main$make <- as.factor(main$make)
    main$model <- as.factor(main$model)
    tests$vehicleId <- as.factor(tests$vehicleId)
    if(get_comments){
      comments$text <- as.factor(comments$text)
    }
    return(list(main = main, tests = tests, comments = comments))
  }
}
