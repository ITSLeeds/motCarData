
if (!require("pacman")) install.packages("pacman")
pacman::p_load(jsonlite, curl, data.table, plyr, dplyr, lubridate, iterators, foreach, doParallel, tcltk, purrr, readr)

dl_mot_allpages <- function(maxpage, apikey){
  ncores <- detectCores()
  ncores <- ncores - 1
  if(ncores == 0) {ncores <- 1}
  cl <- parallel::makeCluster(ncores)
  registerDoParallel(cl)
  bignumber <- maxpage
  
  MOTlist <- foreach(i=1:bignumber,
                   .inorder = FALSE,
                   .errorhandling = "pass",
                   .packages = c("jsonlite", "curl", "data.table", "plyr", "dplyr", "lubridate", "iterators", "foreach", "doParallel", "tcltk"),
                   .verbose = TRUE) %dopar% {
                     URL <- as.character(paste('https://beta.check-mot.service.gov.uk/trade/vehicles/mot-tests?page=',i,sep=""))
                     h <- new_handle()
                     handle_setheaders(h,
                                       "Accept" = "application/json+v3",
                                       "x-api-key" = apikey)
                     d <- curl_fetch_memory(URL, handle = h)
                     if(d$status_code!=404){
                       page.df <- fromJSON(rawToChar(d$content))
                       page.df$LastTestDate <- NA
                       page.df$LastTestResult <- NA
                       for(j in 1:nrow(page.df)){
                         results.df <- page.df[[7]][[j]]
                         page.df$LastTestResult[j] <- results.df[1,2]
                         page.df$LastTestDate[j] <- results.df[1,1]
                       }
                       page.df$motTests <- NULL
                       page.df$pagenumber <- i
                       if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=bignumber)
                       setTkProgressBar(pb, i)
                       return(page.df)
                     }}
parallel::stopCluster(cl)

MOTlist.subset <- purrr::keep(MOTlist, ~ is.data.frame(.x))

result.df <- bind_rows(MOTlist.subset)

result.df$firstUsedDate <- ymd(result.df$firstUsedDate)
result.df$LastTestDate <- ymd_hms(result.df$LastTestDate)

return(result.df)
}

test <- dl_mot_allpages(2, apikey = apikey)
