mot_clean_main <- function(data_main,
                           levels_fuelType = c("CNG","Diesel","Electric",
                                               "Electric Diesel","Fuel Cells",
                                               "Gas","Gas Bi-Fuel","Gas Diesel",
                                               "Hybrid Electric (Clean)", 
                                               "LNG","LPG","Other","Petrol","Steam"),
                           levels_primaryColour = c( "Beige","Black","Blue","Bronze",
                                                     "Brown","Cream","Gold","Green",
                                                     "Grey","Maroon","Multi-colour",
                                                     "Not Stated","Orange","Pink",
                                                     "Purple","Red","Silver",
                                                     "Turquoise","White","Yellow")
                           ){
  data_main$fuelType <- factor(data_main$fuelType, 
                               levels = levels_fuelType)
  
  data_main$primaryColour <- factor(data_main$primaryColour,
                                    levels = levels_primaryColour)
  
  data_main$firstUsedDate <- lubridate::ymd(data_main$firstUsedDate)
  data_main$registrationDate <- lubridate::ymd(data_main$registrationDate)
  data_main$manufactureDate <- lubridate::ymd(data_main$manufactureDate)
  data_main$engineSize <- as.integer(data_main$engineSize)
  return(data_main)
}

mot_clean_tests <- function(data_tests,
                            levels_testResult = c("FAILED", "PASSED"),
                            levels_odometerUnit = c("km","mi"),
                            levels_odometerResultType = c("READ","UNREADABLE", "NO_ODOMETER")
                            ){
  data_tests$completedDate <- lubridate::ymd_hms(data_tests$completedDate)
  data_tests$expiryDate    <- lubridate::ymd(data_tests$expiryDate)
  data_tests$testResult    <- factor(data_tests$testResult,
                                     levels = levels_testResult)
  data_tests$odometerUnit  <- factor(data_tests$odometerUnit,
                                     levels = levels_odometerUnit)
  data_tests$odometerValue <- as.integer(data_tests$odometerValue)
  data_tests$motTestNumber <- as.numeric(data_tests$motTestNumber)
  data_tests$odometerResultType  <- factor(data_tests$odometerResultType,
                                           levels = levels_odometerResultType)
  return(data_tests)
}


mot_clean_comments <- function(data_comments,
                               levels_type = c("ADVISORY","FAIL","MINOR",
                                               "PRS","USER ENTERED")){
  data_comments$motTestNumber <- as.numeric(data_comments$motTestNumber)
  data_comments$type <- factor(data_comments$type,
                               levels = levels_type)
  data_comments$text <- stringr::str_to_sentence(data_comments$text)
  data_comments$text <- as.factor(data_comments$text)
  return(data_comments)
}