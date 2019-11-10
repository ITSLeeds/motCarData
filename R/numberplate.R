get_numberplate_type <- function(df, registration, firstUsedDate){


NumberPlateScheme <- readr::read_csv("NumberPlateScheme.csv")
for (i in 1:length(df$registration)){
  if(is.na(df$firstUsedDate)) {next()}
  first <- substr(df$registration[i], 1, 1)
  second <- substr(df$registration[i], 2, 2)
  third <- substr(df$registration[i], 3, 3)
  forth <- substr(df$registration[i], 4, 4)
  fifth <- substr(df$registration[i], 5, 5)
  sixth <- substr(df$registration[i], 6, 6)
  seventh <- substr(df$registration[i], 7, 7)
  # A	February 1963 – 31 December 1963
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1963-02-01" &
     df$firstUsedDate[i] <= "1963-12-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "A") {df$reg_general[i] <- "Suffix A Reg"}
  # B	1 January 1964 – 31 December 1964
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1964-01-01" &
     df$firstUsedDate[i] <= "1964-12-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "B") {df$reg_general[i] <- "Suffix B Reg"}
  # C	1 January 1965 – 31 December 1965
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1965-01-01" &
     df$firstUsedDate[i] <= "1965-12-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "C") {df$reg_general[i] <- "Suffix C Reg"}
  # D	1 January 1966 – 31 December 1966
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1966-01-01" &
     df$firstUsedDate[i] <= "1966-12-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "D") {df$reg_general[i] <- "Suffix D Reg"}
  # E	1 January 1967 – 31 July 1967
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1967-01-01" &
     df$firstUsedDate[i] <= "1967-07-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "E") {df$reg_general[i] <- "Suffix E Reg"}
  # F	1 August 1967 – 31 July 1968
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1967-08-01" &
     df$firstUsedDate[i] <= "1968-07-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "F") {df$reg_general[i] <- "Suffix F Reg"}
  # G	1 August 1968 – 31 July 1969
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1968-08-01" &
     df$firstUsedDate[i] <= "1969-07-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "G") {df$reg_general[i] <- "Suffix G Reg"}
  # H	1 August 1969 – 31 July 1970
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1969-08-01" &
     df$firstUsedDate[i] <= "1970-07-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "H") {df$reg_general[i] <- "Suffix H Reg"}
  # J	1 August 1970 – 31 July 1971
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1970-08-01" &
     df$firstUsedDate[i] <= "1971-07-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "J") {df$reg_general[i] <- "Suffix J Reg"}
  # K	1 August 1971 – 31 July 1972
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1971-08-01" &
     df$firstUsedDate[i] <= "1972-07-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "K") {df$reg_general[i] <- "Suffix K Reg"}
  # L	1 August 1972 – 31 July 1973
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1972-08-01" &
     df$firstUsedDate[i] <= "1973-07-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "L") {df$reg_general[i] <- "Suffix L Reg"}
  # M	1 August 1973 – 31 July 1974
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1973-08-01" &
     df$firstUsedDate[i] <= "1974-07-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "M") {df$reg_general[i] <- "Suffix M Reg"}
  # N	1 August 1974 – 31 July 1975
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1974-08-01" &
     df$firstUsedDate[i] <= "1975-07-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "N") {df$reg_general[i] <- "Suffix N Reg"}
  # P	1 August 1975 – 31 July 1976
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1975-08-01" &
     df$firstUsedDate[i] <= "1976-07-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "P") {df$reg_general[i] <- "Suffix P Reg"}
  # R	1 August 1976 – 31 July 1977
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1976-08-01" &
     df$firstUsedDate[i] <= "1977-07-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "R") {df$reg_general[i] <- "Suffix R Reg"}
  # S	1 August 1977 – 31 July 1978
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1977-08-01" &
     df$firstUsedDate[i] <= "1978-07-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "S") {df$reg_general[i] <- "Suffix S Reg"}
  # T	1 August 1978 – 31 July 1979
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1978-08-01" &
     df$firstUsedDate[i] <= "1979-07-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "T") {df$reg_general[i] <- "Suffix T Reg"}
  # V	1 August 1979 – 31 July 1980
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1979-08-01" &
     df$firstUsedDate[i] <= "1980-07-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "V") {df$reg_general[i] <- "Suffix V Reg"}
  # W	1 August 1980 – 31 July 1981
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1980-08-01" &
     df$firstUsedDate[i] <= "1981-07-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "W") {df$reg_general[i] <- "Suffix W Reg"}
  # X	1 August 1981 – 31 July 1982
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1981-08-01" &
     df$firstUsedDate[i] <= "1982-07-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "X") {df$reg_general[i] <- "Suffix X Reg"}
  # Y	1 August 1982 – 31 July 1983
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1982-08-01" &
     df$firstUsedDate[i] <= "1983-07-31" &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "Y") {df$reg_general[i] <- "Suffix Y Reg"}
  # A	1 August 1983 – 31 July 1984
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1983-08-01" &
     df$firstUsedDate[i] <= "1984-07-31" &
     first == "A" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix A Reg"}
  # B	1 August 1984 – 31 July 1985
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1984-08-01" &
     df$firstUsedDate[i] <= "1985-07-31" &
     first == "B" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix B Reg"}
  # C	1 August 1985 – 31 July 1986
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1985-08-01" &
     df$firstUsedDate[i] <= "1986-07-31" &
     first == "C" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix C Reg"}
  # D	1 August 1986 – 31 July 1987
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1986-08-01" &
     df$firstUsedDate[i] <= "1987-07-31" &
     first == "D" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix D Reg"}
  # E	1 August 1987 – 31 July 1988
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1987-08-01" &
     df$firstUsedDate[i] <= "1988-07-31" &
     first == "E" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix E Reg"}
  # F	1 August 1988 – 31 July 1989
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1988-08-01" &
     df$firstUsedDate[i] <= "1989-07-31" &
     first == "F" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix F Reg"}
  # G	1 August 1989 – 31 July 1990
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1989-08-01" &
     df$firstUsedDate[i] <= "1990-07-31" &
     first == "G" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix G Reg"}
  # H	1 August 1990 – 31 July 1991
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1990-08-01" &
     df$firstUsedDate[i] <= "1991-07-31" &
     first == "H" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix H Reg"}
  # J	1 August 1991 – 31 July 1992
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1991-08-01" &
     df$firstUsedDate[i] <= "1992-07-31" &
     first == "J" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix J Reg"}
  # K	1 August 1992 – 31 July 1993
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1992-08-01" &
     df$firstUsedDate[i] <= "1993-07-31" &
     first == "K" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix K Reg"}
  # L	1 August 1993 – 31 July 1994
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1993-08-01" &
     df$firstUsedDate[i] <= "1994-07-31" &
     first == "L" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix L Reg"}
  # M	1 August 1994 – 31 July 1995
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1994-08-01" &
     df$firstUsedDate[i] <= "1995-07-31" &
     first == "M" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix M Reg"}
  # N	1 August 1995 – 31 July 1996
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1995-08-01" &
     df$firstUsedDate[i] <= "1996-07-31" &
     first == "N" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix N Reg"}
  # P	1 August 1996 – 31 July 1997
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1996-08-01" &
     df$firstUsedDate[i] <= "1997-07-31" &
     first == "P" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix P Reg"}
  # R	1 August 1997 – 31 July 1998
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1997-08-01" &
     df$firstUsedDate[i] <= "1998-07-31" &
     first == "R" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix R Reg"}
  # S	1 August 1998 – 28 February 1999
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1998-08-01" &
     df$firstUsedDate[i] <= "1999-02-28" &
     first == "S" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix S Reg"}
  # T	1 March 1999 – 31 August 1999
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1998-03-01" &
     df$firstUsedDate[i] <= "1998-08-31" &
     first == "T" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix T Reg"}
  # V	1 September 1999 – 29 February 2000
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "1999-09-01" &
     df$firstUsedDate[i] <= "2000-02-29" &
     first == "V" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix V Reg"}
  # W	1 March 2000 – 31 August 2000
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "2000-03-01" &
     df$firstUsedDate[i] <= "2000-08-31" &
     first == "W" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix W Reg"}
  # X	1 September 2000 – 28 February 2001
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "2000-09-01" &
     df$firstUsedDate[i] <= "2001-02-28" &
     first == "X" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix X Reg"}
  # Y	1 March 2001 – 31 August 2001
  if(nchar(df$registration[i]) == 7 &
     df$firstUsedDate[i] >= "2001-03-01" &
     df$firstUsedDate[i] <= "2001-08-31" &
     first == "Y" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Prefix Y Reg"}
  # Two-digit age identifier scheme
  for(j in 1:nrow(NumberPlateScheme)){
    if(nchar(df$registration[i]) == 7 &
          df$firstUsedDate[i] >= NumberPlateScheme$Start[j] &
          df$firstUsedDate[i] <= NumberPlateScheme$End[j] &
          grepl("[a-zA-Z]", first) &
          grepl("[a-zA-Z]", second) &
          grepl("[0-9]", third) &
          grepl("[0-9]", forth) &
          grepl("[a-zA-Z]", fifth) &
          grepl("[a-zA-Z]", sixth) &
          grepl("[a-zA-Z]", seventh) &
          substr(NumberPlateScheme$`Plate Id`[j], 1, 1) == third &
          substr(NumberPlateScheme$`Plate Id`[j], 2, 2) == forth) {df$reg_general[i] <- paste0(NumberPlateScheme$`Plate Id`[j], " Plate")}
    }
  # Q Plate
  if(nchar(df$registration[i]) == 7 &
     grepl("[a-zA-Z]", first) &
     grepl("[a-zA-Z]", second) &
     grepl("[a-zA-Z]", third) &
     grepl("[0-9]", forth) &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     seventh == "Q") {df$reg_general[i] <- "Q Plate"}
  if(nchar(df$registration[i]) == 7 &
     first == "Q" &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     grepl("[0-9]", forth) &
     grepl("[a-zA-Z]", fifth) &
     grepl("[a-zA-Z]", sixth) &
     grepl("[a-zA-Z]", seventh)) {df$reg_general[i] <- "Q Plate"}
  # Diplomatic & International Organisations
  if(nchar(df$registration[i]) == 7 &
     grepl("[0-9]", first) &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     forth == "D" &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     grepl("[0-9]", seventh)) {df$reg_general[i] <- "Diplomatic"}
  if(nchar(df$registration[i]) == 7 &
     grepl("[0-9]", first) &
     grepl("[0-9]", second) &
     grepl("[0-9]", third) &
     forth == "X" &
     grepl("[0-9]", fifth) &
     grepl("[0-9]", sixth) &
     grepl("[0-9]", seventh)) {df$reg_general[i] <- "International Organisations"}
  # Create progress bar
  pb = utils::txtProgressBar(min = 0, max = nrow(df), style = 3)
  utils::setTxtProgressBar(pb, i)
}
close(pb)
}
