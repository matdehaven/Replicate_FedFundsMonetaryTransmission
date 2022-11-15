##
##  Pull Data Series from FRED
##  We need Unemployment, Fed Funds Rate, Inflation Rate
##
##  Matthew DeHaven
##  2022 11 14
##
require(data.table)
require(fredr) ## Package to pull FRED data

## You can request your own key from FRED and replace it here
apiKey <- readChar("./data/FRED_API_KEY.txt", file.info("./data/FRED_API_KEY.txt")$size)
fredr::fredr_set_key(apiKey)

## Pull FRED Series
series <- c(
  u = "",
  i = "",
  ff = ""
)

