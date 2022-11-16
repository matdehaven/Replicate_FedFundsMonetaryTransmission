##
##  Pull Data Series from FRED
##  We need Unemployment, Fed Funds Rate, Inflation Rate
##
##  Matthew DeHaven
##  2022 11 14
##
require(data.table)
require(fredr) ## Package to pull FRED data
require(ggplot2)
require(seasonal)
require(zoo)

## You can request your own key from FRED and replace it here
apiKey <- readChar("./data/FRED_API_KEY.txt", file.info("./data/FRED_API_KEY.txt")$size)
fredr::fredr_set_key(apiKey)

## Pull FRED Series
series <- c(
  u = "LNS14000061", # UNRATE is overall, using prime age, male UR to match paper
  i = "CPIAUCSL", # Could use GDPCTPI (GDP Deflator)
  ff = "FEDFUNDS"
)
data <- rbindlist(lapply(series, fredr::fredr))[,.(date, series_id, value)] 
data <- data |> dcast(date ~ series_id)
order <- c("date", series)
data <- data[,..order]
names(data) <- c("date", names(series))
data <- data |> melt(id = "date", variable.name = "series_id")

## Seasonally Adjust
## TODO

seasonally_adjust <- function(x, dates){
  
  ts_x <- as.ts(zoo::zoo(x, zoo::as.yearmon(dates), frequency = 12, calendar = "yearmon"))
  
  ts_x_adj <- seasonal::seas(ts_x, x11 = "", na.action = na.exclude)
  
  return(seasonal::final(ts_x_adj))
}

seasonally_adjust(data[series_id=="u", value], data[series_id=="u", date])

data[, value_adj := seasonally_adjust(value, date), by = .(series_id) ]

## Plot the series to inspect
data |>
  ggplot(aes(
    x = date
  )) +
  geom_line(aes(y = value, color = "raw")) + 
  geom_line(aes(y = value_adj, color = "seasonal")) + 
  facet_wrap(vars(series_id), scale = "free_y")

## Small difference from seasonal adjustment

saveRDS(data, "./data/uiff_data.RDS")



