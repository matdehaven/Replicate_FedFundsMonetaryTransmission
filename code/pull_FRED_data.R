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

saveRDS(data, "./data/uiff_data.RDS")


data |>
  ggplot(aes(
    x = date,
    y = value
  )) +
  geom_line() + 
  facet_wrap(vars(series_id), scale = "free_y")
